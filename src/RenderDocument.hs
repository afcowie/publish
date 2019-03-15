{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RenderDocument
    ( program
    )
where

import Control.Monad (filterM, forM_, forever, void)
import Core.Program
import Core.System
import Core.Text
import Data.Char (isSpace)
import qualified Data.List as List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (doesFileExist, doesDirectoryExist
    , copyFileWithMetadata)
import System.Exit (ExitCode(..))
import System.FilePath.Posix (takeBaseName, takeExtension
    , replaceExtension, splitFileName, replaceDirectory)
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import System.IO.Error (userError, IOError)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Temp (mkdtemp)
import System.Posix.User (getEffectiveUserID, getEffectiveGroupID)
import Text.Pandoc (runIOorExplode, readMarkdown, writeLaTeX, def
    , readerExtensions, readerColumns, pandocExtensions
    , writerTopLevelDivision, TopLevelDivision(TopLevelChapter))

import Environment (Env(..))
import NotifyChanges (waitForChange)
import LatexPreamble (preamble, ending)
import OutputParser (parseOutputForError)
import Utilities (ensureDirectory, execProcess, ifNewer, isNewer)

data Mode = Once | Cycle

program :: Program Env ()
program = do
    params <- getCommandLine
    mode <- extractMode params

    event "Identify .book file"
    bookfile <- extractBookFile params

    case mode of
        Once -> do
            -- normal operation, single pass
            void (renderDocument mode bookfile)
        Cycle -> do
            -- use inotify to rebuild on changes
            forever (renderDocument mode bookfile >>= waitForChange)


renderDocument :: Mode -> FilePath -> Program Env [FilePath]
renderDocument mode bookfile = do
    event "Read .book file"
    files <- processBookFile bookfile

    event "Setup temporary directory"
    setupTargetFile bookfile

    event "Convert document fragments to LaTeX"
    mapM_ processFragment files

    event "Write intermediate LaTeX file"
    produceResult

    event "Render document to PDF"
    catch
        (do
            renderPDF
            copyHere
        )
        (\(e :: ExitCode) -> case mode of
            Once -> throw e
            Cycle -> return ()
        )

    return (bookfile:files)


extractMode :: Parameters -> Program Env Mode
extractMode params =
  let
    mode = case lookupOptionFlag "watch" params of
        Just False  -> error "Invalid State"
        Just True   -> Cycle
        Nothing     -> Once
  in
    return mode

{-
For the situation where the .book file is in a location other than '.'
then chdir there first, so any relative paths within _it_ are handled
properly, as are inotify watches later if they are employed.
-}
extractBookFile :: Parameters -> Program Env FilePath
extractBookFile params =
  let
    (relative,bookfile) = case lookupArgument "bookfile" params of
        Nothing -> error "invalid"
        Just file -> splitFileName file
  in do
    debugS "relative" relative
    debugS "bookfile" bookfile
    probe <- liftIO $ do
        changeWorkingDirectory relative
        doesFileExist bookfile
    case probe of
        True  -> return bookfile
        False -> do
            write ("error: specified .book file \"" <> intoRope bookfile <> "\" not found.")
            throw (userError "no such file")


setupTargetFile :: FilePath -> Program Env ()
setupTargetFile book = do
    env <- getApplicationState
    let start = startingDirectoryFrom env
    let dotfile = start ++ "/.target"

    params <- getCommandLine
    tmpdir <- case lookupOptionValue "temp" params of
            Just dir -> do
                -- Append a slash so that /tmp/booga is taken as a directory.
                -- Otherwise, you end up ensuring /tmp exists.
                ensureDirectory (dir ++ "/")
                return dir
            Nothing -> liftIO $ catch
                (do
                    dir' <- readFile dotfile
                    let dir = trim dir'
                    probe <- doesDirectoryExist dir
                    if probe
                        then return dir
                        else throw boom
                )
                (\ (_ :: IOError) -> do
                    dir <- mkdtemp "/tmp/publish-"
                    writeFile dotfile (dir ++ "\n")
                    return dir
                )
    debugS "tmpdir" tmpdir

    let master = tmpdir ++ "/" ++ base ++ ".tex"
        result = tmpdir ++ "/" ++ base ++ ".pdf"

    first <- case lookupOptionFlag "builtin-preamble" params of
        Nothing     -> return []
        Just True   -> do
            let name = "00_Beginning.latex"
            let target = tmpdir ++ "/" ++ name
            liftIO $ withFile target WriteMode $ \handle -> do
                hWrite handle preamble
            return [name]
        Just _      -> invalid

    let env' = env
            { intermediateFilenamesFrom = first
            , masterFilenameFrom = master
            , resultFilenameFrom = result
            , tempDirectoryFrom = tmpdir
            }
    setApplicationState env'
  where
    base = takeBaseName book -- "/directory/file.ext" -> "file"

    boom = userError "Temp dir no longer present"

    trim :: String -> String
    trim = List.dropWhileEnd isSpace


processBookFile :: FilePath -> Program Env [FilePath]
processBookFile file = do
    contents <- liftIO (T.readFile file)
    list <- filterM skipNotFound (possibilities contents)
    debugS "fragments" (length list)
    return list
  where
    -- filter out blank lines and lines commented out
    possibilities :: Text -> [FilePath]
    possibilities = map T.unpack . filter (not . T.null)
        . filter (not . T.isPrefixOf "#") . T.lines

    skipNotFound :: FilePath -> Program t Bool
    skipNotFound fragment = do
        probe <- liftIO (doesFileExist fragment)
        case probe of
            True  -> return True
            False -> do
                write ("warning: Fragment \"" <> intoRope fragment <> "\" not found, skipping")
                return False

{-
Which kind of file is it? Dispatch to the appropriate reader switching on
filename extension.
-}
processFragment :: FilePath -> Program Env ()
processFragment file = do
    debugS "source" file

    -- Read the fragment, process it if Markdown then run it out to LaTeX.
    case takeExtension file of
        ".markdown" -> convertMarkdown file
        ".md"       -> convertMarkdown file
        ".latex"    -> passthroughLaTeX file
        ".tex"      -> passthroughLaTeX file
        ".svg"      -> convertImage file
        _           -> passthroughImage file

{-
Convert Markdown to LaTeX. This is where we "call" Pandoc.

Default behaviour from the command line is to activate all (?) of Pandoc's
Markdown extensions, but invoking via the `readMarkdown` function with
default ReaderOptions doesn't turn any on. Using `pandocExtensions` here
appears to represent the whole set.

When output format is LaTeX, the command-line _pandoc_ tool does some
somewhat convoluted heuristics to decide whether top-level headings (ie
<H1>, ====, #) are to be considered \part, \chapter, or \section.  The fact
that is not deterministic is annoying. Force the issue.

Finally, for some reason, the Markdown -> LaTeX pair strips trailing
whitespace from the block, resulting in a no paragraph boundary between
files. So gratuitously add a break.
-}
convertMarkdown :: FilePath -> Program Env ()
convertMarkdown file =
  let
    readingOptions = def
        { readerExtensions = pandocExtensions
        , readerColumns = 75
        }

    writingOptions = def
        { writerTopLevelDivision = TopLevelChapter
        }

  in do
    env <- getApplicationState
    let tmpdir = tempDirectoryFrom env
        file' = replaceExtension file ".latex"
        target = tmpdir ++ "/" ++ file'
        files = intermediateFilenamesFrom env

    ensureDirectory target
    ifNewer file target $ do
        debugS "target" target
        liftIO $ do
            contents <- T.readFile file

            latex <- runIOorExplode $ do
                parsed <- readMarkdown readingOptions contents
                writeLaTeX writingOptions parsed

            withFile target WriteMode $ \handle -> do
                T.hPutStrLn handle latex

    let env' = env { intermediateFilenamesFrom = file':files }
    setApplicationState env'

{-
If a source fragment is already LaTeX, simply copy it through to
the target file.
-}
passthroughLaTeX :: FilePath -> Program Env ()
passthroughLaTeX file = do
    env <- getApplicationState
    let tmpdir = tempDirectoryFrom env
        target = tmpdir ++ "/" ++ file
        files = intermediateFilenamesFrom env

    ensureDirectory target
    ifNewer file target $ do
        debugS "target" target
        liftIO $ do
            copyFileWithMetadata file target

    let env' = env { intermediateFilenamesFrom = file:files }
    setApplicationState env'

{-
Images in SVG format need to be converted to PDF to be able to be
included in the output as LaTeX doesn't understand SVG natively, which
is slightly shocking.
-}
convertImage :: FilePath -> Program Env ()
convertImage file = do
    env <- getApplicationState
    let tmpdir = tempDirectoryFrom env
        target = tmpdir ++ "/" ++ replaceExtension file ".pdf"

        rsvgConvert =
            [ "rsvg-convert"
            , "--format=pdf"
            , "--output=" ++ target
            , file
            ]

    ifNewer file target $ do
        debugS "target" target
        (exit,out,err) <- do
            ensureDirectory target
            execProcess rsvgConvert

        case exit of
            ExitFailure _ ->  do
                event "Image processing failed"
                debug "stderr" (intoRope err)
                debug "stdout" (intoRope out)
                throw exit
            ExitSuccess -> return ()

passthroughImage :: FilePath -> Program Env ()
passthroughImage file = do
    env <- getApplicationState
    let tmpdir = tempDirectoryFrom env
        target = tmpdir ++ "/" ++ file

    ensureDirectory target
    ifNewer file target $ do
        debugS "target" target
        liftIO $ do
            copyFileWithMetadata file target

{-
Finish up by writing the intermediate "master" file.
-}
produceResult :: Program Env ()
produceResult = do
    env <- getApplicationState
    let tmpdir = tempDirectoryFrom env
        master = masterFilenameFrom env
        files = intermediateFilenamesFrom env

    params <- getCommandLine
    files' <- case lookupOptionFlag "builtin-preamble" params of
        Nothing     -> return files
        Just True   -> do
            let name = "ZZ_Ending.latex"
            let target = tmpdir ++ "/" ++ name
            liftIO $ withFile target WriteMode $ \handle -> do
                hWrite handle ending
            return (name:files)
        Just _      -> invalid

    debugS "master" master
    liftIO $ withFile master WriteMode $ \handle -> do
        hPutStrLn handle ("\\RequirePackage{import}")
        forM_ (reverse files') $ \file -> do
            let (path,name) = splitFileName file
            hPutStrLn handle ("\\subimport{" ++ path ++ "}{" ++ name ++ "}")

getUserID :: Program a String
getUserID = liftIO $ do
    uid <- getEffectiveUserID
    gid <- getEffectiveGroupID
    return (show uid ++ ":" ++ show gid)

renderPDF :: Program Env ()
renderPDF = do
    env <- getApplicationState

    let master = masterFilenameFrom env
        tmpdir = tempDirectoryFrom env

    user <- getUserID

    params <- getCommandLine
    let command = case lookupOptionValue "docker" params of
            Just image  ->
                [ "docker"
                , "run"
                , "--rm=true"
                , "--volume=" ++ tmpdir ++ ":" ++ tmpdir
                , "--user=" ++ user
                , image
                , "latexmk"
                ]
            Nothing ->
                [ "latexmk"
                ]

        options =
                [ "-xelatex"
                , "-output-directory=" ++ tmpdir
                , "-interaction=nonstopmode"
                , "-halt-on-error"
                , "-file-line-error"
                , "-cd"
                , master
                ]

        latexmk = command ++ options

    (exit,out,err) <- execProcess latexmk
    case exit of
        ExitFailure _ -> do
            event "Render failed"
            debug "stderr" (intoRope err)
            debug "stdout" (intoRope out)
            write (parseOutputForError tmpdir out)
            throw exit
        ExitSuccess -> return ()

copyHere :: Program Env ()
copyHere = do
    env <- getApplicationState
    let result = resultFilenameFrom env
        start = startingDirectoryFrom env
        final = replaceDirectory result start       -- ie ./Book.pdf

    changed <- isNewer result final
    case changed of
        True -> do
            event "Copy resultant PDF to starting directory"
            debugS "result" result
            debugS "final" final
            liftIO $ do
                copyFileWithMetadata result final
            event "Complete"
        False -> do
            event "Result unchanged"
