{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Core.Program
import Core.Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import RenderDocument (program)
import Environment (initial)

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    setLocaleEncoding utf8
    env <- initial
    context <- configure version env (simple
        [ Option "builtin-preamble" (Just 'p') Empty [quote|
            Wrap a built-in LaTeX preamble (and ending) around your
            supplied source fragments. Most documents will put their own
            custom preamble as the first fragment in the .book file, but
            for getting started a suitable default can be employed via this
            option.
          |]
        , Option "watch" Nothing Empty [quote|
            Watch all sources listed in the bookfile and re-run the
            rendering engine if changes are detected.
          |]
        , Option "temp" Nothing (Value "TMPDIR") [quote|
            The working location for assembling converted fragments and
            caching intermediate results between runs. By default, a
            temporary directory will be created in /tmp.
          |]
        , Option "docker" Nothing (Value "IMAGE") [quote|
            Run the specified Docker image in a container, mount the target
            directory into it as a volume, and do the build there. This allows
            you to have all of the LaTeX dependencies separate from the machine
            you are editing on.
          |]
        , Argument "bookfile" [quote|
            The file containing the list of fragments making up this book.
            If the argument is specified as "Hobbit.book" then "Hobbit"
            will be used as the basename for the intermediate .latex file
            and the final output .pdf file. The list file should contain
            filenames, one per line, of the fragments you wish to render
            into a complete document.
          |]
        ])

    executeWith context program
