{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module PandocToMarkdown
    ( pandocToMarkdown
    , NotSafe(..)
    , Rectangle(..)
    , rectanglerize
    , combineRectangles
    , buildRow
    , widthOf
    , heightOf
    , tableToMarkdown
    )
where

import Control.DeepSeq (NFData)
import Core.Text
import Core.System
import Data.Foldable (foldl')
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.List (intersperse)
import GHC.Generics (Generic)
import Text.Pandoc (Pandoc(..), Block(..), Inline(..), Attr, Format(..)
    , ListAttributes, Alignment(..), TableCell)
import Text.Pandoc.Shared (orderedListMarkers)

__WIDTH__ :: Int
__WIDTH__ = 78

pandocToMarkdown :: Pandoc -> Rope
pandocToMarkdown (Pandoc _ blocks) =
    blocksToMarkdown blocks

blocksToMarkdown :: [Block] -> Rope
blocksToMarkdown blocks =
    foldl' (\text block -> text <> (convertBlock __WIDTH__ block) <> "\n") emptyRope blocks

convertBlock :: Int -> Block -> Rope
convertBlock margin block =
  let
    msg = "Unfinished block: " ++ show block -- FIXME
  in case block of
    Plain inlines -> paragraphToMarkdown margin inlines
    Para  inlines -> paragraphToMarkdown margin inlines
    Header level _ inlines -> headingToMarkdown level inlines
    Null -> emptyRope
    RawBlock (Format "tex") string -> intoRope string <> "\n"
    RawBlock _ _ -> error msg
    CodeBlock attr string -> codeToMarkdown attr string
    LineBlock list -> poemToMarkdown list
    BlockQuote blocks -> quoteToMarkdown margin blocks
    BulletList blockss -> bulletlistToMarkdown margin blockss
    OrderedList attrs blockss -> orderedlistToMarkdown margin attrs blockss
    HorizontalRule -> "---\n"
    Table caption alignments relatives headers rows -> tableToMarkdown caption alignments relatives headers rows
    _ -> error msg

plaintextToMarkdown :: Int -> [Inline] -> Rope
plaintextToMarkdown margin inlines =
    wrap margin (inlinesToMarkdown inlines)


paragraphToMarkdown :: Int -> [Inline] -> Rope
paragraphToMarkdown margin inlines =
    wrap margin (inlinesToMarkdown inlines) <> "\n"

headingToMarkdown :: Int -> [Inline] -> Rope
headingToMarkdown level inlines =
  let
    text = inlinesToMarkdown inlines
  in
    case level of
        1 -> text <> "\n" <> underline '=' text <> "\n"
        2 -> text <> "\n" <> underline '-' text <> "\n"
        n -> intoRope (replicate n '#') <> " " <> text <> "\n"

codeToMarkdown :: Attr -> String -> Rope
codeToMarkdown (_,tags,_) literal =
  let
    body = intoRope literal
    lang = case tags of
        []      -> ""
        [tag]   -> intoRope tag
        _       -> impureThrow (NotSafe "A code block can't have mulitple langage tags")
  in
    "```" <> lang <> "\n" <>
    body <> "\n" <>
    "```" <> "\n"

poemToMarkdown :: [[Inline]] -> Rope
poemToMarkdown list =
    mconcat (intersperse "\n" (fmap prefix list)) <> "\n"
  where
    prefix inlines = "| " <> inlinesToMarkdown inlines

quoteToMarkdown :: Int -> [Block] -> Rope
quoteToMarkdown margin blocks =
    foldl' (\text block -> text <> prefix block) emptyRope blocks
  where
    prefix :: Block -> Rope
    prefix = foldl' (\text line -> text <> "> " <> line <> "\n") emptyRope . rows

    rows :: Block -> [Rope]
    rows = breakLines . convertBlock (margin - 2)

bulletlistToMarkdown :: Int -> [[Block]] -> Rope
bulletlistToMarkdown = listToMarkdown (repeat "-   ")

orderedlistToMarkdown :: Int -> ListAttributes -> [[Block]] -> Rope
orderedlistToMarkdown margin (num,style,delim) blockss =
    listToMarkdown (intoMarkers (num,style,delim)) margin blockss
  where
    intoMarkers = fmap pad . fmap intoRope . orderedListMarkers
    pad text = text <> if width text > 2 then " " else "  "

listToMarkdown :: [Rope] -> Int -> [[Block]] -> Rope
listToMarkdown markers margin items =
  case pairs of
    [] -> emptyRope
    ((marker1,blocks1):pairsN) -> listitem marker1 blocks1 <> foldl'
        (\text (markerN,blocksN) -> text <> spacer blocksN <> listitem markerN blocksN) emptyRope pairsN
  where
    pairs = zip markers items

    listitem :: Rope -> [Block] -> Rope
    listitem _ [] = emptyRope
    listitem marker (block1:blocks) = indent marker True block1 <> foldl'
        (\ text blockN -> text <> indent marker False blockN) emptyRope blocks

    spacer :: [Block] -> Rope
    spacer [] = emptyRope
    spacer (block:_) = case block of
        Plain _ -> emptyRope
        Para _  -> "\n"
        _       -> "\n"

    indent :: Rope -> Bool -> Block -> Rope
    indent marker first =
        snd . foldl' (f marker) (first,emptyRope) . breakLines . convertBlock (margin - 4)

    f :: Rope -> (Bool,Rope) -> Rope -> (Bool,Rope)
    f marker (first,text) line = if first
        then (False,text <> marker <> line <> "\n")
        else (False,text <> "    " <> line <> "\n")


tableToMarkdown
    :: [Inline]
    -> [Alignment]
    -> [Double]
    -> [TableCell]
    -> [[TableCell]]
    -> Rope
tableToMarkdown _ alignments relatives headers rows =
    mconcat (intersperse "\n"
        [ wrapperLine
        , header
        , underlineHeaders
        , body
        , wrapperLine
        ]) <> "\n"
  where
    header = rowToMarkdown headers

    bodylines = fmap rowToMarkdown rows

    body = mconcat (intersperse "\n\n" bodylines)

    sizes :: [Int]
    sizes =
      let
        total = fromIntegral __WIDTH__

        -- there's a weird thing where sometimes (in pipe tables?) the
        -- value of relative is 0. If that happens, pick a value.
        -- TODO Better heuristic? Because, this will break if cell too wide.
        f x | x == 0.0  = 14
            | otherwise = floor (total * x)
      in
        fmap (fromInteger . f) relatives

    overall = sum sizes + (length headers) - 1
    wrapperLine = intoRope (replicate overall '-')

    rowToMarkdown :: [TableCell] -> Rope
    rowToMarkdown = buildRow sizes . fmap convert . zipWith3
        (\size align (block:_) -> (size,align,block)) sizes alignments

    underlineHeaders :: Rope
    underlineHeaders =
        foldl' (<>) emptyRope . intersperse " "
        . fmap (\size -> intoRope (replicate size '-'))
        . take (length headers) $ sizes

    convert :: (Int,Alignment,Block) -> Rectangle
    convert (size,align,Plain inlines) =
        rectanglerize size align (plaintextToMarkdown size inlines)
    convert (_,_,_) =
        impureThrow (NotSafe "Incorrect Block type encountered")



data NotSafe = NotSafe String
    deriving Show

instance Exception NotSafe

data Rectangle = Rectangle Int Int [Rope]
    deriving (Eq, Show, Generic, NFData)

widthOf :: Rectangle -> Int
widthOf (Rectangle size _ _) = size

heightOf :: Rectangle -> Int
heightOf (Rectangle _ height _) = height

rowsFrom :: Rectangle -> [Rope]
rowsFrom (Rectangle _ _ texts) = texts

instance Semigroup Rectangle where
    (<>) = combineRectangles

instance Monoid Rectangle where
    mempty = Rectangle 0 0 []

rectanglerize :: Int -> Alignment -> Rope -> Rectangle
rectanglerize size align text =
  let
    ls = breakLines (wrap size text)

    fix l | width l <  size =
              let
                padding = size - width l
                (left,remain) = divMod padding 2
                right = left + remain
              in case align of
                AlignCenter  -> intoRope (replicate left ' ') <> l <> intoRope (replicate right ' ')
                AlignRight   -> intoRope (replicate padding ' ') <> l
                AlignLeft    -> l <> intoRope (replicate padding ' ')
                AlignDefault -> l <> intoRope (replicate padding ' ')
          | width l == size = case align of
                AlignRight -> impureThrow (NotSafe "Column width insufficient to show alignment")
                _ -> l
          | otherwise       = impureThrow (NotSafe "Line wider than permitted size")

    result = foldr (\l acc -> fix l:acc) [] ls
  in
    Rectangle size (length result) result

combineRectangles :: Rectangle -> Rectangle -> Rectangle
combineRectangles rect1@(Rectangle size1 height1 _) rect2@(Rectangle size2 height2 _) =
  let
    target = max height1 height2
    extra1 = target - height1
    extra2 = target - height2

    padRows :: Int -> Rectangle -> [Rope]
    padRows count (Rectangle size _ texts) =
      let
        texts' = texts ++ replicate count (intoRope (replicate size ' '))
      in
        texts'

    texts1' = padRows extra1 rect1
    texts2' = padRows extra2 rect2

    pairs = zip texts1' texts2'

    result = foldr (\ (text1,text2) texts -> text1 <> text2 : texts) [] pairs
  in
    Rectangle (size1 + size2) target  result

ensureWidth :: Int -> Rectangle -> Rectangle
ensureWidth request rect =
    if widthOf rect < request
        then rectanglerize request AlignLeft (foldl' (<>) emptyRope (rowsFrom rect))
        else rect


buildRow :: [Int] -> [Rectangle] -> Rope
buildRow cellWidths rects =
  let
    pairs = zip cellWidths rects
    rects' = fmap (\ (desired,rect) -> ensureWidth desired rect) pairs
    wall = vertical ' ' rects'
    result = foldl' (<>) mempty . intersperse wall $ rects'
  in
    foldl' (<>) emptyRope (intersperse "\n" (rowsFrom result))

vertical :: Char -> [Rectangle] -> Rectangle
vertical ch rects =
  let
    height = maximum (fmap heightOf rects)
    border = replicate height (intoRope [ch])
  in
    Rectangle 1 height border

---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

inlinesToMarkdown :: [Inline] -> Rope
inlinesToMarkdown inlines =
    foldl' (\text inline -> append (convertInline inline) text) emptyRope inlines

convertInline :: Inline -> Rope
convertInline inline =
  let
    msg = "Unfinished inline: " ++ show inline
  in case inline of
    Space -> " "
    Str string -> intoRope string
    Emph inlines -> "_" <> inlinesToMarkdown inlines <> "_"
    Strong inlines -> "**" <> inlinesToMarkdown inlines <> "**"
    SoftBreak -> " "
    LineBreak -> "-~<{BR}>~-" -- FIXME
    Image _ inlines (url, _) -> imageToMarkdown inlines url
    Code _ string -> "`" <> intoRope string <> "`"
    RawInline (Format "tex") string -> intoRope string
    Link ("",["uri"],[]) _ (url, _) -> uriToMarkdown url
    Link _ inlines (url, _) -> linkToMarkdown inlines url
    _ -> error msg

imageToMarkdown :: [Inline] -> String -> Rope
imageToMarkdown inlines url =
  let
    text = inlinesToMarkdown inlines
    target = intoRope url
  in
    "![" <> text <> "](" <> target <> ")"
    
uriToMarkdown :: String -> Rope
uriToMarkdown url =
  let
    target = intoRope url
  in
    "<" <> target <> ">"

linkToMarkdown :: [Inline] -> String -> Rope
linkToMarkdown inlines url =
  let
    text = inlinesToMarkdown inlines
    target = intoRope url
  in
    "[" <> text <> "](" <> target <> ")"

