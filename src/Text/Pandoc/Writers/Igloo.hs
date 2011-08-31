{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Igloo 
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to markdown-formatted plain text.

Igloo:  <http://daringfireball.net/projects/markdown/>
-}
module Text.Pandoc.Writers.Igloo (writeIgloo) where
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing hiding (blankline)
import Text.ParserCombinators.Parsec ( runParser, GenParser )
import Data.List ( group, isPrefixOf, find, intersperse, transpose )
import Text.Pandoc.Pretty
import Control.Monad.State

type Notes = [[Block]]
type Refs = [([Inline], Target)]
data WriterState = WriterState { stNotes :: Notes
                               , stRefs :: Refs
                               , stPlain :: Bool }

-- | Convert Pandoc to Igloo.
writeIgloo :: WriterOptions -> Pandoc -> String
writeIgloo opts document = 
  evalState (pandocToIgloo opts document) WriterState{ stNotes = []
                                                        , stRefs  = []
                                                        , stPlain = False }

-- | Return markdown representation of document.
pandocToIgloo :: WriterOptions -> Pandoc -> State WriterState String
pandocToIgloo opts (Pandoc (Meta title authors date) blocks) = do
  title' <- inlineListToIgloo opts title
  authors' <- mapM (inlineListToIgloo opts) authors
  date' <- inlineListToIgloo opts date
  let titleblock = not $ null title && null authors && null date
  let headerBlocks = filter isHeaderBlock blocks
  let toc = if writerTableOfContents opts 
               then tableOfContents opts headerBlocks
               else empty
  body <- blockListToIgloo opts blocks
  st <- get
  notes' <- notesToIgloo opts (reverse $ stNotes st)
  st' <- get  -- note that the notes may contain refs
  refs' <- refsToIgloo opts (reverse $ stRefs st')
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  let main = render colwidth $ body <>
               (if isEmpty notes' then empty else blankline <> notes') <>
               (if isEmpty refs' then empty else blankline <> refs')
  let context  = writerVariables opts ++
                 [ ("toc", render colwidth toc)
                 , ("body", main)
                 , ("title", render colwidth title')
                 , ("date", render colwidth date')
                 ] ++
                 [ ("titleblock", "yes") | titleblock ] ++
                 [ ("author", render colwidth a) | a <- authors' ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

-- | Return markdown representation of reference key table.
refsToIgloo :: WriterOptions -> Refs -> State WriterState Doc
refsToIgloo opts refs = mapM (keyToIgloo opts) refs >>= return . vcat

-- | Return markdown representation of a reference key. 
keyToIgloo :: WriterOptions 
              -> ([Inline], (String, String)) 
              -> State WriterState Doc
keyToIgloo opts (label, (src, tit)) = do
  label' <- inlineListToIgloo opts label
  let tit' = if null tit
                then empty
                else space <> "\"" <> text tit <> "\""
  return $ nest 2 $ hang 2
            ("[" <> label' <> "]:" <> space) (text src <> tit')

-- | Return markdown representation of notes.
notesToIgloo :: WriterOptions -> [[Block]] -> State WriterState Doc
notesToIgloo opts notes = 
  mapM (\(num, note) -> noteToIgloo opts num note) (zip [1..] notes) >>=
  return . vsep

-- | Return markdown representation of a note.
noteToIgloo :: WriterOptions -> Int -> [Block] -> State WriterState Doc
noteToIgloo opts num blocks = do
  contents  <- blockListToIgloo opts blocks
  let num' = text $ show num
  let marker = text "[^" <> num' <> text "]:"
  let markerSize = 4 + offset num'
  let spacer = case writerTabStop opts - markerSize of
                     n | n > 0  -> text $ replicate n ' '
                     _          -> text " "
  return $ hang (writerTabStop opts) (marker <> spacer) contents

-- | Escape special characters for Igloo.
escapeString :: String -> String
escapeString = escapeStringUsing markdownEscapes
  where markdownEscapes = backslashEscapes "\\`*_>#~^"

-- | Construct table of contents from list of header blocks.
tableOfContents :: WriterOptions -> [Block] -> Doc 
tableOfContents opts headers =
  let opts' = opts { writerIgnoreNotes = True }
      contents = BulletList $ map elementToListItem $ hierarchicalize headers
  in  evalState (blockToIgloo opts' contents) WriterState{ stNotes = []
                                                            , stRefs  = []
                                                            , stPlain = False }

-- | Converts an Element to a list item for a table of contents,
elementToListItem :: Element -> [Block]
elementToListItem (Blk _) = []
elementToListItem (Sec _ _ _ headerText subsecs) = [Plain headerText] ++ 
  if null subsecs
     then []
     else [BulletList $ map elementToListItem subsecs]

attrsToIgloo :: Attr -> Doc
attrsToIgloo attribs = braces $ hsep [attribId, attribClasses, attribKeys]
        where attribId = case attribs of
                                ([],_,_) -> empty
                                (i,_,_)  -> "#" <> text i
              attribClasses = case attribs of
                                (_,[],_) -> empty
                                (_,cs,_) -> hsep $
                                            map (text . ('.':))
                                            cs
              attribKeys = case attribs of
                                (_,_,[]) -> empty
                                (_,_,ks) -> hsep $
                                            map (\(k,v) -> text k
                                              <> "=\"" <> text v <> "\"") ks

-- | Ordered list start parser for use in Para below.
olMarker :: GenParser Char ParserState Char
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period && 
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then spaceChar >> spaceChar
                          else spaceChar

-- | True if string begins with an ordered list marker
beginsWithOrderedListMarker :: String -> Bool
beginsWithOrderedListMarker str =
  case runParser olMarker defaultParserState "para start" (take 10 str) of
         Left  _  -> False
         Right _  -> True

-- | Convert Pandoc block element to markdown.
blockToIgloo :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc 
blockToIgloo _ Null = return empty
blockToIgloo opts (Plain inlines) = do
  contents <- inlineListToIgloo opts inlines
  return $ contents <> cr
blockToIgloo opts (Para inlines) = do
  contents <- inlineListToIgloo opts inlines
  -- escape if para starts with ordered list marker
  st <- get
  let esc = if (not (writerStrictMarkdown opts)) &&
               not (stPlain st) &&
               beginsWithOrderedListMarker (render Nothing contents)
               then text "\\"
               else empty
  return $ esc <> contents <> blankline
blockToIgloo _ (RawBlock f str)
  | f == "html" || f == "latex" || f == "tex" || f == "markdown" = do
    st <- get
    if stPlain st
       then return empty
       else return $ text str <> text "\n"
blockToIgloo _ (RawBlock _ _) = return empty
blockToIgloo _ HorizontalRule =
  return $ blankline <> text "* * * * *" <> blankline
blockToIgloo opts (Header level inlines) = do
  contents <- inlineListToIgloo opts inlines
  st <- get
  -- use setext style headers if in literate haskell mode.
  -- ghc interprets '#' characters in column 1 as line number specifiers.
  if writerLiterateHaskell opts || stPlain st
     then let len = offset contents
          in  return $ text (replicate len '=') <> contents <> cr
     else return $
       text ((replicate level '#') ++ " ") <> contents <> blankline
blockToIgloo opts (CodeBlock (_,classes,_) str)
  | "haskell" `elem` classes && "literate" `elem` classes &&
    writerLiterateHaskell opts =
  return $ prefixed "> " (text str) <> blankline
blockToIgloo opts (CodeBlock attribs str) = return $
  if writerStrictMarkdown opts || attribs == nullAttr
     then nest (writerTabStop opts) (text str) <> blankline
     else -- use delimited code block
          flush (tildes <> space <> attrs <> cr <> text str <>
                  cr <> tildes) <> blankline
            where tildes  = text "~~~~"
                  attrs = attrsToIgloo attribs
blockToIgloo opts (BlockQuote blocks) = do
  st <- get
  -- if we're writing literate haskell, put a space before the bird tracks
  -- so they won't be interpreted as lhs...
  let leader = if writerLiterateHaskell opts
                  then " > "
                  else if stPlain st
                          then "  "
                          else "> "
  contents <- blockListToIgloo opts blocks
  return $ (prefixed leader contents) <> blankline
blockToIgloo opts (Table caption aligns widths headers rows) =  do
  caption' <- inlineListToIgloo opts caption
  let caption'' = if null caption
                     then empty
                     else blankline <> ": " <> caption' <> blankline
  headers' <- mapM (blockListToIgloo opts) headers
  let alignHeader alignment = case alignment of
                                AlignLeft    -> lblock
                                AlignCenter  -> cblock
                                AlignRight   -> rblock
                                AlignDefault -> lblock
  rawRows <- mapM (mapM (blockListToIgloo opts)) rows
  let isSimple = all (==0) widths
  let numChars = maximum . map offset
  let widthsInChars =
       if isSimple
          then map ((+2) . numChars) $ transpose (headers' : rawRows)
          else map (floor . (fromIntegral (writerColumns opts) *)) widths
  let makeRow = hcat . intersperse (lblock 1 (text " ")) .
                   (zipWith3 alignHeader aligns widthsInChars)
  let rows' = map makeRow rawRows
  let head' = makeRow headers'
  let maxRowHeight = maximum $ map height (head':rows')
  let underline = cat $ intersperse (text " ") $
                  map (\width -> text (replicate width '-')) widthsInChars
  let border = if maxRowHeight > 1
                  then text (replicate (sum widthsInChars +
                          length widthsInChars - 1) '-')
                  else if all null headers
                          then underline
                          else empty
  let head'' = if all null headers
                  then empty
                  else border <> cr <> head'
  let body = if maxRowHeight > 1
                then vsep rows'
                else vcat rows'
  let bottom = if all null headers
                  then underline
                  else border
  return $ nest 2 $ head'' $$ underline $$ body $$
              bottom $$ blankline $$ caption'' $$ blankline
blockToIgloo opts (BulletList items) = do
  contents <- mapM (bulletListItemToIgloo opts) items
  return $ cat contents <> blankline
blockToIgloo opts (OrderedList attribs items) = do
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- mapM (\(item, num) -> orderedListItemToIgloo opts item num) $
              zip markers' items
  return $ cat contents <> blankline
blockToIgloo opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToIgloo opts) items
  return $ cat contents <> blankline

-- | Convert bullet list item (list of blocks) to markdown.
bulletListItemToIgloo :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToIgloo opts items = do
  contents <- blockListToIgloo opts items
  let sps = replicate (writerTabStop opts - 2) ' '
  let start = text ('-' : ' ' : sps)
  return $ hang (writerTabStop opts) start $ contents <> cr

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToIgloo :: WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToIgloo opts marker items = do
  contents <- blockListToIgloo opts items
  let sps = case length marker - writerTabStop opts of
                   n | n > 0 -> text $ replicate n ' '
                   _         -> text " "
  let start = text marker <> sps
  return $ hang (writerTabStop opts) start $ contents <> cr

-- | Convert definition list item (label, list of blocks) to markdown.
definitionListItemToIgloo :: WriterOptions
                             -> ([Inline],[[Block]]) 
                             -> State WriterState Doc
definitionListItemToIgloo opts (label, defs) = do
  labelText <- inlineListToIgloo opts label
  let tabStop = writerTabStop opts
  st <- get
  let leader  = if stPlain st then "   " else "  ~"
  let sps = case writerTabStop opts - 3 of
                 n | n > 0   -> text $ replicate n ' '
                 _           -> text " "
  defs' <- mapM (mapM (blockToIgloo opts)) defs
  let contents = vcat $ map (\d -> hang tabStop (leader <> sps) $ vcat d <> cr) defs'
  return $ labelText <> cr <> contents <> cr

-- | Convert list of Pandoc block elements to markdown.
blockListToIgloo :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc 
blockListToIgloo opts blocks =
  mapM (blockToIgloo opts) (fixBlocks blocks) >>= return . cat
    -- insert comment between list and indented code block, or the
    -- code block will be treated as a list continuation paragraph
    where fixBlocks (b : CodeBlock attr x : rest)
            | (writerStrictMarkdown opts || attr == nullAttr) && isListBlock b =
               b : RawBlock "html" "<!-- -->\n" : CodeBlock attr x :
                  fixBlocks rest
          fixBlocks (x : xs)             = x : fixBlocks xs
          fixBlocks []                   = []
          isListBlock (BulletList _)     = True
          isListBlock (OrderedList _ _)  = True
          isListBlock (DefinitionList _) = True
          isListBlock _                  = False

-- | Get reference for target; if none exists, create unique one and return.
--   Prefer label if possible; otherwise, generate a unique key.
getReference :: [Inline] -> Target -> State WriterState [Inline]
getReference label (src, tit) = do
  st <- get
  case find ((== (src, tit)) . snd) (stRefs st) of
    Just (ref, _) -> return ref
    Nothing       -> do
      let label' = case find ((== label) . fst) (stRefs st) of
                      Just _ -> -- label is used; generate numerical label
                                 case find (\n -> not (any (== [Str (show n)])
                                           (map fst (stRefs st)))) [1..(10000 :: Integer)] of
                                      Just x  -> [Str (show x)]
                                      Nothing -> error "no unique label"
                      Nothing -> label
      modify (\s -> s{ stRefs = (label', (src,tit)) : stRefs st })
      return label'

-- | Convert list of Pandoc inline elements to markdown.
inlineListToIgloo :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToIgloo opts lst =
  mapM (inlineToIgloo opts) lst >>= return . cat

escapeSpaces :: Inline -> Inline
escapeSpaces (Str s) = Str $ substitute " " "\\ " s
escapeSpaces Space = Str "\\ "
escapeSpaces x = x

-- | Convert Pandoc inline element to markdown.
inlineToIgloo :: WriterOptions -> Inline -> State WriterState Doc
inlineToIgloo opts (Emph lst) = do 
  contents <- inlineListToIgloo opts lst
  return $ "/" <> contents <> "/"
inlineToIgloo opts (Strong lst) = do
  contents <- inlineListToIgloo opts lst
  return $ "*" <> contents <> "*"
inlineToIgloo opts (Strikeout lst) = do
  contents <- inlineListToIgloo opts lst
  return $ "~~" <> contents <> "~~"
inlineToIgloo opts (Superscript lst) = do
  let lst' = bottomUp escapeSpaces lst
  contents <- inlineListToIgloo opts lst'
  return $ "^" <> contents <> "^"
inlineToIgloo opts (Subscript lst) = do
  let lst' = bottomUp escapeSpaces lst
  contents <- inlineListToIgloo opts lst'
  return $ "~" <> contents <> "~"
inlineToIgloo opts (SmallCaps lst) = inlineListToIgloo opts lst
inlineToIgloo opts (Quoted SingleQuote lst) = do
  contents <- inlineListToIgloo opts lst
  return $ "‘" <> contents <> "’"
inlineToIgloo opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToIgloo opts lst
  return $ "“" <> contents <> "”"
inlineToIgloo _ EmDash = return "\8212"
inlineToIgloo _ EnDash = return "\8211"
inlineToIgloo _ Apostrophe = return "\8217"
inlineToIgloo _ Ellipses = return "\8230"
inlineToIgloo opts (Code attr str) =
  let tickGroups = filter (\s -> '`' `elem` s) $ group str 
      longest    = if null tickGroups
                     then 0
                     else maximum $ map length tickGroups 
      marker     = replicate (longest + 1) '`' 
      spacer     = if (longest == 0) then "" else " "
      attrs      = if writerStrictMarkdown opts || attr == nullAttr
                      then empty
                      else attrsToIgloo attr
  in  return $ text (marker ++ spacer ++ str ++ spacer ++ marker) <> attrs
inlineToIgloo _ (Str str) = do
  st <- get
  if stPlain st
     then return $ text str
     else return $ text $ escapeString str
inlineToIgloo _ (Math InlineMath str) =
  return $ "$" <> text str <> "$"
inlineToIgloo _ (Math DisplayMath str) =
  return $ "$$" <> text str <> "$$"
inlineToIgloo _ (RawInline f str)
  | f == "html" || f == "latex" || f == "tex" || f == "markdown" =
    return $ text str
inlineToIgloo _ (RawInline _ _) = return empty
inlineToIgloo opts (LineBreak) = return $
  if writerStrictMarkdown opts
     then "  " <> cr
     else "\\" <> cr
inlineToIgloo _ Space = return space
inlineToIgloo opts (Cite (c:cs) lst)
  | writerCiteMethod opts == Citeproc = inlineListToIgloo opts lst
  | citationMode c == AuthorInText = do
    suffs <- inlineListToIgloo opts $ citationSuffix c
    rest <- mapM convertOne cs
    let inbr = suffs <+> joincits rest
        br   = if isEmpty inbr then empty else char '[' <> inbr <> char ']'
    return $ text ("@" ++ citationId c) <+> br
  | otherwise = do
    cits <- mapM convertOne (c:cs)
    return $ text "[" <> joincits cits <> text "]"
  where
        joincits = hcat . intersperse (text "; ") . filter (not . isEmpty)
        convertOne Citation { citationId      = k
                            , citationPrefix  = pinlines
                            , citationSuffix  = sinlines
                            , citationMode    = m }
                               = do
           pdoc <- inlineListToIgloo opts pinlines
           sdoc <- inlineListToIgloo opts sinlines
           let k' = text (modekey m ++ "@" ++ k)
               r = case sinlines of
                        Str (y:_):_ | y `elem` ",;]@" -> k' <> sdoc
                        _                             -> k' <+> sdoc
           return $ pdoc <+> r
        modekey SuppressAuthor = "-"
        modekey _              = ""
inlineToIgloo _ (Cite _ _) = return $ text ""
inlineToIgloo opts (Link txt (src', tit)) = do
  linktext <- inlineListToIgloo opts txt
  let linktitle = if null tit
                     then empty
                     else text $ " \"" ++ tit ++ "\""
  let src = unescapeURI src'
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  let useRefLinks = writerReferenceLinks opts
  let useAuto = case (tit,txt) of
                      ("", [Code _ s]) | s == srcSuffix -> True
                      _                                 -> False
  ref <- if useRefLinks then getReference txt (src, tit) else return []
  reftext <- inlineListToIgloo opts ref
  return $ "[" <> text src <> linktext <> "]"
inlineToIgloo opts (Image alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) || 
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToIgloo opts (Link txt (source, tit))
  return $ "!" <> linkPart
inlineToIgloo _ (Note contents) = do 
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = show $ (length $ stNotes st)
  return $ "[^" <> text ref <> "]"
