{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2014 Martin Fenner <mf@martinfenner.org>

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
   Module      : Text.Pandoc.Writers.JATS
   Copyright   : Copyright (C) 2014 Martin Fenner
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Fenner <mf@martinfenner.org>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to JATS XML.
-}
module Text.Pandoc.Writers.JATS ( writeJATS) where
import Text.Pandoc.Definition
import Text.Pandoc.XML
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Readers.TeXMath
import Data.List ( isPrefixOf, intercalate, isSuffixOf )
import Data.Char ( toLower )
import Data.Monoid ( Any(..) )
import Text.Pandoc.Highlighting ( languages, languagesByExtension )
import Text.Pandoc.Pretty
import qualified Text.Pandoc.Builder as B
import Text.TeXMath
import qualified Text.XML.Light as Xml
import Data.Generics (everywhere, mkT)

-- | Convert list of authors to a JATS <author> section
authorToJATS :: WriterOptions -> [Inline] -> B.Inlines
authorToJATS opts name' =
  let name = render Nothing $ inlinesToJATS opts name'
      colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  in  B.rawInline "JATS" $ render colwidth $
      if ',' `elem` name
         then -- last name first
              let (lastname, rest) = break (==',') name
                  firstname = triml rest in
              inTagsSimple "firstname" (text $ escapeStringForXML firstname) <>
              inTagsSimple "surname" (text $ escapeStringForXML lastname)
         else -- last name last
              let namewords = words name
                  lengthname = length namewords
                  (firstname, lastname) = case lengthname of
                    0  -> ("","")
                    1  -> ("", name)
                    n  -> (intercalate " " (take (n-1) namewords), last namewords)
               in inTagsSimple "firstname" (text $ escapeStringForXML firstname) $$
                  inTagsSimple "surname" (text $ escapeStringForXML lastname)

-- | Convert Pandoc document to string in JATS format.
writeJATS :: WriterOptions -> Pandoc -> String
writeJATS opts (Pandoc meta blocks) =
  let elements = hierarchicalize blocks
      colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
      render' = render colwidth
      opts' = if "/book>" `isSuffixOf`
                      (trimr $ writerTemplate opts)
                 then opts{ writerChapters = True }
                 else opts
      startLvl = if writerChapters opts' then 0 else 1
      auths'   = map (authorToJATS opts) $ docAuthors meta
      meta'    = B.setMeta "author" auths' meta
      Just metadata = metaToJSON opts
                 (Just . render colwidth . (vcat .
                          (map (elementToJATS opts' startLvl)) . hierarchicalize))
                 (Just . render colwidth . inlinesToJATS opts')
                 meta'
      main     = render' $ vcat (map (elementToJATS opts' startLvl) elements)
      context = defField "body" main
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                        MathML _ -> True
                                        _        -> False)
              $ metadata
  in  if writerStandalone opts
         then renderTemplate' (writerTemplate opts) context
         else main

-- | Convert an Element to JATS.
elementToJATS :: WriterOptions -> Int -> Element -> Doc
elementToJATS opts _   (Blk block) = blockToJATS opts block
elementToJATS opts lvl (Sec _ _num (id',_,_) title elements) =
  -- JATS doesn't allow sections with no content, so insert some if needed
  let elements' = if null elements
                    then [Blk (Para [])]
                    else elements
      tag = case lvl of
                 n | n == 0           -> "chapter"
                   | n >= 1 && n <= 5 -> "sect" ++ show n
                   | otherwise        -> "simplesect"
  in  inTags True tag [("id", writerIdentifierPrefix opts ++ id')] $
      inTagsSimple "title" (inlinesToJATS opts title) $$
      vcat (map (elementToJATS opts (lvl + 1)) elements')

-- | Convert a list of Pandoc blocks to JATS.
blocksToJATS :: WriterOptions -> [Block] -> Doc
blocksToJATS opts = vcat . map (blockToJATS opts)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a list of
-- JATS varlistentrys.
deflistItemsToJATS :: WriterOptions -> [([Inline],[[Block]])] -> Doc
deflistItemsToJATS opts items =
  vcat $ map (\(term, defs) -> deflistItemToJATS opts term defs) items

-- | Convert a term and a list of blocks into a JATS varlistentry.
deflistItemToJATS :: WriterOptions -> [Inline] -> [[Block]] -> Doc
deflistItemToJATS opts term defs =
  let def' = concatMap (map plainToPara) defs
  in  inTagsIndented "varlistentry" $
      inTagsIndented "term" (inlinesToJATS opts term) $$
      inTagsIndented "listitem" (blocksToJATS opts def')

-- | Convert a list of lists of blocks to a list of JATS list items.
listItemsToJATS :: WriterOptions -> [[Block]] -> Doc
listItemsToJATS opts items = vcat $ map (listItemToJATS opts) items

-- | Convert a list of blocks into a JATS list item.
listItemToJATS :: WriterOptions -> [Block] -> Doc
listItemToJATS opts item =
  inTagsIndented "listitem" $ blocksToJATS opts $ map plainToPara item

-- | Convert a Pandoc block element to JATS.
blockToJATS :: WriterOptions -> Block -> Doc
blockToJATS _ Null = empty
blockToJATS opts (Div _ bs) = blocksToJATS opts $ map plainToPara bs
blockToJATS _ (Header _ _ _) = empty -- should not occur after hierarchicalize
blockToJATS opts (Plain lst) = inlinesToJATS opts lst
-- title beginning with fig: indicates that the image is a figure
blockToJATS opts (Para [Image txt (src,'f':'i':'g':':':_)]) =
  let alt  = inlinesToJATS opts txt
      capt = if null txt
                then empty
                else inTagsSimple "title" alt
  in  inTagsIndented "figure" $
        capt $$
        (inTagsIndented "mediaobject" $
           (inTagsIndented "imageobject"
             (selfClosingTag "imagedata" [("fileref",src)])) $$
           inTagsSimple "textobject" (inTagsSimple "phrase" alt))
blockToJATS opts (Para lst)
  | hasLineBreaks lst = flush $ nowrap $ inTagsSimple "literallayout" $ inlinesToJATS opts lst
  | otherwise         = inTagsIndented "para" $ inlinesToJATS opts lst
blockToJATS opts (BlockQuote blocks) =
  inTagsIndented "blockquote" $ blocksToJATS opts blocks
blockToJATS _ (CodeBlock (_,classes,_) str) =
  text ("<programlisting" ++ lang ++ ">") <> cr <>
     flush (text (escapeStringForXML str) <> cr <> text "</programlisting>")
    where lang  = if null langs
                     then ""
                     else " language=\"" ++ escapeStringForXML (head langs) ++
                          "\""
          isLang l    = map toLower l `elem` map (map toLower) languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . map toLower $ s
          langs       = concatMap langsFrom classes
blockToJATS opts (BulletList lst) =
  let attribs = [("spacing", "compact") | isTightList lst]
  in  inTags True "itemizedlist" attribs $ listItemsToJATS opts lst
blockToJATS _ (OrderedList _ []) = empty
blockToJATS opts (OrderedList (start, numstyle, _) (first:rest)) =
  let numeration = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("numeration", "arabic")]
                       Example      -> [("numeration", "arabic")]
                       UpperAlpha   -> [("numeration", "upperalpha")]
                       LowerAlpha   -> [("numeration", "loweralpha")]
                       UpperRoman   -> [("numeration", "upperroman")]
                       LowerRoman   -> [("numeration", "lowerroman")]
      spacing    = [("spacing", "compact") | isTightList (first:rest)]
      attribs    = numeration ++ spacing
      items      = if start == 1
                      then listItemsToJATS opts (first:rest)
                      else (inTags True "listitem" [("override",show start)]
                           (blocksToJATS opts $ map plainToPara first)) $$
                           listItemsToJATS opts rest
  in  inTags True "orderedlist" attribs items
blockToJATS opts (DefinitionList lst) =
  let attribs = [("spacing", "compact") | isTightList $ concatMap snd lst]
  in  inTags True "variablelist" attribs $ deflistItemsToJATS opts lst
blockToJATS _ (RawBlock f str)
  | f == "jats" = text str -- raw XML block
  | f == "html"    = text str -- allow html for backwards compatibility
  | otherwise      = empty
blockToJATS _ HorizontalRule = empty -- not semantic
blockToJATS opts (Table caption aligns widths headers rows) =
  let captionDoc   = if null caption
                        then empty
                        else inTagsIndented "title"
                              (inlinesToJATS opts caption)
      tableType    = if isEmpty captionDoc then "informaltable" else "table"
      percent w    = show (truncate (100*w) :: Integer) ++ "*"
      coltags = vcat $ zipWith (\w al -> selfClosingTag "colspec"
                       ([("colwidth", percent w) | w > 0] ++
                        [("align", alignmentToString al)])) widths aligns
      head' = if all null headers
                 then empty
                 else inTagsIndented "thead" $
                         tableRowToJATS opts headers
      body' = inTagsIndented "tbody" $
              vcat $ map (tableRowToJATS opts) rows
  in  inTagsIndented tableType $ captionDoc $$
        (inTags True "tgroup" [("cols", show (length headers))] $
         coltags $$ head' $$ body')

hasLineBreaks :: [Inline] -> Bool
hasLineBreaks = getAny . query isLineBreak . walk removeNote
  where
    removeNote :: Inline -> Inline
    removeNote (Note _) = Str ""
    removeNote x = x
    isLineBreak :: Inline -> Any
    isLineBreak LineBreak = Any True
    isLineBreak _ = Any False

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

tableRowToJATS :: WriterOptions
                  -> [[Block]]
                  -> Doc
tableRowToJATS opts cols =
  inTagsIndented "row" $ vcat $ map (tableItemToJATS opts) cols

tableItemToJATS :: WriterOptions
                   -> [Block]
                   -> Doc
tableItemToJATS opts item =
  inTags True "entry" [] $ vcat $ map (blockToJATS opts) item

-- | Convert a list of inline elements to JATS.
inlinesToJATS :: WriterOptions -> [Inline] -> Doc
inlinesToJATS opts lst = hcat $ map (inlineToJATS opts) lst

-- | Convert an inline element to JATS.
inlineToJATS :: WriterOptions -> Inline -> Doc
inlineToJATS _ (Str str) = text $ escapeStringForXML str
inlineToJATS opts (Emph lst) =
  inTagsSimple "emphasis" $ inlinesToÏ opts lst
inlineToJATS opts (Strong lst) =
  inTags False "emphasis" [("role", "strong")] $ inlinesToJATS opts lst
inlineToJATS opts (Strikeout lst) =
  inTags False "emphasis" [("role", "strikethrough")] $
  inlinesToJATS opts lst
inlineToJATS opts (Superscript lst) =
  inTagsSimple "superscript" $ inlinesToJATS opts lst
inlineToJATS opts (Subscript lst) =
  inTagsSimple "subscript" $ inlinesToJATS opts lst
inlineToJATS opts (SmallCaps lst) =
  inTags False "emphasis" [("role", "smallcaps")] $
  inlinesToJATS opts lst
inlineToJATS opts (Quoted _ lst) =
  inTagsSimple "quote" $ inlinesToJATS opts lst
inlineToJATS opts (Cite _ lst) =
  inlinesToJATS opts lst
inlineToJATS opts (Span _ ils) =
  inlinesToJATS opts ils
inlineToJATS _ (Code _ str) =
  inTagsSimple "literal" $ text (escapeStringForXML str)
inlineToJATS opts (Math t str)
  | isMathML (writerHTMLMathMethod opts) =
    case texMathToMathML dt str of
      Right r -> inTagsSimple tagtype
                 $ text $ Xml.ppcElement conf
                 $ fixNS
                 $ removeAttr r
      Left  _ -> inlinesToJATS opts
                 $ readTeXMath' t str
  | otherwise = inlinesToJATS opts $ readTeXMath' t str
     where (dt, tagtype) = case t of
                            InlineMath  -> (DisplayInline,"inlineequation")
                            DisplayMath -> (DisplayBlock,"informalequation")
           conf = Xml.useShortEmptyTags (const False) Xml.defaultConfigPP
           removeAttr e = e{ Xml.elAttribs = [] }
           fixNS' qname = qname{ Xml.qPrefix = Just "mml" }
           fixNS = everywhere (mkT fixNS')
inlineToJATS _ (RawInline f x) | f == "html" || f == "JATS" = text x
                                  | otherwise                     = empty
inlineToJATS _ LineBreak = text "\n"
inlineToJATS _ Space = space
inlineToJATS opts (Link txt (src, _)) =
  if isPrefixOf "mailto:" src
     then let src' = drop 7 src
              emailLink = inTagsSimple "email" $ text $
                          escapeStringForXML $ src'
          in  case txt of
               [Str s] | escapeURI s == src' -> emailLink
               _             -> inlinesToJATS opts txt <+>
                                  char '(' <> emailLink <> char ')'
     else (if isPrefixOf "#" src
              then inTags False "link" [("linkend", drop 1 src)]
              else inTags False "ulink" [("url", src)]) $
          inlinesToJATS opts txt
inlineToJATS _ (Image _ (src, tit)) =
  let titleDoc = if null tit
                   then empty
                   else inTagsIndented "objectinfo" $
                        inTagsIndented "title" (text $ escapeStringForXML tit)
  in  inTagsIndented "inlinemediaobject" $ inTagsIndented "imageobject" $
      titleDoc $$ selfClosingTag "imagedata" [("fileref", src)]
inlineToJATS opts (Note contents) =
  inTagsIndented "footnote" $ blocksToJATS opts contents

isMathML :: HTMLMathMethod -> Bool
isMathML (MathML _) = True
isMathML _          = False
