{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module HTML.Render.String
  ( renderHTML
  ) where

import Data.Bool qualified as B
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Shrubbery qualified

import HTML.Attributes.Internal (Attribute(..))
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Render.Internal.Escape qualified as Escape
import HTML.Types qualified as Types

renderHTML :: ChildHTML parent grandparent -> String
renderHTML = renderTag

renderTag :: ChildHTML parent grandparent -> String
renderTag html =
  case html of
    Tag_NoElement ->
      ""

    Tag_Comment content ->
      "<!-- " <> T.unpack content <> " -->"

    Tag_Text content ->
      T.unpack $ Escape.html content

    Tag_RawHTML content ->
      T.unpack content

    Tag_CustomHTML elemName attrs eiCloserOrContent ->
      buildTag (T.unpack elemName) (Map.elems attrs) eiCloserOrContent

    Tag_Anchor attrs content ->
      buildTag "a" (Map.elems attrs) $ Right content

    Tag_Abbreviation attrs content ->
      buildTag "abbr" (Map.elems attrs) $ Right content

    Tag_ContactAddress attrs content ->
      buildTag "address" (Map.elems attrs) $ Right content

    Tag_Area attrs ->
      buildTag "area" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Article attrs content ->
      buildTag "article" (Map.elems attrs) $ Right content

    Tag_Aside attrs content ->
      buildTag "aside" (Map.elems attrs) $ Right content

    Tag_Audio attrs content ->
      buildTag "audio" (Map.elems attrs) $ Right content

    Tag_BringAttentionTo attrs content ->
      buildTag "b" (Map.elems attrs) $ Right content

    Tag_Base attrs ->
      buildTag "base" (Map.elems attrs) $ Left Types.OmitTag

    Tag_BidirectionalIsolation attrs content ->
      buildTag "bdi" (Map.elems attrs) $ Right content

    Tag_BidirectionalOverride attrs content ->
      buildTag "bdo" (Map.elems attrs) $ Right content

    Tag_Blockquote attrs content ->
      buildTag "blockquote" (Map.elems attrs) $ Right content

    Tag_Body attrs content ->
      buildTag "body" (Map.elems attrs) $ Right content

    Tag_LineBreak attrs ->
      buildTag "br" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Button attrs content ->
      buildTag "button" (Map.elems attrs) $ Right content

    Tag_Canvas attrs content ->
      buildTag "canbvas" (Map.elems attrs) $ Right content

    Tag_TableCaption attrs content ->
      buildTag "caption" (Map.elems attrs) $ Right content

    Tag_Citation attrs content ->
      buildTag "cite" (Map.elems attrs) $ Right content

    Tag_Code attrs content ->
      buildTag "code" (Map.elems attrs) $ Right content

    Tag_TableColumn attrs ->
      buildTag "col" (Map.elems attrs) $ Left Types.OmitTag

    Tag_TableColumnGroup attrs content ->
      buildTag "colgroup" (Map.elems attrs) $ Right content

    Tag_Data attrs content ->
      buildTag "data" (Map.elems attrs) $ Right content

    Tag_DataList attrs content ->
      buildTag "datalist" (Map.elems attrs) $ Right content

    Tag_DescriptionDetails attrs content ->
      buildTag "dd" (Map.elems attrs) $ Right content

    Tag_DeletedText attrs content ->
      buildTag "del" (Map.elems attrs) $ Right content

    Tag_Details attrs content ->
      buildTag "details" (Map.elems attrs) $ Right content

    Tag_Definition attrs content ->
      buildTag "dfn" (Map.elems attrs) $ Right content

    Tag_Dialog attrs content ->
      buildTag "dialog" (Map.elems attrs) $ Right content

    Tag_Division attrs content ->
      buildTag "div" (Map.elems attrs) $ Right content

    Tag_DescriptionList attrs content ->
      buildTag "dl" (Map.elems attrs) $ Right content

    Tag_DescriptionTerm attrs content ->
      buildTag "dt" (Map.elems attrs) $ Right content

    Tag_Emphasis attrs content ->
      buildTag "em" (Map.elems attrs) $ Right content

    Tag_Embed attrs ->
      buildTag "embed" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Fieldset attrs content ->
      buildTag "fieldset" (Map.elems attrs) $ Right content

    Tag_FigureCaption attrs content ->
      buildTag "figcaption" (Map.elems attrs) $ Right content

    Tag_Figure attrs content ->
      buildTag "figure" (Map.elems attrs) $ Right content

    Tag_Footer attrs content ->
      buildTag "footer" (Map.elems attrs) $ Right content

    Tag_Form attrs content ->
      buildTag "form" (Map.elems attrs) $ Right content

    Tag_H1 attrs content ->
      buildTag "h1" (Map.elems attrs) $ Right content

    Tag_H2 attrs content ->
      buildTag "h2" (Map.elems attrs) $ Right content

    Tag_H3 attrs content ->
      buildTag "h3" (Map.elems attrs) $ Right content

    Tag_H4 attrs content ->
      buildTag "h4" (Map.elems attrs) $ Right content

    Tag_H5 attrs content ->
      buildTag "h5" (Map.elems attrs) $ Right content

    Tag_H6 attrs content ->
      buildTag "h6" (Map.elems attrs) $ Right content

    Tag_Head attrs content ->
      buildTag "head" (Map.elems attrs) $ Right content

    Tag_Header attrs content ->
      buildTag "header" (Map.elems attrs) $ Right content

    Tag_HeadingGroup attrs content ->
      buildTag "hgroup" (Map.elems attrs) $ Right content

    Tag_HorizontalRule attrs ->
      buildTag "hr" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Html attrs content ->
      ("<!DOCTYPE html>" :: String)
        <> buildTag "html" (Map.elems attrs) (Right content)

    Tag_IdiomaticText attrs content ->
      buildTag "i" (Map.elems attrs) $ Right content

    Tag_IFrame attrs ->
      buildTag "iframe" (Map.elems attrs) $ Left Types.WithTag

    Tag_Image attrs ->
      buildTag "img" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Input attrs ->
      buildTag "input" (Map.elems attrs) $ Left Types.OmitTag

    Tag_InsertedText attrs content ->
      buildTag "ins" (Map.elems attrs) $ Right content

    Tag_KeyboardInput attrs content ->
      buildTag "kbd" (Map.elems attrs) $ Right content

    Tag_Label attrs content ->
      buildTag "label" (Map.elems attrs) $ Right content

    Tag_Legend attrs content ->
      buildTag "legend" (Map.elems attrs) $ Right content

    Tag_ListItem attrs content ->
      buildTag "li" (Map.elems attrs) $ Right content

    Tag_Link attrs ->
      buildTag "link" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Main attrs content ->
      buildTag "main" (Map.elems attrs) $ Right content

    Tag_Map attrs content ->
      buildTag "map" (Map.elems attrs) $ Right content

    Tag_Mark attrs content ->
      buildTag "mark" (Map.elems attrs) $ Right content

    Tag_Menu attrs content ->
      buildTag "menu" (Map.elems attrs) $ Right content

    Tag_Meta attrs ->
      buildTag "meta" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Meter attrs content ->
      buildTag "meter" (Map.elems attrs) $ Right content

    Tag_Nav attrs content ->
      buildTag "nav" (Map.elems attrs) $ Right content

    Tag_NoScript attrs content ->
      buildTag "noscript" (Map.elems attrs) $ Right content

    Tag_Object attrs content ->
      buildTag "object" (Map.elems attrs) $ Right content

    Tag_OrderedList attrs content ->
      buildTag "ol" (Map.elems attrs) $ Right content

    Tag_OptionGroup attrs content ->
      buildTag "optgroup" (Map.elems attrs) $ Right content

    Tag_Option attrs content ->
      buildTag "option" (Map.elems attrs) $ Right content

    Tag_Output attrs content ->
      buildTag "output" (Map.elems attrs) $ Right content

    Tag_Paragraph attrs content ->
      buildTag "p" (Map.elems attrs) $ Right content

    Tag_Picture attrs content ->
      buildTag "picture" (Map.elems attrs) $ Right content

    Tag_PreformattedText attrs content ->
      buildTag "pre" (Map.elems attrs) $ Right content

    Tag_Progress attrs content ->
      buildTag "progress" (Map.elems attrs) $ Right content

    Tag_Quotation attrs content ->
      buildTag "q" (Map.elems attrs) $ Right content

    Tag_RubyParenthesis attrs content ->
      buildTag "rp" (Map.elems attrs) $ Right content

    Tag_RubyText attrs content ->
      buildTag "rt" (Map.elems attrs) $ Right content

    Tag_Ruby attrs content ->
      buildTag "ruby" (Map.elems attrs) $ Right content

    Tag_Strikethrough attrs content ->
      buildTag "s" (Map.elems attrs) $ Right content

    Tag_Sample attrs content ->
      buildTag "sample" (Map.elems attrs) $ Right content

    Tag_Script attrs script ->
      buildTag "script" (Map.elems attrs)
        . Right
        . L.singleton
        $ Tag_RawHTML script

    Tag_Search attrs content ->
      buildTag "search" (Map.elems attrs) $ Right content

    Tag_Section attrs content ->
      buildTag "section" (Map.elems attrs) $ Right content

    Tag_Select attrs content ->
      buildTag "select" (Map.elems attrs) $ Right content

    Tag_Slot attrs content ->
      buildTag "slot" (Map.elems attrs) $ Right content

    Tag_SideComment attrs content ->
      buildTag "small" (Map.elems attrs) $ Right content

    Tag_Source attrs ->
      buildTag "source" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Span attrs content ->
      buildTag "span" (Map.elems attrs) $ Right content

    Tag_Strong attrs content ->
      buildTag "strong" (Map.elems attrs) $ Right content

    Tag_Style attrs style ->
      buildTag "style" (Map.elems attrs)
        . Right
        . L.singleton
        $ Tag_RawHTML style

    Tag_Subscript attrs content ->
      buildTag "sub" (Map.elems attrs) $ Right content

    Tag_Summary attrs content ->
      buildTag "summary" (Map.elems attrs) $ Right content

    Tag_Superscript attrs content ->
      buildTag "sup" (Map.elems attrs) $ Right content

    Tag_Table attrs content ->
      buildTag "table" (Map.elems attrs) $ Right content

    Tag_TableBody attrs content ->
      buildTag "tbody" (Map.elems attrs) $ Right content

    Tag_TableDataCell attrs content ->
      buildTag "td" (Map.elems attrs) $ Right content

    Tag_ContentTemplate attrs content ->
      buildTag "template" (Map.elems attrs) $ Right content

    Tag_TextArea attrs content ->
      buildTag "textarea" (Map.elems attrs) $ Right content

    Tag_TableFoot attrs content ->
      buildTag "tfoot" (Map.elems attrs) $ Right content

    Tag_TableHeader attrs content ->
      buildTag "th" (Map.elems attrs) $ Right content

    Tag_TableHead attrs content ->
      buildTag "thead" (Map.elems attrs) $ Right content

    Tag_Time attrs content ->
      buildTag "time" (Map.elems attrs) $ Right content

    Tag_Title attrs content ->
      buildTag "title" (Map.elems attrs) $ Right content

    Tag_TableRow attrs content ->
      buildTag "tr" (Map.elems attrs) $ Right content

    Tag_Track attrs ->
      buildTag "track" (Map.elems attrs) $ Left Types.OmitTag

    Tag_Underline attrs content ->
      buildTag "u" (Map.elems attrs) $ Right content

    Tag_UnorderedList attrs content ->
      buildTag "ul" (Map.elems attrs) $ Right content

    Tag_Variable attrs content ->
      buildTag "var" (Map.elems attrs) $ Right content

    Tag_Video attrs content ->
      buildTag "video" (Map.elems attrs) $ Right content

    Tag_WordBreakOpportunity attrs ->
      buildTag "wbr" (Map.elems attrs) $ Left Types.OmitTag

-- This represents an element that, for one reason or another, does not contain
-- child elements.
--
buildTag :: String
         -> [Attribute attr]
         -> Either Types.NoContent [ChildHTML parent grandparent]
         -> String
buildTag tag attributes content =
  L.concat
    [ "<"
    , tag
    , B.bool " " "" $ L.null attributes
    , L.unwords $ mapMaybe renderAttribute attributes
    , case content of
        Left  Types.OmitTag -> "/>"
        Left  Types.WithTag -> ">"
        Right _children     -> ">"
    , case content of
        Left  _type    -> ""
        Right children -> foldMap renderTag children
    , case content of
        Left  Types.OmitTag -> ""
        Left  Types.WithTag -> "</" <> tag <> ">"
        Right _children     -> "</" <> tag <> ">"
    ]

renderAttribute :: Attribute any -> Maybe String
renderAttribute attr =
  case attr of
    Attr_Custom name value ->
      Just $
        buildAttribute
          (T.unpack name)
          (T.unpack $ Escape.attribute value)

    Attr_AccessKey key ->
      Just . buildAttribute "accesskey" . T.unpack $ Escape.attributeChar key

    Attr_Autocapitalize option ->
      Just
        . buildAttribute "autocapitalize"
        . T.unpack
        $ Types.autocapitalizeOptionToText option

    Attr_Autofocus autofocus ->
      buildBooleanAttribute "autofocus" autofocus

    Attr_Class _class ->
      Just . buildAttribute "class" . T.unpack $ Escape.attribute _class

    Attr_ContentEditable option ->
      Just
        . buildAttribute "contenteditable"
        . T.unpack
        $ Types.contentEditableOptionToText option

    Attr_CustomData data_ value ->
      Just $
        buildAttribute
          ("data-" <> T.unpack data_)
          (T.unpack $ Escape.attribute value)

    Attr_Dir directionality ->
      Just
        . buildAttribute "dir"
        . T.unpack
        $ Types.directionalityToText directionality

    Attr_Draggable draggable ->
      Just . buildAttribute "draggable" $ enumBoolToText draggable

    Attr_EnterKeyHint option ->
      Just
        . buildAttribute "enterkeyhint"
        . T.unpack
        $ Types.keyHintOptionToText option

    Attr_ExportParts parts ->
      Just
        . buildAttribute "exportparts"
        . L.intercalate ", "
        . fmap (T.unpack . Types.exportPartToText)
        $ NEL.toList parts

    Attr_Hidden hidden ->
      buildBooleanAttribute "hidden" hidden

    Attr_Id _id ->
      Just
        . buildAttribute "id"
        . T.unpack
        . Escape.attribute
        $ Types.idToText _id

    Attr_Inert inert ->
      buildBooleanAttribute "inert" inert

    -- Attr_InputMode mode ->
    --   Just
    --     . buildAttribute "inputmode"
    --     . T.unpack
    --     $ Types.inputModeToText mode

    Attr_Is is ->
      Just . buildAttribute "is" . T.unpack $ Escape.attribute is

    -- Attr_ItemId

    -- Attr_ItemProp

    -- Attr_ItemRef

    -- Attr_ItemScope

    -- Attr_ItemType

    -- Attr_Lang

    -- Attr_Nonce

    Attr_Part parts ->
      Just
        . buildAttribute "part"
        . L.intercalate " "
        . fmap (T.unpack . Types.partToText)
        $ NEL.toList parts

    Attr_Popover state ->
      Just
        . buildAttribute "popover"
        . T.unpack
        $ Types.popoverStateToText state


    -- Attr_Role

    -- Attr_Slot

    Attr_Spellcheck spellcheck ->
      Just . buildAttribute "spellcheck" $ enumBoolToText spellcheck

    Attr_Style style ->
      Just . buildAttribute "style" . T.unpack $ Escape.attribute style

    Attr_TabIndex tabindex ->
      Just . buildAttribute "tabindex" $ show tabindex

    Attr_Title title ->
      Just . buildAttribute "title" . T.unpack $ Escape.attribute title

    Attr_Translate translate ->
      Just . buildAttribute "translate" $ enumBoolToText translate

    -- Scoped Attributes
    --

    Attr_CrossOrigin crossorigin ->
      Just
        . buildAttribute "crossorigin"
        . T.unpack
        $ Types.crossoriginFetchToText crossorigin

    Attr_Disabled disabled ->
      buildBooleanAttribute "disabled" disabled

    Attr_Href href ->
      Just
        . buildAttribute "href"
        . T.unpack
        . ( Shrubbery.dissect
              . Shrubbery.branchBuild
              . Shrubbery.branch @Types.AbsoluteURL (Escape.urlText . Types.absoluteURLToText)
              . Shrubbery.branch @Types.RelativeURL (Escape.urlText . Types.relativeURLToText)
              . Shrubbery.branch @Types.Id (T.cons '#' . Types.idToText)
              . Shrubbery.branch @Types.Email (("mailto:" <>) . Types.emailToText)
              . Shrubbery.branch @Types.RawURL Types.rawURLToText
              $ Shrubbery.branchEnd
          )
        . Types.unHref
        $ href

    -- Attr_Width width ->
    --   Just . buildAttribute "width" $ show width

buildAttribute :: String -> String -> String
buildAttribute attr value =
  attr <> "=\"" <> value <> "\""

buildBooleanAttribute :: String -> Bool -> Maybe String
buildBooleanAttribute attr =
  B.bool Nothing (Just attr)

enumBoolToText :: Bool -> String
enumBoolToText = B.bool "false" "true"
