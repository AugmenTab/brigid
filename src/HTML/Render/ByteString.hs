{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module HTML.Render.ByteString
  ( renderHTML
  , renderLazyHTML
  ) where

import Data.Bool qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Shrubbery qualified

import HTML.Attributes.Internal (Attribute(..))
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Render.Internal.Escape qualified as Escape
import HTML.Types qualified as Types
import HTML.Types.URL (RelativeURL(..))

renderHTML :: ChildHTML parent grandparent -> BS.ByteString
renderHTML = LBS.toStrict . renderLazyHTML

renderLazyHTML :: ChildHTML parent grandparent -> LBS.ByteString
renderLazyHTML = toLazyByteString . renderTag

renderTag :: ChildHTML parent grandparent -> Builder
renderTag html =
  case html of
    -- Global Attributes
    --
    Tag_NoElement ->
      lazyByteString LBS.empty

    Tag_Comment content ->
      lazyByteString "<!-- "
        <> lazyByteString (toBytes content)
        <> lazyByteString " -->"

    Tag_Text content ->
      lazyByteString . toBytes $ Escape.html content

    Tag_RawHTML content ->
      lazyByteString $ toBytes content

    Tag_CustomHTML elemName attrs eiCloserOrContent ->
      buildTag (toBytes elemName) (Map.elems attrs) eiCloserOrContent

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
      lazyByteString "<!DOCTYPE html>"
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

    Tag_Script attrs content ->
      buildTag "script" (Map.elems attrs)
        . Right
        . L.singleton
        $ Tag_RawHTML content

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

buildTag :: LBS.ByteString
         -> [Attribute attr]
         -> Either Types.NoContent [ChildHTML parent grandparent]
         -> Builder
buildTag tag attributes content =
  mconcat
    [ lazyByteString "<"
    , lazyByteString tag
    , lazyByteString . B.bool " " LBS.empty $ L.null attributes
    , mconcat
        . L.intersperse (lazyByteString " ")
        $ mapMaybe renderAttribute attributes
    , case content of
        Left  Types.OmitTag -> lazyByteString "/>"
        Left  Types.WithTag -> lazyByteString ">"
        Right _children     -> lazyByteString ">"
    , case content of
        Left  _type    -> lazyByteString LBS.empty
        Right children -> foldMap renderTag children
    , case content of
        Left Types.OmitTag ->
          lazyByteString LBS.empty

        Left Types.WithTag ->
          lazyByteString "</" <> lazyByteString tag <> lazyByteString ">"

        Right _children ->
          lazyByteString "</" <> lazyByteString tag <> lazyByteString ">"
    ]

renderAttribute :: Attribute any -> Maybe Builder
renderAttribute attr =
  case attr of
    Attr_Custom name value ->
      Just $
        buildAttribute
          (toBytes name)
          (toBytes $ Escape.attribute value)

    Attr_AccessKey key ->
      Just . buildAttribute "accesskey" . toBytes $ Escape.attributeChar key

    Attr_Autocapitalize option ->
      Just
        . buildAttribute "autocapitalize"
        $ Types.autocapitalizeOptionToBytes option

    Attr_Autofocus autofocus ->
      buildBooleanAttribute "autofocus" autofocus

    Attr_Class _class ->
      Just
        . buildAttribute "class"
        . toBytes
        . Escape.attribute
        $ Types.classToText _class

    Attr_ContentEditable option ->
      Just
        . buildAttribute "contenteditable"
        $ Types.contentEditableOptionToBytes option

    Attr_CustomData data_ value ->
      Just $
        buildAttribute
          ("data-" <> toBytes data_)
          (toBytes $ Escape.attribute value)

    Attr_Dir directionality ->
      Just
        . buildAttribute "dir"
        $ Types.directionalityToBytes directionality

    Attr_Draggable draggable ->
      Just . buildAttribute "draggable" $ enumBoolToBytes draggable

    Attr_EnterKeyHint option ->
      Just
        . buildAttribute "enterkeyhint"
        $ Types.keyHintOptionToBytes option

    Attr_ExportParts parts ->
      Just
        . buildAttribute "exportparts"
        . LBS.intercalate ", "
        . fmap Types.exportPartToBytes
        $ NEL.toList parts

    Attr_Hidden hidden ->
      buildBooleanAttribute "hidden" hidden

    Attr_Id _id ->
      Just
        . buildAttribute "id"
        . toBytes
        . Escape.attribute
        $ Types.idToText _id

    Attr_Inert inert ->
      buildBooleanAttribute "inert" inert

    -- Attr_InputMode mode ->
    --   Just
    --     . buildAttribute "inputmode"
    --     $ Types.inputModeToBytes mode

    Attr_Is is ->
      Just . buildAttribute "is" . toBytes $ Escape.attribute is

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
        . LBS.intercalate " "
        . fmap Types.partToBytes
        $ NEL.toList parts

    Attr_Popover state ->
      Just
        . buildAttribute "popover"
        $ Types.popoverStateToBytes state

    -- Attr_Role

    -- Attr_Slot

    Attr_Spellcheck spellcheck ->
      Just . buildAttribute "spellcheck" $ enumBoolToBytes spellcheck

    Attr_Style style ->
      Just . buildAttribute "style" . toBytes $ Escape.attribute style

    Attr_TabIndex tabindex ->
      Just . buildAttribute "tabindex" . LBS8.pack $ show tabindex

    Attr_Title title ->
      Just . buildAttribute "title" . toBytes $ Escape.attribute title

    Attr_Translate translate ->
      Just . buildAttribute "translate" $ enumBoolToBytes translate

    -- Scoped Attributes
    --

    Attr_CrossOrigin crossorigin ->
      Just
        . buildAttribute "crossorigin"
        $ Types.crossoriginFetchToBytes crossorigin

    Attr_Disabled disabled ->
      buildBooleanAttribute "disabled" disabled

    Attr_Href href ->
      Just
        . buildAttribute "href"
        . ( Shrubbery.dissect
              . Shrubbery.branchBuild
              . Shrubbery.branch @Types.AbsoluteURL (Escape.urlByteString . Types.absoluteURLToText)
              . Shrubbery.branch @(Types.RelativeURL _) (Escape.urlByteString . Types.relativeURLToText)
              . Shrubbery.branch @Types.Id (toBytes . T.cons '#' . Types.idToText)
              . Shrubbery.branch @Types.Email (toBytes . ("mailto:" <>) . Types.emailToText)
              . Shrubbery.branch @Types.BlankHref (const "#")
              . Shrubbery.branch @Types.RawURL (toBytes . Types.rawURLToText)
              $ Shrubbery.branchEnd
          )
        . Types.unHref
        $ href

    -- Attr_Width width ->
    --   Just . buildAttribute "width" . LBS8.pack $ show width

    -- HTMX Attributes
    --
    Attr_Htmx url ->
      let (hxAttr, hxPath) =
            case url of
              Relative_Get    path -> ("hx-get", path)
              Relative_Post   path -> ("hx-post", path)
              Relative_Delete path -> ("hx-delete", path)
              Relative_Put    path -> ("hx-put", path)
              Relative_Patch  path -> ("hx-patch", path)

       in Just . buildAttribute hxAttr $ Escape.urlByteString hxPath

    Attr_HxBoost boosted ->
      Just . buildAttribute "hx-boost" $ enumBoolToBytes boosted

    Attr_HxConfirm confirmation ->
      Just
        . buildAttribute "hx-confirm"
        . toBytes
        $ Escape.attribute confirmation

    Attr_HxDisable disabled ->
      buildBooleanAttribute "hx-disable" disabled

    Attr_HxDisinherit disinherit ->
      Just
        . buildAttribute "hx-disinherit"
        $ Types.disinheritToBytes disinherit

    Attr_HxEncoding ->
      Just $ buildAttribute "hx-encoding" "multipart/form-data"

    Attr_HxExt exts ->
      Just
        . buildAttribute "hx-ext"
        . LBS.intercalate ","
        . fmap (toBytes . Types.extensionToText)
        $ NEL.toList exts

    Attr_HxHistory ->
      Just $ buildAttribute "hx-history" "false"

    Attr_HxHistoryElt ->
      buildBooleanAttribute "hx-history" True

    Attr_HxPushURL url ->
      Just . buildAttribute "hx-push-url" $ renderPushURL url

    Attr_HxPrompt prompt ->
      Just . buildAttribute "hx-prompt" . toBytes $ Escape.attribute prompt

    Attr_HxReplaceURL url ->
      Just . buildAttribute "hx-replace-url" $ renderPushURL url

    Attr_HxValidate ->
      buildBooleanAttribute "hx-validate" True

buildAttribute :: LBS.ByteString -> LBS.ByteString -> Builder
buildAttribute attr value =
  lazyByteString attr <> "=\"" <> lazyByteString value <> lazyByteString "\""

buildBooleanAttribute :: LBS.ByteString -> Bool -> Maybe Builder
buildBooleanAttribute attr =
  B.bool Nothing (Just $ lazyByteString attr)

enumBoolToBytes :: Bool -> LBS.ByteString
enumBoolToBytes = B.bool "false" "true"

toBytes :: T.Text -> LBS.ByteString
toBytes = LBS.fromStrict . TE.encodeUtf8

renderPushURL :: Types.PushURL -> LBS.ByteString
renderPushURL =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Types.AbsoluteURL (Escape.urlByteString . Types.absoluteURLToText)
      . Shrubbery.branch @(Types.RelativeURL _) (Escape.urlByteString . Types.relativeURLToText)
      . Shrubbery.branch @Bool enumBoolToBytes
      . Shrubbery.branch @Types.RawURL (toBytes . Types.rawURLToText)
      $ Shrubbery.branchEnd
  ) . Types.unPushURL
