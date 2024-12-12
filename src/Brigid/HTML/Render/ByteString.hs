{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Render.ByteString
  ( renderHTML
  , renderLazyHTML
  ) where

import Data.Bool qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Containers.ListUtils (nubOrdOn)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (mapMaybe)
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Shrubbery qualified

import Brigid.HTML.Attributes.Internal (Attribute (..), attributeText)
import Brigid.HTML.Elements.Internal (ChildHTML (..))
import Brigid.HTML.Render.Internal.Escape qualified as Escape
import Brigid.HTML.Types qualified as Types
import Brigid.HTML.Types.URL (RelativeURL (..))

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

    Tag_Entity entity ->
      lazyByteString $ toBytes entity

    Tag_RawHTML content ->
      lazyByteString $ toBytes content

    Tag_CustomHTML elemName attrs eiCloserOrContent ->
      buildTag (toBytes elemName) attrs eiCloserOrContent

    Tag_Anchor attrs content ->
      buildTag "a" attrs $ Right content

    Tag_Abbreviation attrs content ->
      buildTag "abbr" attrs $ Right content

    Tag_ContactAddress attrs content ->
      buildTag "address" attrs $ Right content

    Tag_Area attrs ->
      buildTag "area" attrs $ Left Types.OmitTag

    Tag_Article attrs content ->
      buildTag "article" attrs $ Right content

    Tag_Aside attrs content ->
      buildTag "aside" attrs $ Right content

    Tag_Audio attrs content ->
      buildTag "audio" attrs $ Right content

    Tag_BringAttentionTo attrs content ->
      buildTag "b" attrs $ Right content

    Tag_Base attrs ->
      buildTag "base" attrs $ Left Types.OmitTag

    Tag_BidirectionalIsolation attrs content ->
      buildTag "bdi" attrs $ Right content

    Tag_BidirectionalOverride attrs content ->
      buildTag "bdo" attrs $ Right content

    Tag_Blockquote attrs content ->
      buildTag "blockquote" attrs $ Right content

    Tag_Body attrs content ->
      buildTag "body" attrs $ Right content

    Tag_LineBreak attrs ->
      buildTag "br" attrs $ Left Types.OmitTag

    Tag_Button attrs content ->
      buildTag "button" attrs $ Right content

    Tag_Canvas attrs content ->
      buildTag "canbvas" attrs $ Right content

    Tag_TableCaption attrs content ->
      buildTag "caption" attrs $ Right content

    Tag_Citation attrs content ->
      buildTag "cite" attrs $ Right content

    Tag_Code attrs content ->
      buildTag "code" attrs $ Right content

    Tag_TableColumn attrs ->
      buildTag "col" attrs $ Left Types.OmitTag

    Tag_TableColumnGroup attrs content ->
      buildTag "colgroup" attrs $ Right content

    Tag_Data attrs content ->
      buildTag "data" attrs $ Right content

    Tag_DataList attrs content ->
      buildTag "datalist" attrs $ Right content

    Tag_DescriptionDetails attrs content ->
      buildTag "dd" attrs $ Right content

    Tag_DeletedText attrs content ->
      buildTag "del" attrs $ Right content

    Tag_Details attrs content ->
      buildTag "details" attrs $ Right content

    Tag_Definition attrs content ->
      buildTag "dfn" attrs $ Right content

    Tag_Dialog attrs content ->
      buildTag "dialog" attrs $ Right content

    Tag_Division attrs content ->
      buildTag "div" attrs $ Right content

    Tag_DescriptionList attrs content ->
      buildTag "dl" attrs $ Right content

    Tag_DescriptionTerm attrs content ->
      buildTag "dt" attrs $ Right content

    Tag_Emphasis attrs content ->
      buildTag "em" attrs $ Right content

    Tag_Embed attrs ->
      buildTag "embed" attrs $ Left Types.OmitTag

    Tag_Fieldset attrs content ->
      buildTag "fieldset" attrs $ Right content

    Tag_FigureCaption attrs content ->
      buildTag "figcaption" attrs $ Right content

    Tag_Figure attrs content ->
      buildTag "figure" attrs $ Right content

    Tag_Footer attrs content ->
      buildTag "footer" attrs $ Right content

    Tag_Form attrs content ->
      buildTag "form" attrs $ Right content

    Tag_H1 attrs content ->
      buildTag "h1" attrs $ Right content

    Tag_H2 attrs content ->
      buildTag "h2" attrs $ Right content

    Tag_H3 attrs content ->
      buildTag "h3" attrs $ Right content

    Tag_H4 attrs content ->
      buildTag "h4" attrs $ Right content

    Tag_H5 attrs content ->
      buildTag "h5" attrs $ Right content

    Tag_H6 attrs content ->
      buildTag "h6" attrs $ Right content

    Tag_Head attrs content ->
      buildTag "head" attrs $ Right content

    Tag_Header attrs content ->
      buildTag "header" attrs $ Right content

    Tag_HeadingGroup attrs content ->
      buildTag "hgroup" attrs $ Right content

    Tag_HorizontalRule attrs ->
      buildTag "hr" attrs $ Left Types.OmitTag

    Tag_Html attrs content ->
      lazyByteString "<!DOCTYPE html>"
        <> buildTag "html" attrs (Right content)

    Tag_IdiomaticText attrs content ->
      buildTag "i" attrs $ Right content

    Tag_IFrame attrs ->
      buildTag "iframe" attrs $ Left Types.WithTag

    Tag_Image attrs ->
      buildTag "img" attrs $ Left Types.OmitTag

    Tag_Input attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InsertedText attrs content ->
      buildTag "ins" attrs $ Right content

    Tag_KeyboardInput attrs content ->
      buildTag "kbd" attrs $ Right content

    Tag_Label attrs content ->
      buildTag "label" attrs $ Right content

    Tag_Legend attrs content ->
      buildTag "legend" attrs $ Right content

    Tag_ListItem attrs content ->
      buildTag "li" attrs $ Right content

    Tag_Link attrs ->
      buildTag "link" attrs $ Left Types.OmitTag

    Tag_Main attrs content ->
      buildTag "main" attrs $ Right content

    Tag_Map attrs content ->
      buildTag "map" attrs $ Right content

    Tag_Mark attrs content ->
      buildTag "mark" attrs $ Right content

    Tag_Menu attrs content ->
      buildTag "menu" attrs $ Right content

    Tag_Meta attrs ->
      buildTag "meta" attrs $ Left Types.OmitTag

    Tag_Meter attrs content ->
      buildTag "meter" attrs $ Right content

    Tag_Nav attrs content ->
      buildTag "nav" attrs $ Right content

    Tag_NoScript attrs content ->
      buildTag "noscript" attrs $ Right content

    Tag_Object attrs content ->
      buildTag "object" attrs $ Right content

    Tag_OrderedList attrs content ->
      buildTag "ol" attrs $ Right content

    Tag_OptionGroup attrs content ->
      buildTag "optgroup" attrs $ Right content

    Tag_Option attrs content ->
      buildTag "option" attrs $ Right content

    Tag_Output attrs content ->
      buildTag "output" attrs $ Right content

    Tag_Paragraph attrs content ->
      buildTag "p" attrs $ Right content

    Tag_Picture attrs content ->
      buildTag "picture" attrs $ Right content

    Tag_PreformattedText attrs content ->
      buildTag "pre" attrs $ Right content

    Tag_Progress attrs content ->
      buildTag "progress" attrs $ Right content

    Tag_Quotation attrs content ->
      buildTag "q" attrs $ Right content

    Tag_RubyParenthesis attrs content ->
      buildTag "rp" attrs $ Right content

    Tag_RubyText attrs content ->
      buildTag "rt" attrs $ Right content

    Tag_Ruby attrs content ->
      buildTag "ruby" attrs $ Right content

    Tag_Strikethrough attrs content ->
      buildTag "s" attrs $ Right content

    Tag_Sample attrs content ->
      buildTag "sample" attrs $ Right content

    Tag_Script attrs mbScript ->
      buildTag "script" attrs $
        maybe
          (Left Types.WithTag)
          (Right . L.singleton . Tag_RawHTML . NET.toText)
          mbScript

    Tag_Search attrs content ->
      buildTag "search" attrs $ Right content

    Tag_Section attrs content ->
      buildTag "section" attrs $ Right content

    Tag_Select attrs content ->
      buildTag "select" attrs $ Right content

    Tag_Slot attrs content ->
      buildTag "slot" attrs $ Right content

    Tag_SideComment attrs content ->
      buildTag "small" attrs $ Right content

    Tag_Source attrs ->
      buildTag "source" attrs $ Left Types.OmitTag

    Tag_Span attrs content ->
      buildTag "span" attrs $ Right content

    Tag_Strong attrs content ->
      buildTag "strong" attrs $ Right content

    Tag_Style attrs style ->
      buildTag "style" attrs
        . Right
        . L.singleton
        $ Tag_RawHTML style

    Tag_Subscript attrs content ->
      buildTag "sub" attrs $ Right content

    Tag_Summary attrs content ->
      buildTag "summary" attrs $ Right content

    Tag_Superscript attrs content ->
      buildTag "sup" attrs $ Right content

    Tag_Table attrs content ->
      buildTag "table" attrs $ Right content

    Tag_TableBody attrs content ->
      buildTag "tbody" attrs $ Right content

    Tag_TableDataCell attrs content ->
      buildTag "td" attrs $ Right content

    Tag_ContentTemplate attrs content ->
      buildTag "template" attrs $ Right content

    Tag_TextArea attrs content ->
      buildTag "textarea" attrs $ Right content

    Tag_TableFoot attrs content ->
      buildTag "tfoot" attrs $ Right content

    Tag_TableHeader attrs content ->
      buildTag "th" attrs $ Right content

    Tag_TableHead attrs content ->
      buildTag "thead" attrs $ Right content

    Tag_Time attrs content ->
      buildTag "time" attrs $ Right content

    Tag_Title attrs content ->
      buildTag "title" attrs $ Right content

    Tag_TableRow attrs content ->
      buildTag "tr" attrs $ Right content

    Tag_Track attrs ->
      buildTag "track" attrs $ Left Types.OmitTag

    Tag_Underline attrs content ->
      buildTag "u" attrs $ Right content

    Tag_UnorderedList attrs content ->
      buildTag "ul" attrs $ Right content

    Tag_Variable attrs content ->
      buildTag "var" attrs $ Right content

    Tag_Video attrs content ->
      buildTag "video" attrs $ Right content

    Tag_WordBreakOpportunity attrs ->
      buildTag "wbr" attrs $ Left Types.OmitTag

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
        . mapMaybe renderAttribute
        $ nubOrdOn attributeText attributes
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
    Attr_NoAttribute ->
      Just $ lazyByteString LBS.empty

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

    Attr_Lang lang ->
      Just . buildAttribute "lang" $ maybe "" Types.bcp47ToBytes lang


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

    Attr_WritingSuggestions writingsuggestions ->
      Just
        . buildAttribute "writingsuggestions"
        $ enumBoolToBytes writingsuggestions

    -- Scoped Attributes
    --
    Attr_Async ->
      buildBooleanAttribute "async" True

    Attr_Autoplay ->
      buildBooleanAttribute "autoplay" True

    Attr_Charset ->
      Just $ buildAttribute "charset" "utf-8"

    Attr_Cite cite ->
      Just . buildAttribute "cite" $ Types.urlToBytes cite

    Attr_Cols cols ->
      Just . buildAttribute "cols" . LBS8.pack $ show cols

    Attr_Colspan colspan ->
      Just . buildAttribute "colspan" . LBS8.pack $ show colspan

    Attr_Content content ->
      Just . buildAttribute "content" $ toBytes content

    Attr_Controls ->
      buildBooleanAttribute "controls" True

    Attr_ControlsList controlslist ->
      Just
        . buildAttribute "controlslist"
        $ Types.controlslistToBytes controlslist

    Attr_CrossOrigin crossorigin ->
      Just
        . buildAttribute "crossorigin"
        $ Types.crossoriginFetchToBytes crossorigin

    Attr_Default ->
      buildBooleanAttribute "default" True

    Attr_Defer ->
      buildBooleanAttribute "defer" True

    Attr_Disabled disabled ->
      buildBooleanAttribute "disabled" disabled

    Attr_DisablePictureInPicture ->
      buildBooleanAttribute "disablepictureinpicture" True

    Attr_DisableRemotePlayback ->
      buildBooleanAttribute "disableremoteplayback" True

    Attr_Headers headers ->
      Just
        . buildAttribute "headers"
        . LBS8.unwords
        . fmap Types.idToBytes
        $ NEL.toList headers

    Attr_Height height ->
      Just . buildAttribute "height" . LBS8.pack $ show height

    Attr_Href href ->
      Just
        . buildAttribute "href"
        . ( Shrubbery.dissect
              . Shrubbery.branchBuild
              . Shrubbery.branch @Types.AbsoluteURL (toBytes . Types.absoluteURLToText)
              . Shrubbery.branch @(Types.RelativeURL _) (toBytes . Types.relativeURLToText)
              . Shrubbery.branch @Types.Id (LBS8.cons '#' . Types.idToBytes)
              . Shrubbery.branch @Types.Email (toBytes . ("mailto:" <>) . Types.emailToText)
              . Shrubbery.branch @Types.BlankHref (const "#")
              . Shrubbery.branch @Types.RawURL (toBytes . Types.rawURLToText)
              $ Shrubbery.branchEnd
          )
        . Types.unHref
        $ href

    Attr_IsMap ->
      buildBooleanAttribute "ismap" True

    Attr_Kind kind ->
      Just . buildAttribute "kind" $ Types.trackKindToBytes kind

    Attr_Label label ->
      Just . buildAttribute "label" $ toBytes label

    Attr_Loop ->
      buildBooleanAttribute "loop" True

    Attr_MaxLength maxlength ->
      Just . buildAttribute "maxlength" . LBS8.pack $ show maxlength

    Attr_MinLength minlength ->
      Just . buildAttribute "minlength" . LBS8.pack $ show minlength

    Attr_Muted muted ->
      buildBooleanAttribute "muted" muted

    Attr_Name name ->
      Just . buildAttribute "name" $ toBytes name

    Attr_NoModule nomodule ->
      buildBooleanAttribute "nomodule" nomodule

    Attr_Ping pings ->
      Just
        . buildAttribute "ping"
        . LBS8.unwords
        . fmap (toBytes . Types.pingToText)
        $ NEL.toList pings

    Attr_PlaysInline playsinline ->
      buildBooleanAttribute "playsinline" playsinline

    Attr_Poster poster ->
      Just . buildAttribute "poster" $ Types.urlToBytes poster

    Attr_Preload preload ->
      Just . buildAttribute "preload" $ Types.preloadToBytes preload

    Attr_ReferrerPolicy referrerpolicy ->
      Just
        . buildAttribute "referrerpolicy"
        $ Types.referrerPolicyToBytes referrerpolicy

    Attr_Rel rel ->
      Just . buildAttribute "rel" $ Types.relationshipToBytes rel

    Attr_Rows rows ->
      Just . buildAttribute "rows" . LBS8.pack $ show rows

    Attr_Rowspan rowspan ->
      Just . buildAttribute "rowspan" . LBS8.pack $ show rowspan

    Attr_Src src ->
      Just
        . buildAttribute "src"
        . Escape.urlByteString
        $ Types.urlToText src

    Attr_SrcLang srclang ->
      Just . buildAttribute "srclang" $ Types.bcp47ToBytes srclang

    Attr_Width width ->
      Just . buildAttribute "width" . LBS8.pack $ show width

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

    Attr_HxDisabledElt disabled ->
      Just
        . buildAttribute "hx-disabled-elt"
        . LBS.intercalate (LBS8.pack ", ")
        . fmap Types.disabledSelectorToBytes
        $ NEL.toList disabled

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

    Attr_HxHeaders headers ->
      Just . buildAttribute "hx-headers" $ Types.htmxHeadersToBytes headers

    Attr_HxHistory ->
      Just $ buildAttribute "hx-history" "false"

    Attr_HxHistoryElt ->
      buildBooleanAttribute "hx-history" True

    Attr_HxInclude include ->
      Just . buildAttribute "hx-include" $ Types.includeSelectorToBytes include

    Attr_HxIndicator indicator ->
      Just . buildAttribute "hx-indicator" $ Types.indicatorToBytes indicator

    Attr_HxOn event action ->
      Just
        . buildAttribute ("hx-on" <> Types.hxOnEventBytes event)
        . toBytes
        $ Escape.attribute action

    Attr_HxParams params ->
      Just
        . buildAttribute "hx-params"
        . toBytes
        . Escape.attribute
        $ Types.requestParamsToText params

    Attr_HxPreserve preserved ->
      buildBooleanAttribute "hx-preserve" preserved

    Attr_HxPrompt prompt ->
      Just . buildAttribute "hx-prompt" . toBytes $ Escape.attribute prompt

    Attr_HxPushURL url ->
      Just . buildAttribute "hx-push-url" $ renderPushURL url

    Attr_HxReplaceURL url ->
      Just . buildAttribute "hx-replace-url" $ renderPushURL url

    Attr_HxSelect selector ->
      Just
        . buildAttribute "hx-select"
        . toBytes
        . Escape.attribute
        $ Types.querySelectorToText selector

    Attr_HxSelectOOB selects ->
      Just
        . buildAttribute "hx-select-oob"
        . LBS8.intercalate (LBS8.pack ", ")
        . fmap Types.outOfBandSelectToBytes
        $ NEL.toList selects

    Attr_HxSwap swap ->
      Just . buildAttribute "hx-swap" $ Types.swapToBytes swap

    Attr_HxSwapOOB mbSwap ->
      Just
        . buildAttribute "hx-swap-oob"
        $ maybe "true" Types.outOfBandSwapToBytes mbSwap

    Attr_HxTarget target ->
      Just . buildAttribute "hx-target" $ Types.targetToBytes target

    Attr_HxTrigger triggers ->
      Just
        . buildAttribute "hx-trigger"
        . LBS.intercalate (LBS8.pack ", ")
        . fmap Types.triggerToBytes
        $ NEL.toList triggers

    Attr_HxValidate ->
      buildBooleanAttribute "hx-validate" True

    Attr_HxVals vals ->
      Just . buildAttribute "hx-vals" $ Types.htmxValsToBytes vals

    -- Other
    --
    Attr_HyperScript hyperscript ->
      Just . buildAttribute "_" $ Types.hyperScriptToBytes hyperscript

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
      . Shrubbery.branch @Types.AbsoluteURL (toBytes . Types.absoluteURLToText)
      . Shrubbery.branch @(Types.RelativeURL _) (toBytes . Types.relativeURLToText)
      . Shrubbery.branch @Bool enumBoolToBytes
      . Shrubbery.branch @Types.RawURL (toBytes . Types.rawURLToText)
      $ Shrubbery.branchEnd
  ) . Types.unPushURL
