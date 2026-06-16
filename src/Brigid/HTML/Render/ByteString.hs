{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Render.ByteString
  ( renderHTML
  , renderLazyHTML
  ) where

import Prelude hiding (id, max, min, span)
import Data.Bool qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, string8, stringUtf8, toLazyByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Ogma qualified
import Shrubbery qualified

import Brigid.HTML.Attributes.Event.Event (eventAttributeToBytesBuilder)
import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Elements.Internal (ChildHTML (..))
import Brigid.HTML.Types qualified as Types
import Brigid.Internal.Escape qualified as Escape
import Brigid.Internal.Render qualified as Render
import Brigid.Types qualified as Types
import Brigid.Types.URL (RelativeURL (..))

renderHTML :: ChildHTML parent grandparent -> BS.ByteString
renderHTML = LBS.toStrict . renderLazyHTML

renderLazyHTML :: ChildHTML parent grandparent -> LBS.ByteString
renderLazyHTML = toLazyByteString . renderTag

renderTag :: ChildHTML parent grandparent -> Builder
renderTag html =
  case html of
    Tag_NoElement ->
      mempty

    Tag_Comment comment ->
      "<!-- " <> Render.textToBytesBuilder comment <> " -->"

    Tag_Text content ->
      Escape.escapeBytesBuilder content

    Tag_Entity entity ->
      string8 entity

    Tag_RawHTML content ->
      Render.textToBytesBuilder content

    Tag_CustomHTML elemName attrs eiCloserOrContent ->
      buildTag (Render.textToBytesBuilder elemName) attrs eiCloserOrContent

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
      buildTag "canvas" attrs $ Right content

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
      "<!DOCTYPE html>" <> buildTag "html" attrs (Right content)

    Tag_IdiomaticText attrs content ->
      buildTag "i" attrs $ Right content

    Tag_IFrame attrs ->
      buildTag "iframe" attrs $ Left Types.WithTag

    Tag_Image attrs ->
      buildTag "img" attrs $ Left Types.OmitTag

    Tag_Input attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputButton attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputCheckbox attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputColor attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputDate attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputDatetimeLocal attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputEmail attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputFile attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputHidden attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputImage attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputMonth attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputNumber attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputPassword attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputRadio attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputRange attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputReset attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputSearch attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputSubmit attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputTel attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputText attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputTime attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputUrl attrs ->
      buildTag "input" attrs $ Left Types.OmitTag

    Tag_InputWeek attrs ->
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
      buildTag "samp" attrs $ Right content

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

    Tag_Group attrs content ->
      buildTag "g" attrs $ Right content

    Tag_Path attrs content ->
      buildTag "path" attrs $ Right content

    Tag_SVG attrs content ->
      buildTag "svg" attrs $ Right content

buildTag :: Builder
         -> [Attribute tag]
         -> Either Types.NoContent [ChildHTML parent grandparent]
         -> Builder
buildTag tag attrs content =
  "<"
    <> tag
    <> foldMap (\attr -> maybe mempty (" " <>) (renderAttribute attr)) attrs
    <> case content of
         Left  Types.OmitTag -> "/>"
         Left  Types.WithTag -> ">"
         Right _children     -> ">"
    <> case content of
         Left  _type    -> mempty
         Right children -> foldMap renderTag children
    <> case content of
         Left  Types.OmitTag -> mempty
         Left  Types.WithTag -> "</" <> tag <> ">"
         Right _children     -> "</" <> tag <> ">"

renderAttribute :: Attribute any -> Maybe Builder
renderAttribute attr =
  case attr of
    -- Global Attributes
    --
    Attr_NoAttribute ->
      Just mempty

    Attr_Custom name value ->
      Just $
        buildAttribute
          (Render.textToBytesBuilder name)
          (Escape.attributeBytesBuilder value)

    Attr_CustomBoolean name value ->
      buildBooleanAttribute (Render.textToBytesBuilder name) value

    Attr_AccessKey key ->
      Just . buildAttribute "accesskey" $ Escape.attributeCharBytesBuilder key

    Attr_Autocapitalize option ->
      Just
        . buildAttribute "autocapitalize"
        $ Types.autocapitalizeOptionToBytesBuilder option

    Attr_Autocorrect autocorrect ->
      Just
        . buildAttribute "autocorrect"
        $ Types.onOffToBytesBuilder autocorrect

    Attr_Autofocus autofocus ->
      buildBooleanAttribute "autofocus" autofocus

    Attr_Class _class ->
      Just
        . buildAttribute "class"
        . Escape.attributeBytesBuilder
        $ Types.classToText _class

    Attr_ContentEditable option ->
      Just
        . buildAttribute "contenteditable"
        $ Types.contentEditableOptionToBytesBuilder option

    Attr_CustomData data_ mbValue ->
      case mbValue of
        Just value ->
          Just $
            buildAttribute
              ("data-" <> Render.textToBytesBuilder data_)
              (Escape.attributeBytesBuilder value)

        Nothing ->
          buildBooleanAttribute ("data-" <> Render.textToBytesBuilder data_) True

    Attr_Dir directionality ->
      Just
        . buildAttribute "dir"
        $ Types.directionalityToBytesBuilder directionality

    Attr_Draggable draggable ->
      Just
        . buildAttribute "draggable"
        $ Render.enumBoolToBytesBuilder draggable

    Attr_EnterKeyHint option ->
      Just
        . buildAttribute "enterkeyhint"
        $ Types.keyHintOptionToBytesBuilder option

    Attr_ExportParts parts ->
      Just
        . buildAttribute "exportparts"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.exportPartToText) ", "
        $ parts

    Attr_Hidden hidden ->
      buildBooleanAttribute "hidden" hidden

    Attr_Id id ->
      Just
        . buildAttribute "id"
        . Escape.attributeBytesBuilder
        $ Types.idToText id

    Attr_Inert inert ->
      buildBooleanAttribute "inert" inert

    Attr_InputMode mode ->
      Just
        . buildAttribute "inputmode"
        $ Types.inputModeToBytesBuilder mode

    Attr_Is is ->
      Just . buildAttribute "is" $ Escape.attributeBytesBuilder is

    Attr_ItemId itemid ->
      Just . buildAttribute "itemid" $ Render.textToBytesBuilder itemid

    Attr_ItemProp itemprop ->
      Just . buildAttribute "itemprop" $ Render.textToBytesBuilder itemprop

    Attr_ItemRef itemref ->
      Just
        . buildAttribute "itemref"
        . Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " "
        $ NEL.toList itemref

    Attr_ItemScope ->
      buildBooleanAttribute "itemscope" True

    Attr_ItemType itemtype ->
      Just
        . buildAttribute "itemtype"
        $ Types.absoluteURLToBytesBuilder itemtype

    Attr_Lang lang ->
      Just
        . buildAttribute "lang"
        . maybe mempty (BSB.lazyByteString . Ogma.bcp_47ToBytes)
        $ lang

    Attr_Nonce nonce ->
      Just . buildAttribute "nonce" $ Render.textToBytesBuilder nonce

    Attr_Part parts ->
      Just
        . buildAttribute "part"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.partToText) " "
        $ parts

    Attr_Popover state ->
      Just
        . buildAttribute "popover"
        $ Types.popoverStateToBytesBuilder state

    Attr_Role role ->
      Just
        . buildAttribute "role"
        $ Types.roleToBytesBuilder role

    Attr_Slot slot ->
      Just
        . buildAttribute "slot"
        $ Types.nameToBytesBuilder slot

    Attr_Spellcheck spellcheck ->
      Just
        . buildAttribute "spellcheck"
        $ Render.enumBoolToBytesBuilder spellcheck

    Attr_Style style ->
      Just . buildAttribute "style" $ Escape.attributeBytesBuilder style

    Attr_TabIndex tabindex ->
      Just . buildAttribute "tabindex" $ Render.showIntegerBytesBuilder tabindex

    Attr_Title title ->
      Just . buildAttribute "title" $ Escape.attributeBytesBuilder title

    Attr_Translate translate ->
      Just
        . buildAttribute "translate"
        $ Types.yesNoToBytesBuilder translate

    Attr_WritingSuggestions writingsuggestions ->
      Just
        . buildAttribute "writingsuggestions"
        $ Render.enumBoolToBytesBuilder writingsuggestions

    -- Scoped Attributes
    --
    Attr_Abbreviation abbr ->
      Just . buildAttribute "abbr" $ Render.textToBytesBuilder abbr

    Attr_Accept accept ->
      Just . buildAttribute "accept" $ BSB.byteString accept

    Attr_AcceptCharset ->
      Just $ buildAttribute "accept-charset" "UTF-8"

    Attr_Action action ->
      Just
        . buildAttribute "action"
        . Render.textToBytesBuilder
        $ Types.actionToText action

    Attr_Allow allow ->
      Just
        . buildAttribute "allow"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.featurePolicyDirectiveToText) "; "
        $ allow

    Attr_Alt alt ->
      Just . buildAttribute "alt" $ Render.textToBytesBuilder alt

    Attr_As as ->
      Just . buildAttribute "as" $ Types.asToBytesBuilder as

    Attr_Async ->
      buildBooleanAttribute "async" True

    Attr_Autocomplete autocomplete ->
      Just
        . buildAttribute "autocomplete"
        . Render.textToBytesBuilder
        $ Types.autocompleteTokenToText autocomplete

    Attr_Autoplay ->
      buildBooleanAttribute "autoplay" True

    Attr_Blocking blocking ->
      Just
        . buildAttribute "blocking"
        $ Types.blockOptionToBytesBuilder blocking

    Attr_Capture mbCapture ->
      maybe
        (buildBooleanAttribute "capture" True)
        (Just . buildAttribute "capture" . Types.captureMethodToBytesBuilder)
        mbCapture

    Attr_Charset ->
      Just $ buildAttribute "charset" "utf-8"

    Attr_Checked checked ->
      buildBooleanAttribute "checked" checked

    Attr_Cite cite ->
      Just
        . buildAttribute "cite"
        . Render.textToBytesBuilder
        $ Types.urlToText cite

    Attr_Cols cols ->
      Just . buildAttribute "cols" $ Render.showIntegerBytesBuilder cols

    Attr_Colspan colspan ->
      Just . buildAttribute "colspan" $ Render.showIntegerBytesBuilder colspan

    Attr_Command command ->
      Just
        . buildAttribute "command"
        $ Types.commandOptionToBytesBuilder command

    Attr_CommandFor commandfor ->
      Just
        . buildAttribute "commandfor"
        $ Types.idToBytesBuilder commandfor

    Attr_Coords coords ->
      Just
        . buildAttribute "coords"
        . Render.foldToBytesBuilderWithSeparator Render.showIntegerBytesBuilder ","
        $ NEL.toList coords

    Attr_Content content ->
      Just . buildAttribute "content" $ Render.textToBytesBuilder content

    Attr_Controls ->
      buildBooleanAttribute "controls" True

    Attr_ControlsList controlslist ->
      Just
        . buildAttribute "controlslist"
        . Render.textToBytesBuilder
        $ Types.controlsListToText controlslist

    Attr_CrossOrigin crossorigin ->
      Just
        . buildAttribute "crossorigin"
        $ Types.crossOriginFetchToBytesBuilder crossorigin

    Attr_Data _data ->
      Just
        . buildAttribute "data"
        $ Types.urlToBytesBuilder _data

    Attr_Datetime datetime ->
      Just . buildAttribute "datetime" $ stringUtf8 datetime

    Attr_Decoding decoding ->
      Just
        . buildAttribute "decoding"
        $ Types.decodingToBytesBuilder decoding

    Attr_Default ->
      buildBooleanAttribute "default" True

    Attr_Defer ->
      buildBooleanAttribute "defer" True

    Attr_Dirname dirname ->
      Just . buildAttribute "dirname" $ Render.textToBytesBuilder dirname

    Attr_Disabled disabled ->
      buildBooleanAttribute "disabled" disabled

    Attr_DisablePictureInPicture ->
      buildBooleanAttribute "disablepictureinpicture" True

    Attr_DisableRemotePlayback ->
      buildBooleanAttribute "disableremoteplayback" True

    Attr_Download download ->
      maybe
        (buildBooleanAttribute "download" True)
        (Just . buildAttribute "download" . Render.textToBytesBuilder . NET.toText)
        download

    Attr_ElementTiming elementtiming ->
      Just
        . buildAttribute "elementtiming"
        $ Render.textToBytesBuilder elementtiming

    Attr_Enctype enctype ->
      Just . buildAttribute "enctype" $ BSB.byteString enctype

    Attr_FetchPriority fetchpriority ->
      Just
        . buildAttribute "fetchpriority"
        $ Types.fetchPriorityToBytesBuilder fetchpriority

    Attr_For for ->
      Just
        . buildAttribute "for"
        $ Types.forOptionToBytesBuilder for

    Attr_Form form ->
      Just
        . buildAttribute "form"
        $ Types.idToBytesBuilder form

    Attr_FormAction formaction ->
      Just
        . buildAttribute "formaction"
        . Render.textToBytesBuilder
        $ Types.actionToText formaction

    Attr_FormEnctype formenctype ->
      Just . buildAttribute "formenctype" $ BSB.byteString formenctype

    Attr_FormMethod formmethod ->
      Just
        . buildAttribute "formmethod"
        $ Types.formMethodToBytesBuilder formmethod

    Attr_FormNoValidate ->
      buildBooleanAttribute "formnovalidate" True

    Attr_FormTarget formtarget ->
      Just
        . buildAttribute "formtarget"
        $ Types.targetToBytesBuilder formtarget

    Attr_Headers headers ->
      Just
        . buildAttribute "headers"
        . Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " "
        $ headers

    Attr_Height height ->
      Just . buildAttribute "height" $ Render.showIntegerBytesBuilder height

    Attr_High high ->
      Just
        . buildAttribute "high"
        . Render.textToBytesBuilder
        $ Types.numberToText high

    Attr_Href href ->
      Just
        . buildAttribute "href"
        . Render.textToBytesBuilder
        $ Types.hrefToText href

    Attr_HrefLang hreflang ->
      Just
        . buildAttribute "hreflang"
        . BSB.lazyByteString
        $ Ogma.bcp_47ToBytes hreflang

    Attr_HttpEquiv httpEquiv ->
      Just
        . buildAttribute "http-equiv"
        $ Types.httpEquivTokenToBytesBuilder httpEquiv

    Attr_ImageSizes imagesizes ->
      Just
        . buildAttribute "imagesizes"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.sizeToText) ", "
        $ NEL.toList imagesizes

    Attr_ImageSrcset imagesrcset ->
      Just
        . buildAttribute "imagesrcset"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.srcsetCandidateToText) ", "
        $ NEL.toList imagesrcset

    Attr_Integrity sha content ->
      Just
        . buildAttribute "integrity"
        . Render.textToBytesBuilder
        $ Types.integrityToText sha content

    Attr_IsMap ->
      buildBooleanAttribute "ismap" True

    Attr_Kind kind ->
      Just
        . buildAttribute "kind"
        $ Types.trackKindToBytesBuilder kind

    Attr_Label label ->
      Just . buildAttribute "label" $ Render.textToBytesBuilder label

    Attr_List list ->
      Just
        . buildAttribute "label"
        $ Types.idToBytesBuilder list

    Attr_Loading loading ->
      Just
        . buildAttribute "loading"
        $ Types.loadOptionToBytesBuilder loading

    Attr_Loop ->
      buildBooleanAttribute "loop" True

    Attr_Low low ->
      Just
        . buildAttribute "low"
        . Render.textToBytesBuilder
        $ Types.numberToText low

    Attr_Max max ->
      Just
        . buildAttribute "max"
        . Render.textToBytesBuilder
        $ Types.rangeBoundToText max

    Attr_MaxLength maxlength ->
      Just . buildAttribute "maxlength" $ Render.showIntegerBytesBuilder maxlength

    Attr_Media media ->
      Just
        . buildAttribute "media"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.mediaQueryToText) ", "
        $ NEL.toList media

    Attr_Method method ->
      Just
        . buildAttribute "method"
        $ Types.formMethodToBytesBuilder method

    Attr_Min min ->
      Just
        . buildAttribute "min"
        . Render.textToBytesBuilder
        $ Types.rangeBoundToText min

    Attr_MinLength minlength ->
      Just . buildAttribute "minlength" $ Render.showIntegerBytesBuilder minlength

    Attr_Multiple ->
      buildBooleanAttribute "multiple" True

    Attr_Muted muted ->
      buildBooleanAttribute "muted" muted

    Attr_Name name ->
      Just
        . buildAttribute "name"
        $ Types.nameOptionToBytesBuilder name

    Attr_NoModule nomodule ->
      buildBooleanAttribute "nomodule" nomodule

    Attr_NoValidate novalidate ->
      buildBooleanAttribute "novalidate" novalidate

    Attr_Open ->
      buildBooleanAttribute "open" True

    Attr_Optimum optimum ->
      Just
        . buildAttribute "optimum"
        . Render.textToBytesBuilder
        $ Types.numberToText optimum

    Attr_Pattern pattern ->
      Just . buildAttribute "pattern" $ Render.textToBytesBuilder pattern

    Attr_Ping pings ->
      Just
        . buildAttribute "ping"
        . Render.foldToBytesBuilderWithSeparator Types.pingToBytesBuilder " "
        $ NEL.toList pings

    Attr_Placeholder placeholder ->
      Just
        . buildAttribute "placeholder"
        $ Render.textToBytesBuilder placeholder

    Attr_PlaysInline playsinline ->
      buildBooleanAttribute "playsinline" playsinline

    Attr_PopoverTarget popovertarget ->
      Just
        . buildAttribute "popovertarget"
        $ Types.idToBytesBuilder popovertarget

    Attr_PopoverTargetAction popovertargetaction ->
      Just
        . buildAttribute "popovertargetaction"
        $ Types.popoverTargetActionToBytesBuilder popovertargetaction

    Attr_Poster poster ->
      Just
        . buildAttribute "poster"
        $ Types.urlToBytesBuilder poster

    Attr_Preload preload ->
      Just
        . buildAttribute "preload"
        $ Types.preloadToBytesBuilder preload

    Attr_ReadOnly ->
      buildBooleanAttribute "readonly" True

    Attr_ReferrerPolicy referrerpolicy ->
      Just
        . buildAttribute "referrerpolicy"
        $ Types.referrerPolicyToBytesBuilder referrerpolicy

    Attr_Rel rel ->
      Just
        . buildAttribute "rel"
        . Render.textToBytesBuilder
        $ Types.relationshipToText rel

    Attr_Required required ->
      buildBooleanAttribute "required" required

    Attr_Reversed reversed ->
      buildBooleanAttribute "reversed" reversed

    Attr_Rows rows ->
      Just . buildAttribute "rows" $ Render.showIntegerBytesBuilder rows

    Attr_Rowspan rowspan ->
      Just . buildAttribute "rowspan" $ Render.showIntegerBytesBuilder rowspan

    Attr_Sandbox sandbox ->
      if null sandbox
        then buildBooleanAttribute "sandbox" True
        else
          Just
            . buildAttribute "sandbox"
            . Render.foldToBytesBuilderWithSeparator Types.sandboxTokenToBytesBuilder " "
            $ sandbox

    Attr_Scope scope ->
      Just
        . buildAttribute "scope"
        $ Types.scopeToBytesBuilder scope

    Attr_Selected selected ->
      buildBooleanAttribute "selected" selected

    Attr_ShadowRootMode shadowrootmode ->
      Just
        . buildAttribute "shadowrootmode"
        $ Types.openClosedToBytesBuilder shadowrootmode

    Attr_ShadowRootDelegatesFocus ->
      buildBooleanAttribute "shadowrootdelegatesfocus" True

    Attr_ShadowRootClonable ->
      buildBooleanAttribute "shadowrootclonable" True

    Attr_Shape shape ->
      Just
        . buildAttribute "shape"
        $ Types.shapeToBytesBuilder shape

    Attr_Size size ->
      Just . buildAttribute "size" $ Render.showIntegerBytesBuilder size

    Attr_Sizes sizes ->
      Just
        . buildAttribute "sizes"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.sizeToText) ", "
        $ NEL.toList sizes

    Attr_Span span ->
      Just . buildAttribute "span" $ Render.showIntegerBytesBuilder span

    Attr_Src src ->
      Just
        . buildAttribute "src"
        $ Types.urlToBytesBuilder src

    Attr_SrcDoc srcdoc ->
      Just
        . buildAttribute "srcdoc"
        $ Escape.lazyBytesAttributeBytesBuilder srcdoc

    Attr_SrcLang srclang ->
      Just
        . buildAttribute "srclang"
        . BSB.lazyByteString
        $ Ogma.bcp_47ToBytes srclang

    Attr_SrcSet srcset ->
      Just
        . buildAttribute "srcset"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.srcsetCandidateToText) ", "
        $ NEL.toList srcset

    Attr_Start start ->
      Just . buildAttribute "start" $ Render.showIntegerBytesBuilder start

    Attr_Step step ->
      Just
        . buildAttribute "step"
        . Render.textToBytesBuilder
        $ Types.stepToText step

    Attr_Target target ->
      Just
        . buildAttribute "target"
        $ Types.targetToBytesBuilder target

    Attr_Type type_ ->
      Just
        . buildAttribute "type"
        $ Types.typeOptionToBytesBuilder type_

    Attr_UseMap usemap ->
      Just
        . buildAttribute "usemap"
        $ ("#" <> Types.nameToBytesBuilder usemap)

    Attr_Value value ->
      Just
        . buildAttribute "value"
        . Render.textToBytesBuilder
        $ Types.valueToText value

    Attr_Width width ->
      Just . buildAttribute "width" $ Render.showIntegerBytesBuilder width

    Attr_Wrap wrap ->
      Just
        . buildAttribute "wrap"
        $ Types.wrapToBytesBuilder wrap

    Attr_XMLNS xmlns ->
      Just
        . buildAttribute "xmlns"
        $ Types.urlToBytesBuilder xmlns

    -- ARIA Attributes
    --
    Attr_Aria aria ->
      Just $
        buildAttribute
          (Render.textToBytesBuilder $ Types.ariaAttributeToText aria)
          (Render.textToBytesBuilder $ Types.ariaValueToText aria)

    -- Event Attributes
    --
    Attr_On event script ->
      Just $
        buildAttribute
          (eventAttributeToBytesBuilder event)
          (Render.textToBytesBuilder $ Types.rawJavaScriptToText script)

    -- HTMX Attributes
    --
    Attr_Htmx url ->
      let
        (hxAttr, hxPath) =
          case url of
            Relative_Get    path -> ("hx-get" :: Builder, path)
            Relative_Post   path -> ("hx-post", path)
            Relative_Delete path -> ("hx-delete", path)
            Relative_Put    path -> ("hx-put", path)
            Relative_Patch  path -> ("hx-patch", path)
      in
        Just . buildAttribute hxAttr $ Render.textToBytesBuilder hxPath

    Attr_HxBoost boosted ->
      Just . buildAttribute "hx-boost" $ Render.enumBoolToBytesBuilder boosted

    Attr_HxConfirm confirmation ->
      Just
        . buildAttribute "hx-confirm"
        $ Escape.attributeBytesBuilder confirmation

    Attr_HxDisable disabled ->
      buildBooleanAttribute "hx-disable" disabled

    Attr_HxDisabledElt disabled ->
      Just
        . buildAttribute "hx-disabled-elt"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.disabledSelectorToText) ", "
        $ NEL.toList disabled

    Attr_HxDisinherit disinherit ->
      Just
        . buildAttribute "hx-disinherit"
        . Render.textToBytesBuilder
        $ Types.disinheritToText disinherit

    Attr_HxEncoding ->
      Just $ buildAttribute "hx-encoding" "multipart/form-data"

    Attr_HxExt exts ->
      Just
        . buildAttribute "hx-ext"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.extensionToText) ","
        $ NEL.toList exts

    Attr_HxHeaders headers ->
      Just
        . buildAttribute "hx-headers"
        . Escape.attributeBytesBuilder
        $ Types.htmxHeadersToText headers

    Attr_HxHistory ->
      Just $ buildAttribute "hx-history" "false"

    Attr_HxHistoryElt ->
      buildBooleanAttribute "hx-history" True

    Attr_HxInclude include ->
      Just
        . buildAttribute "hx-include"
        . Render.textToBytesBuilder
        $ Types.includeSelectorToText include

    Attr_HxIndicator indicator ->
      Just
        . buildAttribute "hx-indicator"
        . Render.textToBytesBuilder
        $ Types.indicatorToText indicator

    Attr_HxOn event action ->
      Just $
        buildAttribute
          ("hx-on" <> Render.textToBytesBuilder (Types.hxOnEventText event))
          (Escape.attributeBytesBuilder action)

    Attr_HxParams params ->
      Just
        . buildAttribute "hx-params"
        . Escape.attributeBytesBuilder
        $ Types.requestParamsToText params

    Attr_HxPreserve preserved ->
      buildBooleanAttribute "hx-preserve" preserved

    Attr_HxPrompt prompt ->
      Just . buildAttribute "hx-prompt" $ Escape.attributeBytesBuilder prompt

    Attr_HxPushURL url ->
      Just . buildAttribute "hx-push-url" $ renderPushURLBuilder url

    Attr_HxReplaceURL url ->
      Just . buildAttribute "hx-replace-url" $ renderPushURLBuilder url

    Attr_HxSelect selector ->
      Just
        . buildAttribute "hx-select"
        . Escape.attributeBytesBuilder
        $ Types.querySelectorToText selector

    Attr_HxSelectOOB selects ->
      Just
        . buildAttribute "hx-select-oob"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.outOfBandSelectToText) ", "
        $ NEL.toList selects

    Attr_HxSwap swap ->
      Just
        . buildAttribute "hx-swap"
        . Render.textToBytesBuilder
        $ Types.swapToText swap

    Attr_HxSwapOOB mbSwap ->
      Just
        . buildAttribute "hx-swap-oob"
        . maybe "true" (Render.textToBytesBuilder . Types.outOfBandSwapToText)
        $ mbSwap

    Attr_HxTarget target ->
      Just
        . buildAttribute "hx-target"
        . Render.textToBytesBuilder
        $ Types.hxTargetToText target

    Attr_HxTrigger triggers ->
      Just
        . buildAttribute "hx-trigger"
        . Render.foldToBytesBuilderWithSeparator (Render.textToBytesBuilder . Types.triggerToText) ", "
        $ NEL.toList triggers

    Attr_HxValidate ->
      buildBooleanAttribute "hx-validate" True

    Attr_HxVals vals ->
      Just
        . buildAttribute "hx-vals"
        . Escape.attributeBytesBuilder
        $ Types.htmxValsToText vals

    -- Other
    --
    Attr_HyperScript hyperscript ->
      Just
        . buildAttribute "_"
        . Render.textToBytesBuilder
        $ Types.hyperScriptToText hyperscript

buildAttribute :: Builder -> Builder -> Builder
buildAttribute attr value =
  attr <> "=\"" <> value <> "\""

buildBooleanAttribute :: Builder -> Bool -> Maybe Builder
buildBooleanAttribute attr =
  B.bool Nothing (Just attr)

renderPushURLBuilder :: Types.PushURL -> Builder
renderPushURLBuilder =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Types.AbsoluteURL      Types.absoluteURLToBytesBuilder
      . Shrubbery.branch @(Types.RelativeURL _)  Types.relativeURLToBytesBuilder
      . Shrubbery.branch @Bool                   Render.enumBoolToBytesBuilder
      . Shrubbery.branch @Types.RawURL           Types.rawURLToBytesBuilder
      $ Shrubbery.branchEnd
  ) . Types.unPushURL
