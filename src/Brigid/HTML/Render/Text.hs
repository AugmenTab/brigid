{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Render.Text
  ( renderHTML
  , renderLazyHTML
  ) where

import Prelude hiding (id, max, min, span)
import Data.Bool qualified as B
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Builder.Linear (Builder, fromText, runBuilder)
import Ogma qualified
import Shrubbery qualified

import Brigid.HTML.Attributes.Event.Event (eventAttributeToText)
import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Elements.Internal (ChildHTML (..))
import Brigid.HTML.Types qualified as Types
import Brigid.Internal.Escape qualified as Escape
import Brigid.Internal.Render qualified as Render
import Brigid.Types qualified as Types
import Brigid.Types.URL (RelativeURL(..))

renderHTML :: ChildHTML parent grandparent -> T.Text
renderHTML = runBuilder . renderTag

renderLazyHTML :: ChildHTML parent grandparent -> TL.Text
renderLazyHTML = TL.fromStrict . runBuilder . renderTag

renderTag :: ChildHTML parent grandparent -> Builder
renderTag html =
  case html of
    Tag_NoElement ->
      mempty

    Tag_Comment comment ->
      fromText "<!-- " <> fromText comment <> fromText " -->"

    Tag_Text content ->
      fromText $ Escape.escape content

    Tag_Entity entity ->
      fromText $ T.pack entity

    Tag_RawHTML content ->
      fromText content

    Tag_CustomHTML elemName attrs eiCloserOrContent ->
      buildTag elemName attrs eiCloserOrContent

    Tag_Anchor attrs content ->
      buildContentTag "a" attrs content

    Tag_Abbreviation attrs content ->
      buildContentTag "abbr" attrs content

    Tag_ContactAddress attrs content ->
      buildContentTag "address" attrs content

    Tag_Area attrs ->
      buildVoidTag "area" attrs Types.OmitTag

    Tag_Article attrs content ->
      buildContentTag "article" attrs content

    Tag_Aside attrs content ->
      buildContentTag "aside" attrs content

    Tag_Audio attrs content ->
      buildContentTag "audio" attrs content

    Tag_BringAttentionTo attrs content ->
      buildContentTag "b" attrs content

    Tag_Base attrs ->
      buildVoidTag "base" attrs Types.OmitTag

    Tag_BidirectionalIsolation attrs content ->
      buildContentTag "bdi" attrs content

    Tag_BidirectionalOverride attrs content ->
      buildContentTag "bdo" attrs content

    Tag_Blockquote attrs content ->
      buildContentTag "blockquote" attrs content

    Tag_Body attrs content ->
      buildContentTag "body" attrs content

    Tag_LineBreak attrs ->
      buildVoidTag "br" attrs Types.OmitTag

    Tag_Button attrs content ->
      buildContentTag "button" attrs content

    Tag_Canvas attrs content ->
      buildContentTag "canvas" attrs content

    Tag_TableCaption attrs content ->
      buildContentTag "caption" attrs content

    Tag_Citation attrs content ->
      buildContentTag "cite" attrs content

    Tag_Code attrs content ->
      buildContentTag "code" attrs content

    Tag_TableColumn attrs ->
      buildVoidTag "col" attrs Types.OmitTag

    Tag_TableColumnGroup attrs content ->
      buildContentTag "colgroup" attrs content

    Tag_Data attrs content ->
      buildContentTag "data" attrs content

    Tag_DataList attrs content ->
      buildContentTag "datalist" attrs content

    Tag_DescriptionDetails attrs content ->
      buildContentTag "dd" attrs content

    Tag_DeletedText attrs content ->
      buildContentTag "del" attrs content

    Tag_Details attrs content ->
      buildContentTag "details" attrs content

    Tag_Definition attrs content ->
      buildContentTag "dfn" attrs content

    Tag_Dialog attrs content ->
      buildContentTag "dialog" attrs content

    Tag_Division attrs content ->
      buildContentTag "div" attrs content

    Tag_DescriptionList attrs content ->
      buildContentTag "dl" attrs content

    Tag_DescriptionTerm attrs content ->
      buildContentTag "dt" attrs content

    Tag_Emphasis attrs content ->
      buildContentTag "em" attrs content

    Tag_Embed attrs ->
      buildVoidTag "embed" attrs Types.OmitTag

    Tag_Fieldset attrs content ->
      buildContentTag "fieldset" attrs content

    Tag_FigureCaption attrs content ->
      buildContentTag "figcaption" attrs content

    Tag_Figure attrs content ->
      buildContentTag "figure" attrs content

    Tag_Footer attrs content ->
      buildContentTag "footer" attrs content

    Tag_Form attrs content ->
      buildContentTag "form" attrs content

    Tag_H1 attrs content ->
      buildContentTag "h1" attrs content

    Tag_H2 attrs content ->
      buildContentTag "h2" attrs content

    Tag_H3 attrs content ->
      buildContentTag "h3" attrs content

    Tag_H4 attrs content ->
      buildContentTag "h4" attrs content

    Tag_H5 attrs content ->
      buildContentTag "h5" attrs content

    Tag_H6 attrs content ->
      buildContentTag "h6" attrs content

    Tag_Head attrs content ->
      buildContentTag "head" attrs content

    Tag_Header attrs content ->
      buildContentTag "header" attrs content

    Tag_HeadingGroup attrs content ->
      buildContentTag "hgroup" attrs content

    Tag_HorizontalRule attrs ->
      buildVoidTag "hr" attrs Types.OmitTag

    Tag_Html attrs content ->
      fromText "<!DOCTYPE html>"
        <> buildContentTag "html" attrs content

    Tag_IdiomaticText attrs content ->
      buildContentTag "i" attrs content

    Tag_IFrame attrs ->
      buildVoidTag "iframe" attrs Types.WithTag

    Tag_Image attrs ->
      buildVoidTag "img" attrs Types.OmitTag

    Tag_Input attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputButton attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputCheckbox attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputColor attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputDate attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputDatetimeLocal attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputEmail attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputFile attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputHidden attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputImage attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputMonth attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputNumber attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputPassword attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputRadio attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputRange attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputReset attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputSearch attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputSubmit attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputTel attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputText attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputTime attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputUrl attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InputWeek attrs ->
      buildVoidTag "input" attrs Types.OmitTag

    Tag_InsertedText attrs content ->
      buildContentTag "ins" attrs content

    Tag_KeyboardInput attrs content ->
      buildContentTag "kbd" attrs content

    Tag_Label attrs content ->
      buildContentTag "label" attrs content

    Tag_Legend attrs content ->
      buildContentTag "legend" attrs content

    Tag_ListItem attrs content ->
      buildContentTag "li" attrs content

    Tag_Link attrs ->
      buildVoidTag "link" attrs Types.OmitTag

    Tag_Main attrs content ->
      buildContentTag "main" attrs content

    Tag_Map attrs content ->
      buildContentTag "map" attrs content

    Tag_Mark attrs content ->
      buildContentTag "mark" attrs content

    Tag_Menu attrs content ->
      buildContentTag "menu" attrs content

    Tag_Meta attrs ->
      buildVoidTag "meta" attrs Types.OmitTag

    Tag_Meter attrs content ->
      buildContentTag "meter" attrs content

    Tag_Nav attrs content ->
      buildContentTag "nav" attrs content

    Tag_NoScript attrs content ->
      buildContentTag "noscript" attrs content

    Tag_Object attrs content ->
      buildContentTag "object" attrs content

    Tag_OrderedList attrs content ->
      buildContentTag "ol" attrs content

    Tag_OptionGroup attrs content ->
      buildContentTag "optgroup" attrs content

    Tag_Option attrs content ->
      buildContentTag "option" attrs content

    Tag_Output attrs content ->
      buildContentTag "output" attrs content

    Tag_Paragraph attrs content ->
      buildContentTag "p" attrs content

    Tag_Picture attrs content ->
      buildContentTag "picture" attrs content

    Tag_PreformattedText attrs content ->
      buildContentTag "pre" attrs content

    Tag_Progress attrs content ->
      buildContentTag "progress" attrs content

    Tag_Quotation attrs content ->
      buildContentTag "q" attrs content

    Tag_RubyParenthesis attrs content ->
      buildContentTag "rp" attrs content

    Tag_RubyText attrs content ->
      buildContentTag "rt" attrs content

    Tag_Ruby attrs content ->
      buildContentTag "ruby" attrs content

    Tag_Strikethrough attrs content ->
      buildContentTag "s" attrs content

    Tag_Sample attrs content ->
      buildContentTag "samp" attrs content

    Tag_Script attrs mbScript ->
      buildTag "script" attrs $
        maybe
          (Left Types.WithTag)
          (Right . L.singleton . Tag_RawHTML . NET.toText)
          mbScript

    Tag_Search attrs content ->
      buildContentTag "search" attrs content

    Tag_Section attrs content ->
      buildContentTag "section" attrs content

    Tag_Select attrs content ->
      buildContentTag "select" attrs content

    Tag_Slot attrs content ->
      buildContentTag "slot" attrs content

    Tag_SideComment attrs content ->
      buildContentTag "small" attrs content

    Tag_Source attrs ->
      buildVoidTag "source" attrs Types.OmitTag

    Tag_Span attrs content ->
      buildContentTag "span" attrs content

    Tag_Strong attrs content ->
      buildContentTag "strong" attrs content

    Tag_Style attrs content ->
      buildContentTag "style" attrs . L.singleton $ Tag_RawHTML content

    Tag_Subscript attrs content ->
      buildContentTag "sub" attrs content

    Tag_Summary attrs content ->
      buildContentTag "summary" attrs content

    Tag_Superscript attrs content ->
      buildContentTag "sup" attrs content

    Tag_Table attrs content ->
      buildContentTag "table" attrs content

    Tag_TableBody attrs content ->
      buildContentTag "tbody" attrs content

    Tag_TableDataCell attrs content ->
      buildContentTag "td" attrs content

    Tag_ContentTemplate attrs content ->
      buildContentTag "template" attrs content

    Tag_TextArea attrs content ->
      buildContentTag "textarea" attrs content

    Tag_TableFoot attrs content ->
      buildContentTag "tfoot" attrs content

    Tag_TableHeader attrs content ->
      buildContentTag "th" attrs content

    Tag_TableHead attrs content ->
      buildContentTag "thead" attrs content

    Tag_Time attrs content ->
      buildContentTag "time" attrs content

    Tag_Title attrs content ->
      buildContentTag "title" attrs content

    Tag_TableRow attrs content ->
      buildContentTag "tr" attrs content

    Tag_Track attrs ->
      buildVoidTag "track" attrs Types.OmitTag

    Tag_Underline attrs content ->
      buildContentTag "u" attrs content

    Tag_UnorderedList attrs content ->
      buildContentTag "ul" attrs content

    Tag_Variable attrs content ->
      buildContentTag "var" attrs content

    Tag_Video attrs content ->
      buildContentTag "video" attrs content

    Tag_WordBreakOpportunity attrs ->
      buildVoidTag "wbr" attrs Types.OmitTag

    Tag_Group attrs content ->
      buildContentTag "g" attrs content

    Tag_Path attrs content ->
      buildContentTag "path" attrs content

    Tag_SVG attrs content ->
      buildContentTag "svg" attrs content

buildTag :: T.Text
         -> [Attribute tag]
         -> Either Types.NoContent [ChildHTML parent grandparent]
         -> Builder
buildTag tag attrs eiCloserOrContent =
  case eiCloserOrContent of
    Left  closer   -> buildVoidTag tag attrs closer
    Right children -> buildContentTag tag attrs children

buildVoidTag :: T.Text -> [Attribute tag] -> Types.NoContent -> Builder
buildVoidTag tag attrs closer =
  fromText "<"
    <> fromText tag
    <> foldMap (\attr -> maybe mempty (fromText " " <>) (renderAttribute attr)) attrs
    <> case closer of
         Types.OmitTag -> fromText "/>"
         Types.WithTag -> fromText ">" <> fromText "</" <> fromText tag <> fromText ">"

buildContentTag :: T.Text -> [Attribute tag] -> [ChildHTML parent grandparent] -> Builder
buildContentTag tag attrs children =
  fromText "<"
    <> fromText tag
    <> foldMap (\attr -> maybe mempty (fromText " " <>) (renderAttribute attr)) attrs
    <> fromText ">"
    <> foldMap renderTag children
    <> fromText "</"
    <> fromText tag
    <> fromText ">"

renderAttribute :: Attribute any -> Maybe Builder
renderAttribute attr =
  case attr of
    -- Global Attributes
    --
    Attr_NoAttribute ->
      Nothing

    Attr_Custom name value ->
      Just . buildAttribute name $ Escape.attributeText value

    Attr_AccessKey key ->
      Just . buildAttribute "accesskey" $ Escape.attributeCharText key

    Attr_Autocapitalize option ->
      Just
        . buildAttribute "autocapitalize"
        $ Types.autocapitalizeOptionToText option

    Attr_Autocorrect autocorrect ->
      Just . buildAttribute "autocorrect" $ Types.onOffToText autocorrect

    Attr_Autofocus autofocus ->
      buildBooleanAttribute "autofocus" autofocus

    Attr_Class _class ->
      Just
        . buildAttribute "class"
        . Escape.attributeText
        $ Types.classToText _class

    Attr_ContentEditable option ->
      Just
        . buildAttribute "contenteditable"
        $ Types.contentEditableOptionToText option

    Attr_CustomData data_ mbValue ->
      case mbValue of
        Just value ->
          Just $
            buildAttribute
              ("data-" <> data_)
              (Escape.attributeText value)

        Nothing ->
          buildBooleanAttribute ("data-" <> data_) True

    Attr_CustomBoolean name value ->
      buildBooleanAttribute name value

    Attr_Dir directionality ->
      Just
        . buildAttribute "dir"
        $ Types.directionalityToText directionality

    Attr_Draggable draggable ->
      Just . buildAttribute "draggable" $ Render.enumBoolToText draggable

    Attr_EnterKeyHint option ->
      Just
        . buildAttribute "enterkeyhint"
        $ Types.keyHintOptionToText option

    Attr_ExportParts parts ->
      Just
        . buildAttribute "exportparts"
        . Render.foldToTextWithSeparator Types.exportPartToText ", "
        $ parts

    Attr_Hidden hidden ->
      buildBooleanAttribute "hidden" hidden

    Attr_Id id ->
      Just . buildAttribute "id" . Escape.attributeText $ Types.idToText id

    Attr_Inert inert ->
      buildBooleanAttribute "inert" inert

    Attr_InputMode mode ->
      Just . buildAttribute "inputmode" $ Types.inputModeToText mode

    Attr_Is is ->
      Just . buildAttribute "is" $ Escape.attributeText is

    Attr_ItemId itemid ->
      Just $ buildAttribute "itemid" itemid

    Attr_ItemProp itemprop ->
      Just $ buildAttribute "itemprop" itemprop

    Attr_ItemRef itemref ->
      Just
        . buildAttribute "itemref"
        . Render.foldToTextWithSeparator Types.idToText " "
        $ NEL.toList itemref

    Attr_ItemScope ->
      buildBooleanAttribute "itemscope" True

    Attr_ItemType itemtype ->
      Just . buildAttribute "itemtype" $ Types.absoluteURLToText itemtype

    Attr_Lang lang ->
      Just . buildAttribute "lang" $ maybe "" Ogma.bcp_47ToText lang

    Attr_Nonce nonce ->
      Just $ buildAttribute "nonce" nonce

    Attr_Part parts ->
      Just
        . buildAttribute "part"
        . Render.foldToTextWithSeparator Types.partToText " "
        $ parts

    Attr_Popover state ->
      Just
        . buildAttribute "popover"
        $ Types.popoverStateToText state

    Attr_Role role ->
      Just . buildAttribute "role" $ Types.roleToText role

    Attr_Slot slot ->
      Just . buildAttribute "slot" $ Types.nameToText slot

    Attr_Spellcheck spellcheck ->
      Just . buildAttribute "spellcheck" $ Render.enumBoolToText spellcheck

    Attr_Style style ->
      Just . buildAttribute "style" $ Escape.attributeText style

    Attr_TabIndex tabindex ->
      Just . buildAttribute "tabindex" $ Render.showText tabindex

    Attr_Title title ->
      Just . buildAttribute "title" $ Escape.attributeText title

    Attr_Translate translate ->
      Just . buildAttribute "translate" $ Types.yesNoToText translate

    Attr_WritingSuggestions writingsuggestions ->
      Just
        . buildAttribute "writingsuggestions"
        $ Render.enumBoolToText writingsuggestions

    -- Scoped Attributes
    --
    Attr_Abbreviation abbr ->
      Just $ buildAttribute "abbr" abbr

    Attr_Accept accept ->
      Just . buildAttribute "accept" $ Render.bytesToText accept

    Attr_AcceptCharset ->
      Just $ buildAttribute "accept-charset" "UTF-8"

    Attr_Action action ->
      Just . buildAttribute "action" $ Types.actionToText action

    Attr_Allow allow ->
      Just
        . buildAttribute "allow"
        . Render.foldToTextWithSeparator Types.featurePolicyDirectiveToText "; "
        $ allow

    Attr_Alt alt ->
      Just $ buildAttribute "alt" alt

    Attr_As as ->
      Just . buildAttribute "as" $ Types.asToText as

    Attr_Async ->
      buildBooleanAttribute "async" True

    Attr_Autocomplete autocomplete ->
      Just
        . buildAttribute "autocomplete"
        $ Types.autocompleteTokenToText autocomplete

    Attr_Autoplay ->
      buildBooleanAttribute "autoplay" True

    Attr_Blocking blocking ->
      Just . buildAttribute "blocking" $ Types.blockOptionToText blocking

    Attr_Capture mbCapture ->
      maybe
        (buildBooleanAttribute "capture" True)
        (Just . buildAttribute "capture" . Types.captureMethodToText)
        mbCapture

    Attr_Charset ->
      Just $ buildAttribute "charset" "utf-8"

    Attr_Checked checked ->
      buildBooleanAttribute "checked" checked

    Attr_Cite cite ->
      Just . buildAttribute "cite" $ Types.urlToText cite

    Attr_Cols cols ->
      Just . buildAttribute "cols" $ Render.showText cols

    Attr_Colspan colspan ->
      Just . buildAttribute "colspan" $ Render.showText colspan

    Attr_Command command ->
      Just . buildAttribute "command" $ Types.commandOptionToText command

    Attr_CommandFor commandfor ->
      Just . buildAttribute "commandfor" $ Types.idToText commandfor

    Attr_Coords coords ->
      Just
        . buildAttribute "coords"
        . Render.foldToTextWithSeparator Render.showText ","
        $ NEL.toList coords

    Attr_Content content ->
      Just $ buildAttribute "content" content

    Attr_Controls ->
      buildBooleanAttribute "controls" True

    Attr_ControlsList controlslist ->
      Just
        . buildAttribute "controlslist"
        $ Types.controlsListToText controlslist

    Attr_CrossOrigin crossorigin ->
      Just
        . buildAttribute "crossorigin"
        $ Types.crossOriginFetchToText crossorigin

    Attr_Data _data ->
      Just . buildAttribute "data" $ Types.urlToText _data

    Attr_Datetime datetime ->
      Just . buildAttribute "datetime" $ T.pack datetime

    Attr_Decoding decoding ->
      Just . buildAttribute "decoding" $ Types.decodingToText decoding

    Attr_Default ->
      buildBooleanAttribute "default" True

    Attr_Defer ->
      buildBooleanAttribute "defer" True

    Attr_Dirname dirname ->
      Just $ buildAttribute "dirname" dirname

    Attr_Disabled disabled ->
      buildBooleanAttribute "disabled" disabled

    Attr_DisablePictureInPicture ->
      buildBooleanAttribute "disablepictureinpicture" True

    Attr_DisableRemotePlayback ->
      buildBooleanAttribute "disableremoteplayback" True

    Attr_Download download ->
      maybe
        (buildBooleanAttribute "download" True)
        (Just . buildAttribute "download" . NET.toText)
        download

    Attr_ElementTiming elementtiming ->
      Just $ buildAttribute "elementtiming" elementtiming

    Attr_Enctype enctype ->
      Just . buildAttribute "enctype" $ Render.bytesToText enctype

    Attr_FetchPriority fetchpriority ->
      Just
        . buildAttribute "fetchpriority"
        $ Types.fetchPriorityToText fetchpriority

    Attr_For for ->
      Just . buildAttribute "for" $ Types.forOptionToText for

    Attr_Form form ->
      Just . buildAttribute "form" $ Types.idToText form

    Attr_FormAction formaction ->
      Just . buildAttribute "formaction" $ Types.actionToText formaction

    Attr_FormEnctype formenctype ->
      Just . buildAttribute "formenctype" $ Render.bytesToText formenctype

    Attr_FormMethod formmethod ->
      Just . buildAttribute "formmethod" $ Types.formMethodToText formmethod

    Attr_FormNoValidate ->
      buildBooleanAttribute "formnovalidate" True

    Attr_FormTarget formtarget ->
      Just . buildAttribute "formtarget" $ Types.targetToText formtarget

    Attr_Headers headers ->
      Just
        . buildAttribute "headers"
        . Render.foldToTextWithSeparator Types.idToText " "
        $ headers

    Attr_Height height ->
      Just . buildAttribute "height" $ Render.showText height

    Attr_High high ->
      Just . buildAttribute "high" $ Types.numberToText high

    Attr_Href href ->
      Just . buildAttribute "href" $ Types.hrefToText href

    Attr_HrefLang hreflang ->
      Just . buildAttribute "hreflang" $ Ogma.bcp_47ToText hreflang

    Attr_HttpEquiv httpEquiv ->
      Just . buildAttribute "http-equiv" $ Types.httpEquivTokenToText httpEquiv

    Attr_ImageSizes imagesizes ->
      Just
        . buildAttribute "imagesizes"
        . Render.foldToTextWithSeparator Types.sizeToText ", "
        $ NEL.toList imagesizes

    Attr_ImageSrcset imagesrcset ->
      Just
        . buildAttribute "imagesrcset"
        . Render.foldToTextWithSeparator Types.srcsetCandidateToText ", "
        $ NEL.toList imagesrcset

    Attr_Integrity sha content ->
      Just . buildAttribute "integrity" $ Types.integrityToText sha content

    Attr_IsMap ->
      buildBooleanAttribute "ismap" True

    Attr_Kind kind ->
      Just . buildAttribute "kind" $ Types.trackKindToText kind

    Attr_Label label ->
      Just $ buildAttribute "label" label

    Attr_List list ->
      Just . buildAttribute "label" $ Types.idToText list

    Attr_Loading loading ->
      Just . buildAttribute "loading" $ Types.loadOptionToText loading

    Attr_Loop ->
      buildBooleanAttribute "loop" True

    Attr_Low low ->
      Just . buildAttribute "low" $ Types.numberToText low

    Attr_Max max ->
      Just . buildAttribute "max" $ Types.rangeBoundToText max

    Attr_MaxLength maxlength ->
      Just . buildAttribute "maxlength" $ Render.showText maxlength

    Attr_Media media ->
      Just
        . buildAttribute "media"
        . Render.foldToTextWithSeparator Types.mediaQueryToText ", "
        $ NEL.toList media

    Attr_Method method ->
      Just . buildAttribute "method" $ Types.formMethodToText method

    Attr_Min min ->
      Just . buildAttribute "min" $ Types.rangeBoundToText min

    Attr_MinLength minlength ->
      Just . buildAttribute "minlength" $ Render.showText minlength

    Attr_Multiple ->
      buildBooleanAttribute "multiple" True

    Attr_Muted muted ->
      buildBooleanAttribute "muted" muted

    Attr_Name name ->
      Just . buildAttribute "name" $ Types.nameOptionToText name

    Attr_NoModule nomodule ->
      buildBooleanAttribute "nomodule" nomodule

    Attr_NoValidate novalidate ->
      buildBooleanAttribute "novalidate" novalidate

    Attr_Open ->
      buildBooleanAttribute "open" True

    Attr_Optimum optimum ->
      Just . buildAttribute "optimum" $ Types.numberToText optimum

    Attr_Pattern pattern ->
      Just $ buildAttribute "pattern" pattern

    Attr_Ping pings ->
      Just
        . buildAttribute "ping"
        . Render.foldToTextWithSeparator Types.pingToText " "
        $ NEL.toList pings

    Attr_Placeholder placeholder ->
      Just $ buildAttribute "placeholder" placeholder

    Attr_PlaysInline playsinline ->
      buildBooleanAttribute "playsinline" playsinline

    Attr_PopoverTarget popovertarget ->
      Just . buildAttribute "popovertarget" $ Types.idToText popovertarget

    Attr_PopoverTargetAction popovertargetaction ->
      Just
        . buildAttribute "popovertargetaction"
        $ Types.popoverTargetActionToText popovertargetaction

    Attr_Poster poster ->
      Just . buildAttribute "poster" $ Types.urlToText poster

    Attr_Preload preload ->
      Just . buildAttribute "preload" $ Types.preloadToText preload

    Attr_ReadOnly ->
      buildBooleanAttribute "readonly" True

    Attr_ReferrerPolicy referrerpolicy ->
      Just
        . buildAttribute "referrerpolicy"
        $ Types.referrerPolicyToText referrerpolicy

    Attr_Rel rel ->
      Just . buildAttribute "rel" $ Types.relationshipToText rel

    Attr_Required required ->
      buildBooleanAttribute "required" required

    Attr_Reversed reversed ->
      buildBooleanAttribute "reversed" reversed

    Attr_Rows rows ->
      Just . buildAttribute "rows" $ Render.showText rows

    Attr_Rowspan rowspan ->
      Just . buildAttribute "rowspan" $ Render.showText rowspan

    Attr_Sandbox sandbox ->
      if null sandbox
        then buildBooleanAttribute "sandbox" True
        else
          Just
            . buildAttribute "sandbox"
            . Render.foldToTextWithSeparator Types.sandboxTokenToText " "
            $ sandbox

    Attr_Scope scope ->
      Just . buildAttribute "scope" $ Types.scopeToText scope

    Attr_Selected selected ->
      buildBooleanAttribute "selected" selected

    Attr_ShadowRootMode shadowrootmode ->
      Just
        . buildAttribute "shadowrootmode"
        $ Types.openClosedToText shadowrootmode

    Attr_ShadowRootDelegatesFocus ->
      buildBooleanAttribute "shadowrootdelegatesfocus" True

    Attr_ShadowRootClonable ->
      buildBooleanAttribute "shadowrootclonable" True

    Attr_Shape shape ->
      Just . buildAttribute "shape" $ Types.shapeToText shape

    Attr_Size size ->
      Just . buildAttribute "size" $ Render.showText size

    Attr_Sizes sizes ->
      Just
        . buildAttribute "sizes"
        . Render.foldToTextWithSeparator Types.sizeToText ", "
        $ NEL.toList sizes

    Attr_Span span ->
      Just . buildAttribute "span" $ Render.showText span

    Attr_Src src ->
      Just . buildAttribute "src" $ Types.urlToText src

    Attr_SrcDoc srcdoc ->
      Just
        . buildAttribute "srcdoc"
        . Render.lazyBytesToText
        $ Escape.attributeBytes srcdoc

    Attr_SrcLang srclang ->
      Just . buildAttribute "srclang" $ Ogma.bcp_47ToText srclang

    Attr_SrcSet srcset ->
      Just
        . buildAttribute "srcset"
        . Render.foldToTextWithSeparator Types.srcsetCandidateToText ", "
        $ NEL.toList srcset

    Attr_Start start ->
      Just . buildAttribute "start" $ Render.showText start

    Attr_Step step ->
      Just . buildAttribute "step" $ Types.stepToText step

    Attr_Target target ->
      Just . buildAttribute "target" $ Types.targetToText target

    Attr_Type type_ ->
      Just . buildAttribute "type" $ Types.typeOptionToText type_

    Attr_UseMap usemap ->
      Just . buildAttribute "usemap" . T.cons '#' $ Types.nameToText usemap

    Attr_Value value ->
      Just . buildAttribute "value" $ Types.valueToText value

    Attr_Width width ->
      Just . buildAttribute "width" $ Render.showText width

    Attr_Wrap wrap ->
      Just . buildAttribute "wrap" $ Types.wrapToText wrap

    Attr_XMLNS xmlns ->
      Just . buildAttribute "xmlns" $ Types.urlToText xmlns

    -- ARIA Attributes
    --
    Attr_Aria aria ->
      Just $
        buildAttribute
          (Types.ariaAttributeToText aria)
          (Types.ariaValueToText aria)

    -- Event Attributes
    --
    Attr_On event script ->
      Just $
        buildAttribute
          (eventAttributeToText event)
          (Types.rawJavaScriptToText script)

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

       in Just $ buildAttribute hxAttr hxPath

    Attr_HxBoost boosted ->
      Just . buildAttribute "hx-boost" $ Render.enumBoolToText boosted

    Attr_HxConfirm confirmation ->
      Just . buildAttribute "hx-confirm" $ Escape.attributeText confirmation

    Attr_HxDisable disabled ->
      buildBooleanAttribute "hx-disable" disabled

    Attr_HxDisabledElt disabled ->
      Just
        . buildAttribute "hx-disabled-elt"
        . Render.foldToTextWithSeparator Types.disabledSelectorToText ", "
        $ NEL.toList disabled

    Attr_HxDisinherit disinherit ->
      Just . buildAttribute "hx-disinherit" $ Types.disinheritToText disinherit

    Attr_HxEncoding ->
      Just $ buildAttribute "hx-encoding" "multipart/form-data"

    Attr_HxExt exts ->
      Just
        . buildAttribute "hx-ext"
        . Render.foldToTextWithSeparator Types.extensionToText ","
        $ NEL.toList exts

    Attr_HxHeaders headers ->
      Just
        . buildAttribute "hx-headers"
        . Escape.attributeText
        $ Types.htmxHeadersToText headers

    Attr_HxHistory ->
      Just $ buildAttribute "hx-history" "false"

    Attr_HxHistoryElt ->
      buildBooleanAttribute "hx-history" True

    Attr_HxInclude include ->
      Just . buildAttribute "hx-include" $ Types.includeSelectorToText include

    Attr_HxIndicator indicator ->
      Just . buildAttribute "hx-indicator" $ Types.indicatorToText indicator

    Attr_HxOn event action ->
      Just
        . buildAttribute ("hx-on" <> Types.hxOnEventText event)
        $ Escape.attributeText action

    Attr_HxParams params ->
      Just
        . buildAttribute "hx-params"
        . Escape.attributeText
        $ Types.requestParamsToText params

    Attr_HxPreserve preserved ->
      buildBooleanAttribute "hx-preserve" preserved

    Attr_HxPrompt prompt ->
      Just . buildAttribute "hx-prompt" $ Escape.attributeText prompt

    Attr_HxPushURL url ->
      Just . buildAttribute "hx-push-url" $ renderPushURL url

    Attr_HxReplaceURL url ->
      Just . buildAttribute "hx-replace-url" $ renderPushURL url

    Attr_HxSelect selector ->
      Just
        . buildAttribute "hx-select"
        . Escape.attributeText
        $ Types.querySelectorToText selector

    Attr_HxSelectOOB selects ->
      Just
        . buildAttribute "hx-select-oob"
        . Render.foldToTextWithSeparator Types.outOfBandSelectToText ", "
        $ NEL.toList selects

    Attr_HxSwap swap ->
      Just . buildAttribute "hx-swap" $ Types.swapToText swap

    Attr_HxSwapOOB mbSwap ->
      Just
        . buildAttribute "hx-swap-oob"
        $ maybe "true" Types.outOfBandSwapToText mbSwap

    Attr_HxTarget target ->
      Just . buildAttribute "hx-target" $ Types.hxTargetToText target

    Attr_HxTrigger triggers ->
      Just
        . buildAttribute "hx-trigger"
        . Render.foldToTextWithSeparator Types.triggerToText ", "
        $ NEL.toList triggers

    Attr_HxValidate ->
      buildBooleanAttribute "hx-validate" True

    Attr_HxVals vals ->
      Just . buildAttribute "hx-vals" $ Escape.attributeText (Types.htmxValsToText vals)

    -- Other
    --
    Attr_HyperScript hyperscript ->
      Just . buildAttribute "_" $ Types.hyperScriptToText hyperscript

buildAttribute :: T.Text -> T.Text -> Builder
buildAttribute attr value =
  fromText attr <> fromText "=\"" <> fromText value <> fromText "\""

buildBooleanAttribute :: T.Text -> Bool -> Maybe Builder
buildBooleanAttribute attr =
  B.bool Nothing (Just $ fromText attr)

renderPushURL :: Types.PushURL -> T.Text
renderPushURL =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Types.AbsoluteURL Types.absoluteURLToText
      . Shrubbery.branch @(Types.RelativeURL _) Types.relativeURLToText
      . Shrubbery.branch @Bool Render.enumBoolToText
      . Shrubbery.branch @Types.RawURL Types.rawURLToText
      $ Shrubbery.branchEnd
  ) . Types.unPushURL
