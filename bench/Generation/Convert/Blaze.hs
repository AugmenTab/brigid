module Generation.Convert.Blaze
  ( toBlaze
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Ogma qualified
import Prelude hiding (div, max, min, span)
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import Brigid.HTML.Attributes.Event.Event (Event, eventAttributeToText)
import Brigid.HTML.Generation (Element (..), ElementNode (..))
import Brigid.HTML.Generation.Attributes qualified as GA
import Brigid.HTML.Generation.Elements (ElementType (..))
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

toBlaze :: Element -> Html
toBlaze (Element tag attrs node) =
  let
    attributes = foldMap toAttribute attrs
    content =
      case node of
        Branch nodes -> foldMap toBlaze nodes
        Leaf txt -> H.toHtml $ NET.toText txt
        Void -> mempty
  in
    case tag of
      Comment
        | Leaf txt <- node -> H.textComment $ NET.toText txt
        | otherwise -> mempty
      Anchor -> H.a ! attributes $ content
      Abbreviation -> H.abbr ! attributes $ content
      ContactAddress -> H.address ! attributes $ content
      Area -> H.area ! attributes
      Article -> H.article ! attributes $ content
      Aside -> H.aside ! attributes $ content
      Audio -> H.audio ! attributes $ content
      BringAttentionTo -> H.b ! attributes $ content
      Base -> H.base ! attributes
      BidirectionalIsolation -> H.bdi ! attributes $ content
      BidirectionalOverride -> H.bdo ! attributes $ content
      Blockquote -> H.blockquote ! attributes $ content
      Body -> H.body ! attributes $ content
      LineBreak -> H.br ! attributes
      Button -> H.button ! attributes $ content
      Canvas -> H.canvas ! attributes $ content
      TableCaption -> H.caption ! attributes $ content
      Citation -> H.cite ! attributes $ content
      Code -> H.code ! attributes $ content
      TableColumn -> H.col ! attributes
      TableColumnGroup -> H.colgroup ! attributes $ content
      Data -> H.data_ ! attributes $ content
      DataList -> H.datalist ! attributes $ content
      DescriptionDetails -> H.dd ! attributes $ content
      DeletedText -> H.del ! attributes $ content
      Details -> H.details ! attributes $ content
      Definition -> H.dfn ! attributes $ content
      Dialog -> H.dialog ! attributes $ content
      Division -> H.div ! attributes $ content
      DescriptionList -> H.dl ! attributes $ content
      DescriptionTerm -> H.dt ! attributes $ content
      Emphasis -> H.em ! attributes $ content
      Embed -> H.embed ! attributes
      Fieldset -> H.fieldset ! attributes $ content
      FigureCaption -> H.figcaption ! attributes $ content
      Figure -> H.figure ! attributes $ content
      Footer -> H.footer ! attributes $ content
      Form -> H.form ! attributes $ content
      H1 -> H.h1 ! attributes $ content
      H2 -> H.h2 ! attributes $ content
      H3 -> H.h3 ! attributes $ content
      H4 -> H.h4 ! attributes $ content
      H5 -> H.h5 ! attributes $ content
      H6 -> H.h6 ! attributes $ content
      Head -> H.head ! attributes $ content
      Header -> H.header ! attributes $ content
      HeadingGroup -> H.hgroup ! attributes $ content
      HorizontalRule -> H.hr ! attributes
      Html -> H.html ! attributes $ content
      IdiomaticText -> H.i ! attributes $ content
      IFrame -> H.iframe ! attributes $ content
      Image -> H.img ! attributes
      Input -> H.input ! attributes
      InsertedText -> H.ins ! attributes $ content
      KeyboardInput -> H.kbd ! attributes $ content
      Label -> H.label ! attributes $ content
      Legend -> H.legend ! attributes $ content
      ListItem -> H.li ! attributes $ content
      Link -> H.link ! attributes
      Main -> H.main ! attributes $ content
      Map -> H.map ! attributes $ content
      Mark -> H.mark ! attributes $ content
      Menu -> H.menu ! attributes $ content
      Meta -> H.meta ! attributes
      Meter -> H.meter ! attributes $ content
      Nav -> H.nav ! attributes $ content
      NoScriptHead -> H.noscript ! attributes $ content
      NoScriptBody -> H.noscript ! attributes $ content
      Object -> H.object ! attributes $ content
      OrderedList -> H.ol ! attributes $ content
      OptionGroup -> H.optgroup ! attributes $ content
      Option -> H.option ! attributes $ content
      Output -> H.output ! attributes $ content
      Paragraph -> H.p ! attributes $ content
      Picture -> H.picture ! attributes $ content
      PreformattedText -> H.pre ! attributes $ content
      Progress -> H.progress ! attributes $ content
      Quotation -> H.q ! attributes $ content
      RubyParenthesis -> H.rp ! attributes $ content
      RubyText -> H.rt ! attributes $ content
      Ruby -> H.ruby ! attributes $ content
      Strikethrough -> H.s ! attributes $ content
      Sample -> H.samp ! attributes $ content
      Script -> H.script ! attributes $ content
      Search -> H.search ! attributes $ content
      Section -> H.section ! attributes $ content
      Select -> H.select ! attributes $ content
      Slot -> H.slot ! attributes $ content
      SideComment -> H.small ! attributes $ content
      Source -> H.source ! attributes
      Span -> H.span ! attributes $ content
      Strong -> H.strong ! attributes $ content
      Style -> H.style ! attributes $ content
      Subscript -> H.sub ! attributes $ content
      Summary -> H.summary ! attributes $ content
      Superscript -> H.sup ! attributes $ content
      Table -> H.table ! attributes $ content
      TableBody -> H.tbody ! attributes $ content
      TableDataCell -> H.td ! attributes $ content
      ContentTemplate -> H.template ! attributes $ content
      TextArea -> H.textarea ! attributes $ content
      TableFoot -> H.tfoot ! attributes $ content
      TableHeader -> H.th ! attributes $ content
      TableHead -> H.thead ! attributes $ content
      Time -> H.time ! attributes $ content
      Title -> H.title ! attributes $ content
      TableRow -> H.tr ! attributes $ content
      Track -> H.track ! attributes
      Underline -> H.u ! attributes $ content
      UnorderedList -> H.ul ! attributes $ content
      Variable -> H.var ! attributes $ content
      Video -> H.video ! attributes $ content
      WordBreakOpportunity -> H.wbr ! attributes

toAttribute :: GA.Attribute -> H.Attribute
toAttribute attr =
  case attr of
    -- Global Attributes
    --
    GA.AccessKey accesskey ->
      A.accesskey $ H.toValue accesskey

    GA.Autocapitalize autocapitalize ->
      H.customAttribute "autocapitalize" $
        textToBlaze Types.autocapitalizeOptionToText autocapitalize

    GA.Autocorrect autocorrect ->
      H.customAttribute "autocorrect" $
        textToBlaze Types.onOffToText autocorrect

    GA.Autofocus autofocus ->
      booleanToBlaze A.autofocus autofocus

    GA.Class class_ ->
      A.class_ $ textToBlaze Types.classToText class_

    GA.ContentEditable contentEditable ->
      A.contenteditable $
        textToBlaze Types.contentEditableOptionToText contentEditable

    GA.CustomData data_ val ->
      H.customAttribute (H.textTag $ "data-" <> data_) $ textToBlaze id val

    GA.Dir dir ->
      A.dir $ textToBlaze Types.directionalityToText dir

    GA.Draggable draggable ->
      A.draggable $ boolToBlaze draggable

    GA.EnterKeyHint enterkeyhint ->
      H.customAttribute "enterkeyhint" $
        textToBlaze Types.keyHintOptionToText enterkeyhint

    GA.ExportParts exportparts ->
      H.customAttribute "exportparts" $
        foldToBlazeWithSeparator Types.exportPartToText ", " exportparts

    GA.Hidden hidden ->
      booleanToBlaze A.hidden hidden

    GA.Id id_ ->
      H.customAttribute "id_" $ textToBlaze Types.idToText id_

    GA.Inert inert ->
      booleanToBlaze (H.customAttribute "inert") inert

    GA.InputMode inputmode ->
      H.customAttribute "inputmode" $
        textToBlaze Types.inputModeToText inputmode

    GA.Is is ->
      H.customAttribute "is" $ textToBlaze id is

    GA.ItemId itemId ->
      H.customAttribute "itemid" $ textToBlaze id itemId

    GA.ItemProp itemprop ->
      A.itemprop $ textToBlaze id itemprop

    GA.ItemRef itemref ->
      H.customAttribute "itemref"
        . foldToBlazeWithSeparator Types.idToText " "
        $ NEL.toList itemref

    GA.ItemScope ->
      A.itemscope mempty

    GA.ItemType itemtype ->
      A.itemtype $ textToBlaze Types.absoluteURLToText itemtype

    GA.Lang lang ->
      A.lang $ maybe mempty (textToBlaze Ogma.bcp_47ToText) lang

    GA.Nonce nonce ->
      H.customAttribute "nonce" $ textToBlaze id nonce

    GA.Part part ->
      H.customAttribute "part" $
        foldToBlazeWithSeparator Types.partToText " " part

    GA.Popover popover ->
      H.customAttribute "popover" $
        textToBlaze Types.popoverStateToText popover

    GA.Role role ->
      A.role $ textToBlaze Types.roleToText role

    GA.Slot slot ->
      H.customAttribute "slot" $ textToBlaze Types.nameToText slot

    GA.Spellcheck spellcheck ->
      A.spellcheck $ boolToBlaze spellcheck

    GA.Style style ->
      A.style $ textToBlaze id style

    GA.TabIndex tabindex ->
      A.tabindex $ showBlaze tabindex

    GA.Title title ->
      A.title $ textToBlaze id title

    GA.Translate translate ->
      H.customAttribute "translate" $
        textToBlaze Types.yesNoToText translate

    GA.WritingSuggestions writingsuggestions ->
      H.customAttribute "writingsuggestions" $ boolToBlaze writingsuggestions

    -- Scoped Attributes
    --
    GA.Abbreviation abbr ->
      H.customAttribute "abbr" $ textToBlaze id abbr

    GA.Accept accept ->
      A.accept $ H.unsafeByteStringValue accept

    GA.AcceptCharset ->
      A.acceptCharset "UTF-8"

    GA.Action action ->
      A.action $ textToBlaze Types.rawURLToText action

    GA.Allow allow ->
      H.customAttribute "allow" $
        foldToBlazeWithSeparator Types.featurePolicyDirectiveToText "; " allow

    GA.As as ->
      H.customAttribute "as" $ textToBlaze Types.asToText as

    GA.Alt alt ->
      A.alt $ textToBlaze id alt

    GA.Async ->
      A.async "true"

    GA.Autocomplete autocomplete ->
      A.autocomplete $ textToBlaze Types.onOffToText autocomplete

    GA.Autoplay ->
      A.autoplay "true"

    GA.Blocking blocking ->
      H.customAttribute "blocking" $
        textToBlaze Types.blockOptionToText blocking

    GA.Capture capture ->
      H.customAttribute "capture" $
        maybe mempty (textToBlaze Types.captureMethodToText) capture

    GA.Charset ->
      A.charset "utf-8"

    GA.Checked checked ->
      booleanToBlaze A.checked checked

    GA.Cite cite ->
      A.cite $ textToBlaze Types.rawURLToText cite

    GA.Cols cols ->
      A.cols $ showBlaze cols

    GA.Colspan colspan ->
      A.colspan $ showBlaze colspan

    GA.Command command ->
      H.customAttribute "command" $
        textToBlaze Types.commandOptionToText command

    GA.CommandFor commandfor ->
      H.customAttribute "commandfor" $ textToBlaze Types.idToText commandfor

    GA.Content content ->
      A.content $ textToBlaze id content

    GA.Controls ->
      A.controls "true"

    GA.ControlsList controlslist ->
      H.customAttribute "controlslist" $
        textToBlaze Types.controlsListToText controlslist

    GA.Coords coords ->
      A.coords . foldToBlazeShowWithSeparator "," $ NEL.toList coords

    GA.CrossOrigin crossorigin ->
      H.customAttribute "crossorigin" $
        textToBlaze Types.crossOriginFetchToText crossorigin

    GA.Data data_ ->
      A.data_ $ textToBlaze Types.rawURLToText data_

    GA.Datetime datetime ->
      H.customAttribute "datetime" $
        textToBlaze (T.pack . iso8601Show) datetime

    GA.Decoding decoding ->
      H.customAttribute "decoding" $ textToBlaze Types.decodingToText decoding

    GA.Default ->
      H.customAttribute "default" "true"

    GA.Defer ->
      A.defer "true"

    GA.Dirname dirname ->
      H.customAttribute "dirname" $ textToBlaze id dirname

    GA.Disabled disabled ->
      booleanToBlaze A.disabled disabled

    GA.DisablePictureInPicture ->
      H.customAttribute "disablepictureinpicture" "true"

    GA.DisableRemotePlayback ->
      H.customAttribute "disableremoteplayback" "true"

    GA.Download download ->
      A.download $ maybe mempty (textToBlaze NET.toText) download

    GA.ElementTiming elementtiming ->
      H.customAttribute "elementtiming" $ textToBlaze id elementtiming

    GA.Enctype enctype ->
      A.enctype $ H.unsafeByteStringValue enctype

    GA.FetchPriority fetchpriority ->
      H.customAttribute "fetchpriority" $
        textToBlaze Types.fetchPriorityToText fetchpriority

    GA.ForLabel for ->
      A.for $ textToBlaze Types.idToText for

    GA.ForOutput for ->
      A.for . foldToBlazeWithSeparator Types.idToText " " $ NEL.toList for

    GA.Form form ->
      A.form $ textToBlaze Types.idToText form

    GA.FormAction formaction ->
      A.formaction $ textToBlaze Types.rawURLToText formaction

    GA.FormEnctype formenctype ->
      A.formenctype $ H.unsafeByteStringValue formenctype

    GA.FormMethod formmethod ->
      A.formmethod $ textToBlaze Types.formMethodToText formmethod

    GA.FormNoValidate ->
      A.formnovalidate "true"

    GA.FormTarget formtarget ->
      A.formtarget $ textToBlaze Types.targetToText formtarget

    GA.Headers headers ->
      A.headers $ foldToBlazeWithSeparator Types.idToText " " headers

    GA.Height height ->
      A.height $ showBlaze height

    GA.High high ->
      A.high $ textToBlaze Types.numberToText high

    GA.Href href ->
      A.href $ textToBlaze Types.rawURLToText href

    GA.HrefLang hreflang ->
      A.hreflang $ textToBlaze Ogma.bcp_47ToText hreflang

    GA.HttpEquiv httpEquiv ->
      A.httpEquiv $ textToBlaze Types.httpEquivTokenToText httpEquiv

    GA.ImageSizes imagesizes ->
      H.customAttribute "imagesizes"
        . foldToBlazeWithSeparator Types.sizeToText ", "
        $ NEL.toList imagesizes

    GA.ImageSrcset imagesrcset ->
      H.customAttribute "imagesrcset"
        . foldToBlazeWithSeparator Types.srcsetCandidateToText ", "
        $ NEL.toList imagesrcset

    GA.Integrity sha content ->
      H.customAttribute "integrity" $
        textToBlaze (Types.integrityToText sha) content

    GA.IsMap ->
      A.ismap "true"

    GA.Kind kind ->
      H.customAttribute "kind" $ textToBlaze Types.trackKindToText kind

    GA.Label label ->
      A.label $ textToBlaze id label

    GA.List list ->
      A.list $ textToBlaze Types.idToText list

    GA.Loading loading ->
      H.customAttribute "loading" $ textToBlaze Types.loadOptionToText loading

    GA.Loop ->
      A.loop "true"

    GA.Low low ->
      A.low $ textToBlaze Types.numberToText low

    GA.Max max ->
      A.max $ textToBlaze Types.rawRangeBoundToText max

    GA.MaxLength maxlength ->
      A.maxlength $ showBlaze maxlength

    GA.Media media ->
      A.media
        . foldToBlazeWithSeparator Types.mediaQueryToText ", "
        $ NEL.toList media

    GA.Method method ->
      A.method $ textToBlaze Types.formMethodToText method

    GA.Min min ->
      A.min $ textToBlaze Types.rawRangeBoundToText min

    GA.MinLength minlength ->
      A.minlength $ showBlaze minlength

    GA.Multiple ->
      A.multiple "true"

    GA.Muted muted ->
      booleanToBlaze A.muted muted

    GA.Name name ->
      A.name $ textToBlaze Types.nameToText name

    GA.NameMeta name ->
      A.name $ textToBlaze Types.metadataNameToText name

    GA.NoModule nomodule ->
      booleanToBlaze (H.customAttribute "nomodule") nomodule

    GA.NoValidate novalidate ->
      booleanToBlaze A.novalidate novalidate

    GA.Open ->
      A.open "true"

    GA.Optimum optimum ->
      A.optimum $ textToBlaze Types.numberToText optimum

    GA.Pattern pattern ->
      A.pattern $ textToBlaze id pattern

    GA.Ping ping ->
      A.ping
        . foldToBlazeWithSeparator Types.pingToText " "
        $ NEL.toList ping

    GA.Placeholder placeholder ->
      A.placeholder $ textToBlaze id placeholder

    GA.PlaysInline playsinline ->
      booleanToBlaze (H.customAttribute "playsinline") playsinline

    GA.PopoverTarget popovertarget ->
      H.customAttribute "popovertarget" $
        textToBlaze Types.idToText popovertarget

    GA.PopoverTargetAction popovertargetaction ->
      H.customAttribute "popovertargetaction" $
        textToBlaze Types.popoverTargetActionToText popovertargetaction

    GA.Poster poster ->
      A.poster $ textToBlaze Types.rawURLToText poster

    GA.Preload preload ->
      A.preload $ textToBlaze Types.preloadToText preload

    GA.ReadOnly ->
      A.readonly "true"

    GA.ReferrerPolicy referrerpolicy ->
      H.customAttribute "referrerpolicy" $
        textToBlaze Types.referrerPolicyToText referrerpolicy

    GA.Rel rel ->
      A.rel . textToBlaze Types.relationshipToText $ Types.mkRelationship rel

    GA.Required required ->
      booleanToBlaze A.required required

    GA.Reversed reversed ->
      booleanToBlaze A.reversed reversed

    GA.Rows rows ->
      A.rows $ showBlaze rows

    GA.Rowspan rowspan ->
      A.rowspan $ showBlaze rowspan

    GA.Sandbox sandbox ->
      A.sandbox $ foldToBlazeWithSeparator Types.sandboxTokenToText " " sandbox

    GA.Scope scope ->
      A.scope $ textToBlaze Types.scopeToText scope

    GA.Selected selected ->
      booleanToBlaze A.selected selected

    GA.ShadowRootMode shadowrootmode ->
      H.customAttribute "shadowrootmode" $
        textToBlaze Types.openClosedToText shadowrootmode

    GA.ShadowRootDelegatesFocus ->
      booleanToBlaze (H.customAttribute "shadowrootdelegatesfocus") True

    GA.ShadowRootClonable ->
      booleanToBlaze (H.customAttribute "shadowrootclonable") True

    GA.Shape shape ->
      A.shape $ textToBlaze Types.shapeToText shape

    GA.Size size ->
      A.size $ showBlaze size

    GA.Sizes sizes ->
      A.sizes
        . foldToBlazeWithSeparator Types.sizeToText ", "
        $ NEL.toList sizes

    GA.Span span ->
      A.span $ showBlaze span

    GA.Src src ->
      A.src $ textToBlaze Types.rawURLToText src

    GA.SrcDoc srcdoc ->
      A.srcdoc $ H.unsafeLazyByteStringValue srcdoc

    GA.SrcLang srclang ->
      H.customAttribute "srclang" $
        textToBlaze Ogma.bcp_47ToText srclang

    GA.SrcSet srcset ->
      H.customAttribute "srcset"
        . foldToBlazeWithSeparator Types.srcsetCandidateToText ", "
        $ NEL.toList srcset

    GA.Start start ->
      A.start $ showBlaze start

    GA.Step step ->
      A.step $ textToBlaze Types.stepToText step

    GA.Target target ->
      A.target $ textToBlaze Types.targetToText target

    GA.Type type_ ->
      A.type_ $ textToBlaze Types.rawTypeOptionToText type_

    GA.UseMap usemap ->
      A.usemap $ textToBlaze Types.nameToText usemap

    GA.Value value ->
      A.value $ textToBlaze id value

    GA.ValueInteger value ->
      A.value $ showBlaze value

    GA.ValueNumber value ->
      A.value $ textToBlaze Types.numberToText value

    GA.Width width ->
      A.width $ showBlaze width

    GA.Wrap wrap ->
      A.wrap $ textToBlaze Types.wrapToText wrap

    GA.XMLNS xmlns ->
      A.xmlns $ textToBlaze Types.rawURLToText xmlns

    -- ARIA Attributes
    --
    GA.Aria aria ->
      H.customAttribute
        (H.textTag $ Types.ariaAttributeToText aria)
        (textToBlaze id $ Types.ariaValueToText aria)

    GA.On event script ->
      H.customAttribute
        (H.textTag $ eventAttributeToText event)
        (textToBlaze id $ Types.rawJavaScriptToText script)

boolToBlaze :: Bool -> H.AttributeValue
boolToBlaze b =
  if b
    then "true"
    else "false"

booleanToBlaze :: (H.AttributeValue -> H.Attribute) -> Bool -> H.Attribute
booleanToBlaze attr b =
  if b
    then attr "true"
    else mempty

foldToBlazeWithSeparator :: (a -> T.Text)
                         -> H.AttributeValue
                         -> [a]
                         -> H.AttributeValue
foldToBlazeWithSeparator toText separator items =
  case items of
    [] ->
      mempty

    (x:[]) ->
      textToBlaze toText x

    (x:xs) ->
      textToBlaze toText x
        <> separator
        <> foldToBlazeWithSeparator toText separator xs

foldToBlazeShowWithSeparator :: Show a
                             => H.AttributeValue -> [a] -> H.AttributeValue
foldToBlazeShowWithSeparator separator items =
  case items of
    [] ->
      mempty

    (x:[]) ->
      showBlaze x

    (x:xs) ->
      showBlaze x <> separator <> foldToBlazeShowWithSeparator separator xs

showBlaze :: Show a => a -> H.AttributeValue
showBlaze =
  H.stringValue . show

textToBlaze :: (a -> T.Text) -> a -> H.AttributeValue
textToBlaze fn =
  H.textValue . fn
