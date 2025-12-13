module Generation.Convert.Lucid
  ( toLucid
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (mapMaybe)
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Format.ISO8601 (iso8601Show)
import Lucid qualified
import Lucid.Base qualified as B
import Lucid.Html5 qualified as H
import Ogma qualified
import Prelude hiding (div, max, min, span)

import Brigid.HTML.Attributes.Event.Event (Event, eventAttributeToText)
import Brigid.HTML.Generation (Element (..), ElementNode (..))
import Brigid.HTML.Generation.Attributes qualified as GA
import Brigid.HTML.Generation.Elements (ElementType (..))
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

toLucid :: Element -> Lucid.Html ()
toLucid (Element tag attrs node) =
  let
    attributes = mapMaybe toAttribute attrs
    content =
      case node of
        Branch nodes -> foldMap toLucid nodes
        Leaf txt -> Lucid.toHtml $ NET.toText txt
        Void -> mempty
  in
    case tag of
      Comment
        | Leaf txt <- node ->
            Lucid.toHtmlRaw $ "<!-- " <> NET.toText txt <> " -->"
        | otherwise ->
            mempty
      Anchor -> H.a_ attributes content
      Abbreviation -> H.abbr_ attributes content
      ContactAddress -> H.address_ attributes content
      Area -> H.area_ attributes
      Article -> H.article_ attributes content
      Aside -> H.aside_ attributes content
      Audio -> H.audio_ attributes content
      BringAttentionTo -> H.b_ attributes content
      Base -> H.base_ attributes
      BidirectionalIsolation -> Lucid.termWith "bdi" attributes content
      BidirectionalOverride -> H.bdo_ attributes content
      Blockquote -> H.blockquote_ attributes content
      Body -> H.body_ attributes content
      LineBreak -> H.br_ attributes
      Button -> H.button_ attributes content
      Canvas -> H.canvas_ attributes content
      TableCaption -> H.caption_ attributes content
      Citation -> H.cite_ attributes content
      Code -> H.code_ attributes content
      TableColumn -> H.col_ attributes
      TableColumnGroup -> H.colgroup_ attributes content
      Data -> Lucid.termWith "data" attributes content
      DataList -> H.datalist_ attributes content
      DescriptionDetails -> H.dd_ attributes content
      DeletedText -> H.del_ attributes content
      Details -> H.details_ attributes content
      Definition -> H.dfn_ attributes content
      Dialog -> Lucid.termWith "dialog" attributes content
      Division -> H.div_ attributes content
      DescriptionList -> H.dl_ attributes content
      DescriptionTerm -> H.dt_ attributes content
      Emphasis -> H.em_ attributes content
      Embed -> H.embed_ attributes
      Fieldset -> H.fieldset_ attributes content
      FigureCaption -> H.figcaption_ attributes content
      Figure -> H.figure_ attributes content
      Footer -> H.footer_ attributes content
      Form -> H.form_ attributes content
      H1 -> H.h1_ attributes content
      H2 -> H.h2_ attributes content
      H3 -> H.h3_ attributes content
      H4 -> H.h4_ attributes content
      H5 -> H.h5_ attributes content
      H6 -> H.h6_ attributes content
      Head -> H.head_ attributes content
      Header -> H.header_ attributes content
      HeadingGroup -> H.hgroup_ attributes content
      HorizontalRule -> H.hr_ attributes
      Html -> H.html_ attributes content
      IdiomaticText -> H.i_ attributes content
      IFrame -> H.iframe_ attributes content
      Image -> H.img_ attributes
      Input -> H.input_ attributes
      InsertedText -> H.ins_ attributes content
      KeyboardInput -> H.kbd_ attributes content
      Label -> H.label_ attributes content
      Legend -> H.legend_ attributes content
      ListItem -> H.li_ attributes content
      Link -> H.link_ attributes
      Main -> H.main_ attributes content
      Map -> H.map_ attributes content
      Mark -> H.mark_ attributes content
      Menu -> H.menu_ attributes content
      Meta -> H.meta_ attributes
      Meter -> H.meter_ attributes content
      Nav -> H.nav_ attributes content
      NoScriptHead -> H.noscript_ attributes content
      NoScriptBody -> H.noscript_ attributes content
      Object -> H.object_ attributes content
      OrderedList -> H.ol_ attributes content
      OptionGroup -> H.optgroup_ attributes content
      Option -> H.option_ attributes content
      Output -> H.output_ attributes content
      Paragraph -> H.p_ attributes content
      Picture -> Lucid.termWith "picture" attributes content
      PreformattedText -> H.pre_ attributes content
      Progress -> H.progress_ attributes content
      Quotation -> H.q_ attributes content
      RubyParenthesis -> H.rp_ attributes content
      RubyText -> H.rt_ attributes content
      Ruby -> H.ruby_ attributes content
      Strikethrough -> Lucid.termWith "s" attributes content
      Sample -> H.samp_ attributes content
      Script -> H.script_ attributes content
      Search -> Lucid.termWith "search" attributes content
      Section -> H.section_ attributes content
      Select -> H.select_ attributes content
      Slot -> Lucid.termWith "slot" attributes content
      SideComment -> H.small_ attributes content
      Source -> H.source_ attributes
      Span -> H.span_ attributes content
      Strong -> H.strong_ attributes content
      Style -> H.style_ attributes content
      Subscript -> H.sub_ attributes content
      Summary -> H.summary_ attributes content
      Superscript -> H.sup_ attributes content
      Table -> H.table_ attributes content
      TableBody -> H.tbody_ attributes content
      TableDataCell -> H.td_ attributes content
      ContentTemplate -> H.template_ attributes content
      TextArea -> H.textarea_ attributes content
      TableFoot -> H.tfoot_ attributes content
      TableHeader -> H.th_ attributes content
      TableHead -> H.thead_ attributes content
      Time -> H.time_ attributes content
      Title -> H.title_ attributes content
      TableRow -> H.tr_ attributes content
      Track -> H.track_ attributes
      Underline -> Lucid.termWith "u" attributes content
      UnorderedList -> H.ul_ attributes content
      Variable -> H.var_ attributes content
      Video -> H.video_ attributes content
      WordBreakOpportunity -> H.wbr_ attributes

toAttribute :: GA.Attribute -> Maybe Lucid.Attribute
toAttribute attr =
  case attr of
    -- Global Attributes
    --
    GA.AccessKey accesskey ->
      Just . H.accesskey_ $ T.singleton accesskey

    GA.Autocapitalize autocapitalize ->
      Just
        . B.makeAttribute "autocapitalize"
        $ Types.autocapitalizeOptionToText autocapitalize

    GA.Autocorrect autocorrect ->
      Just . B.makeAttribute "autocorrect" $ Types.onOffToText autocorrect

    GA.Autofocus autofocus ->
      booleanToLucid H.autofocus_ autofocus

    GA.Class class_ ->
      Just . H.class_ $ Types.classToText class_

    GA.ContentEditable contentEditable ->
      Just
        . H.contenteditable_
        $ Types.contentEditableOptionToText contentEditable

    GA.CustomData data_ val ->
      Just $ H.data_ data_ val

    GA.Dir dir ->
      Just . H.dir_ $ Types.directionalityToText dir

    GA.Draggable draggable ->
      Just . H.draggable_ $ boolText draggable

    GA.EnterKeyHint enterkeyhint ->
      Just
        . B.makeAttribute "enterkeyhint"
        $ Types.keyHintOptionToText enterkeyhint

    GA.ExportParts exportparts ->
      Just
        . B.makeAttribute "exportparts"
        $ foldToLucidWithSeparator Types.exportPartToText ", " exportparts

    GA.Hidden hidden ->
      booleanToLucid (H.hidden_ $ boolText hidden) hidden

    GA.Id id_ ->
      Just . H.id_ $ Types.idToText id_

    GA.Inert inert ->
      booleanToLucid (B.makeAttribute "inert" $ boolText inert) inert

    GA.InputMode inputmode ->
      Just . B.makeAttribute "inputmode" $ Types.inputModeToText inputmode

    GA.Is is ->
      Just . B.makeAttribute "is" $ id is

    GA.ItemId itemId ->
      Just . B.makeAttribute "itemid" $ id itemId

    GA.ItemProp itemprop ->
      Just . H.itemprop_ $ id itemprop

    GA.ItemRef itemref ->
      Just
        . B.makeAttribute "itemref"
        . foldToLucidWithSeparator Types.idToText " "
        $ NEL.toList itemref

    GA.ItemScope ->
      Just $ B.makeAttribute "itemscope" mempty

    GA.ItemType itemtype ->
      Just . B.makeAttribute "itemtype" $ Types.absoluteURLToText itemtype

    GA.Lang lang ->
      Just . H.lang_ $ maybe "" Ogma.bcp_47ToText lang

    GA.Nonce nonce ->
      Just . B.makeAttribute "nonce" $ id nonce

    GA.Part part ->
      Just
        . B.makeAttribute "part"
        $ foldToLucidWithSeparator Types.partToText " " part

    GA.Popover popover ->
      Just
        . B.makeAttribute "popover"
        $ Types.popoverStateToText popover

    GA.Role role ->
      Just . H.role_ $ Types.roleToText role

    GA.Slot slot ->
      Just . B.makeAttribute "slot" $ Types.nameToText slot

    GA.Spellcheck spellcheck ->
      Just . H.spellcheck_ $ boolText spellcheck

    GA.Style style ->
      Just . H.style_ $ id style

    GA.TabIndex tabindex ->
      Just . H.tabindex_ $ showText tabindex

    GA.Title title ->
      Just . H.title_ $ id title

    GA.Translate translate ->
      Just . B.makeAttribute "translate" $ Types.yesNoToText translate

    GA.WritingSuggestions writingsuggestions ->
      Just . B.makeAttribute "writingsuggestions" $ boolText writingsuggestions

    -- Scoped Attributes
    --
    GA.Abbreviation abbr ->
      Just . B.makeAttribute "abbr" $ id abbr

    GA.Accept accept ->
      Just . H.accept_ $ TE.decodeUtf8 accept

    GA.AcceptCharset ->
      Just $ H.acceptCharset_ "UTF-8"

    GA.Action action ->
      Just . H.action_ $ Types.rawURLToText action

    GA.Allow allow ->
      Just
        . B.makeAttribute "allow"
        $ foldToLucidWithSeparator Types.featurePolicyDirectiveToText "; " allow

    GA.As as ->
      Just . B.makeAttribute "as" $ Types.asToText as

    GA.Alt alt ->
      Just $ H.alt_ alt

    GA.Async ->
      Just $ H.async_ "true"

    GA.Autocomplete autocomplete ->
      Just . H.autocomplete_ $ Types.onOffToText autocomplete

    GA.Autoplay ->
      Just $ H.autoplay_ "true"

    GA.Blocking blocking ->
      Just . B.makeAttribute "blocking" $ Types.blockOptionToText blocking

    GA.Capture capture ->
      Just
        . B.makeAttribute "capture"
        $ maybe mempty Types.captureMethodToText capture

    GA.Charset ->
      Just $ H.charset_ "utf-8"

    GA.Checked checked ->
      booleanToLucid H.checked_ checked

    GA.Cite cite ->
      Just . H.cite_ $ Types.rawURLToText cite

    GA.Cols cols ->
      Just . H.cols_ $ showText cols

    GA.Colspan colspan ->
      Just . H.colspan_ $ showText colspan

    GA.Command command ->
      Just . B.makeAttribute "command" $ Types.commandOptionToText command

    GA.CommandFor commandfor ->
      Just . B.makeAttribute "commandfor" $ Types.idToText commandfor

    GA.Content content ->
      Just $ H.content_ content

    GA.Controls ->
      Just $ H.controls_ "true"

    GA.ControlsList controlslist ->
      Just
        . B.makeAttribute "controlslist"
        $ Types.controlsListToText controlslist

    GA.Coords coords ->
      Just
        . H.coords_
        . foldToLucidShowWithSeparator ","
        $ NEL.toList coords

    GA.CrossOrigin crossorigin ->
      Just
        . H.crossorigin_
        $ Types.crossOriginFetchToText crossorigin

    GA.Data data_ ->
      Just . B.makeAttribute "data" $ Types.rawURLToText data_

    GA.Datetime datetime ->
      Just . H.datetime_ . T.pack $ iso8601Show datetime

    GA.Decoding decoding ->
      Just . B.makeAttribute "decoding" $ Types.decodingToText decoding

    GA.Default ->
      Just $ B.makeAttribute "default" "true"

    GA.Defer ->
      Just $ H.defer_ "true"

    GA.Dirname dirname ->
      Just $ B.makeAttribute "dirname" dirname

    GA.Disabled disabled ->
      booleanToLucid (H.disabled_ $ boolText disabled) disabled

    GA.DisablePictureInPicture ->
      Just $ B.makeAttribute "disablepictureinpicture" "true"

    GA.DisableRemotePlayback ->
      Just $ B.makeAttribute "disableremoteplayback" "true"

    GA.Download download ->
      Just . H.download_ $ maybe mempty NET.toText download

    GA.ElementTiming elementtiming ->
      Just $ B.makeAttribute "elementtiming" elementtiming

    GA.Enctype enctype ->
      Just . H.enctype_ $ TE.decodeUtf8 enctype

    GA.FetchPriority fetchpriority ->
      Just
        . B.makeAttribute "fetchpriority"
        $ Types.fetchPriorityToText fetchpriority

    GA.ForLabel for ->
      Just . H.for_ $ Types.idToText for

    GA.ForOutput for ->
      Just
        . H.for_
        . foldToLucidWithSeparator Types.idToText " "
        $ NEL.toList for

    GA.Form form ->
      Just . H.form_ $ Types.idToText form

    GA.FormAction formaction ->
      Just . H.formaction_ $ Types.rawURLToText formaction

    GA.FormEnctype formenctype ->
      Just . H.formenctype_ $ TE.decodeUtf8 formenctype

    GA.FormMethod formmethod ->
      Just . H.formmethod_ $ Types.formMethodToText formmethod

    GA.FormNoValidate ->
      Just $ H.formnovalidate_ "true"

    GA.FormTarget formtarget ->
      Just . H.formtarget_ $ Types.targetToText formtarget

    GA.Headers headers ->
      Just . H.headers_ $ foldToLucidWithSeparator Types.idToText " " headers

    GA.Height height ->
      Just . H.height_ $ showText height

    GA.High high ->
      Just . H.high_ $ Types.numberToText high

    GA.Href href ->
      Just . H.href_ $ Types.rawURLToText href

    GA.HrefLang hreflang ->
      Just . H.hreflang_ $ Ogma.bcp_47ToText hreflang

    GA.HttpEquiv httpEquiv ->
      Just . H.httpEquiv_ $ Types.httpEquivTokenToText httpEquiv

    GA.ImageSizes imagesizes ->
      Just
        . B.makeAttribute "imagesizes"
        . foldToLucidWithSeparator Types.sizeToText ", "
        $ NEL.toList imagesizes

    GA.ImageSrcset imagesrcset ->
      Just
        . B.makeAttribute "imagesrcset"
        . foldToLucidWithSeparator Types.srcsetCandidateToText ", "
        $ NEL.toList imagesrcset

    GA.Integrity sha content ->
      Just . H.integrity_ $ Types.integrityToText sha content

    GA.IsMap ->
      Just $ H.ismap_ "true"

    GA.Kind kind ->
      Just . B.makeAttribute "kind" $ Types.trackKindToText kind

    GA.Label label ->
      Just $ H.label_ label

    GA.List list ->
      Just . H.list_ $ Types.idToText list

    GA.Loading loading ->
      Just . H.loading_ $ Types.loadOptionToText loading

    GA.Loop ->
      Just $ H.loop_ "true"

    GA.Low low ->
      Just . H.low_ $ Types.numberToText low

    GA.Max max ->
      Just . H.max_ $ Types.rawRangeBoundToText max

    GA.MaxLength maxlength ->
      Just . H.maxlength_ $ showText maxlength

    GA.Media media ->
      Just
        . H.media_
        . foldToLucidWithSeparator Types.mediaQueryToText ", "
        $ NEL.toList media

    GA.Method method ->
      Just . H.method_ $ Types.formMethodToText method

    GA.Min min ->
      Just . H.min_ $ Types.rawRangeBoundToText min

    GA.MinLength minlength ->
      Just . H.minlength_ $ showText minlength

    GA.Multiple ->
      Just $ H.multiple_ "true"

    GA.Muted muted ->
      booleanToLucid (B.makeAttribute "muted" $ boolText muted) muted

    GA.Name name ->
      Just . H.name_ $ Types.nameToText name

    GA.NameMeta name ->
      Just . H.name_ $ Types.metadataNameToText name

    GA.NoModule nomodule ->
      booleanToLucid (B.makeAttribute "nomodule" $ boolText nomodule) nomodule

    GA.NoValidate novalidate ->
      booleanToLucid (H.novalidate_ $ boolText novalidate) novalidate

    GA.Open ->
      Just $ H.open_ "true"

    GA.Optimum optimum ->
      Just . H.optimum_ $ Types.numberToText optimum

    GA.Pattern pattern ->
      Just $ H.pattern_ pattern

    GA.Ping ping ->
      Just
        . H.ping_
        . foldToLucidWithSeparator Types.pingToText " "
        $ NEL.toList ping

    GA.Placeholder placeholder ->
      Just $ H.placeholder_ placeholder

    GA.PlaysInline playsinline ->
      booleanToLucid
        (B.makeAttribute "playsinline" $ boolText playsinline)
        playsinline

    GA.PopoverTarget popovertarget ->
      Just . B.makeAttribute "popovertarget" $ Types.idToText popovertarget

    GA.PopoverTargetAction popovertargetaction ->
      Just
        . B.makeAttribute "popovertargetaction"
        $ Types.popoverTargetActionToText popovertargetaction

    GA.Poster poster ->
      Just . H.poster_ $ Types.rawURLToText poster

    GA.Preload preload ->
      Just . H.preload_ $ Types.preloadToText preload

    GA.ReadOnly ->
      Just $ H.readonly_ "true"

    GA.ReferrerPolicy referrerpolicy ->
      Just
        . B.makeAttribute "referrerpolicy"
        $ Types.referrerPolicyToText referrerpolicy

    GA.Rel rel ->
      Just . H.rel_ . Types.relationshipToText $ Types.mkRelationship rel

    GA.Required required ->
      booleanToLucid (H.required_ $ boolText required) required

    GA.Reversed reversed ->
      booleanToLucid (H.reversed_ $ boolText reversed) reversed

    GA.Rows rows ->
      Just . H.rows_ $ showText rows

    GA.Rowspan rowspan ->
      Just . H.rowspan_ $ showText rowspan

    GA.Sandbox sandbox ->
      Just
        . H.sandbox_
        $ foldToLucidWithSeparator Types.sandboxTokenToText " " sandbox

    GA.Scope scope ->
      Just . H.scope_ $ Types.scopeToText scope

    GA.Selected selected ->
      booleanToLucid (H.selected_$ boolText selected) selected

    GA.ShadowRootMode shadowrootmode ->
      Just . B.makeAttribute "shadowrootmode" $
        Types.openClosedToText shadowrootmode

    GA.ShadowRootDelegatesFocus ->
      booleanToLucid (B.makeAttribute "shadowrootdelegatesfocus" T.empty) True

    GA.ShadowRootClonable ->
      booleanToLucid (B.makeAttribute "shadowrootclonable" T.empty) True

    GA.Shape shape ->
      Just . H.shape_ $ Types.shapeToText shape

    GA.Size size ->
      Just . H.size_ $ showText size

    GA.Sizes sizes ->
      Just
        . H.sizes_
        . foldToLucidWithSeparator Types.sizeToText ", "
        $ NEL.toList sizes

    GA.Span span ->
      Just . H.span_ $ showText span

    GA.Src src ->
      Just . H.src_ $ Types.rawURLToText src

    GA.SrcDoc srcdoc ->
      Just . H.srcdoc_ $ lucidLBS srcdoc

    GA.SrcLang srclang ->
      Just. B.makeAttribute "srclang" $ Ogma.bcp_47ToText srclang

    GA.SrcSet srcset ->
      Just
        . B.makeAttribute "srcset"
        . foldToLucidWithSeparator Types.srcsetCandidateToText ", "
        $ NEL.toList srcset

    GA.Start start ->
      Just . H.start_ $ showText start

    GA.Step step ->
      Just . H.step_ $ Types.stepToText step

    GA.Target target ->
      Just . H.target_ $ Types.targetToText target

    GA.Type type_ ->
      Just . H.type_ $ Types.rawTypeOptionToText type_

    GA.UseMap usemap ->
      Just . H.usemap_ $ Types.nameToText usemap

    GA.Value value ->
      Just $ H.value_ value

    GA.ValueInteger value ->
      Just . H.value_ $ showText value

    GA.ValueNumber value ->
      Just . H.value_ $ Types.numberToText value

    GA.Width width ->
      Just . H.width_ $ showText width

    GA.Wrap wrap ->
      Just . H.wrap_ $ Types.wrapToText wrap

    GA.XMLNS xmlns ->
      Just . H.xmlns_ $ Types.rawURLToText xmlns

    -- ARIA Attributes
    --
    GA.Aria aria ->
      Just $
        B.makeAttribute
          (Types.ariaAttributeToText aria)
          (Types.ariaValueToText aria)

    GA.On event script ->
      Just $
        B.makeAttribute
          (eventAttributeToText event)
          (Types.rawJavaScriptToText script)

boolText :: Bool -> T.Text
boolText b =
  if b
    then "true"
    else "false"

booleanToLucid :: Lucid.Attribute -> Bool -> Maybe Lucid.Attribute
booleanToLucid attr b =
  if b
    then Just attr
    else Nothing

foldToLucidWithSeparator :: (a -> T.Text) -> T.Text -> [a] -> T.Text
foldToLucidWithSeparator toText separator items =
  case items of
    [] ->
      mempty

    (x:[]) ->
      toText x

    (x:xs) ->
      toText x
        <> separator
        <> foldToLucidWithSeparator toText separator xs

foldToLucidShowWithSeparator :: Show a => T.Text -> [a] -> T.Text
foldToLucidShowWithSeparator separator items =
  case items of
    [] ->
      mempty

    (x:[]) ->
      showText x

    (x:xs) ->
      showText x <> separator <> foldToLucidShowWithSeparator separator xs

lucidLBS :: LBS.ByteString -> T.Text
lucidLBS =
  TE.decodeUtf8 . LBS.toStrict

showText :: Show a => a -> T.Text
showText =
  T.pack . show
