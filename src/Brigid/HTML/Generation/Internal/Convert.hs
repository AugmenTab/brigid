{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Generation.Internal.Convert
  ( toBrigid
  ) where

import Data.Constraint (Dict (..))
import Data.Functor.Alt ((<!>))
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Validation qualified as V
import Prelude hiding (div, max, min, span)

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Attributes.Internal qualified as Attr
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Generation.Attributes qualified as GA
import Brigid.HTML.Generation.Internal.Types (Element (..), ElementNode (..), ElementType (..), NodeType (..))
import Brigid.Internal.Use (use)

type Validator a =
  V.Validation [String] a

type ValidAttribute tag =
  Validator (A.Attribute tag)

type ValidHTML parent grandparent =
  Validator (E.ChildHTML parent grandparent)

toBrigid :: Element -> Either [String] E.AnyHTML
toBrigid =
  V.toEither . validateBrigid

validateBrigid :: Element -> V.Validation [String] E.AnyHTML
validateBrigid e =
    case elementType e of
      Comment ->
        case elementChildren e of
          Branch _nodes -> V.Failure [ wrongNodeType Comment BranchNode ]
          Leaf net -> V.Success . E.comment $ NET.toText net
          Void -> V.Failure [ wrongNodeType Comment VoidNode ]

      Anchor ->
        mkAnchor (elementAttrs e) (elementChildren e)

      Abbreviation ->
        mkAbbreviation (elementAttrs e) (elementChildren e)

      ContactAddress ->
        mkContactAddress (elementAttrs e) (elementChildren e)

      Area ->
        mkArea (elementAttrs e)

      Article ->
        mkArticle (elementAttrs e) (elementChildren e)

      Aside ->
        mkAside (elementAttrs e) (elementChildren e)

      Audio ->
        mkAudio (elementAttrs e) (elementChildren e)

      BringAttentionTo ->
        mkBringAttentionTo (elementAttrs e) (elementChildren e)

      Base ->
        mkBase (elementAttrs e)

      BidirectionalIsolation ->
        mkBidirectionalIsolation (elementAttrs e) (elementChildren e)

      BidirectionalOverride ->
        mkBidirectionalOverride (elementAttrs e) (elementChildren e)

      Blockquote ->
        mkBlockquote (elementAttrs e) (elementChildren e)

      Body ->
        mkBody (elementAttrs e) (elementChildren e)

      LineBreak ->
        mkLineBreak (elementAttrs e)

      Button ->
        mkButton (elementAttrs e) (elementChildren e)

      Canvas ->
        mkCanvas (elementAttrs e) (elementChildren e)

      TableCaption ->
        mkTableCaption (elementAttrs e) (elementChildren e)

      Citation ->
        mkCitation (elementAttrs e) (elementChildren e)

      Code ->
        mkCode (elementAttrs e) (elementChildren e)

      TableColumn ->
        mkTableColumn (elementAttrs e)

      TableColumnGroup ->
        mkTableColumnGroup (elementAttrs e) (elementChildren e)

      Data ->
        mkData (elementAttrs e) (elementChildren e)

      DataList ->
        mkDataList (elementAttrs e) (elementChildren e)

      DescriptionDetails ->
        mkDescriptionDetails (elementAttrs e) (elementChildren e)

      DeletedText ->
        mkDeletedText (elementAttrs e) (elementChildren e)

      Details ->
        mkDetails (elementAttrs e) (elementChildren e)

      Definition ->
        mkDefinition (elementAttrs e) (elementChildren e)

      Dialog ->
        mkDialog (elementAttrs e) (elementChildren e)

      Division ->
        mkDivision (elementAttrs e) (elementChildren e)

      DescriptionList ->
        mkDescriptionList (elementAttrs e) (elementChildren e)

      DescriptionTerm ->
        mkDescriptionTerm (elementAttrs e) (elementChildren e)

      Emphasis ->
        mkEmphasis (elementAttrs e) (elementChildren e)

      Embed ->
        mkEmbed (elementAttrs e)

      Fieldset ->
        mkFieldset (elementAttrs e) (elementChildren e)

      FigureCaption ->
        mkFigureCaption (elementAttrs e) (elementChildren e)

      Figure ->
        mkFigure (elementAttrs e) (elementChildren e)

      Footer ->
        mkFooter (elementAttrs e) (elementChildren e)

      Form ->
        mkForm (elementAttrs e) (elementChildren e)

      H1 ->
        mkH1 (elementAttrs e) (elementChildren e)

      H2 ->
        mkH2 (elementAttrs e) (elementChildren e)

      H3 ->
        mkH3 (elementAttrs e) (elementChildren e)

      H4 ->
        mkH4 (elementAttrs e) (elementChildren e)

      H5 ->
        mkH5 (elementAttrs e) (elementChildren e)

      H6 ->
        mkH6 (elementAttrs e) (elementChildren e)

      Head ->
        mkHead (elementAttrs e) (elementChildren e)

      Header ->
        mkHeader (elementAttrs e) (elementChildren e)

      HeadingGroup ->
        mkHeadingGroup (elementAttrs e) (elementChildren e)

      HorizontalRule ->
        mkHorizontalRule (elementAttrs e)

      Html ->
        mkHtml (elementAttrs e) (elementChildren e)

      IdiomaticText ->
        mkIdiomaticText (elementAttrs e) (elementChildren e)

      IFrame ->
        mkIFrame (elementAttrs e)

      Image ->
        mkImage (elementAttrs e)

      Input ->
        mkInput (elementAttrs e)

      InsertedText ->
        mkInsertedText (elementAttrs e) (elementChildren e)

      KeyboardInput ->
        mkKeyboardInput (elementAttrs e) (elementChildren e)

      Label ->
        mkLabel (elementAttrs e) (elementChildren e)

      Legend ->
        mkLegend (elementAttrs e) (elementChildren e)

      ListItem ->
        mkListItem (elementAttrs e) (elementChildren e)

      Link ->
        mkLink (elementAttrs e)

      Main ->
        mkMain (elementAttrs e) (elementChildren e)

      Map ->
        mkMap (elementAttrs e) (elementChildren e)

      Mark ->
        mkMark (elementAttrs e) (elementChildren e)

      Menu ->
        mkMenu (elementAttrs e) (elementChildren e)

      Meta ->
        mkMeta (elementAttrs e)

      Meter ->
        mkMeter (elementAttrs e) (elementChildren e)

      Nav ->
        mkNav (elementAttrs e) (elementChildren e)

      NoScriptHead ->
        mkNoScriptHead (elementAttrs e) (elementChildren e)

      NoScriptBody ->
        mkNoScriptBody (elementAttrs e) (elementChildren e)

      Object ->
        mkObject (elementAttrs e) (elementChildren e)

      OrderedList ->
        mkOrderedList (elementAttrs e) (elementChildren e)

      OptionGroup ->
        mkOptionGroup (elementAttrs e) (elementChildren e)

      Option ->
        mkOption (elementAttrs e) (elementChildren e)

      Output ->
        mkOutput (elementAttrs e) (elementChildren e)

      Paragraph ->
        mkParagraph (elementAttrs e) (elementChildren e)

      Picture ->
        mkPicture (elementAttrs e) (elementChildren e)

      PreformattedText ->
        mkPreformattedText (elementAttrs e) (elementChildren e)

      Progress ->
        mkProgress (elementAttrs e) (elementChildren e)

      Quotation ->
        mkQuotation (elementAttrs e) (elementChildren e)

      RubyParenthesis ->
        mkRubyParenthesis (elementAttrs e) (elementChildren e)

      RubyText ->
        mkRubyText (elementAttrs e) (elementChildren e)

      Ruby ->
        mkRuby (elementAttrs e) (elementChildren e)

      Strikethrough ->
        mkStrikethrough (elementAttrs e) (elementChildren e)

      Sample ->
        mkSample (elementAttrs e) (elementChildren e)

      Script ->
        mkScript (elementAttrs e) (elementChildren e)

      Search ->
        mkSearch (elementAttrs e) (elementChildren e)

      Section ->
        mkSection (elementAttrs e) (elementChildren e)

      Select ->
        mkSelect (elementAttrs e) (elementChildren e)

      Slot ->
        mkSlot (elementAttrs e) (elementChildren e)

      SideComment ->
        mkSideComment (elementAttrs e) (elementChildren e)

      Source ->
        mkSource (elementAttrs e)

      Span ->
        mkSpan (elementAttrs e) (elementChildren e)

      Strong ->
        mkStrong (elementAttrs e) (elementChildren e)

      Style ->
        mkStyle (elementAttrs e) (elementChildren e)

      Subscript ->
        mkSubscript (elementAttrs e) (elementChildren e)

      Summary ->
        mkSummary (elementAttrs e) (elementChildren e)

      Superscript ->
        mkSuperscript (elementAttrs e) (elementChildren e)

      Table ->
        mkTable (elementAttrs e) (elementChildren e)

      TableBody ->
        mkTableBody (elementAttrs e) (elementChildren e)

      TableDataCell ->
        mkTableDataCell (elementAttrs e) (elementChildren e)

      ContentTemplate ->
        mkContentTemplate (elementAttrs e) (elementChildren e)

      TextArea ->
        mkTextArea (elementAttrs e) (elementChildren e)

      TableFoot ->
        mkTableFoot (elementAttrs e) (elementChildren e)

      TableHeader ->
        mkTableHeader (elementAttrs e) (elementChildren e)

      TableHead ->
        mkTableHead (elementAttrs e) (elementChildren e)

      Time ->
        mkTime (elementAttrs e) (elementChildren e)

      Title ->
        mkTitle (elementAttrs e) (elementChildren e)

      TableRow ->
        mkTableRow (elementAttrs e) (elementChildren e)

      Track ->
        mkTrack (elementAttrs e)

      Underline ->
        mkUnderline (elementAttrs e) (elementChildren e)

      UnorderedList ->
        mkUnorderedList (elementAttrs e) (elementChildren e)

      Variable ->
        mkVariable (elementAttrs e) (elementChildren e)

      Video ->
        mkVideo (elementAttrs e) (elementChildren e)

      WordBreakOpportunity ->
        mkWordBreakOpportunity (elementAttrs e)

mkAnchor :: forall parent grandparent.
            E.ValidChild E.Anchor parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkAnchor attrs node =
  use @(E.ValidChild E.Anchor parent grandparent) $ \Dict ->
    E.customHTML "a"
      <$> foldValidate mkAnchorAttr attrs
      <*> ( case node of
              Branch _nodes -> V.Failure [ wrongNodeType Anchor BranchNode ]
              Leaf net -> V.Success $ Right [ E.text $ NET.toText net ]
              Void -> V.Failure [ wrongNodeType Anchor BranchNode ]
          )

mkAnchorAttr :: GA.Attribute -> ValidAttribute E.CustomHTML
mkAnchorAttr attr =
  let
    vAttr =
      case attr of
        GA.Download download ->
          V.Success (A.download download :: A.Attribute E.CustomHTML)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.CustomHTML)

        GA.Href href ->
          V.Success (A.href href :: A.Attribute E.CustomHTML)

        GA.HrefLang hreflang ->
          V.Success (A.hreflang hreflang :: A.Attribute E.CustomHTML)

        GA.Ping ping ->
          V.Success (A.ping ping :: A.Attribute E.CustomHTML)

        GA.ReferrerPolicy referrerpolicy ->
          V.Success (A.referrerpolicy referrerpolicy :: A.Attribute E.CustomHTML)

        GA.Rel rel ->
          V.Success (A.rel rel :: A.Attribute E.CustomHTML)

        GA.Target target ->
          V.Success (A.target target :: A.Attribute E.CustomHTML)

        GA.Type type_ ->
          V.Success (A.type_ type_ :: A.Attribute E.CustomHTML)

        _attr ->
          V.Failure [ wrongAttr attr Anchor ]
  in
    vAttr <!> mkGlobalAttr attr

mkAbbreviation :: E.ValidChild E.Abbreviation parent grandparent
               => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkAbbreviation attrs node =
  E.abbr
    <$> foldValidate mkAbbreviationAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkAbbreviationChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Abbreviation VoidNode ]
        )

mkAbbreviationAttr :: GA.Attribute -> ValidAttribute E.Abbreviation
mkAbbreviationAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Abbreviation)

        _attr ->
          V.Failure [ wrongAttr attr Abbreviation ]
  in
    vAttr <!> mkGlobalAttr attr

mkAbbreviationChild :: Element -> ValidHTML E.Abbreviation grandparent
mkAbbreviationChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Abbreviation ]

mkContactAddress :: E.ValidChild E.ContactAddress parent grandparent
                 => [GA.Attribute]
                 -> ElementNode
                 -> ValidHTML parent grandparent
mkContactAddress attrs node =
  E.address
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkContactAddressChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType ContactAddress VoidNode ]
        )

mkContactAddressChild :: Element -> ValidHTML E.ContactAddress grandparent
mkContactAddressChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Form -> mkForm attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element ContactAddress ]

mkArea :: E.ValidChild E.Area parent grandparent
       => [GA.Attribute] -> ValidHTML parent grandparent
mkArea attrs =
  E.area <$> foldValidate mkAreaAttr attrs

mkAreaAttr :: GA.Attribute -> ValidAttribute E.Area
mkAreaAttr attr =
  let
    vAttr =
      case attr of
        GA.Alt alt ->
          V.Success (A.alt alt :: A.Attribute E.Area)

        GA.Coords coords ->
          V.Success (A.coords coords :: A.Attribute E.Area)

        GA.Download download ->
          V.Success (A.download download :: A.Attribute E.Area)

        GA.Href href ->
          V.Success (A.href href :: A.Attribute E.Area)

        GA.Ping ping ->
          V.Success (A.ping ping :: A.Attribute E.Area)

        GA.ReferrerPolicy referrerpolicy ->
          V.Success (A.referrerpolicy referrerpolicy :: A.Attribute E.Area)

        GA.Rel rel ->
          V.Success (A.rel rel :: A.Attribute E.Area)

        GA.Shape shape ->
          V.Success (A.shape shape :: A.Attribute E.Area)

        GA.Target target ->
          V.Success (A.target target :: A.Attribute E.Area)

        _attr ->
          V.Failure [ wrongAttr attr Area ]
  in
    vAttr <!> mkGlobalAttr attr

mkArticle :: E.ValidChild E.Article parent grandparent
          => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkArticle attrs node =
  E.article
    <$> foldValidate mkArticleAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkArticleChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Article VoidNode ]
        )

mkArticleAttr :: GA.Attribute -> ValidAttribute E.Article
mkArticleAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Article)

        _attr ->
          V.Failure [ wrongAttr attr Article ]
  in
    vAttr <!> mkGlobalAttr attr

mkArticleChild :: Element -> ValidHTML E.Article grandparent
mkArticleChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Article ]

mkAside :: E.ValidChild E.Aside parent grandparent
        => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkAside attrs node =
  E.aside
    <$> foldValidate mkAsideAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkAsideChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Aside VoidNode ]
        )

mkAsideAttr :: GA.Attribute -> ValidAttribute E.Aside
mkAsideAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Aside)

        _attr ->
          V.Failure [ wrongAttr attr Aside ]
  in
    vAttr <!> mkGlobalAttr attr

mkAsideChild :: Element -> ValidHTML E.Aside grandparent
mkAsideChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Aside ]

mkAudio :: E.ValidChild E.Audio parent grandparent
        => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkAudio attrs node =
  E.audio
    <$> foldValidate mkAudioAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkAudioChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Audio LeafNode ]
            Void -> V.Failure [ wrongNodeType Audio VoidNode ]
        )

mkAudioAttr :: GA.Attribute -> ValidAttribute E.Audio
mkAudioAttr attr =
  let
    vAttr =
      case attr of
        GA.Autoplay ->
          V.Success (A.autoplay :: A.Attribute E.Audio)

        GA.Controls ->
          V.Success (A.controls :: A.Attribute E.Audio)

        GA.ControlsList controlslist ->
          V.Success (A.controlslist controlslist :: A.Attribute E.Audio)

        GA.CrossOrigin crossorigin ->
          V.Success (A.crossorigin crossorigin :: A.Attribute E.Audio)

        GA.DisableRemotePlayback ->
          V.Success (A.disableremoteplayback :: A.Attribute E.Audio)

        GA.Loop ->
          V.Success (A.loop :: A.Attribute E.Audio)

        GA.Muted muted ->
          V.Success (A.mute muted :: A.Attribute E.Audio)

        GA.Preload preload ->
          V.Success (A.preload preload :: A.Attribute E.Audio)

        GA.Src src ->
          V.Success (A.src src :: A.Attribute E.Audio)

        _attr ->
          V.Failure [ wrongAttr attr Audio ]
  in
    vAttr <!> mkGlobalAttr attr

mkAudioChild :: Element -> ValidHTML E.Audio grandparent
mkAudioChild e =
  case elementType e of
    Source -> mkSource (elementAttrs e)
    Track -> mkTrack (elementAttrs e)
    element -> V.Failure [ wrongChild element Audio ]

mkBringAttentionTo :: E.ValidChild E.BringAttentionTo parent grandparent
                   => [GA.Attribute]
                   -> ElementNode
                   -> ValidHTML parent grandparent
mkBringAttentionTo attrs node =
  E.b
    <$> foldValidate mkBringAttentionToAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkBringAttentionToChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType BringAttentionTo VoidNode ]
        )

mkBringAttentionToAttr :: GA.Attribute -> ValidAttribute E.BringAttentionTo
mkBringAttentionToAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.BringAttentionTo)

        _attr ->
          V.Failure [ wrongAttr attr BringAttentionTo ]
  in
    vAttr <!> mkGlobalAttr attr

mkBringAttentionToChild :: Element -> ValidHTML E.BringAttentionTo grandparent
mkBringAttentionToChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element BringAttentionTo ]

mkBase :: E.ValidChild E.Base parent grandparent
       => [GA.Attribute] -> ValidHTML parent grandparent
mkBase attrs =
  E.base <$> foldValidate mkBaseAttr attrs

mkBaseAttr :: GA.Attribute -> ValidAttribute E.Base
mkBaseAttr attr =
  let
    vAttr =
      case attr of
        GA.Href href -> V.Success (A.href href :: A.Attribute E.Base)
        GA.Target target -> V.Success (A.target target :: A.Attribute E.Base)
        _attr -> V.Failure [ wrongAttr attr Base ]
  in
    vAttr <!> mkGlobalAttr attr

mkBidirectionalIsolation :: E.ValidChild E.BidirectionalIsolation parent grandparent
                         => [GA.Attribute]
                         -> ElementNode
                         -> ValidHTML parent grandparent
mkBidirectionalIsolation attrs node =
  E.bdi
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkBidirectionalIsolationChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType BidirectionalIsolation VoidNode ]
        )

mkBidirectionalIsolationChild :: Element
                              -> ValidHTML E.BidirectionalIsolation grandparent
mkBidirectionalIsolationChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element BidirectionalIsolation ]

mkBidirectionalOverride :: E.ValidChild E.BidirectionalOverride parent grandparent
                        => [GA.Attribute]
                        -> ElementNode
                        -> ValidHTML parent grandparent
mkBidirectionalOverride attrs node =
  E.bdo
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkBidirectionalOverrideChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType BidirectionalOverride VoidNode ]
        )

mkBidirectionalOverrideChild :: Element
                             -> ValidHTML E.BidirectionalOverride grandparent
mkBidirectionalOverrideChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element BidirectionalOverride ]

mkBlockquote :: E.ValidChild E.Blockquote parent grandparent
             => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkBlockquote attrs node =
  E.blockquote
    <$> foldValidate mkBlockquoteAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkBlockquoteChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Blockquote VoidNode ]
        )

mkBlockquoteAttr :: GA.Attribute -> ValidAttribute E.Blockquote
mkBlockquoteAttr attr =
  let
    vAttr =
      case attr of
        GA.Cite cite ->
          V.Success (A.cite cite :: A.Attribute E.Blockquote)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Blockquote)

        _attr ->
          V.Failure [ wrongAttr attr Blockquote ]
  in
    vAttr <!> mkGlobalAttr attr

mkBlockquoteChild :: Element -> ValidHTML E.Blockquote grandparent
mkBlockquoteChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Blockquote ]

mkBody :: E.ValidChild E.Body parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkBody attrs node =
  E.body
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkBodyChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Body VoidNode ]
        )

mkBodyChild :: Element -> ValidHTML E.Body grandparent
mkBodyChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Body ]

mkLineBreak :: E.ValidChild E.LineBreak parent grandparent
            => [GA.Attribute] -> ValidHTML parent grandparent
mkLineBreak attrs =
  E.br <$> foldValidate mkGlobalAttr attrs

mkButton :: E.ValidChild E.Button parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkButton attrs node =
  E.button
    <$> foldValidate mkButtonAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkButtonChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Button VoidNode ]
        )

mkButtonAttr :: GA.Attribute -> ValidAttribute E.Button
mkButtonAttr attr =
  let
    vAttr =
      case attr of
        GA.Command command ->
          V.Success (A.command command :: A.Attribute E.Button)

        GA.CommandFor commandfor ->
          V.Success (A.commandfor commandfor :: A.Attribute E.Button)

        GA.Disabled disabled ->
          V.Success (A.disable disabled :: A.Attribute E.Button)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Button)

        GA.Form form ->
          V.Success (A.form form :: A.Attribute E.Button)

        GA.FormAction formaction ->
          V.Success (A.formaction formaction :: A.Attribute E.Button)

        GA.FormEnctype formenctype ->
          V.Success (A.formenctype formenctype :: A.Attribute E.Button)

        GA.FormMethod formmethod ->
          V.Success (A.formmethod formmethod :: A.Attribute E.Button)

        GA.FormNoValidate ->
          V.Success (A.formnovalidate :: A.Attribute E.Button)

        GA.FormTarget formtarget ->
          V.Success (A.formtarget formtarget :: A.Attribute E.Button)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.Button)

        GA.PopoverTarget popovertarget ->
          V.Success (A.popovertarget popovertarget :: A.Attribute E.Button)

        GA.PopoverTargetAction popovertargetaction ->
          V.Success
            (A.popovertargetaction popovertargetaction :: A.Attribute E.Button)

        GA.Type type_ ->
          V.Success (A.type_ type_ :: A.Attribute E.Button)

        GA.Value value ->
          V.Success (A.value value :: A.Attribute E.Button)

        _attr ->
          V.Failure [ wrongAttr attr Button ]
  in
    vAttr <!> mkGlobalAttr attr

mkButtonChild :: Element -> ValidHTML E.Button grandparent
mkButtonChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      IdiomaticText -> mkIdiomaticText attrs content
      Image -> mkImage attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Button ]

mkCanvas :: [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkCanvas attrs node =
  E.customHTML "canvas"
    <$> foldValidate mkCanvasAttr attrs
    <*> ( case node of
            Branch nodes -> Right <$> foldValidate mkCanvasChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Canvas LeafNode ]
            Void -> V.Failure [ wrongNodeType Canvas VoidNode ]
        )

mkCanvasAttr :: GA.Attribute -> ValidAttribute E.CustomHTML
mkCanvasAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.CustomHTML)

        GA.Height height ->
          V.Success (A.height height :: A.Attribute E.CustomHTML)

        GA.Width width ->
          V.Success (A.width width :: A.Attribute E.CustomHTML)

        _attr ->
          V.Failure [ wrongAttr attr Canvas ]
  in
    vAttr <!> mkGlobalAttr attr

mkCanvasChild :: Element -> ValidHTML E.CustomHTML grandparent
mkCanvasChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Button -> mkButton attrs content
      Input -> mkInput attrs
      element -> V.Failure [ wrongChild element Canvas ]

mkTableCaption :: E.ValidChild E.TableCaption parent grandparent
               => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTableCaption attrs node =
  E.caption
    <$> foldValidate mkTableCaptionAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableCaptionChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType TableCaption VoidNode ]
        )

mkTableCaptionAttr :: GA.Attribute -> ValidAttribute E.TableCaption
mkTableCaptionAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.TableCaption)

        _attr ->
          V.Failure [ wrongAttr attr TableCaption ]
  in
    vAttr <!> mkGlobalAttr attr

mkTableCaptionChild :: Element -> ValidHTML E.TableCaption grandparent
mkTableCaptionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element TableCaption ]

mkCitation :: E.ValidChild E.Citation parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkCitation attrs node =
  E.cite
    <$> foldValidate mkCitationAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkCitationChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Citation VoidNode ]
        )

mkCitationAttr :: GA.Attribute -> ValidAttribute E.Citation
mkCitationAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Citation)

        _attr ->
          V.Failure [ wrongAttr attr Citation ]
  in
    vAttr <!> mkGlobalAttr attr

mkCitationChild :: Element -> ValidHTML E.Citation grandparent
mkCitationChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Citation ]

mkCode :: E.ValidChild E.Code parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkCode attrs node =
  E.code
    <$> foldValidate mkCodeAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkCodeChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Code VoidNode ]
        )

mkCodeAttr :: GA.Attribute -> ValidAttribute E.Code
mkCodeAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Code)

        _attr ->
          V.Failure [ wrongAttr attr Code ]
  in
    vAttr <!> mkGlobalAttr attr

mkCodeChild :: Element -> ValidHTML E.Code grandparent
mkCodeChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Code ]

mkTableColumn :: E.ValidChild E.TableColumn parent grandparent
              => [GA.Attribute] -> ValidHTML parent grandparent
mkTableColumn attrs =
  E.col <$> foldValidate mkTableColumnAttr attrs

mkTableColumnAttr :: GA.Attribute -> ValidAttribute E.TableColumn
mkTableColumnAttr attr =
  let
    vAttr =
      case attr of
        GA.Span span -> V.Success (A.span span :: A.Attribute E.TableColumn)
        _attr -> V.Failure [ wrongAttr attr TableColumn ]
  in
    vAttr <!> mkGlobalAttr attr

mkTableColumnGroup :: E.ValidChild E.TableColumnGroup parent grandparent
                   => [GA.Attribute]
                   -> ElementNode
                   -> ValidHTML parent grandparent
mkTableColumnGroup attrs node =
  E.colgroup
    <$> foldValidate mkTableColumnGroupAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableColumnGroupChild nodes
            Leaf _net -> V.Failure [ wrongNodeType TableColumnGroup LeafNode ]
            Void -> V.Failure [ wrongNodeType TableColumnGroup VoidNode ]
        )

mkTableColumnGroupAttr :: GA.Attribute -> ValidAttribute E.TableColumnGroup
mkTableColumnGroupAttr attr =
  let
    vAttr =
      case attr of
        GA.Span span -> V.Success (A.span span :: A.Attribute E.TableColumnGroup)
        _attr -> V.Failure [ wrongAttr attr TableColumnGroup ]
  in
    vAttr <!> mkGlobalAttr attr

mkTableColumnGroupChild :: Element -> ValidHTML E.TableColumnGroup grandparent
mkTableColumnGroupChild e =
  case elementType e of
    TableColumn -> mkTableColumn $ elementAttrs e
    element -> V.Failure [ wrongChild element TableColumnGroup ]

mkData :: E.ValidChild E.Data parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkData attrs node =
  E.data_
    <$> foldValidate mkDataAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDataChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Data VoidNode ]
        )

mkDataAttr :: GA.Attribute -> ValidAttribute E.Data
mkDataAttr attr =
  let
    vAttr =
      case attr of
        GA.Value value -> V.Success (A.value value :: A.Attribute E.Data)
        _attr -> V.Failure [ wrongAttr attr Data ]
  in
    vAttr <!> mkGlobalAttr attr

mkDataChild :: Element -> ValidHTML E.Data grandparent
mkDataChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Data ]

mkDataList :: E.ValidChild E.DataList parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkDataList attrs node =
  E.datalist
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDataListChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType DataList VoidNode ]
        )

mkDataListChild :: Element -> ValidHTML E.DataList grandparent
mkDataListChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Option -> mkOption attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element DataList ]

mkDescriptionDetails :: E.ValidChild E.DescriptionDetails parent grandparent
                     => [GA.Attribute]
                     -> ElementNode
                     -> ValidHTML parent grandparent
mkDescriptionDetails attrs node =
  E.dd
    <$> foldValidate mkDescriptionDetailsAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDescriptionDetailsChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType DescriptionDetails VoidNode ]
        )

mkDescriptionDetailsAttr :: GA.Attribute -> ValidAttribute E.DescriptionDetails
mkDescriptionDetailsAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.DescriptionDetails)

        _attr ->
          V.Failure [ wrongAttr attr DescriptionDetails ]
  in
    vAttr <!> mkGlobalAttr attr

mkDescriptionDetailsChild :: Element
                          -> ValidHTML E.DescriptionDetails grandparent
mkDescriptionDetailsChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element DescriptionDetails ]

mkDeletedText :: forall parent grandparent.
                 E.ValidChild E.DeletedText parent grandparent
              => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkDeletedText attrs node =
  use @(E.ValidChild E.DeletedText parent grandparent) $ \Dict ->
    E.customHTML "del"
      <$> foldValidate mkDeletedTextAttr attrs
      <*> ( case node of
              Branch _nodes -> V.Failure [ wrongNodeType DeletedText BranchNode ]
              Leaf net -> V.Success $ Right [ E.text $ NET.toText net ]
              Void -> V.Failure [ wrongNodeType DeletedText VoidNode ]
          )

mkDeletedTextAttr :: GA.Attribute -> ValidAttribute E.CustomHTML
mkDeletedTextAttr attr =
  let
    vAttr =
      case attr of
        GA.Cite cite ->
          V.Success (A.cite cite :: A.Attribute E.CustomHTML)

        GA.Datetime datetime ->
          V.Success (A.datetime datetime :: A.Attribute E.CustomHTML)

        _attr ->
          V.Failure [ wrongAttr attr DeletedText ]
  in
    vAttr <!> mkGlobalAttr attr

mkDetails :: E.ValidChild E.Details parent grandparent
          => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkDetails attrs node =
  E.details
    <$> foldValidate mkDetailsAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDetailsChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Details VoidNode ]
        )

mkDetailsAttr :: GA.Attribute -> ValidAttribute E.Details
mkDetailsAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Details)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.Details)

        GA.Open ->
          V.Success (A.open :: A.Attribute E.Details)

        _attr ->
          V.Failure [ wrongAttr attr Details ]
  in
    vAttr <!> mkGlobalAttr attr

mkDetailsChild :: Element -> ValidHTML E.Details grandparent
mkDetailsChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Summary -> mkSummary attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Details ]

mkDefinition :: E.ValidChild E.Definition parent grandparent
             => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkDefinition attrs node =
  E.dfn
    <$> foldValidate mkDefinitionAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDefinitionChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Definition VoidNode ]
        )

mkDefinitionAttr :: GA.Attribute -> ValidAttribute E.Definition
mkDefinitionAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Definition)

        _attr ->
          V.Failure [ wrongAttr attr Definition ]
  in
    vAttr <!> mkGlobalAttr attr

mkDefinitionChild :: Element -> ValidHTML E.Definition grandparent
mkDefinitionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Definition ]

mkDialog :: E.ValidChild E.Dialog parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkDialog attrs node =
  E.dialog
    <$> foldValidate mkDialogAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDialogChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Dialog VoidNode ]
        )

mkDialogAttr :: GA.Attribute -> ValidAttribute E.Dialog
mkDialogAttr attr =
  let
    vAttr =
      case attr of
        GA.Open -> V.Success (A.open :: A.Attribute E.Dialog)
        _attr -> V.Failure [ wrongAttr attr Dialog ]
  in
    vAttr <!> mkGlobalAttr attr

mkDialogChild :: Element -> ValidHTML E.Dialog grandparent
mkDialogChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Dialog ]

mkDivision :: E.ValidChild E.Division parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkDivision attrs node =
  E.div
    <$> foldValidate mkDivisionAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDivisionChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Division VoidNode ]
        )

mkDivisionAttr :: GA.Attribute -> ValidAttribute E.Division
mkDivisionAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Division)

        _attr ->
          V.Failure [ wrongAttr attr Division ]
  in
    vAttr <!> mkGlobalAttr attr

mkDivisionChild :: Element -> ValidHTML E.Division grandparent
mkDivisionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Division ]

mkDescriptionList :: E.ValidChild E.DescriptionList parent grandparent
                  => [GA.Attribute]
                  -> ElementNode
                  -> ValidHTML parent grandparent
mkDescriptionList attrs node =
  E.dl
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDescriptionListChild nodes
            Leaf _net -> V.Failure [ wrongNodeType DescriptionList LeafNode ]
            Void -> V.Failure [ wrongNodeType DescriptionList VoidNode ]
        )

mkDescriptionListChild :: Element -> ValidHTML E.DescriptionList grandparent
mkDescriptionListChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      DescriptionDetails -> mkDescriptionDetails attrs content
      DescriptionTerm -> mkDescriptionTerm attrs content
      Division -> mkDivision attrs content
      Script -> mkScript attrs content
      ContentTemplate ->  mkContentTemplate attrs content
      element -> V.Failure [ wrongChild element DescriptionList ]

mkDescriptionTerm :: E.ValidChild E.DescriptionTerm parent grandparent
                  => [GA.Attribute]
                  -> ElementNode
                  -> ValidHTML parent grandparent
mkDescriptionTerm attrs node =
  E.dt
    <$> foldValidate mkDescriptionTermAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkDescriptionTermChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType DescriptionTerm VoidNode ]
        )

mkDescriptionTermAttr :: GA.Attribute -> ValidAttribute E.DescriptionTerm
mkDescriptionTermAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.DescriptionTerm)

        _attr ->
          V.Failure [ wrongAttr attr DescriptionTerm ]
  in
    vAttr <!> mkGlobalAttr attr

mkDescriptionTermChild :: Element -> ValidHTML E.DescriptionTerm grandparent
mkDescriptionTermChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Form -> mkForm attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element DescriptionTerm ]

mkEmphasis :: E.ValidChild E.Emphasis parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkEmphasis attrs node =
  E.em
    <$> foldValidate mkEmphasisAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkEmphasisChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Emphasis VoidNode ]
        )

mkEmphasisAttr :: GA.Attribute -> ValidAttribute E.Emphasis
mkEmphasisAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Emphasis)

        _attr ->
          V.Failure [ wrongAttr attr Emphasis ]
  in
    vAttr <!> mkGlobalAttr attr

mkEmphasisChild :: Element -> ValidHTML E.Emphasis grandparent
mkEmphasisChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Emphasis ]

mkEmbed :: E.ValidChild E.Embed parent grandparent
        => [GA.Attribute] -> ValidHTML parent grandparent
mkEmbed attrs =
  E.embed <$> foldValidate mkEmbedAttr attrs

mkEmbedAttr :: GA.Attribute -> ValidAttribute E.Embed
mkEmbedAttr attr =
  let
    vAttr =
      case attr of
        GA.Height height ->
          V.Success (A.height height :: A.Attribute E.Embed)

        GA.Src src ->
          V.Success (A.src src :: A.Attribute E.Embed)

        GA.Type type_ ->
          V.Success (A.type_ type_ :: A.Attribute E.Embed)

        GA.Width width ->
          V.Success (A.width width :: A.Attribute E.Embed)

        _attr ->
          V.Failure [ wrongAttr attr Embed ]
  in
    vAttr <!> mkGlobalAttr attr

mkFieldset :: E.ValidChild E.Fieldset parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkFieldset attrs node =
  E.fieldset
    <$> foldValidate mkFieldsetAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkFieldsetChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Fieldset VoidNode ]
        )

mkFieldsetAttr :: GA.Attribute -> ValidAttribute E.Fieldset
mkFieldsetAttr attr =
  let
    vAttr =
      case attr of
        GA.Disabled disabled ->
          V.Success (A.disable disabled :: A.Attribute E.Fieldset)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Fieldset)

        GA.Form form ->
          V.Success (A.form form :: A.Attribute E.Fieldset)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.Fieldset)

        _attr ->
          V.Failure [ wrongAttr attr Fieldset ]
  in
    vAttr <!> mkGlobalAttr attr

mkFieldsetChild :: Element -> ValidHTML E.Fieldset grandparent
mkFieldsetChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Legend -> mkLegend attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Fieldset ]

mkFigureCaption :: E.ValidChild E.FigureCaption parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> ValidHTML parent grandparent
mkFigureCaption attrs node =
  E.figcaption
    <$> foldValidate mkFigureCaptionAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkFigureCaptionChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType FigureCaption VoidNode ]
        )

mkFigureCaptionAttr :: GA.Attribute -> ValidAttribute E.FigureCaption
mkFigureCaptionAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.FigureCaption)

        _attr ->
          V.Failure [ wrongAttr attr FigureCaption ]
  in
    vAttr <!> mkGlobalAttr attr

mkFigureCaptionChild :: Element -> ValidHTML E.FigureCaption grandparent
mkFigureCaptionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element FigureCaption ]

mkFigure :: E.ValidChild E.Figure parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkFigure attrs node =
  E.figure
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkFigureChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Figure VoidNode ]
        )

mkFigureChild :: Element -> ValidHTML E.Figure grandparent
mkFigureChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      FigureCaption -> mkFigureCaption attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Figure ]

mkFooter :: E.ValidChild E.Footer parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkFooter attrs node =
  E.footer
    <$> foldValidate mkFooterAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkFooterChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Footer VoidNode ]
        )

mkFooterAttr :: GA.Attribute -> ValidAttribute E.Footer
mkFooterAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Footer)

        _attr ->
          V.Failure [ wrongAttr attr Footer ]
  in
    vAttr <!> mkGlobalAttr attr

mkFooterChild :: Element -> ValidHTML E.Footer grandparent
mkFooterChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Footer ]

mkForm :: E.ValidChild E.Form parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkForm attrs node =
  E.form
    <$> foldValidate mkFormAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkFormChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Form VoidNode ]
        )

mkFormAttr :: GA.Attribute -> ValidAttribute E.Form
mkFormAttr attr =
  let
    vAttr =
      case attr of
        GA.AcceptCharset ->
          V.Success (A.acceptCharset :: A.Attribute E.Form)

        GA.Action action ->
          V.Success (A.action action :: A.Attribute E.Form)

        GA.Autocomplete autocomplete ->
          V.Success (A.autocomplete autocomplete :: A.Attribute E.Form)

        GA.Enctype enctype ->
          V.Success (A.enctype enctype :: A.Attribute E.Form)

        GA.Method method ->
          V.Success (A.method method :: A.Attribute E.Form)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.Form)

        GA.NoValidate novalidate ->
          V.Success (A.validate novalidate :: A.Attribute E.Form)

        GA.Rel rel ->
          V.Success (A.rel rel :: A.Attribute E.Form)

        GA.Target target ->
          V.Success (A.target target :: A.Attribute E.Form)

        _attr ->
          V.Failure [ wrongAttr attr Form ]
  in
    vAttr <!> mkGlobalAttr attr

mkFormChild :: Element -> ValidHTML E.Form grandparent
mkFormChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Form ]

mkH1 :: E.ValidChild E.H1 parent grandparent
     => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkH1 attrs node =
  E.h1
    <$> foldValidate mkH1Attr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkH1Child nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType H1 VoidNode ]
        )

mkH1Attr :: GA.Attribute -> ValidAttribute E.H1
mkH1Attr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.H1)

        _attr ->
          V.Failure [ wrongAttr attr H1 ]
  in
    vAttr <!> mkGlobalAttr attr

mkH1Child :: Element -> ValidHTML E.H1 grandparent
mkH1Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element H1 ]

mkH2 :: E.ValidChild E.H2 parent grandparent
     => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkH2 attrs node =
  E.h2
    <$> foldValidate mkH2Attr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkH2Child nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType H2 VoidNode ]
        )

mkH2Attr :: GA.Attribute -> ValidAttribute E.H2
mkH2Attr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.H2)

        _attr ->
          V.Failure [ wrongAttr attr H2 ]
  in
    vAttr <!> mkGlobalAttr attr

mkH2Child :: Element -> ValidHTML E.H2 grandparent
mkH2Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element H2 ]

mkH3 :: E.ValidChild E.H3 parent grandparent
     => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkH3 attrs node =
  E.h3
    <$> foldValidate mkH3Attr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkH3Child nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType H3 VoidNode ]
        )

mkH3Attr :: GA.Attribute -> ValidAttribute E.H3
mkH3Attr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.H3)

        _attr ->
          V.Failure [ wrongAttr attr H3 ]
  in
    vAttr <!> mkGlobalAttr attr

mkH3Child :: Element -> ValidHTML E.H3 grandparent
mkH3Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element H3 ]

mkH4 :: E.ValidChild E.H4 parent grandparent
     => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkH4 attrs node =
  E.h4
    <$> foldValidate mkH4Attr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkH4Child nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType H4 VoidNode ]
        )

mkH4Attr :: GA.Attribute -> ValidAttribute E.H4
mkH4Attr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.H4)

        _attr ->
          V.Failure [ wrongAttr attr H4 ]
  in
    vAttr <!> mkGlobalAttr attr

mkH4Child :: Element -> ValidHTML E.H4 grandparent
mkH4Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element H4 ]

mkH5 :: E.ValidChild E.H5 parent grandparent
     => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkH5 attrs node =
  E.h5
    <$> foldValidate mkH5Attr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkH5Child nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType H5 VoidNode ]
        )

mkH5Attr :: GA.Attribute -> ValidAttribute E.H5
mkH5Attr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.H5)

        _attr ->
          V.Failure [ wrongAttr attr H5 ]
  in
    vAttr <!> mkGlobalAttr attr

mkH5Child :: Element -> ValidHTML E.H5 grandparent
mkH5Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element H5 ]

mkH6 :: E.ValidChild E.H6 parent grandparent
     => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkH6 attrs node =
  E.h6
    <$> foldValidate mkH6Attr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkH6Child nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType H6 VoidNode ]
        )

mkH6Attr :: GA.Attribute -> ValidAttribute E.H6
mkH6Attr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.H6)

        _attr ->
          V.Failure [ wrongAttr attr H6 ]
  in
    vAttr <!> mkGlobalAttr attr

mkH6Child :: Element -> ValidHTML E.H6 grandparent
mkH6Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element H6 ]

mkHead :: E.ValidChild E.Head parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkHead attrs node =
  E.head
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkHeadChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Head LeafNode ]
            Void -> V.Failure [ wrongNodeType Head VoidNode ]
        )

mkHeadChild :: Element -> ValidHTML E.Head grandparent
mkHeadChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Base -> mkBase attrs
      Link -> mkLink attrs
      Meta -> mkMeta attrs
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Script -> mkScript attrs content
      Style -> mkStyle attrs content
      Title -> mkTitle attrs content
      element -> V.Failure [ wrongChild element Head ]

mkHeader :: E.ValidChild E.Header parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkHeader attrs node =
  E.header
    <$> foldValidate mkHeaderAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkHeaderChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Header VoidNode ]
        )

mkHeaderAttr :: GA.Attribute -> ValidAttribute E.Header
mkHeaderAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Header)

        _attr ->
          V.Failure [ wrongAttr attr Header ]
  in
    vAttr <!> mkGlobalAttr attr

mkHeaderChild :: Element -> ValidHTML E.Header grandparent
mkHeaderChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Header ]

mkHeadingGroup :: E.ValidChild E.HeadingGroup parent grandparent
               => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkHeadingGroup attrs node =
  E.hgroup
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkHeadingGroupChild nodes
            Leaf _net -> V.Failure [ wrongNodeType HeadingGroup LeafNode ]
            Void -> V.Failure [ wrongNodeType HeadingGroup VoidNode ]
        )

mkHeadingGroupChild :: Element -> ValidHTML E.HeadingGroup grandparent
mkHeadingGroupChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Paragraph -> mkParagraph attrs content
      element -> V.Failure [ wrongChild element HeadingGroup ]

mkHorizontalRule :: E.ValidChild E.HorizontalRule parent grandparent
                 => [GA.Attribute] -> ValidHTML parent grandparent
mkHorizontalRule attrs =
  E.hr <$> foldValidate mkGlobalAttr attrs

mkHtml :: [GA.Attribute] -> ElementNode -> ValidHTML E.CustomHTML grandparent
mkHtml attrs node =
  E.customHTML "html"
    <$> foldValidate mkHtmlAttr attrs
    <*> ( case node of
            Branch nodes -> Right <$> foldValidate mkHtmlChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Html LeafNode ]
            Void -> V.Failure [ wrongNodeType Html VoidNode ]
        )

mkHtmlAttr :: GA.Attribute -> ValidAttribute E.CustomHTML
mkHtmlAttr attr =
  let
    vAttr =
      case attr of
        GA.XMLNS xmlns -> V.Success (A.xmlns xmlns :: A.Attribute E.CustomHTML)
        _attr -> V.Failure [ wrongAttr attr Html ]
  in
    vAttr <!> mkGlobalAttr attr

mkHtmlChild :: Element -> ValidHTML E.CustomHTML grandparent
mkHtmlChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Body -> mkBody attrs content
      Head -> mkHead attrs content
      element -> V.Failure [ wrongChild element Html ]

mkIdiomaticText :: E.ValidChild E.IdiomaticText parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> ValidHTML parent grandparent
mkIdiomaticText attrs node =
  E.i
    <$> foldValidate mkIdiomaticTextAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkIdiomaticTextChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType IdiomaticText VoidNode ]
        )

mkIdiomaticTextAttr :: GA.Attribute -> ValidAttribute E.IdiomaticText
mkIdiomaticTextAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.IdiomaticText)

        _attr ->
          V.Failure [ wrongAttr attr IdiomaticText ]
  in
    vAttr <!> mkGlobalAttr attr

mkIdiomaticTextChild :: Element -> ValidHTML E.IdiomaticText grandparent
mkIdiomaticTextChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element IdiomaticText ]

mkIFrame :: E.ValidChild E.IFrame parent grandparent
         => [GA.Attribute] -> ValidHTML parent grandparent
mkIFrame attrs =
  E.iframe <$> foldValidate mkIFrameAttr attrs

mkIFrameAttr :: GA.Attribute -> ValidAttribute E.IFrame
mkIFrameAttr attr =
  let
    vAttr =
      case attr of
        GA.Allow allow ->
          V.Success (A.allow allow :: A.Attribute E.IFrame)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.IFrame)

        GA.Height height ->
          V.Success (A.height height :: A.Attribute E.IFrame)

        GA.Loading loading ->
          V.Success (A.loading loading :: A.Attribute E.IFrame)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.IFrame)

        GA.ReferrerPolicy referrerpolicy ->
          V.Success (A.referrerpolicy referrerpolicy :: A.Attribute E.IFrame)

        GA.Sandbox sandbox ->
          V.Success (A.sandbox sandbox :: A.Attribute E.IFrame)

        GA.Src src ->
          V.Success (A.src src :: A.Attribute E.IFrame)

        GA.SrcDoc srcdoc ->
          V.Success (A.srcdoc' srcdoc :: A.Attribute E.IFrame)

        GA.Width width ->
          V.Success (A.width width :: A.Attribute E.IFrame)

        _attr ->
          V.Failure [ wrongAttr attr IFrame ]
  in
    vAttr <!> mkGlobalAttr attr

mkImage :: E.ValidChild E.Image parent grandparent
        => [GA.Attribute] -> ValidHTML parent grandparent
mkImage attrs =
  E.img <$> foldValidate mkImageAttr attrs

mkImageAttr :: GA.Attribute -> ValidAttribute E.Image
mkImageAttr attr =
  let
    vAttr =
      case attr of
        GA.Alt alt ->
          V.Success (A.alt alt :: A.Attribute E.Image)

        GA.CrossOrigin crossorigin ->
          V.Success (A.crossorigin crossorigin :: A.Attribute E.Image)

        GA.Decoding decoding ->
          V.Success (A.decoding decoding :: A.Attribute E.Image)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Image)

        GA.FetchPriority fetchpriority ->
          V.Success (A.fetchpriority fetchpriority :: A.Attribute E.Image)

        GA.Height height ->
          V.Success (A.height height :: A.Attribute E.Image)

        GA.IsMap ->
          V.Success (A.ismap :: A.Attribute E.Image)

        GA.Loading loading ->
          V.Success (A.loading loading :: A.Attribute E.Image)

        GA.ReferrerPolicy referrerpolicy ->
          V.Success (A.referrerpolicy referrerpolicy :: A.Attribute E.Image)

        GA.Sizes sizes ->
          V.Success (A.sizes sizes :: A.Attribute E.Image)

        GA.Src src ->
          V.Success (A.src src :: A.Attribute E.Image)

        GA.SrcSet srcset ->
          V.Success (A.srcset srcset :: A.Attribute E.Image)

        GA.UseMap usemap ->
          V.Success (A.usemap usemap :: A.Attribute E.Image)

        GA.Width width ->
          V.Success (A.width width :: A.Attribute E.Image)

        _attr ->
          V.Failure [ wrongAttr attr Image ]
  in
    vAttr <!> mkGlobalAttr attr

mkInput :: E.ValidChild E.Input parent grandparent
        => [GA.Attribute] -> ValidHTML parent grandparent
mkInput attrs =
  E.input <$> foldValidate mkInputAttr attrs

mkInputAttr :: GA.Attribute -> ValidAttribute E.Input
mkInputAttr attr =
  let
    vAttr =
      case attr of
        GA.Accept accept ->
          V.Success (A.accept accept :: A.Attribute E.Input)

        GA.Alt alt ->
          V.Success (A.alt alt :: A.Attribute E.Input)

        GA.Autocomplete autocomplete ->
          V.Success (A.autocomplete autocomplete :: A.Attribute E.Input)

        GA.Capture capture ->
          V.Success (A.capture capture :: A.Attribute E.Input)

        GA.Checked checked ->
          V.Success (A.check checked :: A.Attribute E.Input)

        GA.Dirname dirname ->
          V.Success (A.dirname dirname :: A.Attribute E.Input)

        GA.Disabled disabled ->
          V.Success (A.disable disabled :: A.Attribute E.Input)

        GA.Form form ->
          V.Success (A.form form :: A.Attribute E.Input)

        GA.FormAction formaction ->
          V.Success (A.formaction formaction :: A.Attribute E.Input)

        GA.FormEnctype formenctype ->
          V.Success (A.formenctype formenctype :: A.Attribute E.Input)

        GA.FormMethod formmethod ->
          V.Success (A.formmethod formmethod :: A.Attribute E.Input)

        GA.FormNoValidate ->
          V.Success (A.formnovalidate :: A.Attribute E.Input)

        GA.FormTarget formtarget ->
          V.Success (A.formtarget formtarget :: A.Attribute E.Input)

        GA.Height height ->
          V.Success (A.height height :: A.Attribute E.Input)

        GA.List list ->
          V.Success (A.list list :: A.Attribute E.Input)

        GA.Max max ->
          V.Success (A.max max :: A.Attribute E.Input)

        GA.MaxLength maxlength ->
          V.Success (A.maxlength maxlength :: A.Attribute E.Input)

        GA.Min min ->
          V.Success (A.min min :: A.Attribute E.Input)

        GA.MinLength minlength ->
          V.Success (A.minlength minlength :: A.Attribute E.Input)

        GA.Multiple ->
          V.Success (A.multiple :: A.Attribute E.Input)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.Input)

        GA.Pattern pattern ->
          V.Success (A.pattern pattern :: A.Attribute E.Input)

        GA.Placeholder placeholder ->
          V.Success (A.placeholder placeholder :: A.Attribute E.Input)

        GA.PopoverTarget popovertarget ->
          V.Success (A.popovertarget popovertarget :: A.Attribute E.Input)

        GA.PopoverTargetAction popovertargetaction ->
          V.Success (A.popovertargetaction popovertargetaction :: A.Attribute E.Input)

        GA.ReadOnly ->
          V.Success (A.readonly :: A.Attribute E.Input)

        GA.Required required ->
          V.Success (A.require required :: A.Attribute E.Input)

        GA.Size size ->
          V.Success (A.size size :: A.Attribute E.Input)

        GA.Src src ->
          V.Success (A.src src :: A.Attribute E.Input)

        GA.Step step ->
          V.Success (A.step step :: A.Attribute E.Input)

        GA.Type type_ ->
          V.Success (A.type_ type_ :: A.Attribute E.Input)

        GA.Value value ->
          V.Success (A.value value :: A.Attribute E.Input)

        GA.Width width ->
          V.Success (A.width width :: A.Attribute E.Input)

        _attr ->
          V.Failure [ wrongAttr attr Input ]
  in
    vAttr <!> mkGlobalAttr attr

mkInsertedText :: forall parent grandparent.
                  E.ValidChild E.InsertedText parent grandparent
               => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkInsertedText attrs node =
  use @(E.ValidChild E.InsertedText parent grandparent) $ \Dict ->
    E.customHTML "ins"
      <$> foldValidate mkInsertedTextAttr attrs
      <*> ( case node of
              Branch _nodes -> V.Failure [ wrongNodeType InsertedText BranchNode ]
              Leaf net -> V.Success $ Right [ E.text $ NET.toText net ]
              Void -> V.Failure [ wrongNodeType InsertedText VoidNode ]
          )

mkInsertedTextAttr :: GA.Attribute -> ValidAttribute E.CustomHTML
mkInsertedTextAttr attr =
  let
    vAttr =
      case attr of
        GA.Cite cite ->
          V.Success (A.cite cite :: A.Attribute E.CustomHTML)

        GA.Datetime datetime ->
          V.Success (A.datetime datetime :: A.Attribute E.CustomHTML)

        _attr ->
          V.Failure [ wrongAttr attr InsertedText ]
  in
    vAttr <!> mkGlobalAttr attr

mkKeyboardInput :: E.ValidChild E.KeyboardInput parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> ValidHTML parent grandparent
mkKeyboardInput attrs node =
  E.kbd
    <$> foldValidate mkKeyboardInputAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkKeyboardInputChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType KeyboardInput VoidNode ]
        )

mkKeyboardInputAttr :: GA.Attribute -> ValidAttribute E.KeyboardInput
mkKeyboardInputAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.KeyboardInput)

        _attr ->
          V.Failure [ wrongAttr attr KeyboardInput ]
  in
    vAttr <!> mkGlobalAttr attr

mkKeyboardInputChild :: Element -> ValidHTML E.KeyboardInput grandparent
mkKeyboardInputChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element KeyboardInput ]

mkLabel :: E.ValidChild E.Label parent grandparent
        => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkLabel attrs node =
  E.label
    <$> foldValidate mkLabelAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkLabelChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Label VoidNode ]
        )

mkLabelAttr :: GA.Attribute -> ValidAttribute E.Label
mkLabelAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Label)

        GA.ForLabel for ->
          V.Success (A.for for :: A.Attribute E.Label)

        _attr ->
          V.Failure [ wrongAttr attr Label ]
  in
    vAttr <!> mkGlobalAttr attr

mkLabelChild :: Element -> ValidHTML E.Label grandparent
mkLabelChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Label ]

mkLegend :: E.ValidChild E.Legend parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkLegend attrs node =
  E.legend
    <$> foldValidate mkLegendAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkLegendChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Legend VoidNode ]
        )

mkLegendAttr :: GA.Attribute -> ValidAttribute E.Legend
mkLegendAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Legend)

        _attr ->
          V.Failure [ wrongAttr attr Legend ]
  in
    vAttr <!> mkGlobalAttr attr

mkLegendChild :: Element -> ValidHTML E.Legend grandparent
mkLegendChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Legend ]

mkListItem :: E.ValidChild E.ListItem parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkListItem attrs node =
  E.li
    <$> foldValidate mkListItemAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkListItemChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType ListItem VoidNode ]
        )

mkListItemAttr :: GA.Attribute -> ValidAttribute E.ListItem
mkListItemAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.ListItem)

        GA.ValueInteger value ->
          V.Success (A.value value :: A.Attribute E.ListItem)

        _attr ->
          V.Failure [ wrongAttr attr ListItem ]
  in
    vAttr <!> mkGlobalAttr attr

mkListItemChild :: Element -> ValidHTML E.ListItem grandparent
mkListItemChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element ListItem ]

mkLink :: E.ValidChild E.Link parent grandparent
       => [GA.Attribute] -> ValidHTML parent grandparent
mkLink attrs =
  E.link <$> foldValidate mkLinkAttr attrs

mkLinkAttr :: GA.Attribute -> ValidAttribute E.Link
mkLinkAttr attr =
  let
    vAttr =
      case attr of
        GA.As as ->
          V.Success (A.as as :: A.Attribute E.Link)

        GA.Blocking blocking ->
          V.Success (A.blocking blocking :: A.Attribute E.Link)

        GA.CrossOrigin crossorigin ->
          V.Success (A.crossorigin crossorigin :: A.Attribute E.Link)

        GA.Disabled disabled ->
          V.Success (A.disable disabled :: A.Attribute E.Link)

        GA.FetchPriority fetchpriority ->
          V.Success (A.fetchpriority fetchpriority :: A.Attribute E.Link)

        GA.Href href ->
          V.Success (A.href href :: A.Attribute E.Link)

        GA.HrefLang hreflang ->
          V.Success (A.hreflang hreflang :: A.Attribute E.Link)

        GA.ImageSizes imagesizes ->
          V.Success (A.imagesizes imagesizes :: A.Attribute E.Link)

        GA.ImageSrcset imagesrcset ->
          V.Success (A.imagesrcset imagesrcset :: A.Attribute E.Link)

        GA.Integrity encoding bs ->
          V.Success (A.integrity encoding bs :: A.Attribute E.Link)

        GA.Media media ->
          V.Success (A.media media :: A.Attribute E.Link)

        GA.ReferrerPolicy referrerpolicy ->
          V.Success (A.referrerpolicy referrerpolicy :: A.Attribute E.Link)

        GA.Rel rel ->
          V.Success (A.rel rel :: A.Attribute E.Link)

        GA.Sizes sizes ->
          V.Success (A.sizes sizes :: A.Attribute E.Link)

        GA.Title title ->
          V.Success (A.title title :: A.Attribute E.Link)

        GA.Type type_ ->
          V.Success (A.type_ type_ :: A.Attribute E.Link)

        _attr ->
          V.Failure [ wrongAttr attr Link ]
  in
    vAttr <!> mkGlobalAttr attr

mkMain :: E.ValidChild E.Main parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkMain attrs node =
  E.main
    <$> foldValidate mkMainAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkMainChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Main VoidNode ]
        )

mkMainAttr :: GA.Attribute -> ValidAttribute E.Main
mkMainAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Main)

        _attr ->
          V.Failure [ wrongAttr attr Main ]
  in
    vAttr <!> mkGlobalAttr attr

mkMainChild :: Element -> ValidHTML E.Main grandparent
mkMainChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Main ]

mkMap :: E.ValidChild E.Map parent grandparent
      => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkMap attrs node =
  E.map
    <$> foldValidate mkMapAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkMapChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Map LeafNode ]
            Void -> V.Failure [ wrongNodeType Map VoidNode ]
        )

mkMapAttr :: GA.Attribute -> ValidAttribute E.Map
mkMapAttr attr =
  let
    vAttr =
      case attr of
        GA.Name name -> V.Success (A.name name :: A.Attribute E.Map)
        _attr -> V.Failure [ wrongAttr attr Map ]
  in
    vAttr <!> mkGlobalAttr attr

mkMapChild :: Element -> ValidHTML E.Map grandparent
mkMapChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Audio -> mkAudio attrs content
      Canvas -> mkCanvas attrs content
      DeletedText -> mkDeletedText attrs content
      InsertedText -> mkInsertedText attrs content
      Map -> mkMap attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Slot -> mkSlot attrs content
      Video -> mkVideo attrs content
      element -> V.Failure [ wrongChild element Map ]

mkMark :: E.ValidChild E.Mark parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkMark attrs node =
  E.mark
    <$> foldValidate mkMarkAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkMarkChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Mark VoidNode ]
        )

mkMarkAttr :: GA.Attribute -> ValidAttribute E.Mark
mkMarkAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Mark)

        _attr ->
          V.Failure [ wrongAttr attr Mark ]
  in
    vAttr <!> mkGlobalAttr attr

mkMarkChild :: Element -> ValidHTML E.Mark grandparent
mkMarkChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Mark ]

mkMenu :: E.ValidChild E.Menu parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkMenu attrs node =
  E.menu
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkMenuChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Menu LeafNode ]
            Void -> V.Failure [ wrongNodeType Menu VoidNode ]
        )

mkMenuChild :: Element -> ValidHTML E.Menu grandparent
mkMenuChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      ListItem -> mkListItem attrs content
      Script -> mkScript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      element -> V.Failure [ wrongChild element Menu ]

mkMeta :: E.ValidChild E.Meta parent grandparent
       => [GA.Attribute] -> ValidHTML parent grandparent
mkMeta attrs =
  E.meta <$> foldValidate mkMetaAttr attrs

mkMetaAttr :: GA.Attribute -> ValidAttribute E.Meta
mkMetaAttr attr =
  let
    vAttr =
      case attr of
        GA.Charset ->
          V.Success (A.charset :: A.Attribute E.Meta)

        GA.Content content ->
          V.Success (A.content content :: A.Attribute E.Meta)

        GA.HttpEquiv httpEquiv ->
          V.Success (A.httpEquiv httpEquiv :: A.Attribute E.Meta)

        GA.Media media ->
          V.Success (A.media media :: A.Attribute E.Meta)

        GA.NameMeta name ->
          V.Success (A.name name :: A.Attribute E.Meta)

        _attr ->
          V.Failure [ wrongAttr attr Meta ]
  in
    vAttr <!> mkGlobalAttr attr

mkMeter :: E.ValidChild E.Meter parent grandparent
        => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkMeter attrs node =
  E.meter
    <$> foldValidate mkMeterAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkMeterChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Meter VoidNode ]
        )

mkMeterAttr :: GA.Attribute -> ValidAttribute E.Meter
mkMeterAttr attr =
  let
    vAttr =
      case attr of
        GA.Form form -> V.Success (A.form form :: A.Attribute E.Meter)
        GA.High high -> V.Success (A.high high :: A.Attribute E.Meter)
        GA.Low low -> V.Success (A.low low :: A.Attribute E.Meter)
        GA.Max max -> V.Success (A.max max :: A.Attribute E.Meter)
        GA.Min min -> V.Success (A.min min :: A.Attribute E.Meter)
        GA.Optimum optimum -> V.Success (A.optimum optimum :: A.Attribute E.Meter)
        GA.ValueNumber value -> V.Success (A.value value :: A.Attribute E.Meter)
        _attr -> V.Failure [ wrongAttr attr Meter ]
  in
    vAttr <!> mkGlobalAttr attr

mkMeterChild :: Element -> ValidHTML E.Meter grandparent
mkMeterChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Meter ]

mkNav :: E.ValidChild E.Nav parent grandparent
      => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkNav attrs node =
  E.nav
    <$> foldValidate mkNavAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkNavChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Nav VoidNode ]
        )

mkNavAttr :: GA.Attribute -> ValidAttribute E.Nav
mkNavAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Nav)

        _attr ->
          V.Failure [ wrongAttr attr Nav ]
  in
    vAttr <!> mkGlobalAttr attr

mkNavChild :: Element -> ValidHTML E.Nav grandparent
mkNavChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Nav ]

mkNoScriptHead :: [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkNoScriptHead attrs node =
  E.customHTML "noscript"
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> Right <$> foldValidate mkNoScriptHeadChild nodes
            Leaf _net -> V.Failure [ wrongNodeType NoScriptHead LeafNode ]
            Void -> V.Failure [ wrongNodeType NoScriptHead VoidNode ]
        )

mkNoScriptHeadChild :: Element -> ValidHTML E.CustomHTML grandparent
mkNoScriptHeadChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Link -> mkLink attrs
      Meta -> mkMeta attrs
      Style -> mkStyle attrs content
      element -> V.Failure [ wrongChild element NoScriptHead ]

mkNoScriptBody :: [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkNoScriptBody attrs node =
  E.customHTML "noscript"
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> Right <$> foldValidate mkNoScriptBodyChild nodes
            Leaf net -> V.Success $ Right [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType NoScriptBody VoidNode ]
        )

mkNoScriptBodyChild :: Element -> ValidHTML E.CustomHTML grandparent
mkNoScriptBodyChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Area -> mkArea attrs
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Search -> mkSearch attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element NoScriptBody ]

mkObject :: forall parent grandparent.
            E.ValidChild E.Object parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkObject attrs node =
  use @(E.ValidChild E.Object parent grandparent) $ \Dict ->
    E.customHTML "object"
      <$> foldValidate mkObjectAttr attrs
      <*> ( case node of
              Branch _nodes -> V.Failure [ wrongNodeType Object BranchNode ]
              Leaf net -> V.Success $ Right [ E.text $ NET.toText net ]
              Void -> V.Failure [ wrongNodeType Object VoidNode ]
          )

mkObjectAttr :: GA.Attribute -> ValidAttribute E.CustomHTML
mkObjectAttr attr =
  let
    vAttr =
      case attr of
        GA.Data data_ -> V.Success (A.data_ data_ :: A.Attribute E.CustomHTML)
        GA.Form form -> V.Success (A.form form :: A.Attribute E.CustomHTML)
        GA.Height height -> V.Success (A.height height :: A.Attribute E.CustomHTML)
        GA.Name name -> V.Success (A.name name :: A.Attribute E.CustomHTML)
        GA.Type type_ -> V.Success (A.type_ type_ :: A.Attribute E.CustomHTML)
        GA.Width width -> V.Success (A.width width :: A.Attribute E.CustomHTML)
        _attr -> V.Failure [ wrongAttr attr Object ]
  in
    vAttr <!> mkGlobalAttr attr

mkOrderedList :: E.ValidChild E.OrderedList parent grandparent
              => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkOrderedList attrs node =
  E.ol
    <$> foldValidate mkOrderedListAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkOrderedListChild nodes
            Leaf _net -> V.Failure [ wrongNodeType OrderedList LeafNode ]
            Void -> V.Failure [ wrongNodeType OrderedList VoidNode ]
        )

mkOrderedListAttr :: GA.Attribute -> ValidAttribute E.OrderedList
mkOrderedListAttr attr =
  let
    vAttr =
      case attr of
        GA.Reversed reversed ->
          V.Success (A.reverse reversed :: A.Attribute E.OrderedList)

        GA.Start start ->
          V.Success (A.start start :: A.Attribute E.OrderedList)

        GA.Type type_ ->
          V.Success (A.type_ type_ :: A.Attribute E.OrderedList)

        _attr ->
          V.Failure [ wrongAttr attr OrderedList ]
  in
    vAttr <!> mkGlobalAttr attr

mkOrderedListChild :: Element -> ValidHTML E.OrderedList grandparent
mkOrderedListChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      ListItem -> mkListItem attrs content
      Script -> mkScript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      element -> V.Failure [ wrongChild element OrderedList ]

mkOptionGroup :: E.ValidChild E.OptionGroup parent grandparent
              => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkOptionGroup attrs node =
  E.optgroup
    <$> foldValidate mkOptionGroupAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkOptionGroupChild nodes
            Leaf _net -> V.Failure [ wrongNodeType OptionGroup LeafNode ]
            Void -> V.Failure [ wrongNodeType OptionGroup VoidNode ]
        )

mkOptionGroupAttr :: GA.Attribute -> ValidAttribute E.OptionGroup
mkOptionGroupAttr attr =
  let
    vAttr =
      case attr of
        GA.Disabled disabled ->
          V.Success (A.disable disabled :: A.Attribute E.OptionGroup)

        GA.Label label ->
          V.Success (A.label label :: A.Attribute E.OptionGroup)

        _attr ->
          V.Failure [ wrongAttr attr OptionGroup ]
  in
    vAttr <!> mkGlobalAttr attr

mkOptionGroupChild :: Element -> ValidHTML E.OptionGroup grandparent
mkOptionGroupChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Option -> mkOption attrs content
      element -> V.Failure [ wrongChild element OptionGroup ]

mkOption :: E.ValidChild E.Option parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkOption attrs node =
  E.option
    <$> foldValidate mkOptionAttr attrs
    <*> ( case node of
            Branch _nodes -> V.Failure [ wrongNodeType Option BranchNode ]
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Option VoidNode ]
        )

mkOptionAttr :: GA.Attribute -> ValidAttribute E.Option
mkOptionAttr attr =
  let
    vAttr =
      case attr of
        GA.Disabled disabled ->
          V.Success (A.disable disabled :: A.Attribute E.Option)

        GA.Label label ->
          V.Success (A.label label :: A.Attribute E.Option)

        GA.Selected selected ->
          V.Success (A.select selected :: A.Attribute E.Option)

        GA.Value value ->
          V.Success (A.value value :: A.Attribute E.Option)

        _attr ->
          V.Failure [ wrongAttr attr Option ]
  in
    vAttr <!> mkGlobalAttr attr

mkOutput :: E.ValidChild E.Output parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkOutput attrs node =
  E.output
    <$> foldValidate mkOutputAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkOutputChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Output VoidNode ]
        )

mkOutputAttr :: GA.Attribute -> ValidAttribute E.Output
mkOutputAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Output)

        GA.ForOutput for ->
          V.Success (A.for for :: A.Attribute E.Output)

        GA.Form form ->
          V.Success (A.form form :: A.Attribute E.Output)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.Output)

        _attr ->
          V.Failure [ wrongAttr attr Output ]
  in
    vAttr <!> mkGlobalAttr attr

mkOutputChild :: Element -> ValidHTML E.Output grandparent
mkOutputChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Output ]

mkParagraph :: E.ValidChild E.Paragraph parent grandparent
            => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkParagraph attrs node =
  E.p
    <$> foldValidate mkParagraphAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkParagraphChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Paragraph VoidNode ]
        )

mkParagraphAttr :: GA.Attribute -> ValidAttribute E.Paragraph
mkParagraphAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Paragraph)

        _attr ->
          V.Failure [ wrongAttr attr Paragraph ]
  in
    vAttr <!> mkGlobalAttr attr

mkParagraphChild :: Element -> ValidHTML E.Paragraph grandparent
mkParagraphChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Paragraph ]

mkPicture :: E.ValidChild E.Picture parent grandparent
          => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkPicture attrs node =
  E.picture
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkPictureChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Picture LeafNode ]
            Void -> V.Failure [ wrongNodeType Picture VoidNode ]
        )

mkPictureChild :: Element -> ValidHTML E.Picture grandparent
mkPictureChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Image -> mkImage attrs
      Script -> mkScript attrs content
      Source -> mkSource attrs
      ContentTemplate -> mkContentTemplate attrs content
      element -> V.Failure [ wrongChild element Picture ]

mkPreformattedText :: E.ValidChild E.PreformattedText parent grandparent
                   => [GA.Attribute]
                   -> ElementNode
                   -> ValidHTML parent grandparent
mkPreformattedText attrs node =
  E.pre
    <$> foldValidate mkPreformattedTextAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkPreformattedTextChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType PreformattedText VoidNode ]
        )

mkPreformattedTextAttr :: GA.Attribute -> ValidAttribute E.PreformattedText
mkPreformattedTextAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.PreformattedText)

        _attr ->
          V.Failure [ wrongAttr attr PreformattedText ]
  in
    vAttr <!> mkGlobalAttr attr

mkPreformattedTextChild :: Element -> ValidHTML E.PreformattedText grandparent
mkPreformattedTextChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element PreformattedText ]

mkProgress :: E.ValidChild E.Progress parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkProgress attrs node =
  E.progress
    <$> foldValidate mkProgressAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkProgressChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Progress VoidNode ]
        )

mkProgressAttr :: GA.Attribute -> ValidAttribute E.Progress
mkProgressAttr attr =
  let
    vAttr =
      case attr of
        GA.Max max -> V.Success (A.max max :: A.Attribute E.Progress)
        GA.ValueNumber value -> V.Success (A.value value :: A.Attribute E.Progress)
        _attr -> V.Failure [ wrongAttr attr Progress ]
  in
    vAttr <!> mkGlobalAttr attr

mkProgressChild :: Element -> ValidHTML E.Progress grandparent
mkProgressChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Progress ]

mkQuotation :: E.ValidChild E.Quotation parent grandparent
            => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkQuotation attrs node =
  E.q
    <$> foldValidate mkQuotationAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkQuotationChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Quotation VoidNode ]
        )

mkQuotationAttr :: GA.Attribute -> ValidAttribute E.Quotation
mkQuotationAttr attr =
  let
    vAttr =
      case attr of
        GA.Cite cite ->
          V.Success (A.cite cite :: A.Attribute E.Quotation)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Quotation)

        _attr ->
          V.Failure [ wrongAttr attr Quotation ]
  in
    vAttr <!> mkGlobalAttr attr

mkQuotationChild :: Element -> ValidHTML E.Quotation grandparent
mkQuotationChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Quotation ]

mkRubyParenthesis :: E.ValidChild E.RubyParenthesis parent grandparent
                  => [GA.Attribute]
                  -> ElementNode
                  -> ValidHTML parent grandparent
mkRubyParenthesis attrs node =
  E.rp
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch _nodes ->
              V.Failure [ wrongNodeType RubyParenthesis BranchNode ]

            Leaf net ->
              V.Success [ E.text $ NET.toText net ]

            Void ->
              V.Failure [ wrongNodeType RubyParenthesis VoidNode ]
        )

mkRubyText :: E.ValidChild E.RubyText parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkRubyText attrs node =
  E.rt
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkRubyTextChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType RubyText VoidNode ]
        )

mkRubyTextChild :: Element -> ValidHTML E.RubyText grandparent
mkRubyTextChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element RubyText ]

mkRuby :: E.ValidChild E.Ruby parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkRuby attrs node =
  E.ruby
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkRubyChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Ruby VoidNode ]
        )

mkRubyChild :: Element -> ValidHTML E.Ruby grandparent
mkRubyChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      RubyParenthesis -> mkRubyParenthesis attrs content
      RubyText -> mkRubyText attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Ruby ]

mkStrikethrough :: E.ValidChild E.Strikethrough parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> ValidHTML parent grandparent
mkStrikethrough attrs node =
  E.s
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkStrikethroughChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Strikethrough VoidNode ]
        )

mkStrikethroughChild :: Element -> ValidHTML E.Strikethrough grandparent
mkStrikethroughChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Strikethrough ]

mkSample :: E.ValidChild E.Sample parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSample attrs node =
  E.samp
    <$> foldValidate mkSampleAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSampleChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Sample VoidNode ]
        )

mkSampleAttr :: GA.Attribute -> ValidAttribute E.Sample
mkSampleAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Sample)

        _attr ->
          V.Failure [ wrongAttr attr Sample ]
  in
    vAttr <!> mkGlobalAttr attr

mkSampleChild :: Element -> ValidHTML E.Sample grandparent
mkSampleChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Sample ]

mkScript :: E.ValidChild E.Script parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkScript attrs node =
  E.script
    <$> foldValidate mkScriptAttr attrs
    <*> ( case node of
            Branch _nodes -> V.Failure [ wrongNodeType Script BranchNode ]
            Leaf net -> V.Success $ Just net
            Void -> V.Failure [ wrongNodeType Script VoidNode ]
        )

mkScriptAttr :: GA.Attribute -> ValidAttribute E.Script
mkScriptAttr attr =
  let
    vAttr =
      case attr of
        GA.Async ->
          V.Success (A.async :: A.Attribute E.Script)

        GA.Blocking blocking ->
          V.Success (A.blocking blocking :: A.Attribute E.Script)

        GA.CrossOrigin crossorigin ->
          V.Success (A.crossorigin crossorigin :: A.Attribute E.Script)

        GA.Defer ->
          V.Success (A.defer :: A.Attribute E.Script)

        GA.FetchPriority fetchpriority ->
          V.Success (A.fetchpriority fetchpriority :: A.Attribute E.Script)

        GA.Integrity encoding bs ->
          V.Success (A.integrity encoding bs :: A.Attribute E.Script)

        GA.NoModule nomodule ->
          V.Success (A.nomodule nomodule :: A.Attribute E.Script)

        GA.ReferrerPolicy referrerpolicy ->
          V.Success (A.referrerpolicy referrerpolicy :: A.Attribute E.Script)

        GA.Src src ->
          V.Success (A.src src :: A.Attribute E.Script)

        GA.Type type_ ->
          V.Success (A.type_ type_ :: A.Attribute E.Script)

        _attr ->
          V.Failure [ wrongAttr attr Script ]
  in
    vAttr <!> mkGlobalAttr attr

mkSearch :: E.ValidChild E.Search parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSearch attrs node =
  E.search
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSearchChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Search VoidNode ]
        )

mkSearchChild :: Element -> ValidHTML E.Search grandparent
mkSearchChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Search ]

mkSection :: E.ValidChild E.Section parent grandparent
          => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSection attrs node =
  E.section
    <$> foldValidate mkSectionAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSectionChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Section VoidNode ]
        )

mkSectionAttr :: GA.Attribute -> ValidAttribute E.Section
mkSectionAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Section)

        _attr ->
          V.Failure [ wrongAttr attr Section ]
  in
    vAttr <!> mkGlobalAttr attr

mkSectionChild :: Element -> ValidHTML E.Section grandparent
mkSectionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Section ]

mkSelect :: E.ValidChild E.Select parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSelect attrs node =
  E.select
    <$> foldValidate mkSelectAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSelectChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Select LeafNode ]
            Void -> V.Failure [ wrongNodeType Select VoidNode ]
        )

mkSelectAttr :: GA.Attribute -> ValidAttribute E.Select
mkSelectAttr attr =
  let
    vAttr =
      case attr of
        GA.Autocomplete autocomplete ->
          V.Success (A.autocomplete autocomplete :: A.Attribute E.Select)

        GA.Disabled disabled ->
          V.Success (A.disable disabled :: A.Attribute E.Select)

        GA.Form form ->
          V.Success (A.form form :: A.Attribute E.Select)

        GA.Multiple ->
          V.Success (A.multiple :: A.Attribute E.Select)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.Select)

        GA.Required required ->
          V.Success (A.require required :: A.Attribute E.Select)

        GA.Size size ->
          V.Success (A.size size :: A.Attribute E.Select)

        _attr ->
          V.Failure [ wrongAttr attr Select ]
  in
    vAttr <!> mkGlobalAttr attr

mkSelectChild :: Element -> ValidHTML E.Select grandparent
mkSelectChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Option -> mkOption attrs content
      OptionGroup -> mkOptionGroup attrs content
      element -> V.Failure [ wrongChild element Select ]

mkSlot :: forall parent grandparent.
          E.ValidChild E.Slot parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSlot attrs node =
  use @(E.ValidChild E.Slot parent grandparent) $ \Dict ->
    E.customHTML "slot"
      <$> foldValidate mkSlotAttr attrs
      <*> ( case node of
              Branch _nodes -> V.Failure [ wrongNodeType Slot BranchNode ]
              Leaf net -> V.Success $ Right [ E.text $ NET.toText net ]
              Void -> V.Failure [ wrongNodeType Slot VoidNode ]
          )

mkSlotAttr :: GA.Attribute -> ValidAttribute E.CustomHTML
mkSlotAttr attr =
  let
    vAttr =
      case attr of
        GA.Name name -> V.Success (A.name name :: A.Attribute E.CustomHTML)
        _attr -> V.Failure [ wrongAttr attr Slot ]
  in
    vAttr <!> mkGlobalAttr attr

mkSideComment :: E.ValidChild E.SideComment parent grandparent
              => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSideComment attrs node =
  E.small
    <$> foldValidate mkSideCommentAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSideCommentChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType SideComment VoidNode ]
        )

mkSideCommentAttr :: GA.Attribute -> ValidAttribute E.SideComment
mkSideCommentAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.SideComment)

        _attr ->
          V.Failure [ wrongAttr attr SideComment ]
  in
    vAttr <!> mkGlobalAttr attr

mkSideCommentChild :: Element -> ValidHTML E.SideComment grandparent
mkSideCommentChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element SideComment ]

mkSource :: E.ValidChild E.Source parent grandparent
         => [GA.Attribute] -> ValidHTML parent grandparent
mkSource attrs =
  E.source <$> foldValidate mkSourceAttr attrs

mkSourceAttr :: GA.Attribute -> ValidAttribute E.Source
mkSourceAttr attr =
  let
    vAttr =
      case attr of
        GA.Height height -> V.Success (A.height height :: A.Attribute E.Source)
        GA.Media media -> V.Success (A.media media :: A.Attribute E.Source)
        GA.Sizes sizes -> V.Success (A.sizes sizes :: A.Attribute E.Source)
        GA.Src src -> V.Success (A.src src :: A.Attribute E.Source)
        GA.SrcSet srcset -> V.Success (A.srcset srcset :: A.Attribute E.Source)
        GA.Type type_ -> V.Success (A.type_ type_ :: A.Attribute E.Source)
        GA.Width width -> V.Success (A.width width :: A.Attribute E.Source)
        _attr -> V.Failure [ wrongAttr attr Source ]
  in
    vAttr <!> mkGlobalAttr attr

mkSpan :: E.ValidChild E.Span parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSpan attrs node =
  E.span
    <$> foldValidate mkSpanAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSpanChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Span VoidNode ]
        )

mkSpanAttr :: GA.Attribute -> ValidAttribute E.Span
mkSpanAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Span)

        _attr ->
          V.Failure [ wrongAttr attr Span ]
  in
    vAttr <!> mkGlobalAttr attr

mkSpanChild :: Element -> ValidHTML E.Span grandparent
mkSpanChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Span ]

mkStrong :: E.ValidChild E.Strong parent grandparent
         => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkStrong attrs node =
  E.strong
    <$> foldValidate mkStrongAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkStrongChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Strong VoidNode ]
        )

mkStrongAttr :: GA.Attribute -> ValidAttribute E.Strong
mkStrongAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Strong)

        _attr ->
          V.Failure [ wrongAttr attr Strong ]
  in
    vAttr <!> mkGlobalAttr attr

mkStrongChild :: Element -> ValidHTML E.Strong grandparent
mkStrongChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Strong ]

mkStyle :: E.ValidChild E.Style parent grandparent
        => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkStyle attrs node =
  E.style
    <$> foldValidate mkStyleAttr attrs
    <*> ( case node of
            Branch _nodes -> V.Failure [ wrongNodeType Style BranchNode ]
            Leaf net -> V.Success $ NET.toText net
            Void -> V.Failure [ wrongNodeType Style VoidNode ]
        )

mkStyleAttr :: GA.Attribute -> ValidAttribute E.Style
mkStyleAttr attr =
  let
    vAttr =
      case attr of
        GA.Blocking blocking ->
          V.Success (A.blocking blocking :: A.Attribute E.Style)

        GA.Media media ->
          V.Success (A.media media :: A.Attribute E.Style)

        _attr ->
          V.Failure [ wrongAttr attr Style ]
  in
    vAttr <!> mkGlobalAttr attr

mkSubscript :: E.ValidChild E.Subscript parent grandparent
            => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSubscript attrs node =
  E.sub
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSubscriptChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Subscript VoidNode ]
        )

mkSubscriptChild :: Element -> ValidHTML E.Subscript grandparent
mkSubscriptChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Subscript ]

mkSummary :: E.ValidChild E.Summary parent grandparent
          => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSummary attrs node =
  E.summary
    <$> foldValidate mkSummaryAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSummaryChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Summary VoidNode ]
        )

mkSummaryAttr :: GA.Attribute -> ValidAttribute E.Summary
mkSummaryAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Summary)

        _attr ->
          V.Failure [ wrongAttr attr Summary ]
  in
    vAttr <!> mkGlobalAttr attr

mkSummaryChild :: Element -> ValidHTML E.Summary grandparent
mkSummaryChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Summary ]

mkSuperscript :: E.ValidChild E.Superscript parent grandparent
              => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkSuperscript attrs node =
  E.sup
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkSuperscriptChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Superscript VoidNode ]
        )

mkSuperscriptChild :: Element -> ValidHTML E.Superscript grandparent
mkSuperscriptChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Superscript ]

mkTable :: E.ValidChild E.Table parent grandparent
        => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTable attrs node =
  E.table
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Table LeafNode ]
            Void -> V.Failure [ wrongNodeType Table VoidNode ]
        )

mkTableChild :: Element -> ValidHTML E.Table grandparent
mkTableChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      TableCaption -> mkTableCaption attrs content
      TableColumnGroup -> mkTableColumnGroup attrs content
      TableHead -> mkTableHead attrs content
      TableBody -> mkTableBody attrs content
      TableRow -> mkTableRow attrs content
      TableFoot -> mkTableFoot attrs content
      element -> V.Failure [ wrongChild element Table ]

mkTableBody :: E.ValidChild E.TableBody parent grandparent
            => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTableBody attrs node =
  E.tbody
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableBodyChild nodes
            Leaf _net -> V.Failure [ wrongNodeType TableBody LeafNode ]
            Void -> V.Failure [ wrongNodeType TableBody VoidNode ]
        )

mkTableBodyChild :: Element -> ValidHTML E.TableBody grandparent
mkTableBodyChild e =
  case elementType e of
    TableRow -> mkTableRow (elementAttrs e) (elementChildren e)
    element -> V.Failure [ wrongChild element TableBody ]

mkTableDataCell :: E.ValidChild E.TableDataCell parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> ValidHTML parent grandparent
mkTableDataCell attrs node =
  E.td
    <$> foldValidate mkTableDataCellAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableDataCellChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType TableDataCell VoidNode ]
        )

mkTableDataCellAttr :: GA.Attribute -> ValidAttribute E.TableDataCell
mkTableDataCellAttr attr =
  let
    vAttr =
      case attr of
        GA.Colspan colspan ->
          V.Success (A.colspan colspan :: A.Attribute E.TableDataCell)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.TableDataCell)

        GA.Headers headers ->
          V.Success (A.headers headers :: A.Attribute E.TableDataCell)

        GA.Rowspan rowspan ->
          V.Success (A.rowspan rowspan :: A.Attribute E.TableDataCell)

        _attr ->
          V.Failure [ wrongAttr attr TableDataCell ]
  in
    vAttr <!> mkGlobalAttr attr

mkTableDataCellChild :: Element -> ValidHTML E.TableDataCell grandparent
mkTableDataCellChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Article -> mkArticle attrs content
      Aside -> mkAside attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Footer -> mkFooter attrs content
      Form -> mkForm attrs content
      H1 -> mkH1 attrs content
      H2 -> mkH2 attrs content
      H3 -> mkH3 attrs content
      H4 -> mkH4 attrs content
      H5 -> mkH5 attrs content
      H6 -> mkH6 attrs content
      Header -> mkHeader attrs content
      HeadingGroup -> mkHeadingGroup attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      Nav -> mkNav attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Section -> mkSection attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element TableDataCell ]

mkContentTemplate :: E.ValidChild E.ContentTemplate parent grandparent
                  => [GA.Attribute]
                  -> ElementNode
                  -> ValidHTML parent grandparent
mkContentTemplate attrs node =
  E.template
    <$> foldValidate mkContentTemplateAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkContentTemplateChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType ContentTemplate VoidNode ]
        )

mkContentTemplateAttr :: GA.Attribute -> ValidAttribute E.ContentTemplate
mkContentTemplateAttr attr =
  let
    vAttr =
      case attr of
        GA.ShadowRootMode shadowrootmode ->
          V.Success
            (A.shadowrootmode shadowrootmode :: A.Attribute E.ContentTemplate)

        GA.ShadowRootClonable ->
          V.Success (A.shadowrootclonable :: A.Attribute E.ContentTemplate)

        GA.ShadowRootDelegatesFocus ->
          V.Success (A.shadowrootdelegatesfocus :: A.Attribute E.ContentTemplate)

        _attr ->
          V.Failure [ wrongAttr attr ContentTemplate ]
  in
    vAttr <!> mkGlobalAttr attr

mkContentTemplateChild :: Element -> ValidHTML E.ContentTemplate grandparent
mkContentTemplateChild e =
  case elementType e of
    Comment ->
      case elementChildren e of
        Branch _nodes -> V.Failure [ wrongNodeType Comment BranchNode ]
        Leaf net -> V.Success . E.comment $ NET.toText net
        Void -> V.Failure [ wrongNodeType Comment VoidNode ]

    Anchor ->
      mkAnchor (elementAttrs e) (elementChildren e)

    Abbreviation ->
      mkAbbreviation (elementAttrs e) (elementChildren e)

    ContactAddress ->
      mkContactAddress (elementAttrs e) (elementChildren e)

    Area ->
      mkArea (elementAttrs e)

    Article ->
      mkArticle (elementAttrs e) (elementChildren e)

    Aside ->
      mkAside (elementAttrs e) (elementChildren e)

    Audio ->
      mkAudio (elementAttrs e) (elementChildren e)

    BringAttentionTo ->
      mkBringAttentionTo (elementAttrs e) (elementChildren e)

    Base ->
      mkBase (elementAttrs e)

    BidirectionalIsolation ->
      mkBidirectionalIsolation (elementAttrs e) (elementChildren e)

    BidirectionalOverride ->
      mkBidirectionalOverride (elementAttrs e) (elementChildren e)

    Blockquote ->
      mkBlockquote (elementAttrs e) (elementChildren e)

    Body ->
      mkBody (elementAttrs e) (elementChildren e)

    LineBreak ->
      mkLineBreak (elementAttrs e)

    Button ->
      mkButton (elementAttrs e) (elementChildren e)

    Canvas ->
      mkCanvas (elementAttrs e) (elementChildren e)

    TableCaption ->
      mkTableCaption (elementAttrs e) (elementChildren e)

    Citation ->
      mkCitation (elementAttrs e) (elementChildren e)

    Code ->
      mkCode (elementAttrs e) (elementChildren e)

    TableColumn ->
      mkTableColumn (elementAttrs e)

    TableColumnGroup ->
      mkTableColumnGroup (elementAttrs e) (elementChildren e)

    Data ->
      mkData (elementAttrs e) (elementChildren e)

    DataList ->
      mkDataList (elementAttrs e) (elementChildren e)

    DescriptionDetails ->
      mkDescriptionDetails (elementAttrs e) (elementChildren e)

    DeletedText ->
      mkDeletedText (elementAttrs e) (elementChildren e)

    Details ->
      mkDetails (elementAttrs e) (elementChildren e)

    Definition ->
      mkDefinition (elementAttrs e) (elementChildren e)

    Dialog ->
      mkDialog (elementAttrs e) (elementChildren e)

    Division ->
      mkDivision (elementAttrs e) (elementChildren e)

    DescriptionList ->
      mkDescriptionList (elementAttrs e) (elementChildren e)

    DescriptionTerm ->
      mkDescriptionTerm (elementAttrs e) (elementChildren e)

    Emphasis ->
      mkEmphasis (elementAttrs e) (elementChildren e)

    Embed ->
      mkEmbed (elementAttrs e)

    Fieldset ->
      mkFieldset (elementAttrs e) (elementChildren e)

    FigureCaption ->
      mkFigureCaption (elementAttrs e) (elementChildren e)

    Figure ->
      mkFigure (elementAttrs e) (elementChildren e)

    Footer ->
      mkFooter (elementAttrs e) (elementChildren e)

    Form ->
      mkForm (elementAttrs e) (elementChildren e)

    H1 ->
      mkH1 (elementAttrs e) (elementChildren e)

    H2 ->
      mkH2 (elementAttrs e) (elementChildren e)

    H3 ->
      mkH3 (elementAttrs e) (elementChildren e)

    H4 ->
      mkH4 (elementAttrs e) (elementChildren e)

    H5 ->
      mkH5 (elementAttrs e) (elementChildren e)

    H6 ->
      mkH6 (elementAttrs e) (elementChildren e)

    Head ->
      mkHead (elementAttrs e) (elementChildren e)

    Header ->
      mkHeader (elementAttrs e) (elementChildren e)

    HeadingGroup ->
      mkHeadingGroup (elementAttrs e) (elementChildren e)

    HorizontalRule ->
      mkHorizontalRule (elementAttrs e)

    IdiomaticText ->
      mkIdiomaticText (elementAttrs e) (elementChildren e)

    IFrame ->
      mkIFrame (elementAttrs e)

    Image ->
      mkImage (elementAttrs e)

    Input ->
      mkInput (elementAttrs e)

    InsertedText ->
      mkInsertedText (elementAttrs e) (elementChildren e)

    KeyboardInput ->
      mkKeyboardInput (elementAttrs e) (elementChildren e)

    Label ->
      mkLabel (elementAttrs e) (elementChildren e)

    Legend ->
      mkLegend (elementAttrs e) (elementChildren e)

    ListItem ->
      mkListItem (elementAttrs e) (elementChildren e)

    Link ->
      mkLink (elementAttrs e)

    Main ->
      mkMain (elementAttrs e) (elementChildren e)

    Map ->
      mkMap (elementAttrs e) (elementChildren e)

    Mark ->
      mkMark (elementAttrs e) (elementChildren e)

    Menu ->
      mkMenu (elementAttrs e) (elementChildren e)

    Meta ->
      mkMeta (elementAttrs e)

    Meter ->
      mkMeter (elementAttrs e) (elementChildren e)

    Nav ->
      mkNav (elementAttrs e) (elementChildren e)

    NoScriptHead ->
      mkNoScriptHead (elementAttrs e) (elementChildren e)

    NoScriptBody ->
      mkNoScriptBody (elementAttrs e) (elementChildren e)

    Object ->
      mkObject (elementAttrs e) (elementChildren e)

    OrderedList ->
      mkOrderedList (elementAttrs e) (elementChildren e)

    OptionGroup ->
      mkOptionGroup (elementAttrs e) (elementChildren e)

    Option ->
      mkOption (elementAttrs e) (elementChildren e)

    Output ->
      mkOutput (elementAttrs e) (elementChildren e)

    Paragraph ->
      mkParagraph (elementAttrs e) (elementChildren e)

    Picture ->
      mkPicture (elementAttrs e) (elementChildren e)

    PreformattedText ->
      mkPreformattedText (elementAttrs e) (elementChildren e)

    Progress ->
      mkProgress (elementAttrs e) (elementChildren e)

    Quotation ->
      mkQuotation (elementAttrs e) (elementChildren e)

    RubyParenthesis ->
      mkRubyParenthesis (elementAttrs e) (elementChildren e)

    RubyText ->
      mkRubyText (elementAttrs e) (elementChildren e)

    Ruby ->
      mkRuby (elementAttrs e) (elementChildren e)

    Strikethrough ->
      mkStrikethrough (elementAttrs e) (elementChildren e)

    Sample ->
      mkSample (elementAttrs e) (elementChildren e)

    Script ->
      mkScript (elementAttrs e) (elementChildren e)

    Search ->
      mkSearch (elementAttrs e) (elementChildren e)

    Section ->
      mkSection (elementAttrs e) (elementChildren e)

    Select ->
      mkSelect (elementAttrs e) (elementChildren e)

    Slot ->
      mkSlot (elementAttrs e) (elementChildren e)

    SideComment ->
      mkSideComment (elementAttrs e) (elementChildren e)

    Source ->
      mkSource (elementAttrs e)

    Span ->
      mkSpan (elementAttrs e) (elementChildren e)

    Strong ->
      mkStrong (elementAttrs e) (elementChildren e)

    Style ->
      mkStyle (elementAttrs e) (elementChildren e)

    Subscript ->
      mkSubscript (elementAttrs e) (elementChildren e)

    Summary ->
      mkSummary (elementAttrs e) (elementChildren e)

    Superscript ->
      mkSuperscript (elementAttrs e) (elementChildren e)

    Table ->
      mkTable (elementAttrs e) (elementChildren e)

    TableBody ->
      mkTableBody (elementAttrs e) (elementChildren e)

    TableDataCell ->
      mkTableDataCell (elementAttrs e) (elementChildren e)

    ContentTemplate ->
      mkContentTemplate (elementAttrs e) (elementChildren e)

    TextArea ->
      mkTextArea (elementAttrs e) (elementChildren e)

    TableFoot ->
      mkTableFoot (elementAttrs e) (elementChildren e)

    TableHeader ->
      mkTableHeader (elementAttrs e) (elementChildren e)

    TableHead ->
      mkTableHead (elementAttrs e) (elementChildren e)

    Time ->
      mkTime (elementAttrs e) (elementChildren e)

    Title ->
      mkTitle (elementAttrs e) (elementChildren e)

    TableRow ->
      mkTableRow (elementAttrs e) (elementChildren e)

    Track ->
      mkTrack (elementAttrs e)

    Underline ->
      mkUnderline (elementAttrs e) (elementChildren e)

    UnorderedList ->
      mkUnorderedList (elementAttrs e) (elementChildren e)

    Variable ->
      mkVariable (elementAttrs e) (elementChildren e)

    Video ->
      mkVideo (elementAttrs e) (elementChildren e)

    WordBreakOpportunity ->
        mkWordBreakOpportunity (elementAttrs e)

    element ->
      V.Failure [ wrongChild element ContentTemplate ]

mkTextArea :: E.ValidChild E.TextArea parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTextArea attrs node =
  E.textarea
    <$> foldValidate mkTextAreaAttr attrs
    <*> ( case node of
            Branch _nodes -> V.Failure [ wrongNodeType TextArea BranchNode ]
            Leaf net -> V.Success $ NET.toText net
            Void -> V.Failure [ wrongNodeType TextArea VoidNode ]
        )

mkTextAreaAttr :: GA.Attribute -> ValidAttribute E.TextArea
mkTextAreaAttr attr =
  let
    vAttr =
      case attr of
        GA.Autocomplete autocomplete ->
          V.Success (A.autocomplete autocomplete :: A.Attribute E.TextArea)

        GA.Cols cols ->
          V.Success (A.cols cols :: A.Attribute E.TextArea)

        GA.Dirname dirname ->
          V.Success (A.dirname dirname :: A.Attribute E.TextArea)

        GA.Disabled disabled ->
          V.Success (A.disable disabled :: A.Attribute E.TextArea)

        GA.Form form ->
          V.Success (A.form form :: A.Attribute E.TextArea)

        GA.MaxLength maxlength ->
          V.Success (A.maxlength maxlength :: A.Attribute E.TextArea)

        GA.MinLength minlength ->
          V.Success (A.minlength minlength :: A.Attribute E.TextArea)

        GA.Name name ->
          V.Success (A.name name :: A.Attribute E.TextArea)

        GA.Placeholder placeholder ->
          V.Success (A.placeholder placeholder :: A.Attribute E.TextArea)

        GA.ReadOnly ->
          V.Success (A.readonly :: A.Attribute E.TextArea)

        GA.Required required ->
          V.Success (A.require required :: A.Attribute E.TextArea)

        GA.Rows rows ->
          V.Success (A.rows rows :: A.Attribute E.TextArea)

        GA.Wrap wrap ->
          V.Success (A.wrap wrap :: A.Attribute E.TextArea)

        _attr ->
          V.Failure [ wrongAttr attr TextArea ]
  in
    vAttr <!> mkGlobalAttr attr

mkTableFoot :: E.ValidChild E.TableFoot parent grandparent
            => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTableFoot attrs node =
  E.tfoot
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableFootChild nodes
            Leaf _net -> V.Failure [ wrongNodeType TableFoot LeafNode ]
            Void -> V.Failure [ wrongNodeType TableFoot VoidNode ]
        )

mkTableFootChild :: Element -> ValidHTML E.TableFoot grandparent
mkTableFootChild e =
  case elementType e of
    TableRow -> mkTableRow (elementAttrs e) (elementChildren e)
    element -> V.Failure [ wrongChild element TableFoot ]

mkTableHeader :: E.ValidChild E.TableHeader parent grandparent
              => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTableHeader attrs node =
  E.th
    <$> foldValidate mkTableHeaderAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableHeaderChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType TableHeader VoidNode ]
        )

mkTableHeaderAttr :: GA.Attribute -> ValidAttribute E.TableHeader
mkTableHeaderAttr attr =
  let
    vAttr =
      case attr of
        GA.Abbreviation abbr ->
          V.Success (A.abbr abbr :: A.Attribute E.TableHeader)

        GA.Colspan colspan ->
          V.Success (A.colspan colspan :: A.Attribute E.TableHeader)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.TableHeader)

        GA.Headers headers ->
          V.Success (A.headers headers :: A.Attribute E.TableHeader)

        GA.Rowspan rowspan ->
          V.Success (A.rowspan rowspan :: A.Attribute E.TableHeader)

        GA.Scope scope ->
          V.Success (A.scope scope :: A.Attribute E.TableHeader)

        _attr ->
          V.Failure [ wrongAttr attr TableHeader ]
  in
    vAttr <!> mkGlobalAttr attr

mkTableHeaderChild :: Element -> ValidHTML E.TableHeader grandparent
mkTableHeaderChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> mkAnchor attrs content
      Abbreviation -> mkAbbreviation attrs content
      ContactAddress -> mkContactAddress attrs content
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      Blockquote -> mkBlockquote attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      DeletedText -> mkDeletedText attrs content
      Details -> mkDetails attrs content
      Definition -> mkDefinition attrs content
      Dialog -> mkDialog attrs content
      Division -> mkDivision attrs content
      DescriptionList -> mkDescriptionList attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      Fieldset -> mkFieldset attrs content
      Figure -> mkFigure attrs content
      Form -> mkForm attrs content
      HorizontalRule -> mkHorizontalRule attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      InsertedText -> mkInsertedText attrs content
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Main -> mkMain attrs content
      Map -> mkMap attrs content
      Mark -> mkMark attrs content
      Menu -> mkMenu attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      OrderedList -> mkOrderedList attrs content
      Output -> mkOutput attrs content
      Paragraph -> mkParagraph attrs content
      Picture -> mkPicture attrs content
      PreformattedText -> mkPreformattedText attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Search -> mkSearch attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      Table -> mkTable attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      UnorderedList -> mkUnorderedList attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element TableHeader ]

mkTableHead :: E.ValidChild E.TableHead parent grandparent
            => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTableHead attrs node =
  E.thead
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableHeadChild nodes
            Leaf _net -> V.Failure [ wrongNodeType TableHead LeafNode ]
            Void -> V.Failure [ wrongNodeType TableHead VoidNode ]
        )

mkTableHeadChild :: Element -> ValidHTML E.TableHead grandparent
mkTableHeadChild e =
  case elementType e of
    TableRow -> mkTableRow (elementAttrs e) (elementChildren e)
    element -> V.Failure [ wrongChild element TableHead ]

mkTime :: E.ValidChild E.Time parent grandparent
       => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTime attrs node =
  E.time
    <$> foldValidate mkTimeAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTimeChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Time VoidNode ]
        )

mkTimeAttr :: GA.Attribute -> ValidAttribute E.Time
mkTimeAttr attr =
  let
    vAttr =
      case attr of
        GA.Datetime datetime ->
          V.Success (A.datetime datetime :: A.Attribute E.Time)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Time)

        _attr ->
          V.Failure [ wrongAttr attr Time ]
  in
    vAttr <!> mkGlobalAttr attr

mkTimeChild :: Element -> ValidHTML E.Time grandparent
mkTimeChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Time ]

mkTitle :: E.ValidChild E.Title parent grandparent
        => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTitle attrs node =
  E.title
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch _nodes -> V.Failure [ wrongNodeType Title VoidNode ]
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Title VoidNode ]
        )

mkTableRow :: E.ValidChild E.TableRow parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkTableRow attrs node =
  E.tr
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkTableRowChild nodes
            Leaf _net -> V.Failure [ wrongNodeType TableRow LeafNode ]
            Void -> V.Failure [ wrongNodeType TableRow VoidNode ]
        )

mkTableRowChild :: Element -> ValidHTML E.TableRow grandparent
mkTableRowChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Script -> mkScript attrs content
      TableDataCell -> mkTableDataCell attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TableHeader -> mkTableHeader attrs content
      element -> V.Failure [ wrongChild element TableRow ]

mkTrack :: E.ValidChild E.Track parent grandparent
        => [GA.Attribute] -> ValidHTML parent grandparent
mkTrack attrs =
  E.track <$> foldValidate mkTrackAttr attrs

mkTrackAttr :: GA.Attribute -> ValidAttribute E.Track
mkTrackAttr attr =
  let
    vAttr =
      case attr of
        GA.Default ->
          V.Success (A.default_ :: A.Attribute E.Track)

        GA.Kind kind ->
          V.Success (A.kind kind :: A.Attribute E.Track)

        GA.Label label ->
          V.Success (A.label label :: A.Attribute E.Track)

        GA.Src src ->
          V.Success (A.src src :: A.Attribute E.Track)

        GA.SrcLang srclang ->
          V.Success (A.srclang srclang :: A.Attribute E.Track)

        _attr ->
          V.Failure [ wrongAttr attr Track ]
  in
    vAttr <!> mkGlobalAttr attr

mkUnderline :: E.ValidChild E.Underline parent grandparent
            => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkUnderline attrs node =
  E.u
    <$> foldValidate mkUnderlineAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkUnderlineChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Underline VoidNode ]
        )

mkUnderlineAttr :: GA.Attribute -> ValidAttribute E.Underline
mkUnderlineAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Underline)

        _attr ->
          V.Failure [ wrongAttr attr Underline ]
  in
    vAttr <!> mkGlobalAttr attr

mkUnderlineChild :: Element -> ValidHTML E.Underline grandparent
mkUnderlineChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Underline ]

mkUnorderedList :: E.ValidChild E.UnorderedList parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> ValidHTML parent grandparent
mkUnorderedList attrs node =
  E.ul
    <$> foldValidate mkGlobalAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkUnorderedListChild nodes
            Leaf _net -> V.Failure [ wrongNodeType UnorderedList LeafNode ]
            Void -> V.Failure [ wrongNodeType UnorderedList VoidNode ]
        )

mkUnorderedListChild :: Element -> ValidHTML E.UnorderedList grandparent
mkUnorderedListChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      ListItem -> mkListItem attrs content
      Script -> mkScript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      element -> V.Failure [ wrongChild element UnorderedList ]

mkVariable :: E.ValidChild E.Variable parent grandparent
           => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkVariable attrs node =
  E.var
    <$> foldValidate mkVariableAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkVariableChild nodes
            Leaf net -> V.Success [ E.text $ NET.toText net ]
            Void -> V.Failure [ wrongNodeType Variable VoidNode ]
        )

mkVariableAttr :: GA.Attribute -> ValidAttribute E.Variable
mkVariableAttr attr =
  let
    vAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Variable)

        _attr ->
          V.Failure [ wrongAttr attr Variable ]
  in
    vAttr <!> mkGlobalAttr attr

mkVariableChild :: Element -> ValidHTML E.Variable grandparent
mkVariableChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> mkAbbreviation attrs content
      Area -> mkArea attrs
      Audio -> mkAudio attrs content
      BringAttentionTo -> mkBringAttentionTo attrs content
      BidirectionalIsolation -> mkBidirectionalIsolation attrs content
      BidirectionalOverride -> mkBidirectionalOverride attrs content
      LineBreak -> mkLineBreak attrs
      Button -> mkButton attrs content
      Canvas -> mkCanvas attrs content
      Citation -> mkCitation attrs content
      Code -> mkCode attrs content
      Data -> mkData attrs content
      DataList -> mkDataList attrs content
      Definition -> mkDefinition attrs content
      Emphasis -> mkEmphasis attrs content
      Embed -> mkEmbed attrs
      IdiomaticText -> mkIdiomaticText attrs content
      IFrame -> mkIFrame attrs
      Image -> mkImage attrs
      Input -> mkInput attrs
      KeyboardInput -> mkKeyboardInput attrs content
      Label -> mkLabel attrs content
      Mark -> mkMark attrs content
      Meter -> mkMeter attrs content
      NoScriptHead -> mkNoScriptHead attrs content
      NoScriptBody -> mkNoScriptBody attrs content
      Object -> mkObject attrs content
      Output -> mkOutput attrs content
      Picture -> mkPicture attrs content
      Progress -> mkProgress attrs content
      Quotation -> mkQuotation attrs content
      Ruby -> mkRuby attrs content
      Strikethrough -> mkStrikethrough attrs content
      Sample -> mkSample attrs content
      Script -> mkScript attrs content
      Select -> mkSelect attrs content
      Slot -> mkSlot attrs content
      SideComment -> mkSideComment attrs content
      Span -> mkSpan attrs content
      Strong -> mkStrong attrs content
      Subscript -> mkSubscript attrs content
      Superscript -> mkSuperscript attrs content
      ContentTemplate -> mkContentTemplate attrs content
      TextArea -> mkTextArea attrs content
      Time -> mkTime attrs content
      Underline -> mkUnderline attrs content
      Variable -> mkVariable attrs content
      Video -> mkVideo attrs content
      WordBreakOpportunity -> mkWordBreakOpportunity attrs
      element -> V.Failure [ wrongChild element Variable ]

mkVideo :: E.ValidChild E.Video parent grandparent
        => [GA.Attribute] -> ElementNode -> ValidHTML parent grandparent
mkVideo attrs node =
  E.video
    <$> foldValidate mkVideoAttr attrs
    <*> ( case node of
            Branch nodes -> foldValidate mkVideoChild nodes
            Leaf _net -> V.Failure [ wrongNodeType Video LeafNode ]
            Void -> V.Failure [ wrongNodeType Video VoidNode ]
        )

mkVideoAttr :: GA.Attribute -> ValidAttribute E.Video
mkVideoAttr attr =
  let
    vAttr =
      case attr of
        GA.Autoplay ->
          V.Success (A.autoplay :: A.Attribute E.Video)

        GA.Controls ->
          V.Success (A.controls :: A.Attribute E.Video)

        GA.ControlsList controlslist ->
          V.Success (A.controlslist controlslist :: A.Attribute E.Video)

        GA.CrossOrigin crossorigin ->
          V.Success (A.crossorigin crossorigin :: A.Attribute E.Video)

        GA.DisablePictureInPicture ->
          V.Success (A.disablepictureinpicture :: A.Attribute E.Video)

        GA.DisableRemotePlayback ->
          V.Success (A.disableremoteplayback :: A.Attribute E.Video)

        GA.ElementTiming elementtiming ->
          V.Success (A.elementtiming elementtiming :: A.Attribute E.Video)

        GA.Height height ->
          V.Success (A.height height :: A.Attribute E.Video)

        GA.Loop ->
          V.Success (A.loop :: A.Attribute E.Video)

        GA.Muted muted ->
          V.Success (A.mute muted :: A.Attribute E.Video)

        GA.PlaysInline playsinline ->
          V.Success (A.playInline playsinline :: A.Attribute E.Video)

        GA.Poster poster ->
          V.Success (A.poster poster :: A.Attribute E.Video)

        GA.Preload preload ->
          V.Success (A.preload preload :: A.Attribute E.Video)

        GA.Src src ->
          V.Success (A.src src :: A.Attribute E.Video)

        GA.Width width ->
          V.Success (A.width width :: A.Attribute E.Video)

        _attr ->
          V.Failure [ wrongAttr attr Video ]
  in
    vAttr <!> mkGlobalAttr attr

mkVideoChild :: Element -> ValidHTML E.Video grandparent
mkVideoChild e =
  case elementType e of
    Source -> mkSource (elementAttrs e)
    Track -> mkTrack (elementAttrs e)
    element -> V.Failure [ wrongChild element Video ]

mkWordBreakOpportunity :: E.ValidChild E.WordBreakOpportunity parent grandparent
                       => [GA.Attribute] -> ValidHTML parent grandparent
mkWordBreakOpportunity attrs =
  E.wbr <$> foldValidate mkGlobalAttr attrs

mkGlobalAttr :: GA.Attribute -> ValidAttribute tag
mkGlobalAttr attr =
  case attr of
    GA.AccessKey accesskey ->
      V.Success $ A.accesskey accesskey

    GA.Autocapitalize autocapitalize ->
      V.Success $ A.autocapitalize autocapitalize

    GA.Autocorrect autocorrect ->
      V.Success $ A.autocorrect autocorrect

    GA.Autofocus autofocus ->
      V.Success $ A.autofocus autofocus

    GA.Class class_ ->
      V.Success $ A.class_ class_

    GA.ContentEditable contenteditable ->
      V.Success $ A.contenteditable contenteditable

    GA.CustomData name value ->
      V.Success $ A.customData name value

    GA.Dir dir ->
      V.Success $ A.dir dir

    GA.Draggable draggable ->
      V.Success $ A.draggable draggable

    GA.EnterKeyHint enterkeyhint ->
      V.Success $ A.enterkeyhint enterkeyhint

    GA.ExportParts exportparts ->
      V.Success $ A.exportparts exportparts

    GA.Hidden hidden ->
      V.Success $ A.hide hidden

    GA.Id id_ ->
      V.Success $ A.id id_

    GA.Inert inert ->
      V.Success $ A.inert inert

    GA.InputMode inputmode ->
      V.Success $ A.inputmode inputmode

    GA.Is is ->
      V.Success $ A.is is

    GA.ItemId itemid ->
      V.Success $ A.itemid itemid

    GA.ItemProp itemprop ->
      V.Success $ A.itemprop itemprop

    GA.ItemRef itemref ->
      V.Success $ A.itemref itemref

    GA.ItemScope ->
      V.Success $ A.itemscope

    GA.ItemType itemtype ->
      V.Success $ A.itemtype itemtype

    GA.Lang lang ->
      V.Success $ A.lang lang

    GA.Nonce nonce ->
      V.Success $ A.nonce nonce

    GA.Part part ->
      V.Success $ A.part part

    GA.Popover popover ->
      V.Success $ A.popover popover

    GA.Role role ->
      V.Success $ A.role role

    GA.Slot slot ->
      V.Success $ A.slot slot

    GA.Spellcheck spellcheck ->
      V.Success $ A.spellcheck spellcheck

    GA.Style style ->
      V.Success $ A.style style

    GA.TabIndex tabindex ->
      V.Success $ A.tabindex tabindex

    GA.Title title ->
      V.Success $ A.title title

    GA.Translate translate ->
      V.Success $ A.translate translate

    GA.WritingSuggestions writingsuggestions ->
      V.Success $ A.writingsuggestions writingsuggestions

    GA.Aria aria ->
      V.Success $ Attr.Attr_Aria aria

    GA.On event script ->
      V.Success $ Attr.Attr_On event script

    _attr ->
      V.Failure $
        [ T.unpack (GA.attributeText attr) <> " is not a global attribute." ]

foldValidate :: (a -> Validator b) -> [a] -> Validator [b]
foldValidate fn =
  foldr (liftA2 (:) . fn) (V.Success [])

wrongAttr :: GA.Attribute -> ElementType -> String
wrongAttr attr et =
  "Tried to add the "
    <> T.unpack (GA.attributeText attr)
    <> " attribute to the "
    <> show et
    <> " element."

wrongChild :: ElementType -> ElementType -> String
wrongChild cet pet =
  "Tried to add the "
    <> show cet
    <> " element as a child of the "
    <> show pet
    <> " element."

wrongNodeType :: ElementType -> NodeType -> String
wrongNodeType et nt =
  "Tried to treat the "
    <> show et
    <> " element as a "
    <> show nt
    <> "."
