module Brigid.HTML.Generation.Internal.Convert
  ( toBrigid
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Prelude hiding (div, max, min, span)

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Generation.Attributes qualified as GA
import Brigid.HTML.Generation.Internal.Types (Element (..), ElementNode (..), ElementType (..))
import Brigid.Types qualified as Types

toBrigid :: Element -> E.AnyHTML
toBrigid e =
  case elementType e of
    Comment ->
      case elementChildren e of
        Branch _nodes -> E.noElement
        Leaf net -> E.comment $ NET.toText net
        Void -> E.noElement

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

    NoScript ->
      mkNoScript (elementAttrs e) (elementChildren e)

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

mkAnchor :: [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkAnchor attrs node =
  E.customHTML "a" (mapMaybe mkAnchorAttr attrs) $
    Right $
      case node of
        Branch _nodes -> []
        Leaf net -> [ E.text $ NET.toText net ]
        Void -> []

mkAnchorAttr :: GA.Attribute -> Maybe (A.Attribute E.CustomHTML)
mkAnchorAttr attr =
  let
    mbAttr =
      case attr of
        GA.Download download ->
          Just (A.download download :: A.Attribute E.CustomHTML)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.CustomHTML)

        GA.Href href ->
          Just (A.href href :: A.Attribute E.CustomHTML)

        GA.HrefLang hreflang ->
          Just (A.hreflang hreflang :: A.Attribute E.CustomHTML)

        GA.Ping ping ->
          Just (A.ping ping :: A.Attribute E.CustomHTML)

        GA.ReferrerPolicy referrerpolicy ->
          Just (A.referrerpolicy referrerpolicy :: A.Attribute E.CustomHTML)

        GA.Rel rel ->
          Just (A.rel rel :: A.Attribute E.CustomHTML)

        GA.Target target ->
          Just (A.target target :: A.Attribute E.CustomHTML)

        GA.Type type_ ->
          Just (A.type_ type_ :: A.Attribute E.CustomHTML)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkAbbreviation :: E.ValidChild E.Abbreviation parent grandparent
               => [GA.Attribute]
               -> ElementNode
               -> E.ChildHTML parent grandparent
mkAbbreviation attrs node =
  E.abbr (mapMaybe mkAbbreviationAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkAbbreviationChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkAbbreviationAttr :: GA.Attribute -> Maybe (A.Attribute E.Abbreviation)
mkAbbreviationAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Abbreviation)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkAbbreviationChild :: Element
                    -> Maybe (E.ChildHTML E.Abbreviation grandparent)
mkAbbreviationChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing


mkContactAddress :: E.ValidChild E.ContactAddress parent grandparent
                 => [GA.Attribute]
                 -> ElementNode
                 -> E.ChildHTML parent grandparent
mkContactAddress attrs node =
  E.address (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkContactAddressChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkContactAddressChild :: Element
                      -> Maybe (E.ChildHTML E.ContactAddress grandparent)
mkContactAddressChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Form -> Just $ mkForm attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      -- TODO: Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkArea :: E.ValidChild E.Area parent grandparent
       => [GA.Attribute] -> E.ChildHTML parent grandparent
mkArea attrs =
  E.area (mapMaybe mkAreaAttr attrs)

mkAreaAttr :: GA.Attribute -> Maybe (A.Attribute E.Area)
mkAreaAttr attr =
  let
    mbAttr =
      case attr of
        GA.Alt alt ->
          Just (A.alt alt :: A.Attribute E.Area)

        GA.Coords coords ->
          Just (A.coords coords :: A.Attribute E.Area)

        GA.Download download ->
          Just (A.download download :: A.Attribute E.Area)

        GA.Href href ->
          Just (A.href href :: A.Attribute E.Area)

        GA.Ping ping ->
          Just (A.ping ping :: A.Attribute E.Area)

        GA.ReferrerPolicy referrerpolicy ->
          Just (A.referrerpolicy referrerpolicy :: A.Attribute E.Area)

        GA.Rel rel ->
          Just (A.rel rel :: A.Attribute E.Area)

        GA.Shape shape ->
          Just (A.shape shape :: A.Attribute E.Area)

        GA.Target target ->
          Just (A.target target :: A.Attribute E.Area)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkArticle :: E.ValidChild E.Article parent grandparent
          => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkArticle attrs node =
  E.article (mapMaybe mkArticleAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkArticleChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkArticleAttr :: GA.Attribute -> Maybe (A.Attribute E.Article)
mkArticleAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Article)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkArticleChild :: Element -> Maybe (E.ChildHTML E.Article grandparent)
mkArticleChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkAside :: E.ValidChild E.Aside parent grandparent
        => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkAside attrs node =
  E.aside (mapMaybe mkAsideAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkAsideChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkAsideAttr :: GA.Attribute -> Maybe (A.Attribute E.Aside)
mkAsideAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Aside)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkAsideChild :: Element -> Maybe (E.ChildHTML E.Aside grandparent)
mkAsideChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkAudio :: E.ValidChild E.Audio parent grandparent
        => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkAudio attrs node =
  E.audio (mapMaybe mkAudioAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkAudioChild nodes
      Leaf _net -> []
      Void -> []

mkAudioAttr :: GA.Attribute -> Maybe (A.Attribute E.Audio)
mkAudioAttr attr =
  let
    mbAttr =
      case attr of
        GA.Autoplay ->
          Just (A.autoplay :: A.Attribute E.Audio)

        GA.Controls ->
          Just (A.controls :: A.Attribute E.Audio)

        GA.ControlsList controlslist ->
          Just (A.controlslist controlslist :: A.Attribute E.Audio)

        GA.CrossOrigin crossorigin ->
          Just (A.crossorigin crossorigin :: A.Attribute E.Audio)

        GA.DisableRemotePlayback ->
          Just (A.disableremoteplayback :: A.Attribute E.Audio)

        GA.Loop ->
          Just (A.loop :: A.Attribute E.Audio)

        GA.Muted muted ->
          Just (A.mute muted :: A.Attribute E.Audio)

        GA.Preload preload ->
          Just (A.preload preload :: A.Attribute E.Audio)

        GA.Src src ->
          Just (A.src src :: A.Attribute E.Audio)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkAudioChild :: Element -> Maybe (E.ChildHTML E.Audio grandparent)
mkAudioChild e =
  case elementType e of
    Source -> Just $ mkSource (elementAttrs e)
    Track -> Just $ mkTrack (elementAttrs e)
    _element -> Nothing

mkBringAttentionTo :: E.ValidChild E.BringAttentionTo parent grandparent
                   => [GA.Attribute]
                   -> ElementNode
                   -> E.ChildHTML parent grandparent
mkBringAttentionTo attrs node =
  E.b (mapMaybe mkBringAttentionToAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkBringAttentionToChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkBringAttentionToAttr :: GA.Attribute
                       -> Maybe (A.Attribute E.BringAttentionTo)
mkBringAttentionToAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.BringAttentionTo)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkBringAttentionToChild :: Element -> Maybe (E.ChildHTML E.BringAttentionTo grandparent)
mkBringAttentionToChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkBase :: E.ValidChild E.Base parent grandparent
       => [GA.Attribute] -> E.ChildHTML parent grandparent
mkBase attrs =
  E.base (mapMaybe mkBaseAttr attrs)

mkBaseAttr :: GA.Attribute -> Maybe (A.Attribute E.Base)
mkBaseAttr attr =
  let
    mbAttr =
      case attr of
        GA.Href href -> Just (A.href href :: A.Attribute E.Base)
        GA.Target target -> Just (A.target target :: A.Attribute E.Base)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkBidirectionalIsolation :: E.ValidChild E.BidirectionalIsolation parent grandparent
                         => [GA.Attribute]
                         -> ElementNode
                         -> E.ChildHTML parent grandparent
mkBidirectionalIsolation attrs node =
  E.bdi (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkBidirectionalIsolationChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkBidirectionalIsolationChild :: Element -> Maybe (E.ChildHTML E.BidirectionalIsolation grandparent)
mkBidirectionalIsolationChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkBidirectionalOverride :: E.ValidChild E.BidirectionalOverride parent grandparent
                        => [GA.Attribute]
                        -> ElementNode
                        -> E.ChildHTML parent grandparent
mkBidirectionalOverride attrs node =
  E.bdo (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkBidirectionalOverrideChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkBidirectionalOverrideChild :: Element -> Maybe (E.ChildHTML E.BidirectionalOverride grandparent)
mkBidirectionalOverrideChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkBlockquote :: E.ValidChild E.Blockquote parent grandparent
             => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkBlockquote attrs node =
  E.blockquote (mapMaybe mkBlockquoteAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkBlockquoteChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkBlockquoteAttr :: GA.Attribute -> Maybe (A.Attribute E.Blockquote)
mkBlockquoteAttr attr =
  let
    mbAttr =
      case attr of
        GA.Cite cite ->
          Just (A.cite cite :: A.Attribute E.Blockquote)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Blockquote)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkBlockquoteChild :: Element -> Maybe (E.ChildHTML E.Blockquote grandparent)
mkBlockquoteChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkBody :: E.ValidChild E.Body parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkBody attrs node =
  E.body (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkBodyChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkBodyChild :: Element -> Maybe (E.ChildHTML E.Body grandparent)
mkBodyChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkLineBreak :: E.ValidChild E.LineBreak parent grandparent
            => [GA.Attribute] -> E.ChildHTML parent grandparent
mkLineBreak attrs =
  E.br (mapMaybe mkGlobalAttr attrs)

mkButton :: E.ValidChild E.Button parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkButton attrs node =
  E.button (mapMaybe mkButtonAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkButtonChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkButtonAttr :: GA.Attribute -> Maybe (A.Attribute E.Button)
mkButtonAttr attr =
  let
    mbAttr =
      case attr of
        GA.Command command ->
          Just (A.command command :: A.Attribute E.Button)

        GA.CommandFor commandfor ->
          Just (A.commandfor commandfor :: A.Attribute E.Button)

        GA.Disabled disabled ->
          Just (A.disable disabled :: A.Attribute E.Button)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Button)

        GA.Form form ->
          Just (A.form form :: A.Attribute E.Button)

        GA.FormAction formaction ->
          Just (A.formaction formaction :: A.Attribute E.Button)

        GA.FormEnctype formenctype ->
          Just (A.formenctype formenctype :: A.Attribute E.Button)

        GA.FormMethod formmethod ->
          Just (A.formmethod formmethod :: A.Attribute E.Button)

        GA.FormNoValidate ->
          Just (A.formnovalidate :: A.Attribute E.Button)

        GA.FormTarget formtarget ->
          Just (A.formtarget formtarget :: A.Attribute E.Button)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.Button)

        GA.PopoverTarget popovertarget ->
          Just (A.popovertarget popovertarget :: A.Attribute E.Button)

        GA.PopoverTargetAction popovertargetaction ->
          Just
            (A.popovertargetaction popovertargetaction :: A.Attribute E.Button)

        GA.Type type_ ->
          Just (A.type_ type_ :: A.Attribute E.Button)

        GA.Value value ->
          Just (A.value value :: A.Attribute E.Button)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkButtonChild :: Element -> Maybe (E.ChildHTML E.Button grandparent)
mkButtonChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      Image -> Just $ mkImage attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkCanvas :: E.ValidChild E.Canvas parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkCanvas attrs node =
  E.canvas (mapMaybe mkCanvasAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkCanvasChild nodes
      Leaf _net -> []
      Void -> []

mkCanvasAttr :: GA.Attribute -> Maybe (A.Attribute E.Canvas)
mkCanvasAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Canvas)

        GA.Height height ->
          Just (A.height height :: A.Attribute E.Canvas)

        GA.Width width ->
          Just (A.width width :: A.Attribute E.Canvas)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkCanvasChild :: Element -> Maybe (E.ChildHTML E.Canvas grandparent)
mkCanvasChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      -- TODO: Button -> Just $ mkButton attrs content
      -- TODO: Input -> Just $ mkInput attrs
      _element -> Nothing

mkTableCaption :: E.ValidChild E.TableCaption parent grandparent
               => [GA.Attribute]
               -> ElementNode
               -> E.ChildHTML parent grandparent
mkTableCaption attrs node =
  E.caption (mapMaybe mkTableCaptionAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableCaptionChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkTableCaptionAttr :: GA.Attribute -> Maybe (A.Attribute E.TableCaption)
mkTableCaptionAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.TableCaption)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkTableCaptionChild :: Element -> Maybe (E.ChildHTML E.TableCaption grandparent)
mkTableCaptionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkCitation :: E.ValidChild E.Citation parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkCitation attrs node =
  E.cite (mapMaybe mkCitationAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkCitationChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkCitationAttr :: GA.Attribute -> Maybe (A.Attribute E.Citation)
mkCitationAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Citation)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkCitationChild :: Element -> Maybe (E.ChildHTML E.Citation grandparent)
mkCitationChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkCode :: E.ValidChild E.Code parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkCode attrs node =
  E.code (mapMaybe mkCodeAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkCodeChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkCodeAttr :: GA.Attribute -> Maybe (A.Attribute E.Code)
mkCodeAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Code)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkCodeChild :: Element -> Maybe (E.ChildHTML E.Code grandparent)
mkCodeChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkTableColumn :: E.ValidChild E.TableColumn parent grandparent
              => [GA.Attribute] -> E.ChildHTML parent grandparent
mkTableColumn attrs =
  E.col (mapMaybe mkTableColumnAttr attrs)

mkTableColumnAttr :: GA.Attribute -> Maybe (A.Attribute E.TableColumn)
mkTableColumnAttr attr =
  let
    mbAttr =
      case attr of
        GA.Span span -> Just (A.span span :: A.Attribute E.TableColumn)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkTableColumnGroup :: E.ValidChild E.TableColumnGroup parent grandparent
                   => [GA.Attribute]
                   -> ElementNode
                   -> E.ChildHTML parent grandparent
mkTableColumnGroup attrs node =
  E.colgroup (mapMaybe mkTableColumnGroupAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableColumnGroupChild nodes
      Leaf _net -> []
      Void -> []

mkTableColumnGroupAttr :: GA.Attribute -> Maybe (A.Attribute E.TableColumnGroup)
mkTableColumnGroupAttr attr =
  let
    mbAttr =
      case attr of
        GA.Span span -> Just (A.span span :: A.Attribute E.TableColumnGroup)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkTableColumnGroupChild :: Element
                        -> Maybe (E.ChildHTML E.TableColumnGroup grandparent)
mkTableColumnGroupChild e =
  case elementType e of
    TableColumn -> Just . mkTableColumn $ elementAttrs e
    _element -> Nothing

mkData :: E.ValidChild E.Data parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkData attrs node =
  E.data_ (mapMaybe mkDataAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDataChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkDataAttr :: GA.Attribute -> Maybe (A.Attribute E.Data)
mkDataAttr attr =
  let
    mbAttr =
      case attr of
        GA.Value value -> Just (A.value value :: A.Attribute E.Data)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkDataChild :: Element -> Maybe (E.ChildHTML E.Data grandparent)
mkDataChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkDataList :: E.ValidChild E.DataList parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkDataList attrs node =
  E.datalist (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDataListChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkDataListChild :: Element -> Maybe (E.ChildHTML E.DataList grandparent)
mkDataListChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Option -> Just $ mkOption attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkDescriptionDetails :: E.ValidChild E.DescriptionDetails parent grandparent
                     => [GA.Attribute]
                     -> ElementNode
                     -> E.ChildHTML parent grandparent
mkDescriptionDetails attrs node =
  E.dd (mapMaybe mkDescriptionDetailsAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDescriptionDetailsChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkDescriptionDetailsAttr :: GA.Attribute -> Maybe (A.Attribute E.DescriptionDetails)
mkDescriptionDetailsAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.DescriptionDetails)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkDescriptionDetailsChild :: Element
                          -> Maybe (E.ChildHTML E.DescriptionDetails grandparent)
mkDescriptionDetailsChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

-- TODO: Transparent element, try to figure this out.
--
mkDeletedText :: E.ValidChild E.DeletedText parent grandparent
              => [GA.Attribute]
              -> ElementNode
              -> E.ChildHTML parent grandparent
mkDeletedText attrs node =
  E.del (mapMaybe mkDeletedTextAttr attrs) $
    case node of
      Branch _nodes -> []
      Leaf _net -> []
      Void -> []

mkDeletedTextAttr :: GA.Attribute -> Maybe (A.Attribute E.DeletedText)
mkDeletedTextAttr attr =
  let
    mbAttr =
      case attr of
        GA.Cite cite ->
          Just (A.cite cite :: A.Attribute E.DeletedText)

        GA.Datetime datetime ->
          Just (A.datetime datetime :: A.Attribute E.DeletedText)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkDetails :: E.ValidChild E.Details parent grandparent
          => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkDetails attrs node =
  E.details (mapMaybe mkDetailsAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDetailsChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkDetailsAttr :: GA.Attribute -> Maybe (A.Attribute E.Details)
mkDetailsAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Details)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.Details)

        GA.Open ->
          Just (A.open :: A.Attribute E.Details)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkDetailsChild :: Element -> Maybe (E.ChildHTML E.Details grandparent)
mkDetailsChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Summary -> Just $ mkSummary attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkDefinition :: E.ValidChild E.Definition parent grandparent
             => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkDefinition attrs node =
  E.dfn (mapMaybe mkDefinitionAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDefinitionChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkDefinitionAttr :: GA.Attribute -> Maybe (A.Attribute E.Definition)
mkDefinitionAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Definition)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkDefinitionChild :: Element -> Maybe (E.ChildHTML E.Definition grandparent)
mkDefinitionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkDialog :: E.ValidChild E.Dialog parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkDialog attrs node =
  E.dialog (mapMaybe mkDialogAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDialogChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkDialogAttr :: GA.Attribute -> Maybe (A.Attribute E.Dialog)
mkDialogAttr attr =
  let
    mbAttr =
      case attr of
        -- TODO: GA.Open -> Just (A.open :: A.Attribute E.Dialog)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkDialogChild :: Element -> Maybe (E.ChildHTML E.Dialog grandparent)
mkDialogChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkDivision :: E.ValidChild E.Division parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkDivision attrs node =
  E.div (mapMaybe mkDivisionAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDivisionChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkDivisionAttr :: GA.Attribute -> Maybe (A.Attribute E.Division)
mkDivisionAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Division)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkDivisionChild :: Element -> Maybe (E.ChildHTML E.Division grandparent)
mkDivisionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkDescriptionList :: E.ValidChild E.DescriptionList parent grandparent
                  => [GA.Attribute]
                  -> ElementNode
                  -> E.ChildHTML parent grandparent
mkDescriptionList attrs node =
  E.dl (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDescriptionListChild nodes
      Leaf _net -> []
      Void -> []

mkDescriptionListChild :: Element
                       -> Maybe (E.ChildHTML E.DescriptionList grandparent)
mkDescriptionListChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      DescriptionDetails -> Just $ mkDescriptionDetails attrs content
      DescriptionTerm -> Just $ mkDescriptionTerm attrs content
      Division -> Just $ mkDivision attrs content
      Script -> Just $ mkScript attrs content
      ContentTemplate ->  Just $ mkContentTemplate attrs content
      _element -> Nothing

mkDescriptionTerm :: E.ValidChild E.DescriptionTerm parent grandparent
                  => [GA.Attribute]
                  -> ElementNode
                  -> E.ChildHTML parent grandparent
mkDescriptionTerm attrs node =
  E.dt (mapMaybe mkDescriptionTermAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkDescriptionTermChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkDescriptionTermAttr :: GA.Attribute -> Maybe (A.Attribute E.DescriptionTerm)
mkDescriptionTermAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.DescriptionTerm)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkDescriptionTermChild :: Element
                       -> Maybe (E.ChildHTML E.DescriptionTerm grandparent)
mkDescriptionTermChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Form -> Just $ mkForm attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkEmphasis :: E.ValidChild E.Emphasis parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkEmphasis attrs node =
  E.em (mapMaybe mkEmphasisAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkEmphasisChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkEmphasisAttr :: GA.Attribute -> Maybe (A.Attribute E.Emphasis)
mkEmphasisAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Emphasis)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkEmphasisChild :: Element -> Maybe (E.ChildHTML E.Emphasis grandparent)
mkEmphasisChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkEmbed :: E.ValidChild E.Embed parent grandparent
        => [GA.Attribute] -> E.ChildHTML parent grandparent
mkEmbed attrs =
  E.embed (mapMaybe mkEmbedAttr attrs)

mkEmbedAttr :: GA.Attribute -> Maybe (A.Attribute E.Embed)
mkEmbedAttr attr =
  let
    mbAttr =
      case attr of
        GA.Height height ->
          Just (A.height height :: A.Attribute E.Embed)

        GA.Src src ->
          Just (A.src src :: A.Attribute E.Embed)

        GA.Type type_ ->
          Just (A.type_ type_ :: A.Attribute E.Embed)

        GA.Width width ->
          Just (A.width width :: A.Attribute E.Embed)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkFieldset :: E.ValidChild E.Fieldset parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkFieldset attrs node =
  E.fieldset (mapMaybe mkFieldsetAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkFieldsetChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkFieldsetAttr :: GA.Attribute -> Maybe (A.Attribute E.Fieldset)
mkFieldsetAttr attr =
  let
    mbAttr =
      case attr of
        GA.Disabled disabled ->
          Just (A.disable disabled :: A.Attribute E.Fieldset)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Fieldset)

        GA.Form form ->
          Just (A.form form :: A.Attribute E.Fieldset)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.Fieldset)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkFieldsetChild :: Element -> Maybe (E.ChildHTML E.Fieldset grandparent)
mkFieldsetChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Legend -> Just $ mkLegend attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkFigureCaption :: E.ValidChild E.FigureCaption parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> E.ChildHTML parent grandparent
mkFigureCaption attrs node =
  E.figcaption (mapMaybe mkFigureCaptionAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkFigureCaptionChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkFigureCaptionAttr :: GA.Attribute -> Maybe (A.Attribute E.FigureCaption)
mkFigureCaptionAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.FigureCaption)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkFigureCaptionChild :: Element -> Maybe (E.ChildHTML E.FigureCaption grandparent)
mkFigureCaptionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkFigure :: E.ValidChild E.Figure parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkFigure attrs node =
  E.figure (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkFigureChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkFigureChild :: Element -> Maybe (E.ChildHTML E.Figure grandparent)
mkFigureChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      FigureCaption -> Just $ mkFigureCaption attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkFooter :: E.ValidChild E.Footer parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkFooter attrs node =
  E.footer (mapMaybe mkFooterAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkFooterChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkFooterAttr :: GA.Attribute -> Maybe (A.Attribute E.Footer)
mkFooterAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Footer)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkFooterChild :: Element -> Maybe (E.ChildHTML E.Footer grandparent)
mkFooterChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkForm :: E.ValidChild E.Form parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkForm attrs node =
  E.form (mapMaybe mkFormAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkFormChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkFormAttr :: GA.Attribute -> Maybe (A.Attribute E.Form)
mkFormAttr attr =
  let
    mbAttr =
      case attr of
        GA.AcceptCharset ->
          Just (A.acceptCharset :: A.Attribute E.Form)

        GA.Action action ->
          Just (A.action action :: A.Attribute E.Form)

        GA.Autocomplete autocomplete ->
          Just (A.autocomplete autocomplete :: A.Attribute E.Form)

        GA.Enctype enctype ->
          Just (A.enctype enctype :: A.Attribute E.Form)

        GA.Method method ->
          Just (A.method method :: A.Attribute E.Form)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.Form)

        GA.NoValidate novalidate ->
          Just (A.validate novalidate :: A.Attribute E.Form)

        GA.Rel rel ->
          Just (A.rel rel :: A.Attribute E.Form)

        GA.Target target ->
          Just (A.target target :: A.Attribute E.Form)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkFormChild :: Element -> Maybe (E.ChildHTML E.Form grandparent)
mkFormChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkH1 :: E.ValidChild E.H1 parent grandparent
     => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkH1 attrs node =
  E.h1 (mapMaybe mkH1Attr attrs) $
    case node of
      Branch nodes -> mapMaybe mkH1Child nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkH1Attr :: GA.Attribute -> Maybe (A.Attribute E.H1)
mkH1Attr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.H1)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkH1Child :: Element -> Maybe (E.ChildHTML E.H1 grandparent)
mkH1Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkH2 :: E.ValidChild E.H2 parent grandparent
     => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkH2 attrs node =
  E.h2 (mapMaybe mkH2Attr attrs) $
    case node of
      Branch nodes -> mapMaybe mkH2Child nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkH2Attr :: GA.Attribute -> Maybe (A.Attribute E.H2)
mkH2Attr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.H2)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkH2Child :: Element -> Maybe (E.ChildHTML E.H2 grandparent)
mkH2Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkH3 :: E.ValidChild E.H3 parent grandparent
     => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkH3 attrs node =
  E.h3 (mapMaybe mkH3Attr attrs) $
    case node of
      Branch nodes -> mapMaybe mkH3Child nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkH3Attr :: GA.Attribute -> Maybe (A.Attribute E.H3)
mkH3Attr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.H3)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkH3Child :: Element -> Maybe (E.ChildHTML E.H3 grandparent)
mkH3Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkH4 :: E.ValidChild E.H4 parent grandparent
     => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkH4 attrs node =
  E.h4 (mapMaybe mkH4Attr attrs) $
    case node of
      Branch nodes -> mapMaybe mkH4Child nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkH4Attr :: GA.Attribute -> Maybe (A.Attribute E.H4)
mkH4Attr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.H4)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkH4Child :: Element -> Maybe (E.ChildHTML E.H4 grandparent)
mkH4Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkH5 :: E.ValidChild E.H5 parent grandparent
     => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkH5 attrs node =
  E.h5 (mapMaybe mkH5Attr attrs) $
    case node of
      Branch nodes -> mapMaybe mkH5Child nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkH5Attr :: GA.Attribute -> Maybe (A.Attribute E.H5)
mkH5Attr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.H5)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkH5Child :: Element -> Maybe (E.ChildHTML E.H5 grandparent)
mkH5Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkH6 :: E.ValidChild E.H6 parent grandparent
     => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkH6 attrs node =
  E.h6 (mapMaybe mkH6Attr attrs) $
    case node of
      Branch nodes -> mapMaybe mkH6Child nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkH6Attr :: GA.Attribute -> Maybe (A.Attribute E.H6)
mkH6Attr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.H6)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkH6Child :: Element -> Maybe (E.ChildHTML E.H6 grandparent)
mkH6Child e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkHead :: E.ValidChild E.Head parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkHead attrs node =
  E.head (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkHeadChild nodes
      Leaf _net -> []
      Void -> []

mkHeadChild :: Element -> Maybe (E.ChildHTML E.Head grandparent)
mkHeadChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Base -> Just $ mkBase attrs
      Link -> Just $ mkLink attrs
      Meta -> Just $ mkMeta attrs
      NoScript -> Just $ mkNoScript attrs content
      Script -> Just $ mkScript attrs content
      Style -> Just $ mkStyle attrs content
      Title -> Just $ mkTitle attrs content
      _element -> Nothing

mkHeader :: E.ValidChild E.Header parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkHeader attrs node =
  E.header (mapMaybe mkHeaderAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkHeaderChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkHeaderAttr :: GA.Attribute -> Maybe (A.Attribute E.Header)
mkHeaderAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Header)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkHeaderChild :: Element -> Maybe (E.ChildHTML E.Header grandparent)
mkHeaderChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkHeadingGroup :: E.ValidChild E.HeadingGroup parent grandparent
               => [GA.Attribute]
               -> ElementNode
               -> E.ChildHTML parent grandparent
mkHeadingGroup attrs node =
  E.hgroup (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkHeadingGroupChild nodes
      Leaf _net -> []
      Void -> []

mkHeadingGroupChild :: Element
                    -> Maybe (E.ChildHTML E.HeadingGroup grandparent)
mkHeadingGroupChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Paragraph -> Just $ mkParagraph attrs content
      _element -> Nothing

mkHorizontalRule :: E.ValidChild E.HorizontalRule parent grandparent
                 => [GA.Attribute] -> E.ChildHTML parent grandparent
mkHorizontalRule attrs =
  E.hr (mapMaybe mkGlobalAttr attrs)

mkHtml :: [GA.Attribute] -> ElementNode -> E.ChildHTML E.CustomHTML grandparent
mkHtml attrs node =
  E.customHTML "html" (mapMaybe mkHtmlAttr attrs) $
    case node of
      Branch nodes -> Right $ mapMaybe mkHtmlChild nodes
      Leaf _net -> Left Types.WithTag
      Void -> Left Types.WithTag

mkHtmlAttr :: GA.Attribute -> Maybe (A.Attribute E.CustomHTML)
mkHtmlAttr attr =
  let
    mbAttr =
      case attr of
        GA.XMLNS xmlns -> Just (A.xmlns xmlns :: A.Attribute E.CustomHTML)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkHtmlChild :: Element -> Maybe (E.ChildHTML E.CustomHTML grandparent)
mkHtmlChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Body -> Just $ mkBody attrs content
      Head -> Just $ mkHead attrs content
      _element -> Nothing

mkIdiomaticText :: E.ValidChild E.IdiomaticText parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> E.ChildHTML parent grandparent
mkIdiomaticText attrs node =
  E.i (mapMaybe mkIdiomaticTextAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkIdiomaticTextChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkIdiomaticTextAttr :: GA.Attribute -> Maybe (A.Attribute E.IdiomaticText)
mkIdiomaticTextAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.IdiomaticText)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkIdiomaticTextChild :: Element
                     -> Maybe (E.ChildHTML E.IdiomaticText grandparent)
mkIdiomaticTextChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkIFrame :: E.ValidChild E.IFrame parent grandparent
         => [GA.Attribute] -> E.ChildHTML parent grandparent
mkIFrame attrs =
  E.iframe (mapMaybe mkIFrameAttr attrs)

mkIFrameAttr :: GA.Attribute -> Maybe (A.Attribute E.IFrame)
mkIFrameAttr attr =
  let
    mbAttr =
      case attr of
        GA.Allow allow ->
          Just (A.allow allow :: A.Attribute E.IFrame)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.IFrame)

        GA.Height height ->
          Just (A.height height :: A.Attribute E.IFrame)

        GA.Loading loading ->
          Just (A.loading loading :: A.Attribute E.IFrame)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.IFrame)

        GA.ReferrerPolicy referrerpolicy ->
          Just (A.referrerpolicy referrerpolicy :: A.Attribute E.IFrame)

        GA.Sandbox sandbox ->
          Just (A.sandbox sandbox :: A.Attribute E.IFrame)

        GA.Src src ->
          Just (A.src src :: A.Attribute E.IFrame)

        GA.SrcDoc srcdoc ->
          Just (A.srcdoc' srcdoc :: A.Attribute E.IFrame)

        GA.Width width ->
          Just (A.width width :: A.Attribute E.IFrame)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkImage :: E.ValidChild E.Image parent grandparent
        => [GA.Attribute] -> E.ChildHTML parent grandparent
mkImage attrs =
  E.img (mapMaybe mkImageAttr attrs)

mkImageAttr :: GA.Attribute -> Maybe (A.Attribute E.Image)
mkImageAttr attr =
  let
    mbAttr =
      case attr of
        GA.Alt alt ->
          Just (A.alt alt :: A.Attribute E.Image)

        GA.CrossOrigin crossorigin ->
          Just (A.crossorigin crossorigin :: A.Attribute E.Image)

        GA.Decoding decoding ->
          Just (A.decoding decoding :: A.Attribute E.Image)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Image)

        GA.FetchPriority fetchpriority ->
          Just (A.fetchpriority fetchpriority :: A.Attribute E.Image)

        GA.Height height ->
          Just (A.height height :: A.Attribute E.Image)

        GA.IsMap ->
          Just (A.ismap :: A.Attribute E.Image)

        GA.Loading loading ->
          Just (A.loading loading :: A.Attribute E.Image)

        GA.ReferrerPolicy referrerpolicy ->
          Just (A.referrerpolicy referrerpolicy :: A.Attribute E.Image)

        GA.Sizes sizes ->
          Just (A.sizes sizes :: A.Attribute E.Image)

        GA.Src src ->
          Just (A.src src :: A.Attribute E.Image)

        GA.SrcSet srcset ->
          Just (A.srcset srcset :: A.Attribute E.Image)

        GA.UseMap usemap ->
          Just (A.usemap usemap :: A.Attribute E.Image)

        GA.Width width ->
          Just (A.width width :: A.Attribute E.Image)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkInput :: E.ValidChild E.Input parent grandparent
        => [GA.Attribute] -> E.ChildHTML parent grandparent
mkInput attrs =
  E.input (mapMaybe mkInputAttr attrs)

mkInputAttr :: GA.Attribute -> Maybe (A.Attribute E.Input)
mkInputAttr attr =
  let
    mbAttr =
      case attr of
        GA.Accept accept ->
          Just (A.accept accept :: A.Attribute E.Input)

        GA.Alt alt ->
          Just (A.alt alt :: A.Attribute E.Input)

        GA.Autocomplete autocomplete ->
          Just (A.autocomplete autocomplete :: A.Attribute E.Input)

        GA.Capture capture ->
          Just (A.capture capture :: A.Attribute E.Input)

        GA.Checked checked ->
          Just (A.check checked :: A.Attribute E.Input)

        GA.Dirname dirname ->
          Just (A.dirname dirname :: A.Attribute E.Input)

        GA.Disabled disabled ->
          Just (A.disable disabled :: A.Attribute E.Input)

        GA.Form form ->
          Just (A.form form :: A.Attribute E.Input)

        GA.FormAction formaction ->
          Just (A.formaction formaction :: A.Attribute E.Input)

        GA.FormEnctype formenctype ->
          Just (A.formenctype formenctype :: A.Attribute E.Input)

        GA.FormMethod formmethod ->
          Just (A.formmethod formmethod :: A.Attribute E.Input)

        GA.FormNoValidate ->
          Just (A.formnovalidate :: A.Attribute E.Input)

        GA.FormTarget formtarget ->
          Just (A.formtarget formtarget :: A.Attribute E.Input)

        GA.Height height ->
          Just (A.height height :: A.Attribute E.Input)

        GA.List list ->
          Just (A.list list :: A.Attribute E.Input)

        GA.Max max ->
          Just (A.max max :: A.Attribute E.Input)

        GA.MaxLength maxlength ->
          Just (A.maxlength maxlength :: A.Attribute E.Input)

        GA.Min min ->
          Just (A.min min :: A.Attribute E.Input)

        GA.MinLength minlength ->
          Just (A.minlength minlength :: A.Attribute E.Input)

        GA.Multiple ->
          Just (A.multiple :: A.Attribute E.Input)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.Input)

        GA.Pattern pattern ->
          Just (A.pattern pattern :: A.Attribute E.Input)

        GA.Placeholder placeholder ->
          Just (A.placeholder placeholder :: A.Attribute E.Input)

        GA.PopoverTarget popovertarget ->
          Just (A.popovertarget popovertarget :: A.Attribute E.Input)

        GA.PopoverTargetAction popovertargetaction ->
          Just (A.popovertargetaction popovertargetaction :: A.Attribute E.Input)

        GA.ReadOnly ->
          Just (A.readonly :: A.Attribute E.Input)

        GA.Required required ->
          Just (A.require required :: A.Attribute E.Input)

        GA.Size size ->
          Just (A.size size :: A.Attribute E.Input)

        GA.Src src ->
          Just (A.src src :: A.Attribute E.Input)

        GA.Step step ->
          Just (A.step step :: A.Attribute E.Input)

        GA.Type type_ ->
          Just (A.type_ type_ :: A.Attribute E.Input)

        GA.Value value ->
          Just (A.value value :: A.Attribute E.Input)

        GA.Width width ->
          Just (A.width width :: A.Attribute E.Input)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

-- TODO: Transparent element, try to figure this out.
--
mkInsertedText :: E.ValidChild E.InsertedText parent grandparent
               => [GA.Attribute]
               -> ElementNode
               -> E.ChildHTML parent grandparent
mkInsertedText attrs node =
  E.ins (mapMaybe mkInsertedTextAttr attrs) $
    case node of
      Branch _nodes -> []
      Leaf _net -> []
      Void -> []

mkInsertedTextAttr :: GA.Attribute -> Maybe (A.Attribute E.InsertedText)
mkInsertedTextAttr attr =
  let
    mbAttr =
      case attr of
        GA.Cite cite ->
          Just (A.cite cite :: A.Attribute E.InsertedText)

        GA.Datetime datetime ->
          Just (A.datetime datetime :: A.Attribute E.InsertedText)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkKeyboardInput :: E.ValidChild E.KeyboardInput parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> E.ChildHTML parent grandparent
mkKeyboardInput attrs node =
  E.kbd (mapMaybe mkKeyboardInputAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkKeyboardInputChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkKeyboardInputAttr :: GA.Attribute -> Maybe (A.Attribute E.KeyboardInput)
mkKeyboardInputAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.KeyboardInput)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkKeyboardInputChild :: Element
                     -> Maybe (E.ChildHTML E.KeyboardInput grandparent)
mkKeyboardInputChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkLabel :: E.ValidChild E.Label parent grandparent
        => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkLabel attrs node =
  E.label (mapMaybe mkLabelAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkLabelChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkLabelAttr :: GA.Attribute -> Maybe (A.Attribute E.Label)
mkLabelAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Label)

        GA.ForLabel for ->
          Just (A.for for :: A.Attribute E.Label)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkLabelChild :: Element -> Maybe (E.ChildHTML E.Label grandparent)
mkLabelChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkLegend :: E.ValidChild E.Legend parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkLegend attrs node =
  E.legend (mapMaybe mkLegendAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkLegendChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkLegendAttr :: GA.Attribute -> Maybe (A.Attribute E.Legend)
mkLegendAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Legend)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkLegendChild :: Element -> Maybe (E.ChildHTML E.Legend grandparent)
mkLegendChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkListItem :: E.ValidChild E.ListItem parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkListItem attrs node =
  E.li (mapMaybe mkListItemAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkListItemChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkListItemAttr :: GA.Attribute -> Maybe (A.Attribute E.ListItem)
mkListItemAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.ListItem)

        GA.ValueInteger value ->
          Just (A.value value :: A.Attribute E.ListItem)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkListItemChild :: Element -> Maybe (E.ChildHTML E.ListItem grandparent)
mkListItemChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkLink :: E.ValidChild E.Link parent grandparent
       => [GA.Attribute] -> E.ChildHTML parent grandparent
mkLink attrs =
  E.link (mapMaybe mkLinkAttr attrs)

mkLinkAttr :: GA.Attribute -> Maybe (A.Attribute E.Link)
mkLinkAttr attr =
  let
    mbAttr =
      case attr of
        GA.As as ->
          Just (A.as as :: A.Attribute E.Link)

        GA.Blocking blocking ->
          Just (A.blocking blocking :: A.Attribute E.Link)

        GA.CrossOrigin crossorigin ->
          Just (A.crossorigin crossorigin :: A.Attribute E.Link)

        GA.Disabled disabled ->
          Just (A.disable disabled :: A.Attribute E.Link)

        GA.FetchPriority fetchpriority ->
          Just (A.fetchpriority fetchpriority :: A.Attribute E.Link)

        GA.Href href ->
          Just (A.href href :: A.Attribute E.Link)

        GA.HrefLang hreflang ->
          Just (A.hreflang hreflang :: A.Attribute E.Link)

        GA.ImageSizes imagesizes ->
          Just (A.imagesizes imagesizes :: A.Attribute E.Link)

        GA.ImageSrcset imagesrcset ->
          Just (A.imagesrcset imagesrcset :: A.Attribute E.Link)

        GA.Integrity encoding bs ->
          Just (A.integrity encoding bs :: A.Attribute E.Link)

        GA.Media media ->
          Just (A.media media :: A.Attribute E.Link)

        GA.ReferrerPolicy referrerpolicy ->
          Just (A.referrerpolicy referrerpolicy :: A.Attribute E.Link)

        GA.Rel rel ->
          Just (A.rel rel :: A.Attribute E.Link)

        GA.Sizes sizes ->
          Just (A.sizes sizes :: A.Attribute E.Link)

        GA.Title title ->
          Just (A.title title :: A.Attribute E.Link)

        GA.Type type_ ->
          Just (A.type_ type_ :: A.Attribute E.Link)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkMain :: E.ValidChild E.Main parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkMain attrs node =
  E.main (mapMaybe mkMainAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkMainChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkMainAttr :: GA.Attribute -> Maybe (A.Attribute E.Main)
mkMainAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Main)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkMainChild :: Element -> Maybe (E.ChildHTML E.Main grandparent)
mkMainChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkMap :: E.ValidChild E.Map parent grandparent
      => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkMap attrs node =
  E.map (mapMaybe mkMapAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkMapChild nodes
      Leaf _net -> []
      Void -> []

mkMapAttr :: GA.Attribute -> Maybe (A.Attribute E.Map)
mkMapAttr attr =
  let
    mbAttr =
      case attr of
        GA.Name name -> Just (A.name name :: A.Attribute E.Map)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkMapChild :: Element -> Maybe (E.ChildHTML E.Map grandparent)
mkMapChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Audio -> Just $ mkAudio attrs content
      Canvas -> Just $ mkCanvas attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      InsertedText -> Just $ mkInsertedText attrs content
      Map -> Just $ mkMap attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Slot -> Just $ mkSlot attrs content
      Video -> Just $ mkVideo attrs content
      _element -> Nothing

mkMark :: E.ValidChild E.Mark parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkMark attrs node =
  E.mark (mapMaybe mkMarkAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkMarkChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkMarkAttr :: GA.Attribute -> Maybe (A.Attribute E.Mark)
mkMarkAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Mark)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkMarkChild :: Element -> Maybe (E.ChildHTML E.Mark grandparent)
mkMarkChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkMenu :: E.ValidChild E.Menu parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkMenu attrs node =
  E.menu (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkMenuChild nodes
      Leaf _net -> []
      Void -> []

mkMenuChild :: Element -> Maybe (E.ChildHTML E.Menu grandparent)
mkMenuChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      ListItem -> Just $ mkListItem attrs content
      Script -> Just $ mkScript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      _element -> Nothing

mkMeta :: E.ValidChild E.Meta parent grandparent
       => [GA.Attribute] -> E.ChildHTML parent grandparent
mkMeta attrs =
  E.meta (mapMaybe mkMetaAttr attrs)

mkMetaAttr :: GA.Attribute -> Maybe (A.Attribute E.Meta)
mkMetaAttr attr =
  let
    mbAttr =
      case attr of
        GA.Charset ->
          Just (A.charset :: A.Attribute E.Meta)

        GA.Content content ->
          Just (A.content content :: A.Attribute E.Meta)

        GA.HttpEquiv httpEquiv ->
          Just (A.httpEquiv httpEquiv :: A.Attribute E.Meta)

        GA.Media media ->
          Just (A.media media :: A.Attribute E.Meta)

        GA.NameMeta name ->
          Just (A.name name :: A.Attribute E.Meta)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkMeter :: E.ValidChild E.Meter parent grandparent
        => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkMeter attrs node =
  E.meter (mapMaybe mkMeterAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkMeterChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkMeterAttr :: GA.Attribute -> Maybe (A.Attribute E.Meter)
mkMeterAttr attr =
  let
    mbAttr =
      case attr of
        GA.Form form -> Just (A.form form :: A.Attribute E.Meter)
        GA.High high -> Just (A.high high :: A.Attribute E.Meter)
        GA.Low low -> Just (A.low low :: A.Attribute E.Meter)
        GA.Max max -> Just (A.max max :: A.Attribute E.Meter)
        GA.Min min -> Just (A.min min :: A.Attribute E.Meter)
        GA.Optimum optimum -> Just (A.optimum optimum :: A.Attribute E.Meter)
        GA.ValueNumber value -> Just (A.value value :: A.Attribute E.Meter)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkMeterChild :: Element -> Maybe (E.ChildHTML E.Meter grandparent)
mkMeterChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkNav :: E.ValidChild E.Nav parent grandparent
      => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkNav attrs node =
  E.nav (mapMaybe mkNavAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkNavChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkNavAttr :: GA.Attribute -> Maybe (A.Attribute E.Nav)
mkNavAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Nav)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkNavChild :: Element -> Maybe (E.ChildHTML E.Nav grandparent)
mkNavChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

-- TODO: wtf lol
--
mkNoScript :: E.ValidChild E.NoScript parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkNoScript attrs node =
  E.noscript (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch _nodes -> []
      Leaf _net -> []
      Void -> []

-- TODO: What does this take?
--
mkObject :: E.ValidChild E.Object parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkObject attrs node =
  E.object (mapMaybe mkObjectAttr attrs) $
    case node of
      Branch _nodes -> []
      Leaf _net -> []
      Void -> []

mkObjectAttr :: GA.Attribute -> Maybe (A.Attribute E.Object)
mkObjectAttr attr =
  let
    mbAttr =
      case attr of
        GA.Data data_ -> Just (A.data_ data_ :: A.Attribute E.Object)
        GA.Form form -> Just (A.form form :: A.Attribute E.Object)
        GA.Height height -> Just (A.height height :: A.Attribute E.Object)
        GA.Name name -> Just (A.name name :: A.Attribute E.Object)
        GA.Type type_ -> Just (A.type_ type_ :: A.Attribute E.Object)
        GA.Width width -> Just (A.width width :: A.Attribute E.Object)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkOrderedList :: E.ValidChild E.OrderedList parent grandparent
              => [GA.Attribute]
              -> ElementNode
              -> E.ChildHTML parent grandparent
mkOrderedList attrs node =
  E.ol (mapMaybe mkOrderedListAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkOrderedListChild nodes
      Leaf _net -> []
      Void -> []

mkOrderedListAttr :: GA.Attribute -> Maybe (A.Attribute E.OrderedList)
mkOrderedListAttr attr =
  let
    mbAttr =
      case attr of
        GA.Reversed reversed ->
          Just (A.reverse reversed :: A.Attribute E.OrderedList)

        GA.Start start ->
          Just (A.start start :: A.Attribute E.OrderedList)

        GA.Type type_ ->
          Just (A.type_ type_ :: A.Attribute E.OrderedList)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkOrderedListChild :: Element
                   -> Maybe (E.ChildHTML E.OrderedList grandparent)
mkOrderedListChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      ListItem -> Just $ mkListItem attrs content
      Script -> Just $ mkScript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      _element -> Nothing

mkOptionGroup :: E.ValidChild E.OptionGroup parent grandparent
              => [GA.Attribute]
              -> ElementNode
              -> E.ChildHTML parent grandparent
mkOptionGroup attrs node =
  E.optgroup (mapMaybe mkOptionGroupAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkOptionGroupChild nodes
      Leaf _net -> []
      Void -> []

mkOptionGroupAttr :: GA.Attribute -> Maybe (A.Attribute E.OptionGroup)
mkOptionGroupAttr attr =
  let
    mbAttr =
      case attr of
        GA.Disabled disabled ->
          Just (A.disable disabled :: A.Attribute E.OptionGroup)

        GA.Label label ->
          Just (A.label label :: A.Attribute E.OptionGroup)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkOptionGroupChild :: Element -> Maybe (E.ChildHTML E.OptionGroup grandparent)
mkOptionGroupChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Option -> Just $ mkOption attrs content
      _element -> Nothing

mkOption :: E.ValidChild E.Option parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkOption attrs node =
  E.option (mapMaybe mkOptionAttr attrs) $
    case node of
      Branch _nodes -> []
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkOptionAttr :: GA.Attribute -> Maybe (A.Attribute E.Option)
mkOptionAttr attr =
  let
    mbAttr =
      case attr of
        GA.Disabled disabled ->
          Just (A.disable disabled :: A.Attribute E.Option)

        GA.Label label ->
          Just (A.label label :: A.Attribute E.Option)

        GA.Selected selected ->
          Just (A.select selected :: A.Attribute E.Option)

        GA.Value value ->
          Just (A.value value :: A.Attribute E.Option)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkOutput :: E.ValidChild E.Output parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkOutput attrs node =
  E.output (mapMaybe mkOutputAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkOutputChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkOutputAttr :: GA.Attribute -> Maybe (A.Attribute E.Output)
mkOutputAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Output)

        GA.ForOutput for ->
          Just (A.for for :: A.Attribute E.Output)

        GA.Form form ->
          Just (A.form form :: A.Attribute E.Output)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.Output)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkOutputChild :: Element -> Maybe (E.ChildHTML E.Output grandparent)
mkOutputChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkParagraph :: E.ValidChild E.Paragraph parent grandparent
            => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkParagraph attrs node =
  E.p (mapMaybe mkParagraphAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkParagraphChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkParagraphAttr :: GA.Attribute -> Maybe (A.Attribute E.Paragraph)
mkParagraphAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Paragraph)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkParagraphChild :: Element -> Maybe (E.ChildHTML E.Paragraph grandparent)
mkParagraphChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkPicture :: E.ValidChild E.Picture parent grandparent
          => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkPicture attrs node =
  E.picture (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkPictureChild nodes
      Leaf _net -> []
      Void -> []

mkPictureChild :: Element -> Maybe (E.ChildHTML E.Picture grandparent)
mkPictureChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Image -> Just $ mkImage attrs
      Script -> Just $ mkScript attrs content
      Source -> Just $ mkSource attrs
      ContentTemplate -> Just $ mkContentTemplate attrs content
      _element -> Nothing

mkPreformattedText :: E.ValidChild E.PreformattedText parent grandparent
                   => [GA.Attribute]
                   -> ElementNode
                   -> E.ChildHTML parent grandparent
mkPreformattedText attrs node =
  E.pre (mapMaybe mkPreformattedTextAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkPreformattedTextChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkPreformattedTextAttr :: GA.Attribute -> Maybe (A.Attribute E.PreformattedText)
mkPreformattedTextAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.PreformattedText)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkPreformattedTextChild :: Element
                        -> Maybe (E.ChildHTML E.PreformattedText grandparent)
mkPreformattedTextChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkProgress :: E.ValidChild E.Progress parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkProgress attrs node =
  E.progress (mapMaybe mkProgressAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkProgressChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkProgressAttr :: GA.Attribute -> Maybe (A.Attribute E.Progress)
mkProgressAttr attr =
  let
    mbAttr =
      case attr of
        GA.Max max -> Just (A.max max :: A.Attribute E.Progress)
        GA.ValueNumber value -> Just (A.value value :: A.Attribute E.Progress)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkProgressChild :: Element -> Maybe (E.ChildHTML E.Progress grandparent)
mkProgressChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkQuotation :: E.ValidChild E.Quotation parent grandparent
            => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkQuotation attrs node =
  E.q (mapMaybe mkQuotationAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkQuotationChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkQuotationAttr :: GA.Attribute -> Maybe (A.Attribute E.Quotation)
mkQuotationAttr attr =
  let
    mbAttr =
      case attr of
        GA.Cite cite ->
          Just (A.cite cite :: A.Attribute E.Quotation)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Quotation)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkQuotationChild :: Element -> Maybe (E.ChildHTML E.Quotation grandparent)
mkQuotationChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkRubyParenthesis :: E.ValidChild E.RubyParenthesis parent grandparent
                  => [GA.Attribute]
                  -> ElementNode
                  -> E.ChildHTML parent grandparent
mkRubyParenthesis attrs node =
  E.rp (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch _nodes -> []
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkRubyText :: E.ValidChild E.RubyText parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkRubyText attrs node =
  E.rt (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkRubyTextChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkRubyTextChild :: Element -> Maybe (E.ChildHTML E.RubyText grandparent)
mkRubyTextChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkRuby :: E.ValidChild E.Ruby parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkRuby attrs node =
  E.ruby (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkRubyChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkRubyChild :: Element -> Maybe (E.ChildHTML E.Ruby grandparent)
mkRubyChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      RubyParenthesis -> Just $ mkRubyParenthesis attrs content
      RubyText -> Just $ mkRubyText attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkStrikethrough :: E.ValidChild E.Strikethrough parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> E.ChildHTML parent grandparent
mkStrikethrough attrs node =
  E.s (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkStrikethroughChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkStrikethroughChild :: Element
                     -> Maybe (E.ChildHTML E.Strikethrough grandparent)
mkStrikethroughChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkSample :: E.ValidChild E.Sample parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkSample attrs node =
  E.samp (mapMaybe mkSampleAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSampleChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkSampleAttr :: GA.Attribute -> Maybe (A.Attribute E.Sample)
mkSampleAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Sample)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSampleChild :: Element -> Maybe (E.ChildHTML E.Sample grandparent)
mkSampleChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkScript :: E.ValidChild E.Script parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkScript attrs node =
  E.script (mapMaybe mkScriptAttr attrs) $
    case node of
      Branch _nodes -> Nothing
      Leaf net -> Just net
      Void -> Nothing

mkScriptAttr :: GA.Attribute -> Maybe (A.Attribute E.Script)
mkScriptAttr attr =
  let
    mbAttr =
      case attr of
        GA.Async ->
          Just (A.async :: A.Attribute E.Script)

        GA.Blocking blocking ->
          Just (A.blocking blocking :: A.Attribute E.Script)

        GA.CrossOrigin crossorigin ->
          Just (A.crossorigin crossorigin :: A.Attribute E.Script)

        GA.Defer ->
          Just (A.defer :: A.Attribute E.Script)

        GA.FetchPriority fetchpriority ->
          Just (A.fetchpriority fetchpriority :: A.Attribute E.Script)

        GA.Integrity encoding bs ->
          Just (A.integrity encoding bs :: A.Attribute E.Script)

        GA.NoModule nomodule ->
          Just (A.nomodule nomodule :: A.Attribute E.Script)

        GA.ReferrerPolicy referrerpolicy ->
          Just (A.referrerpolicy referrerpolicy :: A.Attribute E.Script)

        GA.Src src ->
          Just (A.src src :: A.Attribute E.Script)

        GA.Type type_ ->
          Just (A.type_ type_ :: A.Attribute E.Script)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSearch :: E.ValidChild E.Search parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkSearch attrs node =
  E.search (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSearchChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkSearchChild :: Element -> Maybe (E.ChildHTML E.Search grandparent)
mkSearchChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkSection :: E.ValidChild E.Section parent grandparent
          => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkSection attrs node =
  E.section (mapMaybe mkSectionAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSectionChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkSectionAttr :: GA.Attribute -> Maybe (A.Attribute E.Section)
mkSectionAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Section)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSectionChild :: Element -> Maybe (E.ChildHTML E.Section grandparent)
mkSectionChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkSelect :: E.ValidChild E.Select parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkSelect attrs node =
  E.select (mapMaybe mkSelectAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSelectChild nodes
      Leaf _net -> []
      Void -> []

mkSelectAttr :: GA.Attribute -> Maybe (A.Attribute E.Select)
mkSelectAttr attr =
  let
    mbAttr =
      case attr of
        GA.Autocomplete autocomplete ->
          Just (A.autocomplete autocomplete :: A.Attribute E.Select)

        GA.Disabled disabled ->
          Just (A.disable disabled :: A.Attribute E.Select)

        GA.Form form ->
          Just (A.form form :: A.Attribute E.Select)

        GA.Multiple ->
          Just (A.multiple :: A.Attribute E.Select)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.Select)

        GA.Required required ->
          Just (A.require required :: A.Attribute E.Select)

        GA.Size size ->
          Just (A.size size :: A.Attribute E.Select)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSelectChild :: Element -> Maybe (E.ChildHTML E.Select grandparent)
mkSelectChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Option -> Just $ mkOption attrs content
      OptionGroup -> Just $ mkOptionGroup attrs content
      _element -> Nothing

-- TODO: what do?
mkSlot :: E.ValidChild E.Slot parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkSlot attrs node =
  E.slot (mapMaybe mkSlotAttr attrs) $
    case node of
      Branch _nodes -> []
      Leaf _net -> []
      Void -> []

mkSlotAttr :: GA.Attribute -> Maybe (A.Attribute E.Slot)
mkSlotAttr attr =
  let
    mbAttr =
      case attr of
        GA.Name name -> Just (A.name name :: A.Attribute E.Slot)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

-- TODO: Convert all of these to Either String etc, so it can report when a bad
-- combination was generated.
--
mkSideComment :: E.ValidChild E.SideComment parent grandparent
              => [GA.Attribute]
              -> ElementNode
              -> E.ChildHTML parent grandparent
mkSideComment attrs node =
  E.small (mapMaybe mkSideCommentAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSideCommentChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkSideCommentAttr :: GA.Attribute -> Maybe (A.Attribute E.SideComment)
mkSideCommentAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.SideComment)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSideCommentChild :: Element -> Maybe (E.ChildHTML E.SideComment grandparent)
mkSideCommentChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkSource :: E.ValidChild E.Source parent grandparent
         => [GA.Attribute] -> E.ChildHTML parent grandparent
mkSource attrs =
  E.source (mapMaybe mkSourceAttr attrs)

mkSourceAttr :: GA.Attribute -> Maybe (A.Attribute E.Source)
mkSourceAttr attr =
  let
    mbAttr =
      case attr of
        GA.Height height -> Just (A.height height :: A.Attribute E.Source)
        GA.Media media -> Just (A.media media :: A.Attribute E.Source)
        GA.Sizes sizes -> Just (A.sizes sizes :: A.Attribute E.Source)
        GA.Src src -> Just (A.src src :: A.Attribute E.Source)
        GA.SrcSet srcset -> Just (A.srcset srcset :: A.Attribute E.Source)
        GA.Type type_ -> Just (A.type_ type_ :: A.Attribute E.Source)
        GA.Width width -> Just (A.width width :: A.Attribute E.Source)
        _attr -> Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSpan :: E.ValidChild E.Span parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkSpan attrs node =
  E.span (mapMaybe mkSpanAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSpanChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkSpanAttr :: GA.Attribute -> Maybe (A.Attribute E.Span)
mkSpanAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Span)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSpanChild :: Element -> Maybe (E.ChildHTML E.Span grandparent)
mkSpanChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkStrong :: E.ValidChild E.Strong parent grandparent
         => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkStrong attrs node =
  E.strong (mapMaybe mkStrongAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkStrongChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkStrongAttr :: GA.Attribute -> Maybe (A.Attribute E.Strong)
mkStrongAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Strong)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkStrongChild :: Element -> Maybe (E.ChildHTML E.Strong grandparent)
mkStrongChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkStyle :: E.ValidChild E.Style parent grandparent
        => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkStyle attrs node =
  E.style (mapMaybe mkStyleAttr attrs) $
    case node of
      Branch _nodes -> T.empty
      Leaf net -> NET.toText net
      Void -> T.empty

mkStyleAttr :: GA.Attribute -> Maybe (A.Attribute E.Style)
mkStyleAttr attr =
  let
    mbAttr =
      case attr of
        GA.Blocking blocking ->
          Just (A.blocking blocking :: A.Attribute E.Style)

        GA.Media media ->
          Just (A.media media :: A.Attribute E.Style)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSubscript :: E.ValidChild E.Subscript parent grandparent
            => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkSubscript attrs node =
  E.sub (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSubscriptChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkSubscriptChild :: Element -> Maybe (E.ChildHTML E.Subscript grandparent)
mkSubscriptChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkSummary :: E.ValidChild E.Summary parent grandparent
          => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkSummary attrs node =
  E.summary (mapMaybe mkSummaryAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSummaryChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkSummaryAttr :: GA.Attribute -> Maybe (A.Attribute E.Summary)
mkSummaryAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Summary)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkSummaryChild :: Element -> Maybe (E.ChildHTML E.Summary grandparent)
mkSummaryChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkSuperscript :: E.ValidChild E.Superscript parent grandparent
              => [GA.Attribute]
              -> ElementNode
              -> E.ChildHTML parent grandparent
mkSuperscript attrs node =
  E.sup (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkSuperscriptChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkSuperscriptChild :: Element -> Maybe (E.ChildHTML E.Superscript grandparent)
mkSuperscriptChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkTable :: E.ValidChild E.Table parent grandparent
        => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkTable attrs node =
  E.table (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableChild nodes
      Leaf _net -> []
      Void -> []

mkTableChild :: Element -> Maybe (E.ChildHTML E.Table grandparent)
mkTableChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      TableCaption -> Just $ mkTableCaption attrs content
      TableColumnGroup -> Just $ mkTableColumnGroup attrs content
      TableHead -> Just $ mkTableHead attrs content
      TableBody -> Just $ mkTableBody attrs content
      TableRow -> Just $ mkTableRow attrs content
      TableFoot -> Just $ mkTableFoot attrs content
      _element -> Nothing

mkTableBody :: E.ValidChild E.TableBody parent grandparent
            => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkTableBody attrs node =
  E.tbody (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableBodyChild nodes
      Leaf _net -> []
      Void -> []

mkTableBodyChild :: Element -> Maybe (E.ChildHTML E.TableBody grandparent)
mkTableBodyChild e =
  case elementType e of
    TableRow -> Just $ mkTableRow (elementAttrs e) (elementChildren e)
    _element -> Nothing

mkTableDataCell :: E.ValidChild E.TableDataCell parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> E.ChildHTML parent grandparent
mkTableDataCell attrs node =
  E.td (mapMaybe mkTableDataCellAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableDataCellChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkTableDataCellAttr :: GA.Attribute -> Maybe (A.Attribute E.TableDataCell)
mkTableDataCellAttr attr =
  let
    mbAttr =
      case attr of
        GA.Colspan colspan ->
          Just (A.colspan colspan :: A.Attribute E.TableDataCell)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.TableDataCell)

        GA.Headers headers ->
          Just (A.headers headers :: A.Attribute E.TableDataCell)

        GA.Rowspan rowspan ->
          Just (A.rowspan rowspan :: A.Attribute E.TableDataCell)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkTableDataCellChild :: Element
                     -> Maybe (E.ChildHTML E.TableDataCell grandparent)
mkTableDataCellChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Article -> Just $ mkArticle attrs content
      Aside -> Just $ mkAside attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Footer -> Just $ mkFooter attrs content
      Form -> Just $ mkForm attrs content
      H1 -> Just $ mkH1 attrs content
      H2 -> Just $ mkH2 attrs content
      H3 -> Just $ mkH3 attrs content
      H4 -> Just $ mkH4 attrs content
      H5 -> Just $ mkH5 attrs content
      H6 -> Just $ mkH6 attrs content
      Header -> Just $ mkHeader attrs content
      HeadingGroup -> Just $ mkHeadingGroup attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      Nav -> Just $ mkNav attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Section -> Just $ mkSection attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkContentTemplate :: E.ValidChild E.ContentTemplate parent grandparent
                  => [GA.Attribute]
                  -> ElementNode
                  -> E.ChildHTML parent grandparent
mkContentTemplate attrs node =
  E.template (mapMaybe mkContentTemplateAttr attrs) $
    case node of
      Branch _nodes -> [] -- TODO: toBrigid <$> nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkContentTemplateAttr :: GA.Attribute -> Maybe (A.Attribute E.ContentTemplate)
mkContentTemplateAttr attr =
  let
    mbAttr =
      case attr of
        GA.ShadowRootMode shadowrootmode ->
          Just
            (A.shadowrootmode shadowrootmode :: A.Attribute E.ContentTemplate)

        GA.ShadowRootClonable ->
          Just (A.shadowrootclonable :: A.Attribute E.ContentTemplate)

        GA.ShadowRootDelegatesFocus ->
          Just (A.shadowrootdelegatesfocus :: A.Attribute E.ContentTemplate)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkTextArea :: E.ValidChild E.TextArea parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkTextArea attrs node =
  E.textarea (mapMaybe mkTextAreaAttr attrs) $
    case node of
      Branch _nodes -> T.empty
      Leaf net -> NET.toText net
      Void -> T.empty

mkTextAreaAttr :: GA.Attribute -> Maybe (A.Attribute E.TextArea)
mkTextAreaAttr attr =
  let
    mbAttr =
      case attr of
        GA.Autocomplete autocomplete ->
          Just (A.autocomplete autocomplete :: A.Attribute E.TextArea)

        GA.Cols cols ->
          Just (A.cols cols :: A.Attribute E.TextArea)

        GA.Dirname dirname ->
          Just (A.dirname dirname :: A.Attribute E.TextArea)

        GA.Disabled disabled ->
          Just (A.disable disabled :: A.Attribute E.TextArea)

        GA.Form form ->
          Just (A.form form :: A.Attribute E.TextArea)

        GA.MaxLength maxlength ->
          Just (A.maxlength maxlength :: A.Attribute E.TextArea)

        GA.MinLength minlength ->
          Just (A.minlength minlength :: A.Attribute E.TextArea)

        GA.Name name ->
          Just (A.name name :: A.Attribute E.TextArea)

        GA.Placeholder placeholder ->
          Just (A.placeholder placeholder :: A.Attribute E.TextArea)

        GA.ReadOnly ->
          Just (A.readonly :: A.Attribute E.TextArea)

        GA.Required required ->
          Just (A.require required :: A.Attribute E.TextArea)

        GA.Rows rows ->
          Just (A.rows rows :: A.Attribute E.TextArea)

        GA.Wrap wrap ->
          Just (A.wrap wrap :: A.Attribute E.TextArea)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkTableFoot :: E.ValidChild E.TableFoot parent grandparent
            => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkTableFoot attrs node =
  E.tfoot (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableFootChild nodes
      Leaf _net -> []
      Void -> []

mkTableFootChild :: Element -> Maybe (E.ChildHTML E.TableFoot grandparent)
mkTableFootChild e =
  case elementType e of
    TableRow -> Just $ mkTableRow (elementAttrs e) (elementChildren e)
    _element -> Nothing

mkTableHeader :: E.ValidChild E.TableHeader parent grandparent
              => [GA.Attribute]
              -> ElementNode
              -> E.ChildHTML parent grandparent
mkTableHeader attrs node =
  E.th (mapMaybe mkTableHeaderAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableHeaderChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkTableHeaderAttr :: GA.Attribute -> Maybe (A.Attribute E.TableHeader)
mkTableHeaderAttr attr =
  let
    mbAttr =
      case attr of
        GA.Abbreviation abbr ->
          Just (A.abbr abbr :: A.Attribute E.TableHeader)

        GA.Colspan colspan ->
          Just (A.colspan colspan :: A.Attribute E.TableHeader)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.TableHeader)

        GA.Headers headers ->
          Just (A.headers headers :: A.Attribute E.TableHeader)

        GA.Rowspan rowspan ->
          Just (A.rowspan rowspan :: A.Attribute E.TableHeader)

        GA.Scope scope ->
          Just (A.scope scope :: A.Attribute E.TableHeader)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkTableHeaderChild :: Element -> Maybe (E.ChildHTML E.TableHeader grandparent)
mkTableHeaderChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Anchor -> Just $ mkAnchor attrs content
      Abbreviation -> Just $ mkAbbreviation attrs content
      ContactAddress -> Just $ mkContactAddress attrs content
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      Blockquote -> Just $ mkBlockquote attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      DeletedText -> Just $ mkDeletedText attrs content
      Details -> Just $ mkDetails attrs content
      Definition -> Just $ mkDefinition attrs content
      Dialog -> Just $ mkDialog attrs content
      Division -> Just $ mkDivision attrs content
      DescriptionList -> Just $ mkDescriptionList attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      Fieldset -> Just $ mkFieldset attrs content
      Figure -> Just $ mkFigure attrs content
      Form -> Just $ mkForm attrs content
      HorizontalRule -> Just $ mkHorizontalRule attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      InsertedText -> Just $ mkInsertedText attrs content
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Main -> Just $ mkMain attrs content
      Map -> Just $ mkMap attrs content
      Mark -> Just $ mkMark attrs content
      Menu -> Just $ mkMenu attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      OrderedList -> Just $ mkOrderedList attrs content
      Output -> Just $ mkOutput attrs content
      Paragraph -> Just $ mkParagraph attrs content
      Picture -> Just $ mkPicture attrs content
      PreformattedText -> Just $ mkPreformattedText attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Search -> Just $ mkSearch attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      Table -> Just $ mkTable attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      UnorderedList -> Just $ mkUnorderedList attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkTableHead :: E.ValidChild E.TableHead parent grandparent
            => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkTableHead attrs node =
  E.thead (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableHeadChild nodes
      Leaf _net -> []
      Void -> []

mkTableHeadChild :: Element -> Maybe (E.ChildHTML E.TableHead grandparent)
mkTableHeadChild e =
  case elementType e of
    TableRow -> Just $ mkTableRow (elementAttrs e) (elementChildren e)
    _element -> Nothing

mkTime :: E.ValidChild E.Time parent grandparent
       => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkTime attrs node =
  E.time (mapMaybe mkTimeAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTimeChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkTimeAttr :: GA.Attribute -> Maybe (A.Attribute E.Time)
mkTimeAttr attr =
  let
    mbAttr =
      case attr of
        GA.Datetime datetime ->
          Just (A.datetime datetime :: A.Attribute E.Time)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Time)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkTimeChild :: Element -> Maybe (E.ChildHTML E.Time grandparent)
mkTimeChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkTitle :: E.ValidChild E.Title parent grandparent
        => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkTitle attrs node =
  E.title (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch _nodes -> []
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkTableRow :: E.ValidChild E.TableRow parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkTableRow attrs node =
  E.tr (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkTableRowChild nodes
      Leaf _net -> []
      Void -> []

mkTableRowChild :: Element -> Maybe (E.ChildHTML E.TableRow grandparent)
mkTableRowChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Script -> Just $ mkScript attrs content
      TableDataCell -> Just $ mkTableDataCell attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TableHeader -> Just $ mkTableHeader attrs content
      _element -> Nothing

mkTrack :: E.ValidChild E.Track parent grandparent
        => [GA.Attribute] -> E.ChildHTML parent grandparent
mkTrack attrs =
  E.track (mapMaybe mkTrackAttr attrs)

mkTrackAttr :: GA.Attribute -> Maybe (A.Attribute E.Track)
mkTrackAttr attr =
  let
    mbAttr =
      case attr of
        GA.Default ->
          Just (A.default_ :: A.Attribute E.Track)

        GA.Kind kind ->
          Just (A.kind kind :: A.Attribute E.Track)

        GA.Label label ->
          Just (A.label label :: A.Attribute E.Track)

        GA.Src src ->
          Just (A.src src :: A.Attribute E.Track)

        GA.SrcLang srclang ->
          Just (A.srclang srclang :: A.Attribute E.Track)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkUnderline :: E.ValidChild E.Underline parent grandparent
            => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkUnderline attrs node =
  E.u (mapMaybe mkUnderlineAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkUnderlineChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkUnderlineAttr :: GA.Attribute -> Maybe (A.Attribute E.Underline)
mkUnderlineAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Underline)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkUnderlineChild :: Element -> Maybe (E.ChildHTML E.Underline grandparent)
mkUnderlineChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkUnorderedList :: E.ValidChild E.UnorderedList parent grandparent
                => [GA.Attribute]
                -> ElementNode
                -> E.ChildHTML parent grandparent
mkUnorderedList attrs node =
  E.ul (mapMaybe mkGlobalAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkUnorderedListChild nodes
      Leaf _net -> []
      Void -> []

mkUnorderedListChild :: Element
                     -> Maybe (E.ChildHTML E.UnorderedList grandparent)
mkUnorderedListChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      ListItem -> Just $ mkListItem attrs content
      Script -> Just $ mkScript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      _element -> Nothing

mkVariable :: E.ValidChild E.Variable parent grandparent
           => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkVariable attrs node =
  E.var (mapMaybe mkVariableAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkVariableChild nodes
      Leaf net -> [ E.text $ NET.toText net ]
      Void -> []

mkVariableAttr :: GA.Attribute -> Maybe (A.Attribute E.Variable)
mkVariableAttr attr =
  let
    mbAttr =
      case attr of
        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Variable)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkVariableChild :: Element -> Maybe (E.ChildHTML E.Variable grandparent)
mkVariableChild e =
  let
    attrs = elementAttrs e
    content = elementChildren e
  in
    case elementType e of
      Abbreviation -> Just $ mkAbbreviation attrs content
      Area -> Just $ mkArea attrs
      Audio -> Just $ mkAudio attrs content
      BringAttentionTo -> Just $ mkBringAttentionTo attrs content
      BidirectionalIsolation -> Just $ mkBidirectionalIsolation attrs content
      BidirectionalOverride -> Just $ mkBidirectionalOverride attrs content
      LineBreak -> Just $ mkLineBreak attrs
      Button -> Just $ mkButton attrs content
      Canvas -> Just $ mkCanvas attrs content
      Citation -> Just $ mkCitation attrs content
      Code -> Just $ mkCode attrs content
      Data -> Just $ mkData attrs content
      DataList -> Just $ mkDataList attrs content
      Definition -> Just $ mkDefinition attrs content
      Emphasis -> Just $ mkEmphasis attrs content
      Embed -> Just $ mkEmbed attrs
      IdiomaticText -> Just $ mkIdiomaticText attrs content
      IFrame -> Just $ mkIFrame attrs
      Image -> Just $ mkImage attrs
      Input -> Just $ mkInput attrs
      KeyboardInput -> Just $ mkKeyboardInput attrs content
      Label -> Just $ mkLabel attrs content
      Mark -> Just $ mkMark attrs content
      Meter -> Just $ mkMeter attrs content
      NoScript -> Just $ mkNoScript attrs content
      Object -> Just $ mkObject attrs content
      Output -> Just $ mkOutput attrs content
      Picture -> Just $ mkPicture attrs content
      Progress -> Just $ mkProgress attrs content
      Quotation -> Just $ mkQuotation attrs content
      Ruby -> Just $ mkRuby attrs content
      Strikethrough -> Just $ mkStrikethrough attrs content
      Sample -> Just $ mkSample attrs content
      Script -> Just $ mkScript attrs content
      Select -> Just $ mkSelect attrs content
      Slot -> Just $ mkSlot attrs content
      SideComment -> Just $ mkSideComment attrs content
      Span -> Just $ mkSpan attrs content
      Strong -> Just $ mkStrong attrs content
      Subscript -> Just $ mkSubscript attrs content
      Superscript -> Just $ mkSuperscript attrs content
      ContentTemplate -> Just $ mkContentTemplate attrs content
      TextArea -> Just $ mkTextArea attrs content
      Time -> Just $ mkTime attrs content
      Underline -> Just $ mkUnderline attrs content
      Variable -> Just $ mkVariable attrs content
      Video -> Just $ mkVideo attrs content
      WordBreakOpportunity -> Just $ mkWordBreakOpportunity attrs
      _element -> Nothing

mkVideo :: E.ValidChild E.Video parent grandparent
        => [GA.Attribute] -> ElementNode -> E.ChildHTML parent grandparent
mkVideo attrs node =
  E.video (mapMaybe mkVideoAttr attrs) $
    case node of
      Branch nodes -> mapMaybe mkVideoChild nodes
      Leaf _net -> []
      Void -> []

mkVideoAttr :: GA.Attribute -> Maybe (A.Attribute E.Video)
mkVideoAttr attr =
  let
    mbAttr =
      case attr of
        GA.Autoplay ->
          Just (A.autoplay :: A.Attribute E.Video)

        GA.Controls ->
          Just (A.controls :: A.Attribute E.Video)

        GA.ControlsList controlslist ->
          Just (A.controlslist controlslist :: A.Attribute E.Video)

        GA.CrossOrigin crossorigin ->
          Just (A.crossorigin crossorigin :: A.Attribute E.Video)

        GA.DisablePictureInPicture ->
          Just (A.disablepictureinpicture :: A.Attribute E.Video)

        GA.DisableRemotePlayback ->
          Just (A.disableremoteplayback :: A.Attribute E.Video)

        GA.ElementTiming elementtiming ->
          Just (A.elementtiming elementtiming :: A.Attribute E.Video)

        GA.Height height ->
          Just (A.height height :: A.Attribute E.Video)

        GA.Loop ->
          Just (A.loop :: A.Attribute E.Video)

        GA.Muted muted ->
          Just (A.mute muted :: A.Attribute E.Video)

        GA.PlaysInline playsinline ->
          Just (A.playInline playsinline :: A.Attribute E.Video)

        GA.Poster poster ->
          Just (A.poster poster :: A.Attribute E.Video)

        GA.Preload preload ->
          Just (A.preload preload :: A.Attribute E.Video)

        GA.Src src ->
          Just (A.src src :: A.Attribute E.Video)

        GA.Width width ->
          Just (A.width width :: A.Attribute E.Video)

        _attr ->
          Nothing
  in
    mbAttr <|> mkGlobalAttr attr

mkVideoChild :: Element -> Maybe (E.ChildHTML E.Video grandparent)
mkVideoChild e =
  case elementType e of
    Source -> Just $ mkSource (elementAttrs e)
    Track -> Just $ mkTrack (elementAttrs e)
    _element -> Nothing

mkWordBreakOpportunity :: E.ValidChild E.WordBreakOpportunity parent grandparent
                       => [GA.Attribute] -> E.ChildHTML parent grandparent
mkWordBreakOpportunity attrs =
  E.wbr (mapMaybe mkGlobalAttr attrs)

mkGlobalAttr :: GA.Attribute -> Maybe (A.Attribute tag)
mkGlobalAttr attr =
  case attr of
    GA.AccessKey accesskey ->
      Just $ A.accesskey accesskey

    GA.Autocapitalize autocapitalize ->
      Just $ A.autocapitalize autocapitalize

    GA.Autofocus autofocus ->
      Just $ A.autofocus autofocus

    GA.Class class_ ->
      Just $ A.class_ class_

    GA.ContentEditable contenteditable ->
      Just $ A.contenteditable contenteditable

    GA.Dir dir ->
      Just $ A.dir dir

    GA.Draggable draggable ->
      Just $ A.draggable draggable

    GA.EnterKeyHint enterkeyhint ->
      Just $ A.enterkeyhint enterkeyhint

    GA.ExportParts exportparts ->
      Just $ A.exportparts exportparts

    GA.Hidden hidden ->
      Just $ A.hide hidden

    GA.Id id_ ->
      Just $ A.id id_

    GA.Inert inert ->
      Just $ A.inert inert

    GA.InputMode inputmode ->
      Just $ A.inputmode inputmode

    GA.Is is ->
      Just $ A.is is

    GA.ItemId itemid ->
      Just $ A.itemid itemid

    GA.ItemProp itemprop ->
      Just $ A.itemprop itemprop

    GA.ItemRef itemref ->
      Just $ A.itemref itemref

    GA.ItemScope ->
      Just $ A.itemscope

    GA.ItemType itemtype ->
      Just $ A.itemtype itemtype

    GA.Lang lang ->
      Just $ A.lang lang

    GA.Nonce nonce ->
      Just $ A.nonce nonce

    GA.Part part ->
      Just $ A.part part

    GA.Popover popover ->
      Just $ A.popover popover

    GA.Role role ->
      Just $ A.role role

    GA.Slot slot ->
      Just $ A.slot slot

    GA.Spellcheck spellcheck ->
      Just $ A.spellcheck spellcheck

    GA.Style style ->
      Just $ A.style style

    GA.TabIndex tabindex ->
      Just $ A.tabindex tabindex

    GA.Title title ->
      Just $ A.title title

    GA.Translate translate ->
      Just $ A.translate translate

    GA.WritingSuggestions writingsuggestions ->
      Just $ A.writingsuggestions writingsuggestions

    _attr ->
      Nothing
