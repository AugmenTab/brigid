{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generation.Analysis
  ( totalNodes
  ) where

import Prelude hiding (div, head, map, span)

import Generation.Element (Element (..))

newtype Count =
  Count
    { unCount :: Int
    } deriving (Eq, Num, Ord)

instance Semigroup Count where
  (<>) = (+)

instance Monoid Count where
  mempty = 0

totalNodes :: Element -> Int
totalNodes =
  unCount . countNodes

countNodes :: Element -> Count
countNodes element =
  case element of
    Text _text -> Count 1
    Comment _comment -> Count 1
    Anchor _attrs a -> Count 1 + foldMap countNodes a
    Abbreviation _attrs abbr -> Count 1 + foldMap countNodes abbr
    ContactAddress _attrs address -> Count 1 + foldMap countNodes address
    Area _attrs -> Count 1
    Article _attrs article -> Count 1 + foldMap countNodes article
    Aside _attrs aside -> Count 1 + foldMap countNodes aside
    Audio _attrs audio -> Count 1 + foldMap countNodes audio
    BringAttentionTo _attrs b -> Count 1 + foldMap countNodes b
    Base _attrs -> Count 1
    BidirectionalIsolation _attrs bdi -> Count 1 + foldMap countNodes bdi
    BidirectionalOverride _attrs bdo -> Count 1 + foldMap countNodes bdo
    Blockquote _attrs blockquote -> Count 1 + foldMap countNodes blockquote
    Body _attrs body -> Count 1 + foldMap countNodes body
    LineBreak _attrs -> Count 1
    Button _attrs button -> Count 1 + foldMap countNodes button
    Canvas _attrs canvas -> Count 1 + foldMap countNodes canvas
    TableCaption _attrs caption -> Count 1 + foldMap countNodes caption
    Citation _attrs cite -> Count 1 + foldMap countNodes cite
    Code _attrs code -> Count 1 + foldMap countNodes code
    TableColumn _attrs -> Count 1
    TableColumnGroup _attrs colgroup -> Count 1 + foldMap countNodes colgroup
    Data _attrs data_ -> Count 1 + foldMap countNodes data_
    DataList _attrs datalist -> Count 1 + foldMap countNodes datalist
    DescriptionDetails _attrs dd -> Count 1 + foldMap countNodes dd
    DeletedText _attrs del -> Count 1 + foldMap countNodes del
    Details _attrs details -> Count 1 + foldMap countNodes details
    Definition _attrs dfn -> Count 1 + foldMap countNodes dfn
    Dialog _attrs dialog -> Count 1 + foldMap countNodes dialog
    Division _attrs div -> Count 1 + foldMap countNodes div
    DescriptionList _attrs dl -> Count 1 + foldMap countNodes dl
    DescriptionTerm _attrs dt -> Count 1 + foldMap countNodes dt
    Emphasis _attrs em -> Count 1 + foldMap countNodes em
    Embed _attrs -> Count 1
    Fieldset _attrs fieldset -> Count 1 + foldMap countNodes fieldset
    FigureCaption _attrs figcaption -> Count 1 + foldMap countNodes figcaption
    Figure _attrs figure -> Count 1 + foldMap countNodes figure
    Footer _attrs footer -> Count 1 + foldMap countNodes footer
    Form _attrs form -> Count 1 + foldMap countNodes form
    H1 _attrs h1 -> Count 1 + foldMap countNodes h1
    H2 _attrs h2 -> Count 1 + foldMap countNodes h2
    H3 _attrs h3 -> Count 1 + foldMap countNodes h3
    H4 _attrs h4 -> Count 1 + foldMap countNodes h4
    H5 _attrs h5 -> Count 1 + foldMap countNodes h5
    H6 _attrs h6 -> Count 1 + foldMap countNodes h6
    Head _attrs head -> Count 1 + foldMap countNodes head
    Header _attrs header -> Count 1 + foldMap countNodes header
    HeadingGroup _attrs hgroup -> Count 1 + foldMap countNodes hgroup
    HorizontalRule _attrs -> Count 1
    Html _attrs html -> foldMap countNodes html
    IdiomaticText _attrs i -> Count 1 + foldMap countNodes i
    IFrame _attrs -> Count 1
    Image _attrs -> Count 1
    Input _attrs -> Count 1
    InsertedText _attrs ins -> Count 1 + foldMap countNodes ins
    KeyboardInput _attrs kbd -> Count 1 + foldMap countNodes kbd
    Label _attrs label -> Count 1 + foldMap countNodes label
    Legend _attrs legend -> Count 1 + foldMap countNodes legend
    ListItem _attrs li -> Count 1 + foldMap countNodes li
    Link _attrs -> Count 1
    Main _attrs main -> Count 1 + foldMap countNodes main
    Map _attrs map -> Count 1 + foldMap countNodes map
    Mark _attrs mark -> Count 1 + foldMap countNodes mark
    Menu _attrs menu -> Count 1 + foldMap countNodes menu
    Meta _attrs -> Count 1
    Meter _attrs meter -> Count 1 + foldMap countNodes meter
    Nav _attrs nav -> Count 1 + foldMap countNodes nav
    NoScript _attrs noscript -> Count 1 + foldMap countNodes noscript
    Object _attrs object -> Count 1 + foldMap countNodes object
    OrderedList _attrs ol -> Count 1 + foldMap countNodes ol
    OptionGroup _attrs optgroup -> Count 1 + foldMap countNodes optgroup
    Option _attrs _option -> Count 1
    Output _attrs output -> Count 1 + foldMap countNodes output
    Paragraph _attrs p -> Count 1 + foldMap countNodes p
    Picture _attrs picture -> Count 1 + foldMap countNodes picture
    PreformattedText _attrs pre -> Count 1 + foldMap countNodes pre
    Progress _attrs progress -> Count 1 + foldMap countNodes progress
    Quotation _attrs q -> Count 1 + foldMap countNodes q
    RubyParenthesis _attrs _rp -> Count 1
    RubyText _attrs rt -> Count 1 + foldMap countNodes rt
    Ruby _attrs ruby -> Count 1 + foldMap countNodes ruby
    Strikethrough _attrs s -> Count 1 + foldMap countNodes s
    Sample _attrs sample -> Count 1 + foldMap countNodes sample
    Script _attrs _script -> Count 1
    Search _attrs search -> Count 1 + foldMap countNodes search
    Section _attrs section -> Count 1 + foldMap countNodes section
    Select _attrs select -> Count 1 + foldMap countNodes select
    Slot _attrs slot -> Count 1 + foldMap countNodes slot
    SideComment _attrs small -> Count 1 + foldMap countNodes small
    Source _attrs -> Count 1
    Span _attrs span -> Count 1 + foldMap countNodes span
    Strong _attrs strong -> Count 1 + foldMap countNodes strong
    Style _attrs -> Count 1
    Subscript _attrs sub -> Count 1 + foldMap countNodes sub
    Summary _attrs summary -> Count 1 + foldMap countNodes summary
    Superscript _attrs sup -> Count 1 + foldMap countNodes sup
    Table _attrs table -> Count 1 + foldMap countNodes table
    TableBody _attrs tbody -> Count 1 + foldMap countNodes tbody
    TableDataCell _attrs td -> Count 1 + foldMap countNodes td
    ContentTemplate _attrs template -> Count 1 + foldMap countNodes template
    TextArea _attrs textarea -> Count 1 + foldMap countNodes textarea
    TableFoot _attrs tfoot -> Count 1 + foldMap countNodes tfoot
    TableHeader _attrs th -> Count 1 + foldMap countNodes th
    TableHead _attrs thead -> Count 1 + foldMap countNodes thead
    Time _attrs time -> Count 1 + foldMap countNodes time
    Title _attrs _title -> Count 1
    TableRow _attrs tr -> Count 1 + foldMap countNodes tr
    Track _attrs -> Count 1
    Underline _attrs u -> Count 1 + foldMap countNodes u
    UnorderedList _attrs ul -> Count 1 + foldMap countNodes ul
    Variable _attrs var -> Count 1 + foldMap countNodes var
    Video _attrs video -> Count 1 + foldMap countNodes video
    WordBreakOpportunity _attrs -> Count 1
