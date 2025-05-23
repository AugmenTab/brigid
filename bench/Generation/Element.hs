module Generation.Element
  ( Element (..)
  , comment
  , text
  , a
  , abbr
  , address
  , area
  , article
  , aside
  , audio
  , b
  , base
  , bdi
  , bdo
  , blockquote
  , body
  , br
  , button
  , canvas
  , caption
  , cite
  , code
  , col
  , colgroup
  , data_
  , datalist
  , dd
  , del
  , details
  , dfn
  , dialog
  , div
  , dl
  , dt
  , em
  , embed
  , fieldset
  , figcaption
  , figure
  , footer
  , form
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , head
  , header
  , hgroup
  , hr
  , html
  , i
  , iframe
  , img
  , input
  , ins
  , kbd
  , label
  , legend
  , li
  , link
  , main
  , map
  , mark
  , menu
  , meta
  , meter
  , nav
  , noscript
  , object
  , ol
  , optgroup
  , option
  , output
  , p
  , picture
  , pre
  , progress
  , q
  , ruby
  , s
  , samp
  , script
  , search
  , section
  , select
  , slot
  , small
  , source
  , span
  , strong
  , style
  , sub
  , summary
  , sup
  , table
  , tbody
  , td
  , template
  , textarea
  , tfoot
  , th
  , thead
  , time
  , title
  , tr
  , track
  , u
  , ul
  , var
  , video
  , wbr
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude hiding (div, head, map, span)

import Generation.Attribute qualified as A
import Generation.Generators qualified as Generators

data Element
  = Comment T.Text
  | Text T.Text
  | Anchor [A.Attribute] [Element]
  | Abbreviation [A.Attribute] [Element]
  | ContactAddress [A.Attribute] [Element]
  | Area [A.Attribute]
  | Article [A.Attribute] [Element]
  | Aside [A.Attribute] [Element]
  | Audio [A.Attribute] [Element]
  | BringAttentionTo [A.Attribute] [Element]
  | Base [A.Attribute]
  | BidirectionalIsolation [A.Attribute] [Element]
  | BidirectionalOverride [A.Attribute] [Element]
  | Blockquote [A.Attribute] [Element]
  | Body [A.Attribute] [Element]
  | LineBreak [A.Attribute]
  | Button [A.Attribute] [Element]
  | Canvas [A.Attribute] [Element]
  | TableCaption [A.Attribute] [Element]
  | Citation [A.Attribute] [Element]
  | Code [A.Attribute] [Element]
  | TableColumn [A.Attribute]
  | TableColumnGroup [A.Attribute] [Element]
  | Data [A.Attribute] [Element]
  | DataList [A.Attribute] [Element]
  | DescriptionDetails [A.Attribute] [Element]
  | DeletedText [A.Attribute] [Element]
  | Details [A.Attribute] [Element]
  | Definition [A.Attribute] [Element]
  | Dialog [A.Attribute] [Element]
  | Division [A.Attribute] [Element]
  | DescriptionList [A.Attribute] [Element]
  | DescriptionTerm [A.Attribute] [Element]
  | Emphasis [A.Attribute] [Element]
  | Embed [A.Attribute]
  | Fieldset [A.Attribute] [Element]
  | FigureCaption [A.Attribute] [Element]
  | Figure [A.Attribute] [Element]
  | Footer [A.Attribute] [Element]
  | Form [A.Attribute] [Element]
  | H1 [A.Attribute] [Element]
  | H2 [A.Attribute] [Element]
  | H3 [A.Attribute] [Element]
  | H4 [A.Attribute] [Element]
  | H5 [A.Attribute] [Element]
  | H6 [A.Attribute] [Element]
  | Head [A.Attribute] [Element]
  | Header [A.Attribute] [Element]
  | HeadingGroup [A.Attribute] [Element]
  | HorizontalRule [A.Attribute]
  | Html [A.Attribute] [Element]
  | IdiomaticText [A.Attribute] [Element]
  | IFrame [A.Attribute]
  | Image [A.Attribute]
  | Input [A.Attribute]
  | InsertedText [A.Attribute] [Element]
  | KeyboardInput [A.Attribute] [Element]
  | Label [A.Attribute] [Element]
  | Legend [A.Attribute] [Element]
  | ListItem [A.Attribute] [Element]
  | Link [A.Attribute]
  | Main [A.Attribute] [Element]
  | Map [A.Attribute] [Element]
  | Mark [A.Attribute] [Element]
  | Menu [A.Attribute] [Element]
  | Meta [A.Attribute]
  | Meter [A.Attribute] [Element]
  | Nav [A.Attribute] [Element]
  | NoScript [A.Attribute] [Element]
  | Object [A.Attribute] [Element]
  | OrderedList [A.Attribute] [Element]
  | OptionGroup [A.Attribute] [Element]
  | Option [A.Attribute] T.Text
  | Output [A.Attribute] [Element]
  | Paragraph [A.Attribute] [Element]
  | Picture [A.Attribute] [Element]
  | PreformattedText [A.Attribute] [Element]
  | Progress [A.Attribute] [Element]
  | Quotation [A.Attribute] [Element]
  | RubyParenthesis [A.Attribute] T.Text
  | RubyText [A.Attribute] [Element]
  | Ruby [A.Attribute] [Element]
  | Strikethrough [A.Attribute] [Element]
  | Sample [A.Attribute] [Element]
  | Script [A.Attribute] NET.NonEmptyText
  | Search [A.Attribute] [Element]
  | Section [A.Attribute] [Element]
  | Select [A.Attribute] [Element]
  | Slot [A.Attribute] [Element]
  | SideComment [A.Attribute] [Element]
  | Source [A.Attribute]
  | Span [A.Attribute] [Element]
  | Strong [A.Attribute] [Element]
  | Style [A.Attribute]
  | Subscript [A.Attribute] [Element]
  | Summary [A.Attribute] [Element]
  | Superscript [A.Attribute] [Element]
  | Table [A.Attribute] [Element]
  | TableBody [A.Attribute] [Element]
  | TableDataCell [A.Attribute] [Element]
  | ContentTemplate [A.Attribute] [Element]
  | TextArea [A.Attribute] [Element]
  | TableFoot [A.Attribute] [Element]
  | TableHeader [A.Attribute] [Element]
  | TableHead [A.Attribute] [Element]
  | Time [A.Attribute] [Element]
  | Title [A.Attribute] T.Text
  | TableRow [A.Attribute] [Element]
  | Track [A.Attribute]
  | Underline [A.Attribute] [Element]
  | UnorderedList [A.Attribute] [Element]
  | Variable [A.Attribute] [Element]
  | Video [A.Attribute] [Element]
  | WordBreakOpportunity [A.Attribute]
  deriving Show

comment :: Gen Element
comment =
  Comment <$> Generators.text

text :: Gen Element
text =
  Text <$> Generators.text

a :: Int -> Int -> Gen Element
a maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    aChildren = NEL.toList anchorContent

  Anchor
    <$> withGlobalAttrs attrs anchorAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice aChildren)

anchorAttrs :: [Gen A.Attribute]
anchorAttrs =
  [ A.href
  , A.target
  , A.download
  , A.ping
  , A.rel
  , A.hreflang
  , A.type_
  , A.referrerpolicy
  ]

anchorContent :: NonEmpty (Gen Element)
anchorContent = NEL.singleton text

abbr :: Int -> Int -> Gen Element
abbr maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    abbrChildren = NEL.toList $ abbreviationContent maxAttrs maxChildren

  Abbreviation
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice abbrChildren)

abbreviationContent :: Int -> Int -> NonEmpty (Gen Element)
abbreviationContent = phrasingContent

address :: Int -> Int -> Gen Element
address maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    contactAddressChildren =
      NEL.toList $ contactAddressContent maxAttrs maxChildren

  ContactAddress
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice contactAddressChildren)

contactAddressContent :: Int -> Int -> NonEmpty (Gen Element)
contactAddressContent maxAttrs maxChildren =
  text
    :| [ comment
       , a maxAttrs maxChildren
       , abbr maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , del maxAttrs maxChildren
       , details maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , div maxAttrs maxChildren
       , dl maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , form maxAttrs maxChildren
       , hr maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , ins maxAttrs maxChildren
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , ol maxAttrs maxChildren
       , output maxAttrs maxChildren
       , p maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , pre maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , table maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , ul maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

area :: Int -> Gen Element
area maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Area <$> withGlobalAttrs attrs areaAttrs

areaAttrs :: [Gen A.Attribute]
areaAttrs =
  [ A.alt
  , A.coords
  , A.shape
  , A.href
  , A.target
  , A.download
  , A.ping
  , A.rel
  , A.referrerpolicy
  ]

article :: Int -> Int -> Gen Element
article maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    articleChildren = NEL.toList $ articleContent maxAttrs maxChildren

  Article
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice articleChildren)

articleContent :: Int -> Int -> NonEmpty (Gen Element)
articleContent = flowContent

aside :: Int -> Int -> Gen Element
aside maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    asideChildren = NEL.toList $ asideContent maxAttrs maxChildren

  Aside
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice asideChildren)

asideContent :: Int -> Int -> NonEmpty (Gen Element)
asideContent = flowContent

audio :: Int -> Int -> Gen Element
audio maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    audioChildren = NEL.toList $ audioContent maxAttrs

  Audio
    <$> withGlobalAttrs attrs audioAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice audioChildren)

audioAttrs :: [Gen A.Attribute]
audioAttrs =
  [ A.src
  , A.preload
  , A.autoplay
  , A.loop
  , A.muted
  , A.controls
  , A.crossorigin
  , A.controlslist
  ]

audioContent :: Int -> NonEmpty (Gen Element)
audioContent = audioVideoContent

b :: Int -> Int -> Gen Element
b maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    bChildren = NEL.toList $ bringAttentionToContent maxAttrs maxChildren

  BringAttentionTo
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice bChildren)

bringAttentionToContent :: Int -> Int -> NonEmpty (Gen Element)
bringAttentionToContent = phrasingContent

base :: Int -> Gen Element
base maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Base <$> withGlobalAttrs attrs baseAttrs

baseAttrs :: [Gen A.Attribute]
baseAttrs =
  [ A.href
  , A.target
  ]

bdi :: Int -> Int -> Gen Element
bdi maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    bdiChildren = NEL.toList $ bidirectionalIsolationContent maxAttrs maxChildren

  BidirectionalIsolation
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice bdiChildren)

bidirectionalIsolationContent :: Int -> Int -> NonEmpty (Gen Element)
bidirectionalIsolationContent = phrasingContent

bdo :: Int -> Int -> Gen Element
bdo maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    bdoChildren = NEL.toList $ bidirectionalOverrideContent maxAttrs maxChildren

  BidirectionalOverride
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice bdoChildren)

bidirectionalOverrideContent :: Int -> Int -> NonEmpty (Gen Element)
bidirectionalOverrideContent = phrasingContent

blockquote :: Int -> Int -> Gen Element
blockquote maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    blockquoteChildren = NEL.toList $ blockquoteContent maxAttrs maxChildren

  Blockquote
    <$> withGlobalAttrs attrs [A.cite]
    <*> Gen.list (Range.singleton children) (Gen.choice blockquoteChildren)

blockquoteContent :: Int -> Int -> NonEmpty (Gen Element)
blockquoteContent = flowContent

body :: Int -> Int -> Gen Element
body maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    bodyChildren = NEL.toList $ bodyContent maxAttrs maxChildren

  Body
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice bodyChildren)

bodyContent :: Int -> Int -> NonEmpty (Gen Element)
bodyContent = flowContent

br :: Int -> Gen Element
br maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  LineBreak <$> withGlobalAttrs attrs []

button :: Int -> Int -> Gen Element
button maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    buttonChildren = NEL.toList $ buttonContent maxAttrs maxChildren

  Button
    <$> withGlobalAttrs attrs buttonAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice buttonChildren)

buttonAttrs :: [Gen A.Attribute]
buttonAttrs =
  [ A.autofocus
  , A.disabled
  , A.form
  , A.formaction
  , A.formenctype
  , A.formmethod
  , A.formnovalidate
  , A.formtarget
  , A.name
  , A.type_
  , A.value
  , A.popovertarget
  , A.popovertargetaction
  ]

buttonContent :: Int -> Int -> NonEmpty (Gen Element)
buttonContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbr maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , br maxAttrs
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , em maxAttrs maxChildren
       , i maxAttrs maxChildren
       , img maxAttrs
       , kbd maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , script maxAttrs
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , template maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

canvas :: Int -> Int -> Gen Element
canvas maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    canvasChildren = NEL.toList canvasContent

  Canvas
    <$> withGlobalAttrs attrs canvasAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice canvasChildren)

canvasAttrs :: [Gen A.Attribute]
canvasAttrs =
  [ A.height
  , A.width
  ]

canvasContent :: NonEmpty (Gen Element)
canvasContent = NEL.singleton text

caption :: Int -> Int -> Gen Element
caption maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tableCaptionChildren = NEL.toList $ tableCaptionContent maxAttrs maxChildren

  TableCaption
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice tableCaptionChildren)

tableCaptionContent :: Int -> Int -> NonEmpty (Gen Element)
tableCaptionContent = flowContent

cite :: Int -> Int -> Gen Element
cite maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    citeChildren = NEL.toList $ citationContent maxAttrs maxChildren

  Citation
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice citeChildren)

citationContent :: Int -> Int -> NonEmpty (Gen Element)
citationContent = phrasingContent

code :: Int -> Int -> Gen Element
code maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    codeChildren = NEL.toList $ codeContent maxAttrs maxChildren

  Code
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice codeChildren)

codeContent :: Int -> Int -> NonEmpty (Gen Element)
codeContent = phrasingContent

col :: Int -> Gen Element
col maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  TableColumn <$> withGlobalAttrs attrs [A.span]

colgroup :: Int -> Int -> Gen Element
colgroup maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    colgroupChildren = NEL.toList $ tableColumnGroupContent maxAttrs

  TableColumnGroup
    <$> withGlobalAttrs attrs [A.span]
    <*> Gen.list (Range.singleton children) (Gen.choice colgroupChildren)

tableColumnGroupContent :: Int -> NonEmpty (Gen Element)
tableColumnGroupContent maxAttrs =
  comment :| [ col maxAttrs ]

data_ :: Int -> Int -> Gen Element
data_ maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dataChildren = NEL.toList $ dataContent maxAttrs maxChildren

  Data
    <$> withGlobalAttrs attrs [A.data_]
    <*> Gen.list (Range.singleton children) (Gen.choice dataChildren)

dataContent :: Int -> Int -> NonEmpty (Gen Element)
dataContent = phrasingContent

datalist :: Int -> Int -> Gen Element
datalist maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    datalistChildren = NEL.toList $ dataListContent maxAttrs maxChildren

  DataList
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice datalistChildren)

dataListContent :: Int -> Int -> NonEmpty (Gen Element)
dataListContent maxAttrs maxChildren =
  NEL.cons
    (option maxAttrs)
    (phrasingContent maxAttrs maxChildren)

dd :: Int -> Int -> Gen Element
dd maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    ddChildren = NEL.toList $ descriptionDetailsContent maxAttrs maxChildren

  DescriptionDetails
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice ddChildren)

descriptionDetailsContent :: Int -> Int -> NonEmpty (Gen Element)
descriptionDetailsContent = flowContent

del :: Int -> Int -> Gen Element
del maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    delChildren = NEL.toList deletedTextContent

  DeletedText
    <$> withGlobalAttrs attrs deletedTextAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice delChildren)

deletedTextAttrs :: [Gen A.Attribute]
deletedTextAttrs =
  [ A.cite
  , A.datetime
  ]

deletedTextContent :: NonEmpty (Gen Element)
deletedTextContent = NEL.singleton text

details :: Int -> Int -> Gen Element
details maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    detailsChildren = NEL.toList $ detailsContent maxAttrs maxChildren

  Details
    <$> withGlobalAttrs attrs [A.open]
    <*> Gen.list (Range.singleton children) (Gen.choice detailsChildren)

detailsContent :: Int -> Int -> NonEmpty (Gen Element)
detailsContent maxAttrs maxChildren =
  NEL.cons
    (summary maxAttrs maxChildren)
    (flowContent maxAttrs maxChildren)

dfn :: Int -> Int -> Gen Element
dfn maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dfnChildren = NEL.toList $ definitionContent maxAttrs maxChildren

  Definition
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice dfnChildren)

definitionContent :: Int -> Int -> NonEmpty (Gen Element)
definitionContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbr maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

dialog :: Int -> Int -> Gen Element
dialog maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dialogChildren = NEL.toList $ dialogContent maxAttrs maxChildren

  Dialog
    <$> withGlobalAttrs attrs [A.open]
    <*> Gen.list (Range.singleton children) (Gen.choice dialogChildren)

dialogContent :: Int -> Int -> NonEmpty (Gen Element)
dialogContent = flowContent

div :: Int -> Int -> Gen Element
div maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    divChildren = NEL.toList $ divisionContent maxAttrs maxChildren

  Division
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice divChildren)

divisionContent :: Int -> Int -> NonEmpty (Gen Element)
divisionContent = flowContent

dl :: Int -> Int -> Gen Element
dl maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dlChildren = NEL.toList $ descriptionListContent maxAttrs maxChildren

  DescriptionList
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice dlChildren)

descriptionListContent :: Int -> Int -> NonEmpty (Gen Element)
descriptionListContent maxAttrs maxChildren =
  NEL.cons (dt maxAttrs maxChildren)
    . NEL.cons (dd maxAttrs maxChildren)
    . NEL.cons (div maxAttrs maxChildren)
    $ scriptSupportingContent maxAttrs maxChildren

dt :: Int -> Int -> Gen Element
dt maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dtChildren =
      NEL.toList $ descriptionTermContent maxAttrs maxChildren

  DescriptionTerm
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice dtChildren)

descriptionTermContent :: Int -> Int -> NonEmpty (Gen Element)
descriptionTermContent maxAttrs maxChildren =
  text
    :| [ comment
       , a maxAttrs maxChildren
       , abbr maxAttrs maxChildren
       , address maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , del maxAttrs maxChildren
       , details maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , div maxAttrs maxChildren
       , dl maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , form maxAttrs maxChildren
       , hr maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , ins maxAttrs maxChildren
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , ol maxAttrs maxChildren
       , output maxAttrs maxChildren
       , p maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , pre maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , table maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , ul maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

em :: Int -> Int -> Gen Element
em maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    emChildren = NEL.toList $ emphasisContent maxAttrs maxChildren

  Emphasis
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice emChildren)

emphasisContent :: Int -> Int -> NonEmpty (Gen Element)
emphasisContent = phrasingContent

embed :: Int -> Gen Element
embed maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Embed <$> withGlobalAttrs attrs embedAttrs

embedAttrs :: [Gen A.Attribute]
embedAttrs =
  [ A.src
  , A.type_
  , A.width
  , A.height
  ]

fieldset :: Int -> Int -> Gen Element
fieldset maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    fieldsetChildren = NEL.toList $ fieldsetContent maxAttrs maxChildren

  Fieldset
    <$> withGlobalAttrs attrs fieldsetAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice fieldsetChildren)

fieldsetAttrs :: [Gen A.Attribute]
fieldsetAttrs =
  [ A.disabled
  , A.form
  , A.name
  ]

fieldsetContent :: Int -> Int -> NonEmpty (Gen Element)
fieldsetContent maxAttrs maxChildren =
  NEL.cons
    (legend maxAttrs maxChildren)
    (flowContent maxAttrs maxChildren)

figcaption :: Int -> Int -> Gen Element
figcaption maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    figcaptionChildren = NEL.toList $ figureCaptionContent maxAttrs maxChildren

  FigureCaption
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice figcaptionChildren)

figureCaptionContent :: Int -> Int -> NonEmpty (Gen Element)
figureCaptionContent = flowContent

figure :: Int -> Int -> Gen Element
figure maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    figureChildren = NEL.toList $ figureContent maxAttrs maxChildren

  Figure
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice figureChildren)

figureContent :: Int -> Int -> NonEmpty (Gen Element)
figureContent maxAttrs maxChildren =
  NEL.cons
    (figcaption maxAttrs maxChildren)
    (flowContent maxAttrs maxChildren)

footer :: Int -> Int -> Gen Element
footer maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    footerChildren = NEL.toList $ footerContent maxAttrs maxChildren

  Footer
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice footerChildren)

footerContent :: Int -> Int -> NonEmpty (Gen Element)
footerContent = marginalContent

form :: Int -> Int -> Gen Element
form maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    formChildren = NEL.toList $ formContent maxAttrs maxChildren

  Form
    <$> withGlobalAttrs attrs formAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice formChildren)

formAttrs :: [Gen A.Attribute]
formAttrs =
  [ A.acceptcharset
  , A.autocomplete
  , A.name
  , A.rel
  , A.action
  , A.enctype
  , A.method
  , A.novalidate
  , A.target
  ]

formContent :: Int -> Int -> NonEmpty (Gen Element)
formContent maxAttrs maxChildren =
  text
    :| [ comment
       , a maxAttrs maxChildren
       , abbr maxAttrs maxChildren
       , address maxAttrs maxChildren
       , article maxAttrs maxChildren
       , aside maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , del maxAttrs maxChildren
       , details maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , div maxAttrs maxChildren
       , dl maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , footer maxAttrs maxChildren
       , h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       , header maxAttrs maxChildren
       , hgroup maxAttrs maxChildren
       , hr maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , ins maxAttrs maxChildren
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , nav maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , ol maxAttrs maxChildren
       , output maxAttrs maxChildren
       , p maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , pre maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , section maxAttrs maxChildren
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , table maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , ul maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

h1 :: Int -> Int -> Gen Element
h1 maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h1Children = NEL.toList $ h1Content maxAttrs maxChildren

  H1
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice h1Children)

h1Content :: Int -> Int -> NonEmpty (Gen Element)
h1Content = phrasingContent

h2 :: Int -> Int -> Gen Element
h2 maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h2Children = NEL.toList $ h2Content maxAttrs maxChildren

  H2
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice h2Children)

h2Content :: Int -> Int -> NonEmpty (Gen Element)
h2Content = phrasingContent

h3 :: Int -> Int -> Gen Element
h3 maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h3Children = NEL.toList $ h3Content maxAttrs maxChildren

  H3
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice h3Children)

h3Content :: Int -> Int -> NonEmpty (Gen Element)
h3Content = phrasingContent

h4 :: Int -> Int -> Gen Element
h4 maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h4Children = NEL.toList $ h4Content maxAttrs maxChildren

  H4
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice h4Children)

h4Content :: Int -> Int -> NonEmpty (Gen Element)
h4Content = phrasingContent

h5 :: Int -> Int -> Gen Element
h5 maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h5Children = NEL.toList $ h5Content maxAttrs maxChildren

  H5
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice h5Children)

h5Content :: Int -> Int -> NonEmpty (Gen Element)
h5Content = phrasingContent

h6 :: Int -> Int -> Gen Element
h6 maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h6Children = NEL.toList $ h6Content maxAttrs maxChildren

  H6
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice h6Children)

h6Content :: Int -> Int -> NonEmpty (Gen Element)
h6Content = phrasingContent

head :: Int -> Int -> Gen Element
head maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    headChildren = NEL.toList $ headContent maxAttrs maxChildren

  Head
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice headChildren)

headContent :: Int -> Int -> NonEmpty (Gen Element)
headContent maxAttrs maxChildren =
  comment
    :| [ base maxAttrs
       , link maxAttrs
       , meta maxAttrs
       , noscript maxAttrs maxChildren
       , script maxAttrs
       , style maxAttrs
       , title maxAttrs
       ]

header :: Int -> Int -> Gen Element
header maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    headerChildren = NEL.toList $ headerContent maxAttrs maxChildren

  Header
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice headerChildren)

headerContent :: Int -> Int -> NonEmpty (Gen Element)
headerContent = marginalContent

hgroup :: Int -> Int -> Gen Element
hgroup maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    hgroupChildren = NEL.toList $ headingGroupContent maxAttrs maxChildren

  HeadingGroup
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice hgroupChildren)

headingGroupContent :: Int -> Int -> NonEmpty (Gen Element)
headingGroupContent maxAttrs maxChildren =
  NEL.cons comment
    . NEL.cons (p maxAttrs maxChildren)
    $ headings maxAttrs maxChildren

hr :: Int -> Gen Element
hr maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  HorizontalRule <$> withGlobalAttrs attrs []

html :: Int -> Int -> Gen Element
html maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)

  Html
    <$> withGlobalAttrs attrs []
    <*> sequence [ head maxAttrs maxChildren, body maxAttrs maxChildren ]

i :: Int -> Int -> Gen Element
i maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    iChildren = NEL.toList $ idiomaticTextContent maxAttrs maxChildren

  IdiomaticText
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice iChildren)

idiomaticTextContent :: Int -> Int -> NonEmpty (Gen Element)
idiomaticTextContent = phrasingContent

iframe :: Int -> Gen Element
iframe maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  IFrame <$> withGlobalAttrs attrs iFrameAttrs

iFrameAttrs :: [Gen A.Attribute]
iFrameAttrs =
  [ A.src
  , A.srcdoc
  , A.name
  , A.width
  , A.height
  , A.sandbox
  , A.allow
  , A.loading
  , A.referrerpolicy
  ]

img :: Int -> Gen Element
img maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Image <$> withGlobalAttrs attrs imageAttrs

imageAttrs :: [Gen A.Attribute]
imageAttrs =
  [ A.alt
  , A.crossorigin
  , A.decoding
  , A.fetchpriority
  , A.height
  , A.ismap
  , A.loading
  , A.referrerpolicy
  , A.sizes
  , A.src
  , A.srcset
  , A.width
  , A.usemap
  ]

input :: Int -> Gen Element
input maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Input <$> withGlobalAttrs attrs inputAttrs

inputAttrs :: [Gen A.Attribute]
inputAttrs =
  [ A.accept
  , A.alt
  , A.autocapitalize
  , A.autocomplete
  , A.capture
  , A.checked
  , A.dirname
  , A.disabled
  , A.form
  , A.formaction
  , A.formenctype
  , A.formmethod
  , A.formnovalidate
  , A.formtarget
  , A.height
  , A.list
  , A.max
  , A.maxlength
  , A.min
  , A.minlength
  , A.multiple
  , A.name
  , A.pattern
  , A.placeholder
  , A.popovertarget
  , A.popovertargetaction
  , A.readonly
  , A.required
  , A.size
  , A.src
  , A.step
  , A.type_
  , A.value
  , A.width
  ]

ins :: Int -> Int -> Gen Element
ins maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    insChildren = NEL.toList insertedTextContent

  InsertedText
    <$> withGlobalAttrs attrs insertedTextAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice insChildren)

insertedTextAttrs :: [Gen A.Attribute]
insertedTextAttrs =
  [ A.cite
  , A.datetime
  ]

insertedTextContent :: NonEmpty (Gen Element)
insertedTextContent = NEL.singleton text

kbd :: Int -> Int -> Gen Element
kbd maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    kbdChildren = NEL.toList $ keyboardInputContent maxAttrs maxChildren

  KeyboardInput
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice kbdChildren)

keyboardInputContent :: Int -> Int -> NonEmpty (Gen Element)
keyboardInputContent = phrasingContent

label :: Int -> Int -> Gen Element
label maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    labelChildren = NEL.toList $ labelContent maxAttrs maxChildren

  Label
    <$> withGlobalAttrs attrs [A.forLabel]
    <*> Gen.list (Range.singleton children) (Gen.choice labelChildren)

labelContent :: Int -> Int -> NonEmpty (Gen Element)
labelContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbr maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , kbd maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

legend :: Int -> Int -> Gen Element
legend maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    legendChildren = NEL.toList $ legendContent maxAttrs maxChildren

  Legend
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice legendChildren)

legendContent :: Int -> Int -> NonEmpty (Gen Element)
legendContent maxAttrs maxChildren =
  headings maxAttrs maxChildren
    <> phrasingContent maxAttrs maxChildren

li :: Int -> Int -> Gen Element
li maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    liChildren = NEL.toList $ listItemContent maxAttrs maxChildren

  ListItem
    <$> withGlobalAttrs attrs [A.value]
    <*> Gen.list (Range.singleton children) (Gen.choice liChildren)

listItemContent :: Int -> Int -> NonEmpty (Gen Element)
listItemContent = flowContent

link :: Int -> Gen Element
link maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Link <$> withGlobalAttrs attrs linkAttrs

linkAttrs :: [Gen A.Attribute]
linkAttrs =
  [ A.as
  , A.crossorigin
  , A.disabled
  , A.fetchpriority
  , A.href
  , A.hreflang
  , A.imagesizes
  , A.imagesrcset
  , A.integrity
  , A.media
  , A.referrerpolicy
  , A.rel
  , A.sizes
  , A.title
  , A.type_
  ]

main :: Int -> Int -> Gen Element
main maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    mainChildren = NEL.toList $ mainContent maxAttrs maxChildren

  Main
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice mainChildren)

mainContent :: Int -> Int -> NonEmpty (Gen Element)
mainContent = flowContent

map :: Int -> Int -> Gen Element
map maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    mapChildren = NEL.toList $ mapContent maxAttrs maxChildren

  Map
    <$> withGlobalAttrs attrs [A.name]
    <*> Gen.list (Range.singleton children) (Gen.choice mapChildren)

mapContent :: Int -> Int -> NonEmpty (Gen Element)
mapContent maxAttrs maxChildren =
  comment
    :| [ a maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , del maxAttrs maxChildren
       , ins maxAttrs maxChildren
       , map maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , video maxAttrs maxChildren
       ]

mark :: Int -> Int -> Gen Element
mark maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    markChildren = NEL.toList $ markContent maxAttrs maxChildren

  Mark
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice markChildren)

markContent :: Int -> Int -> NonEmpty (Gen Element)
markContent = phrasingContent

menu :: Int -> Int -> Gen Element
menu maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    menuChildren = NEL.toList $ menuContent maxAttrs maxChildren

  Menu
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice menuChildren)

menuContent :: Int -> Int -> NonEmpty (Gen Element)
menuContent = listContent

meta :: Int -> Gen Element
meta maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Meta <$> withGlobalAttrs attrs metaAttrs

metaAttrs :: [Gen A.Attribute]
metaAttrs =
  [ A.charset
  , A.content
  , A.httpEquiv
  , A.media
  , A.nameMeta
  ]

meter :: Int -> Int -> Gen Element
meter maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    meterChildren = NEL.toList $ meterContent maxAttrs maxChildren

  Meter
    <$> withGlobalAttrs attrs meterAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice meterChildren)

meterAttrs :: [Gen A.Attribute]
meterAttrs =
  [ A.value
  , A.min
  , A.max
  , A.low
  , A.high
  , A.optimum
  , A.form
  ]

meterContent :: Int -> Int -> NonEmpty (Gen Element)
meterContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbr maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

nav :: Int -> Int -> Gen Element
nav maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    navChildren = NEL.toList $ navContent maxAttrs maxChildren

  Nav
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice navChildren)

navContent :: Int -> Int -> NonEmpty (Gen Element)
navContent = flowContent

noscript :: Int -> Int -> Gen Element
noscript maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    noScriptChildren = NEL.toList noScriptContent

  NoScript
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice noScriptChildren)

noScriptContent :: NonEmpty (Gen Element)
noScriptContent = NEL.singleton text

object :: Int -> Int -> Gen Element
object maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    objectChildren = NEL.toList objectContent

  Object
    <$> withGlobalAttrs attrs objectAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice objectChildren)

objectAttrs :: [Gen A.Attribute]
objectAttrs =
  [ A.data_
  , A.form
  , A.height
  , A.name
  , A.type_
  , A.width
  ]

objectContent :: NonEmpty (Gen Element)
objectContent = NEL.singleton text

ol :: Int -> Int -> Gen Element
ol maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    olChildren = NEL.toList $ orderedListContent maxAttrs maxChildren

  OrderedList
    <$> withGlobalAttrs attrs orderedListAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice olChildren)

orderedListAttrs :: [Gen A.Attribute]
orderedListAttrs =
  [ A.reversed
  , A.start
  , A.type_
  ]

orderedListContent :: Int -> Int -> NonEmpty (Gen Element)
orderedListContent = listContent

optgroup :: Int -> Int -> Gen Element
optgroup maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    optGroupChildren = NEL.toList $ optionGroupContent maxAttrs

  OptionGroup
    <$> withGlobalAttrs attrs optionGroupAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice optGroupChildren)

optionGroupAttrs :: [Gen A.Attribute]
optionGroupAttrs =
  [ A.disabled
  , A.label
  ]

optionGroupContent :: Int -> NonEmpty (Gen Element)
optionGroupContent maxAttrs =
  comment :| [ option maxAttrs ]

option :: Int -> Gen Element
option maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)

  Option
    <$> withGlobalAttrs attrs optionAttrs
    <*> Generators.text

optionAttrs :: [Gen A.Attribute]
optionAttrs =
  [ A.disabled
  , A.label
  , A.selected
  , A.value
  ]

output :: Int -> Int -> Gen Element
output maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    outputChildren = NEL.toList $ outputContent maxAttrs maxChildren

  Output
    <$> withGlobalAttrs attrs outputAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice outputChildren)

outputAttrs :: [Gen A.Attribute]
outputAttrs =
  [ A.forOutput
  , A.form
  , A.name
  ]

outputContent :: Int -> Int -> NonEmpty (Gen Element)
outputContent = phrasingContent

p :: Int -> Int -> Gen Element
p maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    pChildren = NEL.toList $ paragraphContent maxAttrs maxChildren

  Paragraph
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice pChildren)

paragraphContent :: Int -> Int -> NonEmpty (Gen Element)
paragraphContent = phrasingContent

picture :: Int -> Int -> Gen Element
picture maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    pictureChildren = NEL.toList $ pictureContent maxAttrs maxChildren

  Picture
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice pictureChildren)

pictureContent :: Int -> Int -> NonEmpty (Gen Element)
pictureContent maxAttrs maxChildren =
  NEL.cons comment
    . NEL.cons (source maxAttrs)
    . NEL.cons (img maxAttrs)
    $ scriptSupportingContent maxAttrs maxChildren

pre :: Int -> Int -> Gen Element
pre maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    preChildren = NEL.toList $ preformattedTextContent maxAttrs maxChildren

  PreformattedText
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice preChildren)

preformattedTextContent :: Int -> Int -> NonEmpty (Gen Element)
preformattedTextContent = phrasingContent

progress :: Int -> Int -> Gen Element
progress maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    progressChildren = NEL.toList $ progressContent maxAttrs maxChildren

  Progress
    <$> withGlobalAttrs attrs progressAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice progressChildren)

progressAttrs :: [Gen A.Attribute]
progressAttrs =
  [ A.max
  , A.value
  ]

progressContent :: Int -> Int -> NonEmpty (Gen Element)
progressContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbr maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

q :: Int -> Int -> Gen Element
q maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    qChildren = NEL.toList $ quotationContent maxAttrs maxChildren

  Quotation
    <$> withGlobalAttrs attrs [A.cite]
    <*> Gen.list (Range.singleton children) (Gen.choice qChildren)

quotationContent :: Int -> Int -> NonEmpty (Gen Element)
quotationContent = phrasingContent

rubyText :: Int -> Int -> Gen Element
rubyText maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    rtChildren = NEL.toList $ rubyTextContent maxAttrs maxChildren

  RubyText
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice rtChildren)

rubyTextContent :: Int -> Int -> NonEmpty (Gen Element)
rubyTextContent = phrasingContent

ruby :: Int -> Int -> Gen Element
ruby maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)
  rubyContent <- Gen.list (Range.singleton children) text
  rubyTexts <- Gen.list (Range.singleton children) (rubyText maxAttrs maxChildren)

  let
    addParens (content, rt) =
      [ content
      , RubyParenthesis [] "("
      , rt
      , RubyParenthesis [] ")"
      ]

  Ruby
    <$> withGlobalAttrs attrs []
    <*> pure (concatMap addParens $ zip rubyContent rubyTexts)

s :: Int -> Int -> Gen Element
s maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    sChildren = NEL.toList $ strikethroughContent maxAttrs maxChildren

  Strikethrough
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice sChildren)

strikethroughContent :: Int -> Int -> NonEmpty (Gen Element)
strikethroughContent = phrasingContent

samp :: Int -> Int -> Gen Element
samp maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    sampleChildren = NEL.toList $ sampleContent maxAttrs maxChildren

  Sample
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice sampleChildren)

sampleContent :: Int -> Int -> NonEmpty (Gen Element)
sampleContent = phrasingContent

script :: Int -> Gen Element
script maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Script
    <$> withGlobalAttrs attrs scriptAttrs
    <*> Generators.nonEmptyText

scriptAttrs :: [Gen A.Attribute]
scriptAttrs =
  [ A.async
  , A.crossorigin
  , A.defer
  , A.fetchpriority
  , A.integrity
  , A.nomodule
  , A.nonce
  , A.referrerpolicy
  , A.src
  , A.type_
  ]

search :: Int -> Int -> Gen Element
search maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    searchChildren = NEL.toList $ searchContent maxAttrs maxChildren

  Search
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice searchChildren)

searchContent :: Int -> Int -> NonEmpty (Gen Element)
searchContent = flowContent

section :: Int -> Int -> Gen Element
section maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    sectionChildren = NEL.toList $ sectionContent maxAttrs maxChildren

  Section
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice sectionChildren)

sectionContent :: Int -> Int -> NonEmpty (Gen Element)
sectionContent = flowContent

select :: Int -> Int -> Gen Element
select maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    selectChildren = NEL.toList $ selectContent maxAttrs maxChildren

  Select
    <$> withGlobalAttrs attrs selectAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice selectChildren)

selectAttrs :: [Gen A.Attribute]
selectAttrs =
  [ A.autocomplete
  , A.autofocus
  , A.disabled
  , A.form
  , A.multiple
  , A.name
  , A.required
  , A.size
  ]

selectContent :: Int -> Int -> NonEmpty (Gen Element)
selectContent maxAttrs maxChildren =
  comment
    :| [ option maxAttrs
       , optgroup maxAttrs maxChildren
       ]

slot :: Int -> Int -> Gen Element
slot maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    slotChildren = NEL.toList slotContent

  Slot
    <$> withGlobalAttrs attrs [A.name]
    <*> Gen.list (Range.singleton children) (Gen.choice slotChildren)

slotContent :: NonEmpty (Gen Element)
slotContent = NEL.singleton text

small :: Int -> Int -> Gen Element
small maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    smallChildren = NEL.toList $ sideCommentContent maxAttrs maxChildren

  SideComment
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice smallChildren)

sideCommentContent :: Int -> Int -> NonEmpty (Gen Element)
sideCommentContent = phrasingContent

source :: Int -> Gen Element
source maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Source <$> withGlobalAttrs attrs sourceAttrs

sourceAttrs :: [Gen A.Attribute]
sourceAttrs =
  [ A.type_
  , A.src
  , A.srcset
  , A.sizes
  , A.media
  , A.height
  , A.width
  ]

span :: Int -> Int -> Gen Element
span maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    spanChildren = NEL.toList $ spanContent maxAttrs maxChildren

  Span
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice spanChildren)

spanContent :: Int -> Int -> NonEmpty (Gen Element)
spanContent = phrasingContent

strong :: Int -> Int -> Gen Element
strong maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    strongChildren = NEL.toList $ strongContent maxAttrs maxChildren

  Strong
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice strongChildren)

strongContent :: Int -> Int -> NonEmpty (Gen Element)
strongContent = phrasingContent

style :: Int -> Gen Element
style maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Style <$> withGlobalAttrs attrs styleAttrs

styleAttrs :: [Gen A.Attribute]
styleAttrs =
  [ A.media
  , A.nonce
  , A.title
  ]

sub :: Int -> Int -> Gen Element
sub maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    subChildren = NEL.toList $ subscriptContent maxAttrs maxChildren

  Subscript
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice subChildren)

subscriptContent :: Int -> Int -> NonEmpty (Gen Element)
subscriptContent = phrasingContent

summary :: Int -> Int -> Gen Element
summary maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    summaryChildren = NEL.toList $ summaryContent maxAttrs maxChildren

  Summary
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice summaryChildren)

summaryContent :: Int -> Int -> NonEmpty (Gen Element)
summaryContent maxAttrs maxChildren =
  NEL.cons (hgroup maxAttrs maxChildren) $
    headings maxAttrs maxChildren
      <> phrasingContent maxAttrs maxChildren

sup :: Int -> Int -> Gen Element
sup maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    supChildren = NEL.toList $ superscriptContent maxAttrs maxChildren

  Superscript
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice supChildren)

superscriptContent :: Int -> Int -> NonEmpty (Gen Element)
superscriptContent = phrasingContent

table :: Int -> Int -> Gen Element
table maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tableChildren = NEL.toList $ tableContent maxAttrs maxChildren

  Table
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice tableChildren)

tableContent :: Int -> Int -> NonEmpty (Gen Element)
tableContent maxAttrs maxChildren =
  caption maxAttrs maxChildren
    :| [ colgroup maxAttrs maxChildren
       , thead maxAttrs maxChildren
       , tbody maxAttrs maxChildren
       , tr maxAttrs maxChildren
       , tfoot maxAttrs maxChildren
       ]

tbody :: Int -> Int -> Gen Element
tbody maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tbodyChildren = NEL.toList $ tableBodyContent maxAttrs maxChildren

  TableBody
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice tbodyChildren)

tableBodyContent :: Int -> Int -> NonEmpty (Gen Element)
tableBodyContent maxAttrs maxChildren =
  comment :| [ tr maxAttrs maxChildren ]

td :: Int -> Int -> Gen Element
td maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tdChildren = NEL.toList $ tableDataCellContent maxAttrs maxChildren

  TableDataCell
    <$> withGlobalAttrs attrs tableDataCellAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice tdChildren)

tableDataCellAttrs :: [Gen A.Attribute]
tableDataCellAttrs =
  [ A.colspan
  , A.headers
  , A.rowspan
  ]

tableDataCellContent :: Int -> Int -> NonEmpty (Gen Element)
tableDataCellContent = flowContent

template :: Int -> Int -> Gen Element
template maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    contentTemplateChildren = NEL.toList $ contentTemplateContent maxAttrs maxChildren

  ContentTemplate
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice contentTemplateChildren)

contentTemplateContent :: Int -> Int -> NonEmpty (Gen Element)
contentTemplateContent = allElements

textarea :: Int -> Int -> Gen Element
textarea maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  TextArea
    <$> withGlobalAttrs attrs textAreaAttrs
    <*> Gen.list (Range.singleton children) text

textAreaAttrs :: [Gen A.Attribute]
textAreaAttrs =
  [ A.autocapitalize
  , A.autocomplete
  , A.autofocus
  , A.cols
  , A.dirname
  , A.disabled
  , A.form
  , A.maxlength
  , A.minlength
  , A.name
  , A.placeholder
  , A.readonly
  , A.required
  , A.rows
  , A.spellcheck
  , A.wrap
  ]

tfoot :: Int -> Int -> Gen Element
tfoot maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tfootChildren = NEL.toList $ tableFootContent maxAttrs maxChildren

  TableFoot
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice tfootChildren)

tableFootContent :: Int -> Int -> NonEmpty (Gen Element)
tableFootContent maxAttrs maxChildren =
  comment :| [ tr maxAttrs maxChildren ]

th :: Int -> Int -> Gen Element
th maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    theadChildren = NEL.toList $ tableHeaderContent maxAttrs maxChildren

  TableHeader
    <$> withGlobalAttrs attrs tableHeaderAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice theadChildren)

tableHeaderAttrs :: [Gen A.Attribute]
tableHeaderAttrs =
  [ A.abbr
  , A.colspan
  , A.headers
  , A.rowspan
  , A.scope
  ]

tableHeaderContent :: Int -> Int -> NonEmpty (Gen Element)
tableHeaderContent maxAttrs maxChildren =
  text
    :| [ comment
       , a maxAttrs maxChildren
       , abbr maxAttrs maxChildren
       , address maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , del maxAttrs maxChildren
       , details maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , div maxAttrs maxChildren
       , dl maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , form maxAttrs maxChildren
       , hr maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , ins maxAttrs maxChildren
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , ol maxAttrs maxChildren
       , output maxAttrs maxChildren
       , p maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , pre maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , table maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , ul maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

thead :: Int -> Int -> Gen Element
thead maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    thChildren = NEL.toList $ tableHeadContent maxAttrs maxChildren

  TableHead
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice thChildren)

tableHeadContent :: Int -> Int -> NonEmpty (Gen Element)
tableHeadContent maxAttrs maxChildren =
  comment :| [ tr maxAttrs maxChildren ]

time :: Int -> Int -> Gen Element
time maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    timeChildren = NEL.toList $ timeContent maxAttrs maxChildren

  Time
    <$> withGlobalAttrs attrs [A.datetime]
    <*> Gen.list (Range.singleton children) (Gen.choice timeChildren)

timeContent :: Int -> Int -> NonEmpty (Gen Element)
timeContent = phrasingContent

title :: Int -> Gen Element
title maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)

  Title
    <$> withGlobalAttrs attrs []
    <*> Generators.text

tr :: Int -> Int -> Gen Element
tr maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tableRowChildren = NEL.toList $ tableRowContent maxAttrs maxChildren

  TableRow
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice tableRowChildren)

tableRowContent :: Int -> Int -> NonEmpty (Gen Element)
tableRowContent maxAttrs maxChildren =
  NEL.cons (td maxAttrs maxChildren)
    . NEL.cons (th maxAttrs maxChildren)
    $ scriptSupportingContent maxAttrs maxChildren

track :: Int -> Gen Element
track maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  Track <$> withGlobalAttrs attrs trackAttrs

trackAttrs :: [Gen A.Attribute]
trackAttrs =
  [ A.default_
  , A.kind
  , A.label
  , A.src
  , A.srclang
  ]

u :: Int -> Int -> Gen Element
u maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    uChildren = NEL.toList $ underlineContent maxAttrs maxChildren

  Underline
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice uChildren)

underlineContent :: Int -> Int -> NonEmpty (Gen Element)
underlineContent = phrasingContent

ul :: Int -> Int -> Gen Element
ul maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    ulChildren = NEL.toList $ unorderedListContent maxAttrs maxChildren

  UnorderedList
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice ulChildren)

unorderedListContent :: Int -> Int -> NonEmpty (Gen Element)
unorderedListContent = listContent

var :: Int -> Int -> Gen Element
var maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    varChildren = NEL.toList $ variableContent maxAttrs maxChildren

  Variable
    <$> withGlobalAttrs attrs []
    <*> Gen.list (Range.singleton children) (Gen.choice varChildren)

variableContent :: Int -> Int -> NonEmpty (Gen Element)
variableContent = phrasingContent

video :: Int -> Int -> Gen Element
video maxAttrs maxChildren = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    videoChildren = NEL.toList $ videoContent maxAttrs

  Video
    <$> withGlobalAttrs attrs videoAttrs
    <*> Gen.list (Range.singleton children) (Gen.choice videoChildren)

videoAttrs :: [Gen A.Attribute]
videoAttrs =
  [ A.autoplay
  , A.controls
  , A.controlslist
  , A.crossorigin
  , A.disablepictureinpicture
  , A.disableremoteplayback
  , A.height
  , A.loop
  , A.muted
  , A.playsinline
  , A.poster
  , A.preload
  , A.src
  , A.width
  ]

videoContent :: Int -> NonEmpty (Gen Element)
videoContent = audioVideoContent

wbr :: Int -> Gen Element
wbr maxAttrs = do
  attrs <- Gen.int (Range.linear 0 maxAttrs)

  WordBreakOpportunity <$> withGlobalAttrs attrs []

-- Content Categories
--

allElements :: Int -> Int -> NonEmpty (Gen Element)
allElements maxAttrs maxChildren =
  comment
    :| [ text
       , a maxAttrs maxChildren
       , abbr maxAttrs maxChildren
       , address maxAttrs maxChildren
       , area maxAttrs
       , article maxAttrs maxChildren
       , aside maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , base maxAttrs
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , body maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , caption maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , col maxAttrs
       , colgroup maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , dd maxAttrs maxChildren
       , del maxAttrs maxChildren
       , details maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , div maxAttrs maxChildren
       , dl maxAttrs maxChildren
       , dt maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figcaption maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , footer maxAttrs maxChildren
       , form maxAttrs maxChildren
       , h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       , head maxAttrs maxChildren
       , header maxAttrs maxChildren
       , hgroup maxAttrs maxChildren
       , hr maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , ins maxAttrs maxChildren
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , legend maxAttrs maxChildren
       , li maxAttrs maxChildren
       , link maxAttrs
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meta maxAttrs
       , meter maxAttrs maxChildren
       , nav maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , ol maxAttrs maxChildren
       , optgroup maxAttrs maxChildren
       , option maxAttrs
       , output maxAttrs maxChildren
       , p maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , pre maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , script maxAttrs
       , search maxAttrs maxChildren
       , section maxAttrs maxChildren
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , source maxAttrs
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , style maxAttrs
       , sub maxAttrs maxChildren
       , summary maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , table maxAttrs maxChildren
       , tbody maxAttrs maxChildren
       , td maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , tfoot maxAttrs maxChildren
       , th maxAttrs maxChildren
       , thead maxAttrs maxChildren
       , time maxAttrs maxChildren
       , title maxAttrs
       , tr maxAttrs maxChildren
       , track maxAttrs
       , u maxAttrs maxChildren
       , ul maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

audioVideoContent :: Int -> NonEmpty (Gen Element)
audioVideoContent maxAttrs =
  comment
    :| [ source maxAttrs
       , track maxAttrs
       ]

flowContent :: Int -> Int -> NonEmpty (Gen Element)
flowContent maxAttrs maxChildren =
  text
    :| [ comment
       , a maxAttrs maxChildren
       , abbr maxAttrs maxChildren
       , address maxAttrs maxChildren
       , article maxAttrs maxChildren
       , aside maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , del maxAttrs maxChildren
       , details maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , div maxAttrs maxChildren
       , dl maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , footer maxAttrs maxChildren
       , form maxAttrs maxChildren
       , h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       , header maxAttrs maxChildren
       , hgroup maxAttrs maxChildren
       , hr maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , ins maxAttrs maxChildren
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , nav maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , ol maxAttrs maxChildren
       , output maxAttrs maxChildren
       , p maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , pre maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , section maxAttrs maxChildren
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , table maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , ul maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

headings :: Int -> Int -> NonEmpty (Gen Element)
headings maxAttrs maxChildren =
  comment
    :| [ h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       ]

listContent :: Int -> Int -> NonEmpty (Gen Element)
listContent maxAttrs maxChildren =
  NEL.cons
    (li maxAttrs maxChildren)
    (scriptSupportingContent maxAttrs maxChildren)

marginalContent :: Int -> Int -> NonEmpty (Gen Element)
marginalContent maxAttrs maxChildren =
  text
    :| [ comment
       , a maxAttrs maxChildren
       , abbr maxAttrs maxChildren
       , address maxAttrs maxChildren
       , article maxAttrs maxChildren
       , aside maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , del maxAttrs maxChildren
       , details maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , div maxAttrs maxChildren
       , dl maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , form maxAttrs maxChildren
       , h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       , hgroup maxAttrs maxChildren
       , hr maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , ins maxAttrs maxChildren
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , nav maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , ol maxAttrs maxChildren
       , output maxAttrs maxChildren
       , p maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , pre maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , section maxAttrs maxChildren
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , table maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , ul maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

phrasingContent :: Int -> Int -> NonEmpty (Gen Element)
phrasingContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbr maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , b maxAttrs maxChildren
       , bdi maxAttrs maxChildren
       , bdo maxAttrs maxChildren
       , br maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , cite maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , datalist maxAttrs maxChildren
       , dfn maxAttrs maxChildren
       , em maxAttrs maxChildren
       , embed maxAttrs
       , i maxAttrs maxChildren
       , iframe maxAttrs
       , img maxAttrs
       , input maxAttrs
       , kbd maxAttrs maxChildren
       , label maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noscript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , q maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , s maxAttrs maxChildren
       , samp maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , small maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , sub maxAttrs maxChildren
       , sup maxAttrs maxChildren
       , template maxAttrs maxChildren
       , textarea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , u maxAttrs maxChildren
       , var maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wbr maxAttrs
       ]

scriptSupportingContent :: Int -> Int -> NonEmpty (Gen Element)
scriptSupportingContent maxAttrs maxChildren =
  comment
    :| [ script maxAttrs
       , template maxAttrs maxChildren
       ]

withGlobalAttrs :: Int -> [Gen A.Attribute] -> Gen [A.Attribute]
withGlobalAttrs attrs =
  Gen.list (Range.singleton attrs)
    . Gen.choice
    . mappend
        [ A.accessKey
        , A.autocapitalize
        , A.autofocus
        , A.class_
        , A.contentEditable
        , A.dir
        , A.draggable
        , A.enterKeyHint
        , A.exportParts
        , A.hidden
        , A.id
        , A.inert
        , A.inputMode
        , A.is
        , A.itemId
        , A.itemProp
        , A.itemRef
        , A.itemScope
        , A.itemType
        , A.lang
        , A.nonce
        , A.part
        , A.popover
        , A.role
        , A.slot
        , A.spellcheck
        , A.style
        , A.tabIndex
        , A.title
        , A.translate
        , A.writingSuggestions
        ]
