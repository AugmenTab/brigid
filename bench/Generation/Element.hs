module Generation.Element
  ( Element (..)
  , ElementType (..)
  , elementGenerator
  ) where

import Control.Monad (replicateM, when)
import Control.Monad.Reader (ask, liftIO, local)
import Control.Monad.Trans.Class (lift)
import Data.IORef (atomicModifyIORef')
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Hedgehog (GenT)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude hiding (div, head, map, span)

import Generation.Attribute qualified as A
import Generation.Generators qualified as Generators
import Generation.Types (GenM, GenContext (..), consumeNode)

data Element =
  Element
    { elementType :: ElementType
    , elementAttrs :: [A.Attribute]
    , elementChildren :: Either NET.NonEmptyText [Element]
    }

instance Show Element where
  show e =
    unwords
      [ show (elementType e)
      , show (elementAttrs e)
      , either show show (elementChildren e)
      ]

-- This is effectively just `Brigid.HTML.Elements.TagType`, but we don't want
-- to expose that ADT, and this has fewer constructors because we don't
-- particularly care to generate the CustomHTML constructor of that type.
--
data ElementType
  = Comment
  | Text
  | Anchor
  | Abbreviation
  | ContactAddress
  | Area
  | Article
  | Aside
  | Audio
  | BringAttentionTo
  | Base
  | BidirectionalIsolation
  | BidirectionalOverride
  | Blockquote
  | Body
  | LineBreak
  | Button
  | Canvas
  | TableCaption
  | Citation
  | Code
  | TableColumn
  | TableColumnGroup
  | Data
  | DataList
  | DescriptionDetails
  | DeletedText
  | Details
  | Definition
  | Dialog
  | Division
  | DescriptionList
  | DescriptionTerm
  | Emphasis
  | Embed
  | Fieldset
  | FigureCaption
  | Figure
  | Footer
  | Form
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | Head
  | Header
  | HeadingGroup
  | HorizontalRule
  | Html
  | IdiomaticText
  | IFrame
  | Image
  | Input
  | InsertedText
  | KeyboardInput
  | Label
  | Legend
  | ListItem
  | Link
  | Main
  | Map
  | Mark
  | Menu
  | Meta
  | Meter
  | Nav
  | NoScript
  | Object
  | OrderedList
  | OptionGroup
  | Option
  | Output
  | Paragraph
  | Picture
  | PreformattedText
  | Progress
  | Quotation
  | RubyParenthesis
  | RubyText
  | Ruby
  | Strikethrough
  | Sample
  | Script
  | Search
  | Section
  | Select
  | Slot
  | SideComment
  | Source
  | Span
  | Strong
  | Style
  | Subscript
  | Summary
  | Superscript
  | Table
  | TableBody
  | TableDataCell
  | ContentTemplate
  | TextArea
  | TableFoot
  | TableHeader
  | TableHead
  | Time
  | Title
  | TableRow
  | Track
  | Underline
  | UnorderedList
  | Variable
  | Video
  | WordBreakOpportunity
  deriving Show

elementGenerator :: ElementType -> GenM Element
elementGenerator element =
  case element of
    Comment -> comment
    Text -> text
    Anchor -> a
    Abbreviation -> abbr
    ContactAddress -> address
    Area -> area
    Article -> article
    Aside -> aside
    Audio -> audio
    BringAttentionTo -> b
    Base -> base
    BidirectionalIsolation -> bdi
    BidirectionalOverride -> bdo
    Blockquote -> blockquote
    Body -> body
    LineBreak -> br
    Button -> button
    Canvas -> canvas
    TableCaption -> caption
    Citation -> cite
    Code -> code
    TableColumn -> col
    TableColumnGroup -> colgroup
    Data -> data_
    DataList -> datalist
    DescriptionDetails -> dd
    DeletedText -> del
    Details -> details
    Definition -> dfn
    Dialog -> dialog
    Division -> div
    DescriptionList -> dl
    DescriptionTerm -> dt
    Emphasis -> em
    Embed -> embed
    Fieldset -> fieldset
    FigureCaption -> figcaption
    Figure -> figure
    Footer -> footer
    Form -> form
    H1 -> h1
    H2 -> h2
    H3 -> h3
    H4 -> h4
    H5 -> h5
    H6 -> h6
    Head -> head
    Header -> header
    HeadingGroup -> hgroup
    HorizontalRule -> hr
    Html -> html
    IdiomaticText -> i
    IFrame -> iframe
    Image -> img
    Input -> input
    InsertedText -> ins
    KeyboardInput -> kbd
    Label -> label
    Legend -> legend
    ListItem -> li
    Link -> link
    Main -> main
    Map -> map
    Mark -> mark
    Menu -> menu
    Meta -> meta
    Meter -> meter
    Nav -> nav
    NoScript -> noscript
    Object -> object
    OrderedList -> ol
    OptionGroup -> optgroup
    Option -> option
    Output -> output
    Paragraph -> p
    Picture -> picture
    PreformattedText -> pre
    Progress -> progress
    Quotation -> q
    RubyParenthesis -> rp
    RubyText -> rt
    Ruby -> ruby
    Strikethrough -> s
    Sample -> samp
    Script -> script
    Search -> search
    Section -> section
    Select -> select
    Slot -> slot
    SideComment -> small
    Source -> source
    Span -> span
    Strong -> strong
    Style -> style
    Subscript -> sub
    Summary -> summary
    Superscript -> sup
    Table -> table
    TableBody -> tbody
    TableDataCell -> td
    ContentTemplate -> template
    TextArea -> textarea
    TableFoot -> tfoot
    TableHeader -> th
    TableHead -> thead
    Time -> time
    Title -> title
    TableRow -> tr
    Track -> track
    Underline -> u
    UnorderedList -> ul
    Variable -> var
    Video -> video
    WordBreakOpportunity -> wbr

data ContentType
  = NoContent
  | TextContent NET.NonEmptyText
  | ElementContent (NonEmpty (GenM Element))

mkElement :: ElementType
          -> [GenT IO A.Attribute]
          -> ContentType
          -> GenM Element
mkElement element attrs content = do
  ok <- consumeNode
  when (not ok) $ lift Gen.discard

  ctx <- ask

  children <-
    case content of
      NoContent -> pure $ Right []
      TextContent net -> pure $ Left net
      ElementContent elementContent ->
        if currentDepth ctx >= maxDepth ctx
          then pure $ Right []
          else do
            n <- lift . Gen.int . Range.linear 0 $ maxChildrenPerNode ctx

            let
              bumpDepth =
                ctx
                  { currentDepth = currentDepth ctx + 1
                  }

            fmap Right
              . replicateM n
              . local (const bumpDepth)
              $ chooseAndRun elementContent

  Element
    <$> pure element
    <*> withGlobalAttrs attrs
    <*> pure children

chooseAndRun :: NonEmpty (GenM a) -> GenM a
chooseAndRun xs =
  id =<< lift (Gen.element $ NEL.toList xs)

comment :: GenM Element
comment =
  mkElement Comment [] NoContent

text :: GenM Element
text =
  mkElement Text [] NoContent

a :: GenM Element
a =
  mkElement Anchor anchorAttrs $ ElementContent anchorContent

anchorAttrs :: [GenT IO A.Attribute]
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

anchorContent :: NonEmpty (GenM Element)
anchorContent = NEL.singleton text

abbr :: GenM Element
abbr =
  mkElement Abbreviation [] $ ElementContent abbreviationContent

abbreviationContent :: NonEmpty (GenM Element)
abbreviationContent = phrasingContent

address :: GenM Element
address =
  mkElement ContactAddress [] $ ElementContent contactAddressContent

contactAddressContent :: NonEmpty (GenM Element)
contactAddressContent =
  text
    :| [ comment
       , a
       , abbr
       , audio
       , b
       , bdi
       , bdo
       , blockquote
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , del
       , details
       , dfn
       , dialog
       , div
       , dl
       , em
       , embed
       , fieldset
       , figure
       , form
       , hr
       , i
       , iframe
       , img
       , input
       , ins
       , kbd
       , label
       , main
       , map
       , mark
       , menu
       , meter
       , noscript
       , object
       , ol
       , output
       , p
       , picture
       , pre
       , progress
       , q
       , ruby
       , s
       , samp
       , search
       , script
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , table
       , template
       , textarea
       , time
       , u
       , ul
       , var
       , video
       , wbr
       ]

area :: GenM Element
area =
  mkElement Area areaAttrs NoContent

areaAttrs :: [GenT IO A.Attribute]
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

article :: GenM Element
article =
  mkElement Article [] $ ElementContent articleContent

articleContent :: NonEmpty (GenM Element)
articleContent = flowContent

aside :: GenM Element
aside =
  mkElement Aside [] $ ElementContent asideContent

asideContent :: NonEmpty (GenM Element)
asideContent = flowContent

audio :: GenM Element
audio =
  mkElement Audio audioAttrs $ ElementContent audioContent

audioAttrs :: [GenT IO A.Attribute]
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

audioContent :: NonEmpty (GenM Element)
audioContent = audioVideoContent

b :: GenM Element
b =
  mkElement BringAttentionTo [] $ ElementContent bringAttentionToContent

bringAttentionToContent :: NonEmpty (GenM Element)
bringAttentionToContent = phrasingContent

base :: GenM Element
base =
  mkElement Base baseAttrs NoContent

baseAttrs :: [GenT IO A.Attribute]
baseAttrs =
  [ A.href
  , A.target
  ]

bdi :: GenM Element
bdi =
  mkElement
    BidirectionalIsolation
    []
    (ElementContent bidirectionalIsolationContent)

bidirectionalIsolationContent :: NonEmpty (GenM Element)
bidirectionalIsolationContent = phrasingContent

bdo :: GenM Element
bdo =
  mkElement
    BidirectionalOverride
    []
    (ElementContent bidirectionalOverrideContent)

bidirectionalOverrideContent :: NonEmpty (GenM Element)
bidirectionalOverrideContent = phrasingContent

blockquote :: GenM Element
blockquote =
  mkElement Blockquote [A.cite] $ ElementContent blockquoteContent

blockquoteContent :: NonEmpty (GenM Element)
blockquoteContent = flowContent

body :: GenM Element
body =
  mkElement Body [] $ ElementContent bodyContent

bodyContent :: NonEmpty (GenM Element)
bodyContent = flowContent

br :: GenM Element
br =
  mkElement LineBreak [] NoContent

button :: GenM Element
button =
  mkElement Button buttonAttrs $ ElementContent buttonContent

buttonAttrs :: [GenT IO A.Attribute]
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

buttonContent :: NonEmpty (GenM Element)
buttonContent =
  text
    :| [ comment
       , abbr
       , area
       , audio
       , b
       , bdi
       , bdo
       , br
       , canvas
       , cite
       , code
       , data_
       , datalist
       , dfn
       , em
       , i
       , img
       , kbd
       , mark
       , meter
       , noscript
       , object
       , output
       , picture
       , progress
       , q
       , ruby
       , s
       , samp
       , script
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , template
       , time
       , u
       , var
       , video
       , wbr
       ]

canvas :: GenM Element
canvas =
  mkElement Canvas canvasAttrs $ ElementContent canvasContent

canvasAttrs :: [GenT IO A.Attribute]
canvasAttrs =
  [ A.height
  , A.width
  ]

canvasContent :: NonEmpty (GenM Element)
canvasContent = NEL.singleton text

caption :: GenM Element
caption =
  mkElement TableCaption [] $ ElementContent tableCaptionContent

tableCaptionContent :: NonEmpty (GenM Element)
tableCaptionContent = flowContent

cite :: GenM Element
cite =
  mkElement Citation [] $ ElementContent citationContent

citationContent :: NonEmpty (GenM Element)
citationContent = phrasingContent

code :: GenM Element
code =
  mkElement Code [] $ ElementContent codeContent

codeContent :: NonEmpty (GenM Element)
codeContent = phrasingContent

col :: GenM Element
col =
  mkElement TableColumn [A.span] NoContent

colgroup :: GenM Element
colgroup =
  mkElement TableColumnGroup [A.span] $ ElementContent tableColumnGroupContent

tableColumnGroupContent :: NonEmpty (GenM Element)
tableColumnGroupContent =
  comment :| [ col ]

data_ :: GenM Element
data_ =
  mkElement Data [A.data_] $ ElementContent dataContent

dataContent :: NonEmpty (GenM Element)
dataContent = phrasingContent

datalist :: GenM Element
datalist =
  mkElement DataList [] $ ElementContent dataListContent

dataListContent :: NonEmpty (GenM Element)
dataListContent =
  NEL.cons option phrasingContent

dd :: GenM Element
dd =
  mkElement DescriptionDetails [] $ ElementContent descriptionDetailsContent

descriptionDetailsContent :: NonEmpty (GenM Element)
descriptionDetailsContent = flowContent

del :: GenM Element
del =
  mkElement DeletedText deletedTextAttrs $ ElementContent deletedTextContent

deletedTextAttrs :: [GenT IO A.Attribute]
deletedTextAttrs =
  [ A.cite
  , A.datetime
  ]

deletedTextContent :: NonEmpty (GenM Element)
deletedTextContent = NEL.singleton text

details :: GenM Element
details =
  mkElement Details [A.open] $ ElementContent detailsContent

detailsContent :: NonEmpty (GenM Element)
detailsContent =
  NEL.cons summary flowContent

dfn :: GenM Element
dfn =
  mkElement Definition [] $ ElementContent definitionContent

definitionContent :: NonEmpty (GenM Element)
definitionContent =
  text
    :| [ comment
       , abbr
       , area
       , audio
       , b
       , bdi
       , bdo
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , em
       , embed
       , i
       , iframe
       , img
       , input
       , kbd
       , label
       , mark
       , meter
       , noscript
       , object
       , output
       , picture
       , progress
       , q
       , ruby
       , s
       , samp
       , script
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , template
       , textarea
       , time
       , u
       , var
       , video
       , wbr
       ]

dialog :: GenM Element
dialog =
  mkElement Dialog [A.open] $ ElementContent dialogContent

dialogContent :: NonEmpty (GenM Element)
dialogContent = flowContent

div :: GenM Element
div =
  mkElement Division [] $ ElementContent divisionContent

divisionContent :: NonEmpty (GenM Element)
divisionContent = flowContent

dl :: GenM Element
dl =
  mkElement DescriptionList [] $ ElementContent descriptionListContent

descriptionListContent :: NonEmpty (GenM Element)
descriptionListContent =
  NEL.cons dt
    . NEL.cons dd
    . NEL.cons div
    $ scriptSupportingContent

dt :: GenM Element
dt =
  mkElement DescriptionTerm [] $ ElementContent descriptionTermContent

descriptionTermContent :: NonEmpty (GenM Element)
descriptionTermContent =
  text
    :| [ comment
       , a
       , abbr
       , address
       , audio
       , b
       , bdi
       , bdo
       , blockquote
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , del
       , details
       , dfn
       , dialog
       , div
       , dl
       , em
       , embed
       , fieldset
       , figure
       , form
       , hr
       , i
       , iframe
       , img
       , input
       , ins
       , kbd
       , label
       , main
       , map
       , mark
       , menu
       , meter
       , noscript
       , object
       , ol
       , output
       , p
       , picture
       , pre
       , progress
       , q
       , ruby
       , s
       , samp
       , search
       , script
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , table
       , template
       , textarea
       , time
       , u
       , ul
       , var
       , video
       , wbr
       ]

em :: GenM Element
em =
  mkElement Emphasis [] $ ElementContent emphasisContent

emphasisContent :: NonEmpty (GenM Element)
emphasisContent = phrasingContent

embed :: GenM Element
embed =
  mkElement Embed embedAttrs NoContent

embedAttrs :: [GenT IO A.Attribute]
embedAttrs =
  [ A.src
  , A.type_
  , A.width
  , A.height
  ]

fieldset :: GenM Element
fieldset =
  mkElement Fieldset fieldsetAttrs $ ElementContent fieldsetContent

fieldsetAttrs :: [GenT IO A.Attribute]
fieldsetAttrs =
  [ A.disabled
  , A.form
  , A.name
  ]

fieldsetContent :: NonEmpty (GenM Element)
fieldsetContent =
  NEL.cons legend flowContent

figcaption :: GenM Element
figcaption =
  mkElement FigureCaption [] $ ElementContent figureCaptionContent

figureCaptionContent :: NonEmpty (GenM Element)
figureCaptionContent = flowContent

figure :: GenM Element
figure =
  mkElement Figure [] $ ElementContent figureContent

figureContent :: NonEmpty (GenM Element)
figureContent =
  NEL.cons figcaption flowContent

footer :: GenM Element
footer =
  mkElement Footer [] $ ElementContent footerContent

footerContent :: NonEmpty (GenM Element)
footerContent = marginalContent

form :: GenM Element
form =
  mkElement Form formAttrs $ ElementContent formContent

formAttrs :: [GenT IO A.Attribute]
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

formContent :: NonEmpty (GenM Element)
formContent =
  text
    :| [ comment
       , a
       , abbr
       , address
       , article
       , aside
       , audio
       , b
       , bdi
       , bdo
       , blockquote
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , del
       , details
       , dfn
       , dialog
       , div
       , dl
       , em
       , embed
       , fieldset
       , figure
       , footer
       , h1
       , h2
       , h3
       , h4
       , h5
       , h6
       , header
       , hgroup
       , hr
       , i
       , iframe
       , img
       , input
       , ins
       , kbd
       , label
       , main
       , map
       , mark
       , menu
       , meter
       , nav
       , noscript
       , object
       , ol
       , output
       , p
       , picture
       , pre
       , progress
       , q
       , ruby
       , s
       , samp
       , search
       , script
       , section
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , table
       , template
       , textarea
       , time
       , u
       , ul
       , var
       , video
       , wbr
       ]

h1 :: GenM Element
h1 =
  mkElement H1 [] $ ElementContent h1Content

h1Content :: NonEmpty (GenM Element)
h1Content = phrasingContent

h2 :: GenM Element
h2 =
  mkElement H2 [] $ ElementContent h2Content

h2Content :: NonEmpty (GenM Element)
h2Content = phrasingContent

h3 :: GenM Element
h3 =
  mkElement H3 [] $ ElementContent h3Content

h3Content :: NonEmpty (GenM Element)
h3Content = phrasingContent

h4 :: GenM Element
h4 =
  mkElement H4 [] $ ElementContent h4Content

h4Content :: NonEmpty (GenM Element)
h4Content = phrasingContent

h5 :: GenM Element
h5 =
  mkElement H5 [] $ ElementContent h5Content

h5Content :: NonEmpty (GenM Element)
h5Content = phrasingContent

h6 :: GenM Element
h6 =
  mkElement H6 [] $ ElementContent h6Content

h6Content :: NonEmpty (GenM Element)
h6Content = phrasingContent

head :: GenM Element
head =
  mkElement Head [] $ ElementContent headContent

headContent :: NonEmpty (GenM Element)
headContent =
  comment
    :| [ base
       , link
       , meta
       , noscript
       , script
       , style
       , title
       ]

header :: GenM Element
header =
  mkElement Header [] $ ElementContent headerContent

headerContent :: NonEmpty (GenM Element)
headerContent = marginalContent

hgroup :: GenM Element
hgroup =
  mkElement HeadingGroup [] $ ElementContent headingGroupContent

headingGroupContent :: NonEmpty (GenM Element)
headingGroupContent =
  NEL.cons comment
    . NEL.cons p
    $ headings

hr :: GenM Element
hr =
  mkElement HorizontalRule [] NoContent

html :: GenM Element
html = do
  ok <- consumeNode
  when (not ok) $ lift Gen.discard

  ctx <- ask

  children <-
    if currentDepth ctx >= maxDepth ctx
      then pure []
      else do
        n <-
          lift
            . Gen.int
            . Range.singleton
            . min 2
            $ maxChildrenPerNode ctx

        let
          bumpDepth =
            ctx
              { currentDepth = currentDepth ctx + 1
              }

        replicateM n
          . local (const bumpDepth)
          $ chooseAndRun htmlContent

  Element
    <$> pure Html
    <*> withGlobalAttrs []
    <*> pure (Right children)

htmlContent :: NonEmpty (GenM Element)
htmlContent =
  head :| [ body ]

i :: GenM Element
i =
  mkElement IdiomaticText [] $ ElementContent idiomaticTextContent

idiomaticTextContent :: NonEmpty (GenM Element)
idiomaticTextContent = phrasingContent

iframe :: GenM Element
iframe =
  mkElement IFrame iFrameAttrs NoContent

iFrameAttrs :: [GenT IO A.Attribute]
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

img :: GenM Element
img =
  mkElement Image imageAttrs NoContent

imageAttrs :: [GenT IO A.Attribute]
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

input :: GenM Element
input =
  mkElement Input inputAttrs NoContent

inputAttrs :: [GenT IO A.Attribute]
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

ins :: GenM Element
ins =
  mkElement InsertedText insertedTextAttrs $ ElementContent insertedTextContent

insertedTextAttrs :: [GenT IO A.Attribute]
insertedTextAttrs =
  [ A.cite
  , A.datetime
  ]

insertedTextContent :: NonEmpty (GenM Element)
insertedTextContent = NEL.singleton text

kbd :: GenM Element
kbd =
  mkElement KeyboardInput [] $ ElementContent keyboardInputContent

keyboardInputContent :: NonEmpty (GenM Element)
keyboardInputContent = phrasingContent

label :: GenM Element
label =
  mkElement Label [A.forLabel] $ ElementContent labelContent

labelContent :: NonEmpty (GenM Element)
labelContent =
  text
    :| [ comment
       , abbr
       , area
       , audio
       , b
       , bdi
       , bdo
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , dfn
       , em
       , embed
       , i
       , iframe
       , img
       , input
       , kbd
       , mark
       , meter
       , noscript
       , object
       , output
       , picture
       , progress
       , q
       , ruby
       , s
       , samp
       , script
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , template
       , textarea
       , time
       , u
       , var
       , video
       , wbr
       ]

legend :: GenM Element
legend =
  mkElement Legend [] $ ElementContent legendContent

legendContent :: NonEmpty (GenM Element)
legendContent =
  headings <> phrasingContent

li :: GenM Element
li =
  mkElement ListItem [A.value] $ ElementContent listItemContent

listItemContent :: NonEmpty (GenM Element)
listItemContent = flowContent

link :: GenM Element
link =
  mkElement Link linkAttrs NoContent

linkAttrs :: [GenT IO A.Attribute]
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

main :: GenM Element
main =
  mkElement Main [] $ ElementContent mainContent

mainContent :: NonEmpty (GenM Element)
mainContent = flowContent

map :: GenM Element
map =
  mkElement Map [A.name] $ ElementContent mapContent

mapContent :: NonEmpty (GenM Element)
mapContent =
  comment
    :| [ a
       , audio
       , canvas
       , del
       , ins
       , map
       , noscript
       , object
       , slot
       , video
       ]

mark :: GenM Element
mark =
  mkElement Mark [] $ ElementContent markContent

markContent :: NonEmpty (GenM Element)
markContent = phrasingContent

menu :: GenM Element
menu =
  mkElement Menu [] $ ElementContent menuContent

menuContent :: NonEmpty (GenM Element)
menuContent = listContent

meta :: GenM Element
meta =
  mkElement Meta metaAttrs NoContent

metaAttrs :: [GenT IO A.Attribute]
metaAttrs =
  [ A.charset
  , A.content
  , A.httpEquiv
  , A.media
  , A.nameMeta
  ]

meter :: GenM Element
meter =
  mkElement Meter meterAttrs $ ElementContent meterContent

meterAttrs :: [GenT IO A.Attribute]
meterAttrs =
  [ A.value
  , A.min
  , A.max
  , A.low
  , A.high
  , A.optimum
  , A.form
  ]

meterContent :: NonEmpty (GenM Element)
meterContent =
  text
    :| [ comment
       , abbr
       , area
       , audio
       , b
       , bdi
       , bdo
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , dfn
       , em
       , embed
       , i
       , iframe
       , img
       , input
       , kbd
       , label
       , mark
       , noscript
       , object
       , output
       , picture
       , progress
       , q
       , ruby
       , s
       , samp
       , script
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , template
       , textarea
       , time
       , u
       , var
       , video
       , wbr
       ]

nav :: GenM Element
nav =
  mkElement Nav [] $ ElementContent navContent

navContent :: NonEmpty (GenM Element)
navContent = flowContent

noscript :: GenM Element
noscript =
  mkElement NoScript [] $ ElementContent noScriptContent

noScriptContent :: NonEmpty (GenM Element)
noScriptContent = NEL.singleton text

object :: GenM Element
object =
  mkElement Object objectAttrs $ ElementContent objectContent

objectAttrs :: [GenT IO A.Attribute]
objectAttrs =
  [ A.data_
  , A.form
  , A.height
  , A.name
  , A.type_
  , A.width
  ]

objectContent :: NonEmpty (GenM Element)
objectContent = NEL.singleton text

ol :: GenM Element
ol =
  mkElement OrderedList orderedListAttrs $ ElementContent orderedListContent

orderedListAttrs :: [GenT IO A.Attribute]
orderedListAttrs =
  [ A.reversed
  , A.start
  , A.type_
  ]

orderedListContent :: NonEmpty (GenM Element)
orderedListContent = listContent

optgroup :: GenM Element
optgroup =
  mkElement OptionGroup optionGroupAttrs $ ElementContent optionGroupContent

optionGroupAttrs :: [GenT IO A.Attribute]
optionGroupAttrs =
  [ A.disabled
  , A.label
  ]

optionGroupContent :: NonEmpty (GenM Element)
optionGroupContent =
  comment :| [ option ]

option :: GenM Element
option =
  mkElement Option optionAttrs . TextContent =<< Generators.nonEmptyText

optionAttrs :: [GenT IO A.Attribute]
optionAttrs =
  [ A.disabled
  , A.label
  , A.selected
  , A.value
  ]

output :: GenM Element
output =
  mkElement Output outputAttrs $ ElementContent outputContent

outputAttrs :: [GenT IO A.Attribute]
outputAttrs =
  [ A.forOutput
  , A.form
  , A.name
  ]

outputContent :: NonEmpty (GenM Element)
outputContent = phrasingContent

p :: GenM Element
p =
  mkElement Paragraph [] $ ElementContent paragraphContent

paragraphContent :: NonEmpty (GenM Element)
paragraphContent = phrasingContent

picture :: GenM Element
picture =
  mkElement Picture [] $ ElementContent pictureContent

pictureContent :: NonEmpty (GenM Element)
pictureContent =
  NEL.cons comment
    . NEL.cons source
    . NEL.cons img
    $ scriptSupportingContent

pre :: GenM Element
pre =
  mkElement PreformattedText [] $ ElementContent preformattedTextContent

preformattedTextContent :: NonEmpty (GenM Element)
preformattedTextContent = phrasingContent

progress :: GenM Element
progress =
  mkElement Progress progressAttrs $ ElementContent progressContent

progressAttrs :: [GenT IO A.Attribute]
progressAttrs =
  [ A.max
  , A.value
  ]

progressContent :: NonEmpty (GenM Element)
progressContent =
  text
    :| [ comment
       , abbr
       , area
       , audio
       , b
       , bdi
       , bdo
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , dfn
       , em
       , embed
       , i
       , iframe
       , img
       , input
       , kbd
       , label
       , mark
       , meter
       , noscript
       , object
       , output
       , picture
       , q
       , ruby
       , s
       , samp
       , script
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , template
       , textarea
       , time
       , u
       , var
       , video
       , wbr
       ]

q :: GenM Element
q =
  mkElement Quotation [A.cite] $ ElementContent quotationContent

quotationContent :: NonEmpty (GenM Element)
quotationContent = phrasingContent

rp :: GenM Element
rp =
  mkElement RubyParenthesis [] . TextContent =<< Generators.nonEmptyText

rt :: GenM Element
rt =
  mkElement RubyText [] $ ElementContent rubyTextContent

rubyTextContent :: NonEmpty (GenM Element)
rubyTextContent = phrasingContent

ruby :: GenM Element
ruby = do
  ok <- consumeNode
  when (not ok) $ lift Gen.discard

  ctx <- ask
  nPairs <- lift . Gen.int . Range.linear 0 $ maxChildrenPerNode ctx

  let
    totalChildNodes = 4 * nPairs
    addParents (content, rubyText) =
      [ content
      , Element RubyParenthesis [] . Left $ NET.singleton '('
      , rubyText
      , Element RubyParenthesis [] . Left $ NET.singleton ')'
      ]


  canProceed <-
    liftIO $
      atomicModifyIORef' (remainingNodes ctx) $
        \remaining ->
          if remaining < totalChildNodes
            then (remaining, False)
            else (remaining - totalChildNodes, True)

  when (not canProceed) $ lift Gen.discard

  rubyContent <- replicateM nPairs text
  rubyTexts <- replicateM nPairs rt

  Element
    <$> pure Ruby
    <*> withGlobalAttrs []
    <*> pure (Right . concatMap addParents $ zip rubyContent rubyTexts)

s :: GenM Element
s =
  mkElement Strikethrough [] $ ElementContent strikethroughContent

strikethroughContent :: NonEmpty (GenM Element)
strikethroughContent = phrasingContent

samp :: GenM Element
samp =
  mkElement Sample [] $ ElementContent sampleContent

sampleContent :: NonEmpty (GenM Element)
sampleContent = phrasingContent

script :: GenM Element
script =
  mkElement Script scriptAttrs . TextContent =<< Generators.nonEmptyText

scriptAttrs :: [GenT IO A.Attribute]
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

search :: GenM Element
search =
  mkElement Search [] $ ElementContent searchContent

searchContent :: NonEmpty (GenM Element)
searchContent = flowContent

section :: GenM Element
section =
  mkElement Section [] $ ElementContent sectionContent

sectionContent :: NonEmpty (GenM Element)
sectionContent = flowContent

select :: GenM Element
select =
  mkElement Select selectAttrs $ ElementContent selectContent

selectAttrs :: [GenT IO A.Attribute]
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

selectContent :: NonEmpty (GenM Element)
selectContent =
  comment
    :| [ option
       , optgroup
       ]

slot :: GenM Element
slot =
  mkElement Slot [A.name] $ ElementContent slotContent

slotContent :: NonEmpty (GenM Element)
slotContent = NEL.singleton text

small :: GenM Element
small =
  mkElement SideComment [] $ ElementContent sideCommentContent

sideCommentContent :: NonEmpty (GenM Element)
sideCommentContent = phrasingContent

source :: GenM Element
source =
  mkElement Source sourceAttrs NoContent

sourceAttrs :: [GenT IO A.Attribute]
sourceAttrs =
  [ A.type_
  , A.src
  , A.srcset
  , A.sizes
  , A.media
  , A.height
  , A.width
  ]

span :: GenM Element
span =
  mkElement Span [] $ ElementContent spanContent

spanContent :: NonEmpty (GenM Element)
spanContent = phrasingContent

strong :: GenM Element
strong =
  mkElement Strong [] $ ElementContent strongContent

strongContent :: NonEmpty (GenM Element)
strongContent = phrasingContent

style :: GenM Element
style =
  mkElement Style styleAttrs NoContent

styleAttrs :: [GenT IO A.Attribute]
styleAttrs =
  [ A.media
  , A.nonce
  , A.title
  ]

sub :: GenM Element
sub =
  mkElement Subscript [] $ ElementContent subscriptContent

subscriptContent :: NonEmpty (GenM Element)
subscriptContent = phrasingContent

summary :: GenM Element
summary =
  mkElement Summary [] $ ElementContent summaryContent

summaryContent :: NonEmpty (GenM Element)
summaryContent =
  NEL.cons hgroup $ headings <> phrasingContent

sup :: GenM Element
sup =
  mkElement Superscript [] $ ElementContent superscriptContent

superscriptContent :: NonEmpty (GenM Element)
superscriptContent = phrasingContent

table :: GenM Element
table =
  mkElement Table [] $ ElementContent tableContent

tableContent :: NonEmpty (GenM Element)
tableContent =
  caption
    :| [ colgroup
       , thead
       , tbody
       , tr
       , tfoot
       ]

tbody :: GenM Element
tbody =
  mkElement TableBody [] $ ElementContent tableBodyContent

tableBodyContent :: NonEmpty (GenM Element)
tableBodyContent =
  comment :| [ tr ]

td :: GenM Element
td =
  mkElement
    TableDataCell
    tableDataCellAttrs
    (ElementContent tableDataCellContent)

tableDataCellAttrs :: [GenT IO A.Attribute]
tableDataCellAttrs =
  [ A.colspan
  , A.headers
  , A.rowspan
  ]

tableDataCellContent :: NonEmpty (GenM Element)
tableDataCellContent = flowContent

template :: GenM Element
template =
  mkElement ContentTemplate [] $ ElementContent contentTemplateContent

contentTemplateContent :: NonEmpty (GenM Element)
contentTemplateContent = allElements

textarea :: GenM Element
textarea =
  mkElement TextArea textAreaAttrs . ElementContent $ NEL.singleton text

textAreaAttrs :: [GenT IO A.Attribute]
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

tfoot :: GenM Element
tfoot =
  mkElement TableFoot [] $ ElementContent tableFootContent

tableFootContent :: NonEmpty (GenM Element)
tableFootContent =
  comment :| [ tr ]

th :: GenM Element
th =
  mkElement TableHeader tableHeaderAttrs $ ElementContent tableHeaderContent

tableHeaderAttrs :: [GenT IO A.Attribute]
tableHeaderAttrs =
  [ A.abbr
  , A.colspan
  , A.headers
  , A.rowspan
  , A.scope
  ]

tableHeaderContent :: NonEmpty (GenM Element)
tableHeaderContent =
  text
    :| [ comment
       , a
       , abbr
       , address
       , audio
       , b
       , bdi
       , bdo
       , blockquote
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , del
       , details
       , dfn
       , dialog
       , div
       , dl
       , em
       , embed
       , fieldset
       , figure
       , form
       , hr
       , i
       , iframe
       , img
       , input
       , ins
       , kbd
       , label
       , main
       , map
       , mark
       , menu
       , meter
       , noscript
       , object
       , ol
       , output
       , p
       , picture
       , pre
       , progress
       , q
       , ruby
       , s
       , samp
       , search
       , script
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , table
       , template
       , textarea
       , time
       , u
       , ul
       , var
       , video
       , wbr
       ]

thead :: GenM Element
thead =
  mkElement TableHead [] $ ElementContent tableHeadContent

tableHeadContent :: NonEmpty (GenM Element)
tableHeadContent =
  comment :| [ tr ]

time :: GenM Element
time =
  mkElement Time [A.datetime] $ ElementContent timeContent

timeContent :: NonEmpty (GenM Element)
timeContent = phrasingContent

title :: GenM Element
title =
  mkElement Title [] . TextContent =<< Generators.nonEmptyText

tr :: GenM Element
tr =
  mkElement TableRow [] $ ElementContent tableRowContent

tableRowContent :: NonEmpty (GenM Element)
tableRowContent =
  NEL.cons td
    . NEL.cons th
    $ scriptSupportingContent

track :: GenM Element
track =
  mkElement Track trackAttrs NoContent

trackAttrs :: [GenT IO A.Attribute]
trackAttrs =
  [ A.default_
  , A.kind
  , A.label
  , A.src
  , A.srclang
  ]

u :: GenM Element
u =
  mkElement Underline [] $ ElementContent underlineContent

underlineContent :: NonEmpty (GenM Element)
underlineContent = phrasingContent

ul :: GenM Element
ul =
  mkElement UnorderedList [] $ ElementContent unorderedListContent

unorderedListContent :: NonEmpty (GenM Element)
unorderedListContent = listContent

var :: GenM Element
var =
  mkElement Variable [] $ ElementContent variableContent

variableContent :: NonEmpty (GenM Element)
variableContent = phrasingContent

video :: GenM Element
video =
  mkElement Video videoAttrs $ ElementContent videoContent

videoAttrs :: [GenT IO A.Attribute]
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

videoContent :: NonEmpty (GenM Element)
videoContent = audioVideoContent

wbr :: GenM Element
wbr =
  mkElement WordBreakOpportunity [] NoContent

-- Content Categories
--

allElements :: NonEmpty (GenM Element)
allElements =
  comment
    :| [ text
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
       ]

audioVideoContent :: NonEmpty (GenM Element)
audioVideoContent =
  comment
    :| [ source
       , track
       ]

flowContent :: NonEmpty (GenM Element)
flowContent =
  text
    :| [ comment
       , a
       , abbr
       , address
       , article
       , aside
       , audio
       , b
       , bdi
       , bdo
       , blockquote
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , del
       , details
       , dfn
       , dialog
       , div
       , dl
       , em
       , embed
       , fieldset
       , figure
       , footer
       , form
       , h1
       , h2
       , h3
       , h4
       , h5
       , h6
       , header
       , hgroup
       , hr
       , i
       , iframe
       , img
       , input
       , ins
       , kbd
       , label
       , main
       , map
       , mark
       , menu
       , meter
       , nav
       , noscript
       , object
       , ol
       , output
       , p
       , picture
       , pre
       , progress
       , q
       , ruby
       , s
       , samp
       , search
       , script
       , section
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , table
       , template
       , textarea
       , time
       , u
       , ul
       , var
       , video
       , wbr
       ]

headings :: NonEmpty (GenM Element)
headings =
  comment
    :| [ h1
       , h2
       , h3
       , h4
       , h5
       , h6
       ]

listContent :: NonEmpty (GenM Element)
listContent =
  NEL.cons li scriptSupportingContent

marginalContent :: NonEmpty (GenM Element)
marginalContent =
  text
    :| [ comment
       , a
       , abbr
       , address
       , article
       , aside
       , audio
       , b
       , bdi
       , bdo
       , blockquote
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , del
       , details
       , dfn
       , dialog
       , div
       , dl
       , em
       , embed
       , fieldset
       , figure
       , form
       , h1
       , h2
       , h3
       , h4
       , h5
       , h6
       , hgroup
       , hr
       , i
       , iframe
       , img
       , input
       , ins
       , kbd
       , label
       , main
       , map
       , mark
       , menu
       , meter
       , nav
       , noscript
       , object
       , ol
       , output
       , p
       , picture
       , pre
       , progress
       , q
       , ruby
       , s
       , samp
       , search
       , script
       , section
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , table
       , template
       , textarea
       , time
       , u
       , ul
       , var
       , video
       , wbr
       ]

phrasingContent :: NonEmpty (GenM Element)
phrasingContent =
  text
    :| [ comment
       , abbr
       , area
       , audio
       , b
       , bdi
       , bdo
       , br
       , button
       , canvas
       , cite
       , code
       , data_
       , datalist
       , dfn
       , em
       , embed
       , i
       , iframe
       , img
       , input
       , kbd
       , label
       , mark
       , meter
       , noscript
       , object
       , output
       , picture
       , progress
       , q
       , ruby
       , s
       , samp
       , script
       , select
       , slot
       , small
       , span
       , strong
       , sub
       , sup
       , template
       , textarea
       , time
       , u
       , var
       , video
       , wbr
       ]

scriptSupportingContent :: NonEmpty (GenM Element)
scriptSupportingContent =
  comment
    :| [ script
       , template
       ]

withGlobalAttrs :: [GenT IO A.Attribute] -> GenM [A.Attribute]
withGlobalAttrs attrs = do
  ctx <- ask

  lift
    . Gen.list (Range.linear 0 $ maxAttributesPerNode ctx)
    . Gen.choice
    . mappend attrs
    $ [ A.accessKey
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
