module HTML.Types.ElementSelector
  ( ElementSelector
  , elementSelectorToBytes
  , elementSelectorToText
  , customTag
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
  , citeTag
  , code
  , col
  , colgroup
  , dataTag
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
  , formTag
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
  , labelTag
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
  , rp
  , rt
  , ruby
  , s
  , sample
  , script
  , search
  , section
  , select
  , slotTag
  , small
  , source
  , spanTag
  , strong
  , styleTag
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
  , titleTag
  , tr
  , track
  , u
  , ul
  , var
  , video
  , wbr
  , ElementType
  ) where

import Prelude hiding (div, head, map)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import HTML.Types.AttributeSelector qualified as AS
import HTML.Types.ClassSelector qualified as CS
import HTML.Types.NoContent (NoContent)

data ElementSelector =
  ElementSelector
    { elementSelectorType :: ElementType
    , elementSelectorAttr :: Maybe AS.AttributeSelector
    , elementSelectorClasses :: [CS.ClassSelector]
    , elementSelectorChild :: Maybe ElementSelector
    }

elementSelectorToBytes :: ElementSelector -> LBS.ByteString
elementSelectorToBytes element =
  LBS.concat
    [ elementTypeToBytes $ elementSelectorType element
    , maybe "" AS.attributeSelectorToBytes $ elementSelectorAttr element
    , foldMap CS.classSelectorToBytes $ elementSelectorClasses element
    , maybe
        ""
        (LBS8.cons ' ' . elementSelectorToBytes)
        (elementSelectorChild element)
    ]

elementSelectorToText :: ElementSelector -> T.Text
elementSelectorToText element =
  T.concat
    [ elementTypeToText $ elementSelectorType element
    , maybe "" AS.attributeSelectorToText $ elementSelectorAttr element
    , foldMap CS.classSelectorToText $ elementSelectorClasses element
    , maybe
        ""
        (T.cons ' ' . elementSelectorToText)
        (elementSelectorChild element)
    ]

customTag :: T.Text
          -> Maybe AS.AttributeSelector
          -> [CS.ClassSelector]
          -> Either NoContent ElementSelector
          -> ElementSelector
customTag elementName mbAttr classes eiNoContentOrElement =
  ElementSelector
    (CustomElement elementName)
    mbAttr
    classes
    (either (const Nothing) Just eiNoContentOrElement)

a :: Maybe AS.AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
a = ElementSelector Anchor

abbr :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
abbr = ElementSelector Abbreviation

address :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
address = ElementSelector ContactAddress

area :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
area mbAttr classes = ElementSelector Area mbAttr classes Nothing

article :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
article = ElementSelector Article

aside :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
aside = ElementSelector Aside

audio :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
audio = ElementSelector Audio

b :: Maybe AS.AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
b = ElementSelector BringAttentionTo

base :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
base mbAttr classes = ElementSelector Base mbAttr classes Nothing

bdi :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
bdi = ElementSelector BidirectionalIsolation

bdo :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
bdo = ElementSelector BidirectionalOverride

blockquote :: Maybe AS.AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
blockquote = ElementSelector Blockquote

body :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
body = ElementSelector Body

br :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
br mbAttr classes = ElementSelector LineBreak mbAttr classes Nothing

button :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
button = ElementSelector Button

canvas :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
canvas = ElementSelector Canvas

caption :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
caption = ElementSelector TableCaption

citeTag :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
citeTag = ElementSelector Citation

code :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
code = ElementSelector Code

col :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
col mbAttr classes = ElementSelector TableColumn mbAttr classes Nothing

colgroup :: Maybe AS.AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
colgroup = ElementSelector TableColumnGroup

dataTag :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
dataTag = ElementSelector Data

datalist :: Maybe AS.AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
datalist = ElementSelector DataList

dd :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dd = ElementSelector DescriptionDetails

del :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
del = ElementSelector DeletedText

details :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
details = ElementSelector Details

dfn :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
dfn = ElementSelector Definition

dialog :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
dialog = ElementSelector Dialog

div :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
div = ElementSelector Division

dl :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dl = ElementSelector DescriptionList

dt :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dt = ElementSelector DescriptionTerm

em :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
em = ElementSelector Emphasis

embed :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
embed mbAttr classes = ElementSelector Embed mbAttr classes Nothing

fieldset :: Maybe AS.AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
fieldset = ElementSelector Fieldset

figcaption :: Maybe AS.AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
figcaption = ElementSelector FigureCaption

figure :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
figure = ElementSelector Figure

footer :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
footer = ElementSelector Footer

formTag :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
formTag = ElementSelector Form

h1 :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h1 = ElementSelector H1

h2 :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h2 = ElementSelector H1

h3 :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h3 = ElementSelector H3

h4 :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h4 = ElementSelector H4

h5 :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h5 = ElementSelector H5

h6 :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h6 = ElementSelector H6

head :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
head = ElementSelector Head

header :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
header = ElementSelector Header

hgroup :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
hgroup = ElementSelector HeadingGroup

hr :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
hr mbAttr classes = ElementSelector HorizontalRule mbAttr classes Nothing

html :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
html = ElementSelector Html

i :: Maybe AS.AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
i = ElementSelector IdiomaticText

iframe :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
iframe mbAttr classes = ElementSelector IFrame mbAttr classes Nothing

img :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
img mbAttr classes = ElementSelector Image mbAttr classes Nothing

input :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
input mbAttr classes = ElementSelector Input mbAttr classes Nothing

ins :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
ins = ElementSelector InsertedText

kbd :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
kbd = ElementSelector KeyboardInput

labelTag :: Maybe AS.AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
labelTag = ElementSelector Label

legend :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
legend = ElementSelector Legend

li :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
li = ElementSelector ListItem

link :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
link mbAttr classes = ElementSelector Link mbAttr classes Nothing

main :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
main = ElementSelector Main

map :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
map = ElementSelector Map

mark :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
mark = ElementSelector Mark

menu :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
menu = ElementSelector Menu

meta :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
meta mbAttr classes = ElementSelector Meta mbAttr classes Nothing

meter :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
meter = ElementSelector Meter

nav :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
nav = ElementSelector Nav

noscript :: Maybe AS.AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
noscript = ElementSelector NoScript

object :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
object = ElementSelector Object

ol :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
ol = ElementSelector OrderedList

optgroup :: Maybe AS.AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
optgroup = ElementSelector OptionGroup

-- option does not take a child argument, since it only holds text values,
-- which cannot be targeted as part of a CSS query.
option :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
option mbAttr classes = ElementSelector Option mbAttr classes Nothing

output :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
output = ElementSelector Output

p :: Maybe AS.AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
p = ElementSelector Paragraph

picture :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
picture = ElementSelector Picture

pre :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
pre = ElementSelector PreformattedText

progress :: Maybe AS.AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
progress = ElementSelector Progress

q :: Maybe AS.AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
q = ElementSelector Quotation

-- rp does not take a child argument, since it only holds text values, which
-- cannot be targeted as part of a CSS query.
rp :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
rp mbAttr classes = ElementSelector RubyParenthesis mbAttr classes Nothing

rt :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
rt = ElementSelector RubyText

ruby :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
ruby = ElementSelector Ruby

s :: Maybe AS.AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
s = ElementSelector Strikethrough

sample :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
sample = ElementSelector Sample

script :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
script mbAttr classes = ElementSelector Script mbAttr classes Nothing

search :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
search = ElementSelector Search

section :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
section = ElementSelector Section

select :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
select = ElementSelector Select

slotTag :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
slotTag = ElementSelector Slot

small :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
small = ElementSelector SideComment

source :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
source mbAttr classes = ElementSelector Source mbAttr classes Nothing

spanTag :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
spanTag = ElementSelector Span

strong :: Maybe AS.AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
strong = ElementSelector Strong

styleTag :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
styleTag mbAttr classes = ElementSelector Style mbAttr classes Nothing

sub :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
sub = ElementSelector Subscript

summary :: Maybe AS.AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
summary = ElementSelector Summary

sup :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
sup = ElementSelector Superscript

table :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
table = ElementSelector Table

tbody :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tbody = ElementSelector TableBody

td :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
td = ElementSelector TableDataCell

template :: Maybe AS.AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
template = ElementSelector ContentTemplate

-- textarea does not take a child argument, since it only holds text values,
-- which cannot be targeted as part of a CSS query.
textarea :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
textarea mbAttr classes = ElementSelector TextArea mbAttr classes Nothing

tfoot :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tfoot = ElementSelector TableFoot

th :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
th = ElementSelector TableHeader

thead :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
thead = ElementSelector TableHead

time :: Maybe AS.AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
time = ElementSelector Time

-- title does not take a child argument, since it only holds text values, which
-- cannot be targeted as part of a CSS query.
titleTag :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
titleTag mbAttr classes = ElementSelector Title mbAttr classes Nothing

tr :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
tr = ElementSelector TableRow

track :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
track mbAttr classes = ElementSelector Track mbAttr classes Nothing

u :: Maybe AS.AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
u = ElementSelector Underline

ul :: Maybe AS.AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
ul = ElementSelector UnorderedList

var :: Maybe AS.AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
var = ElementSelector Variable

video :: Maybe AS.AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
video = ElementSelector Video

wbr :: Maybe AS.AttributeSelector -> [CS.ClassSelector] -> ElementSelector
wbr mbAttr classes =
  ElementSelector WordBreakOpportunity mbAttr classes Nothing

data ElementType
  = CustomElement T.Text
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

elementTypeToBytes :: ElementType -> LBS.ByteString
elementTypeToBytes element =
  case element of
    CustomElement name     -> LBS.fromStrict $ TE.encodeUtf8 name
    Anchor                 -> "a"
    Abbreviation           -> "abbr"
    ContactAddress         -> "address"
    Area                   -> "area"
    Article                -> "article"
    Aside                  -> "aside"
    Audio                  -> "audio"
    BringAttentionTo       -> "b"
    Base                   -> "base"
    BidirectionalIsolation -> "bdi"
    BidirectionalOverride  -> "bdo"
    Blockquote             -> "blockquote"
    Body                   -> "body"
    LineBreak              -> "br"
    Button                 -> "button"
    Canvas                 -> "canvas"
    TableCaption           -> "caption"
    Citation               -> "cite"
    Code                   -> "code"
    TableColumn            -> "col"
    TableColumnGroup       -> "colgroup"
    Data                   -> "data"
    DataList               -> "datalist"
    DescriptionDetails     -> "dd"
    DeletedText            -> "del"
    Details                -> "details"
    Definition             -> "dfn"
    Dialog                 -> "dialog"
    Division               -> "div"
    DescriptionList        -> "dl"
    DescriptionTerm        -> "dt"
    Emphasis               -> "em"
    Embed                  -> "embed"
    Fieldset               -> "fieldset"
    FigureCaption          -> "figcaption"
    Figure                 -> "figure"
    Footer                 -> "footer"
    Form                   -> "form"
    H1                     -> "h1"
    H2                     -> "h2"
    H3                     -> "h3"
    H4                     -> "h4"
    H5                     -> "h5"
    H6                     -> "h6"
    Head                   -> "head"
    Header                 -> "header"
    HeadingGroup           -> "hgroup"
    HorizontalRule         -> "hr"
    Html                   -> "html"
    IdiomaticText          -> "i"
    IFrame                 -> "iframe"
    Image                  -> "img"
    Input                  -> "input"
    InsertedText           -> "ins"
    KeyboardInput          -> "kbd"
    Label                  -> "label"
    Legend                 -> "legend"
    ListItem               -> "li"
    Link                   -> "link"
    Main                   -> "main"
    Map                    -> "map"
    Mark                   -> "mark"
    Menu                   -> "menu"
    Meta                   -> "meta"
    Meter                  -> "metere"
    Nav                    -> "nav"
    NoScript               -> "noscript"
    Object                 -> "object"
    OrderedList            -> "ol"
    OptionGroup            -> "optgroup"
    Option                 -> "option"
    Output                 -> "output"
    Paragraph              -> "p"
    Picture                -> "picture"
    PreformattedText       -> "pre"
    Progress               -> "progress"
    Quotation              -> "q"
    RubyParenthesis        -> "rp"
    RubyText               -> "rt"
    Ruby                   -> "ruby"
    Strikethrough          -> "s"
    Sample                 -> "sample"
    Script                 -> "script"
    Search                 -> "search"
    Section                -> "section"
    Select                 -> "select"
    Slot                   -> "slot"
    SideComment            -> "small"
    Source                 -> "source"
    Span                   -> "span"
    Strong                 -> "strong"
    Style                  -> "style"
    Subscript              -> "sub"
    Summary                -> "summary"
    Superscript            -> "sup"
    Table                  -> "table"
    TableBody              -> "tbody"
    TableDataCell          -> "td"
    ContentTemplate        -> "template"
    TextArea               -> "textarea"
    TableFoot              -> "tfoot"
    TableHeader            -> "th"
    TableHead              -> "thead"
    Time                   -> "time"
    Title                  -> "title"
    TableRow               -> "tr"
    Track                  -> "track"
    Underline              -> "u"
    UnorderedList          -> "ul"
    Variable               -> "var"
    Video                  -> "video"
    WordBreakOpportunity   -> "wbr"

elementTypeToText :: ElementType -> T.Text
elementTypeToText element =
  case element of
    CustomElement name     -> name
    Anchor                 -> "a"
    Abbreviation           -> "abbr"
    ContactAddress         -> "address"
    Area                   -> "area"
    Article                -> "article"
    Aside                  -> "aside"
    Audio                  -> "audio"
    BringAttentionTo       -> "b"
    Base                   -> "base"
    BidirectionalIsolation -> "bdi"
    BidirectionalOverride  -> "bdo"
    Blockquote             -> "blockquote"
    Body                   -> "body"
    LineBreak              -> "br"
    Button                 -> "button"
    Canvas                 -> "canvas"
    TableCaption           -> "caption"
    Citation               -> "cite"
    Code                   -> "code"
    TableColumn            -> "col"
    TableColumnGroup       -> "colgroup"
    Data                   -> "data"
    DataList               -> "datalist"
    DescriptionDetails     -> "dd"
    DeletedText            -> "del"
    Details                -> "details"
    Definition             -> "dfn"
    Dialog                 -> "dialog"
    Division               -> "div"
    DescriptionList        -> "dl"
    DescriptionTerm        -> "dt"
    Emphasis               -> "em"
    Embed                  -> "embed"
    Fieldset               -> "fieldset"
    FigureCaption          -> "figcaption"
    Figure                 -> "figure"
    Footer                 -> "footer"
    Form                   -> "form"
    H1                     -> "h1"
    H2                     -> "h2"
    H3                     -> "h3"
    H4                     -> "h4"
    H5                     -> "h5"
    H6                     -> "h6"
    Head                   -> "head"
    Header                 -> "header"
    HeadingGroup           -> "hgroup"
    HorizontalRule         -> "hr"
    Html                   -> "html"
    IdiomaticText          -> "i"
    IFrame                 -> "iframe"
    Image                  -> "img"
    Input                  -> "input"
    InsertedText           -> "ins"
    KeyboardInput          -> "kbd"
    Label                  -> "label"
    Legend                 -> "legend"
    ListItem               -> "li"
    Link                   -> "link"
    Main                   -> "main"
    Map                    -> "map"
    Mark                   -> "mark"
    Menu                   -> "menu"
    Meta                   -> "meta"
    Meter                  -> "metere"
    Nav                    -> "nav"
    NoScript               -> "noscript"
    Object                 -> "object"
    OrderedList            -> "ol"
    OptionGroup            -> "optgroup"
    Option                 -> "option"
    Output                 -> "output"
    Paragraph              -> "p"
    Picture                -> "picture"
    PreformattedText       -> "pre"
    Progress               -> "progress"
    Quotation              -> "q"
    RubyParenthesis        -> "rp"
    RubyText               -> "rt"
    Ruby                   -> "ruby"
    Strikethrough          -> "s"
    Sample                 -> "sample"
    Script                 -> "script"
    Search                 -> "search"
    Section                -> "section"
    Select                 -> "select"
    Slot                   -> "slot"
    SideComment            -> "small"
    Source                 -> "source"
    Span                   -> "span"
    Strong                 -> "strong"
    Style                  -> "style"
    Subscript              -> "sub"
    Summary                -> "summary"
    Superscript            -> "sup"
    Table                  -> "table"
    TableBody              -> "tbody"
    TableDataCell          -> "td"
    ContentTemplate        -> "template"
    TextArea               -> "textarea"
    TableFoot              -> "tfoot"
    TableHeader            -> "th"
    TableHead              -> "thead"
    Time                   -> "time"
    Title                  -> "title"
    TableRow               -> "tr"
    Track                  -> "track"
    Underline              -> "u"
    UnorderedList          -> "ul"
    Variable               -> "var"
    Video                  -> "video"
    WordBreakOpportunity   -> "wbr"
