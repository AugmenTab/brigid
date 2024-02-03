{-# LANGUAGE DataKinds #-}

module HTML.Elements
  ( Document
  , HTML
  , ChildHTML
  , noElement
  , Tags.Comment, comment
  , Tags.Text, text, texts
  , Tags.RawHTML, rawHTML
  , Tags.CustomHTML, customHTML
  , Tags.Anchor, a
  , Tags.Abbreviation, abbr
  , Tags.ContactAddress, address
  , Tags.Area, area
  , Tags.Article, article
  , Tags.Aside, aside
  , Tags.Audio, audio
  , Tags.BringAttentionTo, b
  , Tags.Base, base
  , Tags.BidirectionalIsolation, bdi
  , Tags.BidirectionalOverride, bdo
  , Tags.Blockquote, blockquote
  , Tags.Body, body
  , Tags.LineBreak, br
  , Tags.Button, button
  , Tags.Canvas, canvas
  , Tags.TableCaption, caption
  , Tags.Citation, cite
  , Tags.Code, code
  , Tags.TableColumn, col
  , Tags.TableColumnGroup, colgroup
  , Tags.Data, data_
  , Tags.DataList, datalist
  , Tags.DescriptionDetails, dd
  , Tags.DeletedText, del
  , Tags.Details, details
  , Tags.Definition, dfn
  , Tags.Dialog, dialog
  , Tags.Division, div
  , Tags.DescriptionList, dl
  , Tags.DescriptionTerm, dt
  , Tags.Emphasis, em
  , Tags.Embed, embed
  , Tags.Fieldset, fieldset
  , Tags.FigureCaption, figcaption
  , Tags.Figure, figure
  , Tags.Footer, footer
  , Tags.Form, form
  , Tags.H1, h1
  , Tags.H2, h2
  , Tags.H3, h3
  , Tags.H4, h4
  , Tags.H5, h5
  , Tags.H6, h6
  , Tags.Head, head
  , Tags.Header, header
  , Tags.HeadingGroup, hgroup
  , Tags.HorizontalRule, hr
  , Tags.Html, html
  , Tags.IdiomaticText, i
  , Tags.IFrame, iframe
  , Tags.Image, img
  , Tags.Input, input
  , Tags.InsertedText, ins
  , Tags.KeyboardInput, kbd
  , Tags.Label, label
  , Tags.Legend, legend
  , Tags.ListItem, li
  , Tags.Link, link
  , Tags.Main, main
  , Tags.Map, map
  , Tags.Mark, mark
  , Tags.Menu, menu
  , Tags.Meta, meta
  , Tags.Meter, meter
  , Tags.Nav, nav
  , Tags.NoScript, noscript
  , Tags.Object, object
  , Tags.OrderedList, ol
  , Tags.OptionGroup, optgroup
  , Tags.Option, option
  , Tags.Output, output
  , Tags.Paragraph, p
  , Tags.Picture, picture
  , Tags.PreformattedText, pre
  , Tags.Progress, progress
  , Tags.Quotation, q
  , Tags.RubyParenthesis, rp
  , Tags.RubyText, rt
  , Tags.Ruby, ruby
  , Tags.Strikethrough, s
  , Tags.Sample, sample
  , Tags.Script, script
  , Tags.Search, search
  , Tags.Section, section
  , Tags.Select, select
  , Tags.Slot, slot
  , Tags.SideComment, small
  , Tags.Source, source
  , Tags.Span, span
  , Tags.Strong, strong
  , Tags.Style, style
  , Tags.Subscript, sub
  , Tags.Summary, summary
  , Tags.Superscript, sup
  , Tags.Table, table
  , Tags.TableBody, tbody
  , Tags.TableDataCell, td
  , Tags.ContentTemplate, template
  , Tags.TextArea, textarea
  , Tags.TableFoot, tfoot
  , Tags.TableHeader, th
  , Tags.TableHead, thead
  , Tags.Time, time
  , Tags.Title, title
  , Tags.TableRow, tr
  , Tags.Track, track
  , Tags.Underline, u
  , Tags.UnorderedList, ul
  , Tags.Variable, var
  , Tags.Video, video
  , Tags.WordBreakOpportunity, wbr, wbrs
  ) where

import Prelude hiding (div, head, map, span)
import Data.List qualified as L
import Data.Text qualified as T

import HTML.Attributes.Internal (Attribute, buildAttrMap)
import HTML.Elements.Children (ValidChild)
import HTML.Elements.Internal (Document, HTML, ChildHTML(..))
import HTML.Elements.Tags qualified as Tags
import HTML.Types (NoContent)

noElement :: ChildHTML parent
noElement = Tag_NoElement

comment :: T.Text
        -> ChildHTML parent
comment = Tag_Comment

text :: ValidChild Tags.Text parent
     => T.Text
     -> ChildHTML parent
text = Tag_Text

texts :: ValidChild Tags.Text parent
      => [T.Text]
      -> ChildHTML parent
texts = text . T.unwords

rawHTML :: T.Text -> ChildHTML parent
rawHTML = Tag_RawHTML

customHTML :: T.Text
           -> [Attribute Tags.CustomHTML]
           -> Either NoContent [ChildHTML Tags.CustomHTML]
           -> ChildHTML parent
customHTML elemName attrs content =
  Tag_CustomHTML elemName (buildAttrMap attrs) content

a :: ValidChild Tags.Anchor parent
  => [Attribute Tags.Anchor]
  -> [ChildHTML Tags.Anchor]
  -> ChildHTML parent
a = Tag_Anchor . buildAttrMap

abbr :: ValidChild Tags.Abbreviation parent
     => [Attribute Tags.Abbreviation]
     -> [ChildHTML Tags.Abbreviation]
     -> ChildHTML parent
abbr = Tag_Abbreviation . buildAttrMap

address :: ValidChild Tags.ContactAddress parent
        => [Attribute Tags.ContactAddress]
        -> [ChildHTML Tags.ContactAddress]
        -> ChildHTML parent
address = Tag_ContactAddress . buildAttrMap

area :: ValidChild Tags.Area parent
     => [Attribute Tags.Area]
     -> ChildHTML parent
area = Tag_Area . buildAttrMap

article :: ValidChild Tags.Article parent
        => [Attribute Tags.Article]
        -> [ChildHTML Tags.Article]
        -> ChildHTML parent
article = Tag_Article . buildAttrMap

aside :: ValidChild Tags.Aside parent
      => [Attribute Tags.Aside]
      -> [ChildHTML Tags.Aside]
      -> ChildHTML parent
aside = Tag_Aside . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- If the element has a src attribute: zero or more <track> elements followed
-- by transparent content that contains no <audio> or <video> media elements.
--
-- Else: zero or more <source> elements followed by zero or more <track>
-- elements followed by transparent content that contains no <audio> or <video>
-- media elements.
--
audio :: ValidChild Tags.Audio parent
      => [Attribute Tags.Audio]
      -> [ChildHTML Tags.Audio]
      -> ChildHTML parent
audio = Tag_Audio . buildAttrMap

b :: ValidChild Tags.BringAttentionTo parent
  => [Attribute Tags.BringAttentionTo]
  -> [ChildHTML Tags.BringAttentionTo]
  -> ChildHTML parent
b = Tag_BringAttentionTo . buildAttrMap

base :: ValidChild Tags.Base parent
     => [Attribute Tags.Base]
     -> ChildHTML parent
base = Tag_Base . buildAttrMap

bdi :: ValidChild Tags.BidirectionalIsolation parent
    => [Attribute Tags.BidirectionalIsolation]
    -> [ChildHTML Tags.BidirectionalIsolation]
    -> ChildHTML parent
bdi = Tag_BidirectionalIsolation . buildAttrMap

bdo :: ValidChild Tags.BidirectionalOverride parent
    => [Attribute Tags.BidirectionalOverride]
    -> [ChildHTML Tags.BidirectionalOverride]
    -> ChildHTML parent
bdo = Tag_BidirectionalOverride . buildAttrMap

blockquote :: ValidChild Tags.Blockquote parent
           => [Attribute Tags.Blockquote]
           -> [ChildHTML Tags.Blockquote]
           -> ChildHTML parent
blockquote = Tag_Blockquote . buildAttrMap

body :: ValidChild Tags.Body parent
     => [Attribute Tags.Body]
     -> [ChildHTML Tags.Body]
     -> ChildHTML parent
body = Tag_Body . buildAttrMap

br :: ValidChild Tags.LineBreak parent
   => [Attribute Tags.LineBreak]
   -> ChildHTML parent
br = Tag_LineBreak . buildAttrMap

button :: ValidChild Tags.Button parent
       => [Attribute Tags.Button]
       -> [ChildHTML Tags.Button]
       -> ChildHTML parent
button = Tag_Button . buildAttrMap

canvas :: ValidChild Tags.Canvas parent
       => [Attribute Tags.Canvas]
       -> [ChildHTML Tags.Canvas]
       -> ChildHTML parent
canvas = Tag_Canvas . buildAttrMap

caption :: ValidChild Tags.TableCaption parent
        => [Attribute Tags.TableCaption]
        -> [ChildHTML Tags.TableCaption]
        -> ChildHTML parent
caption = Tag_TableCaption . buildAttrMap

cite :: ValidChild Tags.Citation parent
     => [Attribute Tags.Citation]
     -> [ChildHTML Tags.Citation]
     -> ChildHTML parent
cite = Tag_Citation . buildAttrMap

code :: ValidChild Tags.Code parent
     => [Attribute Tags.Code]
     -> [ChildHTML Tags.Code]
     -> ChildHTML parent
code = Tag_Code . buildAttrMap

col :: ValidChild Tags.TableColumn parent
    => [Attribute Tags.TableColumn]
    -> ChildHTML parent
col = Tag_TableColumn . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- If the span attribute is present: none.
-- If the attribute is not present: zero or more <col> element
--
colgroup :: ValidChild Tags.TableColumnGroup parent
         => [Attribute Tags.TableColumnGroup]
         -> [ChildHTML Tags.TableColumnGroup]
         -> ChildHTML parent
colgroup = Tag_TableColumnGroup . buildAttrMap

data_ :: ValidChild Tags.Data parent
      => [Attribute Tags.Data]
      -> [ChildHTML Tags.Data]
      -> ChildHTML parent
data_ = Tag_Data . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Either phrasing content or zero or more <option> elements.
--
datalist :: ValidChild Tags.DataList parent
         => [Attribute Tags.DataList]
         -> [ChildHTML Tags.DataList]
         -> ChildHTML parent
datalist = Tag_DataList . buildAttrMap

dd :: ValidChild Tags.DescriptionDetails parent
   => [Attribute Tags.DescriptionDetails]
   -> [ChildHTML Tags.DescriptionDetails]
   -> ChildHTML parent
dd = Tag_DescriptionDetails . buildAttrMap

del :: ValidChild Tags.DeletedText parent
    => [Attribute Tags.DeletedText]
    -> [ChildHTML parent]
    -> ChildHTML parent
del = Tag_DeletedText . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- One <summary> element followed by flow content.
--
details :: ValidChild Tags.Details parent
        => [Attribute Tags.Details]
        -> [ChildHTML Tags.Details]
        -> ChildHTML parent
details = Tag_Details . buildAttrMap

dfn :: ValidChild Tags.Definition parent
    => [Attribute Tags.Definition]
    -> [ChildHTML Tags.Definition]
    -> ChildHTML parent
dfn = Tag_Definition . buildAttrMap

dialog :: ValidChild Tags.Dialog parent
       => [Attribute Tags.Dialog]
       -> [ChildHTML Tags.Dialog]
       -> ChildHTML parent
dialog = Tag_Dialog . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Flow content.
--
-- Or (in WHATWG HTML): If the parent is a <dl> element: one or more <dt>
-- elements followed by one or more <dd> elements, optionally intermixed with
-- <script> and <template> elements.
--
div :: ValidChild Tags.Division parent
    => [Attribute Tags.Division]
    -> [ChildHTML Tags.Division]
    -> ChildHTML parent
div = Tag_Division . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Either: Zero or more groups each consisting of one or more <dt> elements
-- followed by one or more <dd> elements, optionally intermixed with <script>
-- and <template> elements.
--
-- Or: (in WHATWG HTML, W3C HTML 5.2 and later) One or more <div> elements,
-- optionally intermixed with <script> and <template> elements.
--
dl :: ValidChild Tags.DescriptionList parent
   => [Attribute Tags.DescriptionList]
   -> [ChildHTML Tags.DescriptionList]
   -> ChildHTML parent
dl = Tag_DescriptionList . buildAttrMap

dt :: ValidChild Tags.DescriptionTerm parent
   => [Attribute Tags.DescriptionTerm]
   -> [ChildHTML Tags.DescriptionTerm]
   -> ChildHTML parent
dt = Tag_DescriptionTerm . buildAttrMap

em :: ValidChild Tags.Emphasis parent
   => [Attribute Tags.Emphasis]
   -> [ChildHTML Tags.Emphasis]
   -> ChildHTML parent
em = Tag_Emphasis . buildAttrMap

embed :: ValidChild Tags.Embed parent
      => [Attribute Tags.Embed]
      -> ChildHTML parent
embed = Tag_Embed . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- An optional <legend> element, followed by flow content.
--
fieldset :: ValidChild Tags.Fieldset parent
         => [Attribute Tags.Fieldset]
         -> [ChildHTML Tags.Fieldset]
         -> ChildHTML parent
fieldset = Tag_Fieldset . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- A <figcaption> element, followed by flow content; or flow content followed
-- by a <figcaption> element; or flow content.
--
figcaption :: ValidChild Tags.FigureCaption parent
           => [Attribute Tags.FigureCaption]
           -> [ChildHTML Tags.FigureCaption]
           -> ChildHTML parent
figcaption = Tag_FigureCaption . buildAttrMap

figure :: ValidChild Tags.Figure parent
       => [Attribute Tags.Figure]
       -> [ChildHTML Tags.Figure]
       -> ChildHTML parent
figure = Tag_Figure . buildAttrMap

footer :: ValidChild Tags.Footer parent
       => [Attribute Tags.Footer]
       -> [ChildHTML Tags.Footer]
       -> ChildHTML parent
footer = Tag_Footer . buildAttrMap

-- This would be a good candidate for a safe constructor, particularly in
-- relation to <input> elements.
--
form :: ValidChild Tags.Form parent
     => [Attribute Tags.Form]
     -> [ChildHTML Tags.Form]
     -> ChildHTML parent
form = Tag_Form . buildAttrMap

h1 :: ValidChild Tags.H1 parent
   => [Attribute Tags.H1]
   -> [ChildHTML Tags.H1]
   -> ChildHTML parent
h1 = Tag_H1 . buildAttrMap

h2 :: ValidChild Tags.H2 parent
   => [Attribute Tags.H2]
   -> [ChildHTML Tags.H2]
   -> ChildHTML parent
h2 = Tag_H2 . buildAttrMap

h3 :: ValidChild Tags.H3 parent
   => [Attribute Tags.H3]
   -> [ChildHTML Tags.H3]
   -> ChildHTML parent
h3 = Tag_H3 . buildAttrMap

h4 :: ValidChild Tags.H4 parent
   => [Attribute Tags.H4]
   -> [ChildHTML Tags.H4]
   -> ChildHTML parent
h4 = Tag_H4 . buildAttrMap

h5 :: ValidChild Tags.H5 parent
   => [Attribute Tags.H5]
   -> [ChildHTML Tags.H5]
   -> ChildHTML parent
h5 = Tag_H5 . buildAttrMap

h6 :: ValidChild Tags.H6 parent
   => [Attribute Tags.H6]
   -> [ChildHTML Tags.H6]
   -> ChildHTML parent
h6 = Tag_H6 . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- If the document is an <iframe> srcdoc document, or if title information is
-- available from a higher level protocol (like the subject line in HTML
-- email), zero or more elements of metadata content.
--
-- Otherwise, one or more elements of metadata content where exactly one is a
-- <title> element.
--
head :: ValidChild Tags.Head parent
     => [Attribute Tags.Head]
     -> [ChildHTML Tags.Head]
     -> ChildHTML parent
head = Tag_Head . buildAttrMap

header :: ValidChild Tags.Header parent
       => [Attribute Tags.Header]
       -> [ChildHTML Tags.Header]
       -> ChildHTML parent
header = Tag_Header . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <p> elements, followed by one h1, h2, h3, h4, h5, or h6
-- element, followed by zero or more <p> elements.
--
hgroup :: ValidChild Tags.HeadingGroup parent
       => [Attribute Tags.HeadingGroup]
       -> [ChildHTML Tags.HeadingGroup]
       -> ChildHTML parent
hgroup = Tag_HeadingGroup . buildAttrMap

hr :: ValidChild Tags.HorizontalRule parent
   => [Attribute Tags.HorizontalRule]
   -> ChildHTML parent
hr = Tag_HorizontalRule . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <p> elements, followed by one h1, h2, h3, h4, h5, or h6
-- element, followed by zero or more <p> elements.
--
html :: [Attribute Tags.Html]
     -> [ChildHTML Tags.Html]
     -> Document
html = Tag_Html . buildAttrMap

i :: ValidChild Tags.IdiomaticText parent
  => [Attribute Tags.IdiomaticText]
  -> [ChildHTML Tags.IdiomaticText]
  -> ChildHTML parent
i = Tag_IdiomaticText . buildAttrMap

iframe :: ValidChild Tags.IFrame parent
       => [Attribute Tags.IFrame]
       -> ChildHTML parent
iframe = Tag_IFrame . buildAttrMap

img :: ValidChild Tags.Image parent
    => [Attribute Tags.Image]
    -> ChildHTML parent
img = Tag_Image . buildAttrMap

input :: ValidChild Tags.Input parent
      => [Attribute Tags.Input]
      -> ChildHTML parent
input = Tag_Input . buildAttrMap

ins :: ValidChild Tags.InsertedText parent
    => [Attribute Tags.InsertedText]
    -> [ChildHTML parent]
    -> ChildHTML parent
ins = Tag_InsertedText . buildAttrMap

kbd :: ValidChild Tags.KeyboardInput parent
    => [Attribute Tags.KeyboardInput]
    -> [ChildHTML Tags.KeyboardInput]
    -> ChildHTML parent
kbd = Tag_KeyboardInput . buildAttrMap

-- Phrasing content, but no descendant label elements. No labelable elements
-- other than the labeled control are allowed.
--
-- labeledInput :: ValidChild Tags.Label parent
--              => [Attribute Tags.Label]
--              -> T.Text
--              -> ChildHTML Tags.Label
--              -> ChildHTML parent
-- labeledInput attrs name =
--   label (A.for name : attrs)
--     [ addAttribute child $ A.name name
--     ]
--
label :: ValidChild Tags.Label parent
      => [Attribute Tags.Label]
      -> [ChildHTML Tags.Label]
      -> ChildHTML parent
label = Tag_Label . buildAttrMap

legend :: ValidChild Tags.Legend parent
       => [Attribute Tags.Legend]
       -> [ChildHTML Tags.Legend]
       -> ChildHTML parent
legend = Tag_Legend . buildAttrMap

li :: ValidChild Tags.ListItem parent
   => [Attribute Tags.ListItem]
   -> [ChildHTML Tags.ListItem]
   -> ChildHTML parent
li = Tag_ListItem . buildAttrMap

link :: ValidChild Tags.Link parent
     => [Attribute Tags.Link]
     -> ChildHTML parent
link = Tag_Link . buildAttrMap

main :: ValidChild Tags.Main parent
     => [Attribute Tags.Main]
     -> [ChildHTML Tags.Main]
     -> ChildHTML parent
main = Tag_Main . buildAttrMap

map :: ValidChild Tags.Map parent
    => [Attribute Tags.Map]
    -> [ChildHTML Tags.Map]
    -> ChildHTML parent
map = Tag_Map . buildAttrMap

mark :: ValidChild Tags.Mark parent
     => [Attribute Tags.Mark]
     -> [ChildHTML Tags.Mark]
     -> ChildHTML parent
mark = Tag_Mark . buildAttrMap

menu :: ValidChild Tags.Menu parent
     => [Attribute Tags.Menu]
     -> [ChildHTML Tags.Menu]
     -> ChildHTML parent
menu = Tag_Menu . buildAttrMap

meta :: ValidChild Tags.Meta parent
     => [Attribute Tags.Meta]
     -> ChildHTML parent
meta = Tag_Meta . buildAttrMap

meter :: ValidChild Tags.Meter parent
      => [Attribute Tags.Meter]
      -> [ChildHTML Tags.Meter]
      -> ChildHTML parent
meter = Tag_Meter . buildAttrMap

nav :: ValidChild Tags.Nav parent
    => [Attribute Tags.Nav]
    -> [ChildHTML Tags.Nav]
    -> ChildHTML parent
nav = Tag_Nav . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- When scripting is disabled and when it is a descendant of the <head>
-- element: in any order, zero or more <link> elements, zero or more <style>
-- elements, and zero or more <meta> elements.
--
-- When scripting is disabled and when it isn't a descendant of the <head>
-- element: any transparent content, but no <noscript> element must be among
-- its descendants.
--
-- Otherwise: flow content or phrasing content.
--
noscript :: ValidChild Tags.NoScript parent
         => [Attribute Tags.NoScript]
         -> [ChildHTML Tags.NoScript]
         -> ChildHTML parent
noscript = Tag_NoScript . buildAttrMap

object :: ValidChild Tags.Object parent
       => [Attribute Tags.Object]
       -> [ChildHTML Tags.Object]
       -> ChildHTML parent
object = Tag_Object . buildAttrMap

ol :: ValidChild Tags.OrderedList parent
   => [Attribute Tags.OrderedList]
   -> [ChildHTML Tags.OrderedList]
   -> ChildHTML parent
ol = Tag_OrderedList . buildAttrMap

optgroup :: ValidChild Tags.OptionGroup parent
         => [Attribute Tags.OptionGroup]
         -> [ChildHTML Tags.OptionGroup]
         -> ChildHTML parent
optgroup = Tag_OptionGroup . buildAttrMap

option :: ValidChild Tags.Option parent
       => [Attribute Tags.Option]
       -> [ChildHTML Tags.Option]
       -> ChildHTML parent
option = Tag_Option . buildAttrMap

output :: ValidChild Tags.Output parent
       => [Attribute Tags.Output]
       -> [ChildHTML Tags.Output]
       -> ChildHTML parent
output = Tag_Output . buildAttrMap

p :: ValidChild Tags.Paragraph parent
  => [Attribute Tags.Paragraph]
  -> [ChildHTML Tags.Paragraph]
  -> ChildHTML parent
p = Tag_Paragraph . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <source> elements, followed by one <img> element,
-- optionally intermixed with script-supporting elements.
--
picture :: ValidChild Tags.Picture parent
        => [Attribute Tags.Picture]
        -> [ChildHTML Tags.Picture]
        -> ChildHTML parent
picture = Tag_Picture . buildAttrMap

pre :: ValidChild Tags.PreformattedText parent
    => [Attribute Tags.PreformattedText]
    -> [ChildHTML Tags.PreformattedText]
    -> ChildHTML parent
pre = Tag_PreformattedText . buildAttrMap

progress :: ValidChild Tags.Progress parent
         => [Attribute Tags.Progress]
         -> [ChildHTML Tags.Progress]
         -> ChildHTML parent
progress = Tag_Progress . buildAttrMap

q :: ValidChild Tags.Quotation parent
  => [Attribute Tags.Quotation]
  -> [ChildHTML Tags.Quotation]
  -> ChildHTML parent
q = Tag_Quotation . buildAttrMap

rp :: ValidChild Tags.RubyParenthesis parent
   => [Attribute Tags.RubyParenthesis]
   -> [ChildHTML Tags.RubyParenthesis]
   -> ChildHTML parent
rp = Tag_RubyParenthesis . buildAttrMap

rt :: ValidChild Tags.RubyText parent
   => [Attribute Tags.RubyText]
   -> [ChildHTML Tags.RubyText]
   -> ChildHTML parent
rt = Tag_RubyText . buildAttrMap

ruby :: ValidChild Tags.Ruby parent
     => [Attribute Tags.Ruby]
     -> [ChildHTML Tags.Ruby]
     -> ChildHTML parent
ruby = Tag_Ruby . buildAttrMap

s :: ValidChild Tags.Strikethrough parent
  => [Attribute Tags.Strikethrough]
  -> [ChildHTML Tags.Strikethrough]
  -> ChildHTML parent
s = Tag_Strikethrough . buildAttrMap

sample :: ValidChild Tags.Sample parent
       => [Attribute Tags.Sample]
       -> [ChildHTML Tags.Sample]
       -> ChildHTML parent
sample = Tag_Sample . buildAttrMap

-- This should be changed to take script content instead of children.
--
script :: ValidChild Tags.Script parent
       => [Attribute Tags.Script]
       -> [ChildHTML Tags.Script]
       -> ChildHTML parent
script = Tag_Script . buildAttrMap

search :: ValidChild Tags.Search parent
       => [Attribute Tags.Search]
       -> [ChildHTML Tags.Search]
       -> ChildHTML parent
search = Tag_Search . buildAttrMap

section :: ValidChild Tags.Section parent
        => [Attribute Tags.Section]
        -> [ChildHTML Tags.Section]
        -> ChildHTML parent
section = Tag_Section . buildAttrMap

select :: ValidChild Tags.Select parent
       => [Attribute Tags.Select]
       -> [ChildHTML Tags.Select]
       -> ChildHTML parent
select = Tag_Select . buildAttrMap

slot :: ValidChild Tags.Slot parent
     => [Attribute Tags.Slot]
     -> [ChildHTML parent]
     -> ChildHTML parent
slot = Tag_Slot . buildAttrMap

small :: ValidChild Tags.SideComment parent
      => [Attribute Tags.SideComment]
      -> [ChildHTML Tags.SideComment]
      -> ChildHTML parent
small = Tag_SideComment . buildAttrMap

source :: ValidChild Tags.Source parent
       => [Attribute Tags.Source]
       -> ChildHTML parent
source = Tag_Source . buildAttrMap

span :: ValidChild Tags.Span parent
     => [Attribute Tags.Span]
     -> [ChildHTML Tags.Span]
     -> ChildHTML parent
span = Tag_Span . buildAttrMap

strong :: ValidChild Tags.Strong parent
       => [Attribute Tags.Strong]
       -> [ChildHTML Tags.Strong]
       -> ChildHTML parent
strong = Tag_Strong . buildAttrMap

-- This should be changed to take CSS content instead of children.
--
style :: ValidChild Tags.Style parent
      => [Attribute Tags.Style]
      -> [ChildHTML Tags.Style]
      -> ChildHTML parent
style = Tag_Style . buildAttrMap

sub :: ValidChild Tags.Subscript parent
    => [Attribute Tags.Subscript]
    -> [ChildHTML Tags.Subscript]
    -> ChildHTML parent
sub = Tag_Subscript . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- 	Phrasing content or one element of Heading content
--
summary :: ValidChild Tags.Summary parent
        => [Attribute Tags.Summary]
        -> [ChildHTML Tags.Summary]
        -> ChildHTML parent
summary = Tag_Summary . buildAttrMap

sup :: ValidChild Tags.Superscript parent
    => [Attribute Tags.Superscript]
    -> [ChildHTML Tags.Superscript]
    -> ChildHTML parent
sup = Tag_Superscript . buildAttrMap

table :: ValidChild Tags.Table parent
      => [Attribute Tags.Table]
      -> [ChildHTML Tags.Table]
      -> ChildHTML parent
table = Tag_Table . buildAttrMap

tbody :: ValidChild Tags.TableBody parent
      => [Attribute Tags.TableBody]
      -> [ChildHTML Tags.TableBody]
      -> ChildHTML parent
tbody = Tag_TableBody . buildAttrMap

td :: ValidChild Tags.TableDataCell parent
   => [Attribute Tags.TableDataCell]
   -> [ChildHTML Tags.TableDataCell]
   -> ChildHTML parent
td = Tag_TableDataCell . buildAttrMap

-- No content restrictions - remove the constraint?
--
template :: ValidChild Tags.ContentTemplate parent
         => [Attribute Tags.ContentTemplate]
         -> [ChildHTML Tags.ContentTemplate]
         -> ChildHTML parent
template = Tag_ContentTemplate . buildAttrMap

textarea :: ValidChild Tags.TextArea parent
         => [Attribute Tags.TextArea]
         -> [ChildHTML Tags.TextArea]
         -> ChildHTML parent
textarea = Tag_TextArea . buildAttrMap

tfoot :: ValidChild Tags.TableFoot parent
      => [Attribute Tags.TableFoot]
      -> [ChildHTML Tags.TableFoot]
      -> ChildHTML parent
tfoot = Tag_TableFoot . buildAttrMap

th :: ValidChild Tags.TableHeader parent
   => [Attribute Tags.TableHeader]
   -> [ChildHTML Tags.TableHeader]
   -> ChildHTML parent
th = Tag_TableHeader . buildAttrMap

thead :: ValidChild Tags.TableHead parent
      => [Attribute Tags.TableHead]
      -> [ChildHTML Tags.TableHead]
      -> ChildHTML parent
thead = Tag_TableHead . buildAttrMap

time :: ValidChild Tags.Time parent
     => [Attribute Tags.Time]
     -> [ChildHTML Tags.Time]
     -> ChildHTML parent
time = Tag_Time . buildAttrMap

title :: ValidChild Tags.Title parent
      => [Attribute Tags.Title]
      -> [ChildHTML Tags.Title]
      -> ChildHTML parent
title = Tag_Title . buildAttrMap

tr :: ValidChild Tags.TableRow parent
   => [Attribute Tags.TableRow]
   -> [ChildHTML Tags.TableRow]
   -> ChildHTML parent
tr = Tag_TableRow . buildAttrMap

track :: ValidChild Tags.Track parent
      => [Attribute Tags.Track]
      -> ChildHTML parent
track = Tag_Track . buildAttrMap

u :: ValidChild Tags.Underline parent
  => [Attribute Tags.Underline]
  -> [ChildHTML Tags.Underline]
  -> ChildHTML parent
u = Tag_Underline . buildAttrMap

ul :: ValidChild Tags.UnorderedList parent
   => [Attribute Tags.UnorderedList]
   -> [ChildHTML Tags.UnorderedList]
   -> ChildHTML parent
ul = Tag_UnorderedList . buildAttrMap

var :: ValidChild Tags.Variable parent
    => [Attribute Tags.Variable]
    -> [ChildHTML Tags.Variable]
    -> ChildHTML parent
var = Tag_Variable . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- If the element has a src attribute: zero or more <track> elements,
-- followed by transparent content that contains no media
-- elements–that is no <audio> or <video>.
--
-- Else: zero or more <source> elements, followed by zero or more
-- <track> elements, followed by transparent content that contains no
-- media elements–that is no <audio> or <video>.
--
video :: ValidChild Tags.Video parent
      => [Attribute Tags.Video]
      -> [ChildHTML Tags.Video]
      -> ChildHTML parent
video = Tag_Video . buildAttrMap

wbr :: ValidChild Tags.WordBreakOpportunity parent
    => [Attribute Tags.WordBreakOpportunity]
    -> ChildHTML parent
wbr = Tag_WordBreakOpportunity . buildAttrMap

wbrs :: ( ValidChild Tags.WordBreakOpportunity parent
        , ValidChild Tags.Text parent
        )
     => [T.Text]
     -> [ChildHTML parent]
wbrs = L.intersperse (wbr []) . fmap text

-- wbrURL: Add wbr around a URL.
