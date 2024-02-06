{-# LANGUAGE DataKinds #-}

module HTML.Elements
  ( Document
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
import HTML.Elements.Internal (ChildHTML(..), Document(..))
import HTML.Elements.Tags qualified as Tags
import HTML.Types (NoContent)

noElement :: ChildHTML parent grandparent
noElement = Tag_NoElement

comment :: T.Text
        -> ChildHTML parent grandparent
comment = Tag_Comment

text :: ValidChild Tags.Text parent
     => T.Text
     -> ChildHTML parent grandparent
text = Tag_Text

texts :: ValidChild Tags.Text parent
      => [T.Text]
      -> ChildHTML parent grandparent
texts = text . T.unwords

rawHTML :: T.Text -> ChildHTML parent grandparent
rawHTML = Tag_RawHTML

customHTML :: T.Text
           -> [Attribute Tags.CustomHTML]
           -> Either NoContent [ChildHTML Tags.CustomHTML grandparent]
           -> ChildHTML parent grandparent
customHTML elemName attrs content =
  Tag_CustomHTML elemName (buildAttrMap attrs) content

a :: ValidChild Tags.Anchor parent
  => [Attribute Tags.Anchor]
  -> [ChildHTML Tags.Anchor grandparent]
  -> ChildHTML parent grandparent
a = Tag_Anchor . buildAttrMap

abbr :: ValidChild Tags.Abbreviation parent
     => [Attribute Tags.Abbreviation]
     -> [ChildHTML Tags.Abbreviation grandparent]
     -> ChildHTML parent grandparent
abbr = Tag_Abbreviation . buildAttrMap

address :: ValidChild Tags.ContactAddress parent
        => [Attribute Tags.ContactAddress]
        -> [ChildHTML Tags.ContactAddress grandparent]
        -> ChildHTML parent grandparent
address = Tag_ContactAddress . buildAttrMap

area :: ValidChild Tags.Area parent
     => [Attribute Tags.Area]
     -> ChildHTML parent grandparent
area = Tag_Area . buildAttrMap

article :: ValidChild Tags.Article parent
        => [Attribute Tags.Article]
        -> [ChildHTML Tags.Article grandparent]
        -> ChildHTML parent grandparent
article = Tag_Article . buildAttrMap

aside :: ValidChild Tags.Aside parent
      => [Attribute Tags.Aside]
      -> [ChildHTML Tags.Aside grandparent]
      -> ChildHTML parent grandparent
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
      -> [ChildHTML Tags.Audio grandparent]
      -> ChildHTML parent grandparent
audio = Tag_Audio . buildAttrMap

b :: ValidChild Tags.BringAttentionTo parent
  => [Attribute Tags.BringAttentionTo]
  -> [ChildHTML Tags.BringAttentionTo grandparent]
  -> ChildHTML parent grandparent
b = Tag_BringAttentionTo . buildAttrMap

base :: ValidChild Tags.Base parent
     => [Attribute Tags.Base]
     -> ChildHTML parent grandparent
base = Tag_Base . buildAttrMap

bdi :: ValidChild Tags.BidirectionalIsolation parent
    => [Attribute Tags.BidirectionalIsolation]
    -> [ChildHTML Tags.BidirectionalIsolation grandparent]
    -> ChildHTML parent grandparent
bdi = Tag_BidirectionalIsolation . buildAttrMap

bdo :: ValidChild Tags.BidirectionalOverride parent
    => [Attribute Tags.BidirectionalOverride]
    -> [ChildHTML Tags.BidirectionalOverride grandparent]
    -> ChildHTML parent grandparent
bdo = Tag_BidirectionalOverride . buildAttrMap

blockquote :: ValidChild Tags.Blockquote parent
           => [Attribute Tags.Blockquote]
           -> [ChildHTML Tags.Blockquote grandparent]
           -> ChildHTML parent grandparent
blockquote = Tag_Blockquote . buildAttrMap

body :: ValidChild Tags.Body parent
     => [Attribute Tags.Body]
     -> [ChildHTML Tags.Body grandparent]
     -> ChildHTML parent grandparent
body = Tag_Body . buildAttrMap

br :: ValidChild Tags.LineBreak parent
   => [Attribute Tags.LineBreak]
   -> ChildHTML parent grandparent
br = Tag_LineBreak . buildAttrMap

button :: ValidChild Tags.Button parent
       => [Attribute Tags.Button]
       -> [ChildHTML Tags.Button grandparent]
       -> ChildHTML parent grandparent
button = Tag_Button . buildAttrMap

canvas :: ValidChild Tags.Canvas parent
       => [Attribute Tags.Canvas]
       -> [ChildHTML Tags.Canvas grandparent]
       -> ChildHTML parent grandparent
canvas = Tag_Canvas . buildAttrMap

caption :: ValidChild Tags.TableCaption parent
        => [Attribute Tags.TableCaption]
        -> [ChildHTML Tags.TableCaption grandparent]
        -> ChildHTML parent grandparent
caption = Tag_TableCaption . buildAttrMap

cite :: ValidChild Tags.Citation parent
     => [Attribute Tags.Citation]
     -> [ChildHTML Tags.Citation grandparent]
     -> ChildHTML parent grandparent
cite = Tag_Citation . buildAttrMap

code :: ValidChild Tags.Code parent
     => [Attribute Tags.Code]
     -> [ChildHTML Tags.Code grandparent]
     -> ChildHTML parent grandparent
code = Tag_Code . buildAttrMap

col :: ValidChild Tags.TableColumn parent
    => [Attribute Tags.TableColumn]
    -> ChildHTML parent grandparent
col = Tag_TableColumn . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- If the span attribute is present: none.
-- If the attribute is not present: zero or more <col> element
--
colgroup :: ValidChild Tags.TableColumnGroup parent
         => [Attribute Tags.TableColumnGroup]
         -> [ChildHTML Tags.TableColumnGroup grandparent]
         -> ChildHTML parent grandparent
colgroup = Tag_TableColumnGroup . buildAttrMap

data_ :: ValidChild Tags.Data parent
      => [Attribute Tags.Data]
      -> [ChildHTML Tags.Data grandparent]
      -> ChildHTML parent grandparent
data_ = Tag_Data . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Either phrasing content or zero or more <option> elements.
--
datalist :: ValidChild Tags.DataList parent
         => [Attribute Tags.DataList]
         -> [ChildHTML Tags.DataList grandparent]
         -> ChildHTML parent grandparent
datalist = Tag_DataList . buildAttrMap

dd :: ValidChild Tags.DescriptionDetails parent
   => [Attribute Tags.DescriptionDetails]
   -> [ChildHTML Tags.DescriptionDetails grandparent]
   -> ChildHTML parent grandparent
dd = Tag_DescriptionDetails . buildAttrMap

del :: ValidChild Tags.DeletedText parent
    => [Attribute Tags.DeletedText]
    -> [ChildHTML parent grandparent]
    -> ChildHTML parent grandparent
del = Tag_DeletedText . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- One <summary> element followed by flow content.
--
details :: ValidChild Tags.Details parent
        => [Attribute Tags.Details]
        -> [ChildHTML Tags.Details grandparent]
        -> ChildHTML parent grandparent
details = Tag_Details . buildAttrMap

dfn :: ValidChild Tags.Definition parent
    => [Attribute Tags.Definition]
    -> [ChildHTML Tags.Definition grandparent]
    -> ChildHTML parent grandparent
dfn = Tag_Definition . buildAttrMap

dialog :: ValidChild Tags.Dialog parent
       => [Attribute Tags.Dialog]
       -> [ChildHTML Tags.Dialog grandparent]
       -> ChildHTML parent grandparent
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
    -> [ChildHTML Tags.Division grandparent]
    -> ChildHTML parent grandparent
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
   -> [ChildHTML Tags.DescriptionList grandparent]
   -> ChildHTML parent grandparent
dl = Tag_DescriptionList . buildAttrMap

dt :: ValidChild Tags.DescriptionTerm parent
   => [Attribute Tags.DescriptionTerm]
   -> [ChildHTML Tags.DescriptionTerm grandparent]
   -> ChildHTML parent grandparent
dt = Tag_DescriptionTerm . buildAttrMap

em :: ValidChild Tags.Emphasis parent
   => [Attribute Tags.Emphasis]
   -> [ChildHTML Tags.Emphasis grandparent]
   -> ChildHTML parent grandparent
em = Tag_Emphasis . buildAttrMap

embed :: ValidChild Tags.Embed parent
      => [Attribute Tags.Embed]
      -> ChildHTML parent grandparent
embed = Tag_Embed . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- An optional <legend> element, followed by flow content.
--
fieldset :: ValidChild Tags.Fieldset parent
         => [Attribute Tags.Fieldset]
         -> [ChildHTML Tags.Fieldset grandparent]
         -> ChildHTML parent grandparent
fieldset = Tag_Fieldset . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- A <figcaption> element, followed by flow content; or flow content followed
-- by a <figcaption> element; or flow content.
--
figcaption :: ValidChild Tags.FigureCaption parent
           => [Attribute Tags.FigureCaption]
           -> [ChildHTML Tags.FigureCaption grandparent]
           -> ChildHTML parent grandparent
figcaption = Tag_FigureCaption . buildAttrMap

figure :: ValidChild Tags.Figure parent
       => [Attribute Tags.Figure]
       -> [ChildHTML Tags.Figure grandparent]
       -> ChildHTML parent grandparent
figure = Tag_Figure . buildAttrMap

footer :: ValidChild Tags.Footer parent
       => [Attribute Tags.Footer]
       -> [ChildHTML Tags.Footer grandparent]
       -> ChildHTML parent grandparent
footer = Tag_Footer . buildAttrMap

-- This would be a good candidate for a safe constructor, particularly in
-- relation to <input> elements.
--
form :: ValidChild Tags.Form parent
     => [Attribute Tags.Form]
     -> [ChildHTML Tags.Form grandparent]
     -> ChildHTML parent grandparent
form = Tag_Form . buildAttrMap

h1 :: ValidChild Tags.H1 parent
   => [Attribute Tags.H1]
   -> [ChildHTML Tags.H1 grandparent]
   -> ChildHTML parent grandparent
h1 = Tag_H1 . buildAttrMap

h2 :: ValidChild Tags.H2 parent
   => [Attribute Tags.H2]
   -> [ChildHTML Tags.H2 grandparent]
   -> ChildHTML parent grandparent
h2 = Tag_H2 . buildAttrMap

h3 :: ValidChild Tags.H3 parent
   => [Attribute Tags.H3]
   -> [ChildHTML Tags.H3 grandparent]
   -> ChildHTML parent grandparent
h3 = Tag_H3 . buildAttrMap

h4 :: ValidChild Tags.H4 parent
   => [Attribute Tags.H4]
   -> [ChildHTML Tags.H4 grandparent]
   -> ChildHTML parent grandparent
h4 = Tag_H4 . buildAttrMap

h5 :: ValidChild Tags.H5 parent
   => [Attribute Tags.H5]
   -> [ChildHTML Tags.H5 grandparent]
   -> ChildHTML parent grandparent
h5 = Tag_H5 . buildAttrMap

h6 :: ValidChild Tags.H6 parent
   => [Attribute Tags.H6]
   -> [ChildHTML Tags.H6 grandparent]
   -> ChildHTML parent grandparent
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
     -> [ChildHTML Tags.Head grandparent]
     -> ChildHTML parent grandparent
head = Tag_Head . buildAttrMap

header :: ValidChild Tags.Header parent
       => [Attribute Tags.Header]
       -> [ChildHTML Tags.Header grandparent]
       -> ChildHTML parent grandparent
header = Tag_Header . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <p> elements, followed by one h1, h2, h3, h4, h5, or h6
-- element, followed by zero or more <p> elements.
--
hgroup :: ValidChild Tags.HeadingGroup parent
       => [Attribute Tags.HeadingGroup]
       -> [ChildHTML Tags.HeadingGroup grandparent]
       -> ChildHTML parent grandparent
hgroup = Tag_HeadingGroup . buildAttrMap

hr :: ValidChild Tags.HorizontalRule parent
   => [Attribute Tags.HorizontalRule]
   -> ChildHTML parent grandparent
hr = Tag_HorizontalRule . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <p> elements, followed by one h1, h2, h3, h4, h5, or h6
-- element, followed by zero or more <p> elements.
--
html :: [Attribute Tags.Html]
     -> [ChildHTML Tags.Html Tags.Document]
     -> Document
html = Tag_Html . buildAttrMap

i :: ValidChild Tags.IdiomaticText parent
  => [Attribute Tags.IdiomaticText]
  -> [ChildHTML Tags.IdiomaticText grandparent]
  -> ChildHTML parent grandparent
i = Tag_IdiomaticText . buildAttrMap

iframe :: ValidChild Tags.IFrame parent
       => [Attribute Tags.IFrame]
       -> ChildHTML parent grandparent
iframe = Tag_IFrame . buildAttrMap

img :: ValidChild Tags.Image parent
    => [Attribute Tags.Image]
    -> ChildHTML parent grandparent
img = Tag_Image . buildAttrMap

input :: ValidChild Tags.Input parent
      => [Attribute Tags.Input]
      -> ChildHTML parent grandparent
input = Tag_Input . buildAttrMap

ins :: ValidChild Tags.InsertedText parent
    => [Attribute Tags.InsertedText]
    -> [ChildHTML parent grandparent]
    -> ChildHTML parent grandparent
ins = Tag_InsertedText . buildAttrMap

kbd :: ValidChild Tags.KeyboardInput parent
    => [Attribute Tags.KeyboardInput]
    -> [ChildHTML Tags.KeyboardInput grandparent]
    -> ChildHTML parent grandparent
kbd = Tag_KeyboardInput . buildAttrMap

-- Phrasing content, but no descendant label elements. No labelable elements
-- other than the labeled control are allowed.
--
-- labeledInput :: ValidChild Tags.Label parent
--              => [Attribute Tags.Label]
--              -> T.Text
--              -> ChildHTML Tags.Label
--              -> ChildHTML parent grandparent
-- labeledInput attrs name =
--   label (A.for name : attrs)
--     [ addAttribute child $ A.name name
--     ]
--
label :: ValidChild Tags.Label parent
      => [Attribute Tags.Label]
      -> [ChildHTML Tags.Label grandparent]
      -> ChildHTML parent grandparent
label = Tag_Label . buildAttrMap

legend :: ValidChild Tags.Legend parent
       => [Attribute Tags.Legend]
       -> [ChildHTML Tags.Legend grandparent]
       -> ChildHTML parent grandparent
legend = Tag_Legend . buildAttrMap

li :: ValidChild Tags.ListItem parent
   => [Attribute Tags.ListItem]
   -> [ChildHTML Tags.ListItem grandparent]
   -> ChildHTML parent grandparent
li = Tag_ListItem . buildAttrMap

link :: ValidChild Tags.Link parent
     => [Attribute Tags.Link]
     -> ChildHTML parent grandparent
link = Tag_Link . buildAttrMap

main :: ValidChild Tags.Main parent
     => [Attribute Tags.Main]
     -> [ChildHTML Tags.Main grandparent]
     -> ChildHTML parent grandparent
main = Tag_Main . buildAttrMap

map :: ValidChild Tags.Map parent
    => [Attribute Tags.Map]
    -> [ChildHTML Tags.Map grandparent]
    -> ChildHTML parent grandparent
map = Tag_Map . buildAttrMap

mark :: ValidChild Tags.Mark parent
     => [Attribute Tags.Mark]
     -> [ChildHTML Tags.Mark grandparent]
     -> ChildHTML parent grandparent
mark = Tag_Mark . buildAttrMap

menu :: ValidChild Tags.Menu parent
     => [Attribute Tags.Menu]
     -> [ChildHTML Tags.Menu grandparent]
     -> ChildHTML parent grandparent
menu = Tag_Menu . buildAttrMap

meta :: ValidChild Tags.Meta parent
     => [Attribute Tags.Meta]
     -> ChildHTML parent grandparent
meta = Tag_Meta . buildAttrMap

meter :: ValidChild Tags.Meter parent
      => [Attribute Tags.Meter]
      -> [ChildHTML Tags.Meter grandparent]
      -> ChildHTML parent grandparent
meter = Tag_Meter . buildAttrMap

nav :: ValidChild Tags.Nav parent
    => [Attribute Tags.Nav]
    -> [ChildHTML Tags.Nav grandparent]
    -> ChildHTML parent grandparent
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
         -> [ChildHTML Tags.NoScript grandparent]
         -> ChildHTML parent grandparent
noscript = Tag_NoScript . buildAttrMap

object :: ValidChild Tags.Object parent
       => [Attribute Tags.Object]
       -> [ChildHTML parent grandparent]
       -> ChildHTML parent grandparent
object = Tag_Object . buildAttrMap

ol :: ValidChild Tags.OrderedList parent
   => [Attribute Tags.OrderedList]
   -> [ChildHTML Tags.OrderedList grandparent]
   -> ChildHTML parent grandparent
ol = Tag_OrderedList . buildAttrMap

optgroup :: ValidChild Tags.OptionGroup parent
         => [Attribute Tags.OptionGroup]
         -> [ChildHTML Tags.OptionGroup grandparent]
         -> ChildHTML parent grandparent
optgroup = Tag_OptionGroup . buildAttrMap

option :: ValidChild Tags.Option parent
       => [Attribute Tags.Option]
       -> [ChildHTML Tags.Option grandparent]
       -> ChildHTML parent grandparent
option = Tag_Option . buildAttrMap

output :: ValidChild Tags.Output parent
       => [Attribute Tags.Output]
       -> [ChildHTML Tags.Output grandparent]
       -> ChildHTML parent grandparent
output = Tag_Output . buildAttrMap

p :: ValidChild Tags.Paragraph parent
  => [Attribute Tags.Paragraph]
  -> [ChildHTML Tags.Paragraph grandparent]
  -> ChildHTML parent grandparent
p = Tag_Paragraph . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <source> elements, followed by one <img> element,
-- optionally intermixed with script-supporting elements.
--
picture :: ValidChild Tags.Picture parent
        => [Attribute Tags.Picture]
        -> [ChildHTML Tags.Picture grandparent]
        -> ChildHTML parent grandparent
picture = Tag_Picture . buildAttrMap

pre :: ValidChild Tags.PreformattedText parent
    => [Attribute Tags.PreformattedText]
    -> [ChildHTML Tags.PreformattedText grandparent]
    -> ChildHTML parent grandparent
pre = Tag_PreformattedText . buildAttrMap

progress :: ValidChild Tags.Progress parent
         => [Attribute Tags.Progress]
         -> [ChildHTML Tags.Progress grandparent]
         -> ChildHTML parent grandparent
progress = Tag_Progress . buildAttrMap

q :: ValidChild Tags.Quotation parent
  => [Attribute Tags.Quotation]
  -> [ChildHTML Tags.Quotation grandparent]
  -> ChildHTML parent grandparent
q = Tag_Quotation . buildAttrMap

rp :: ValidChild Tags.RubyParenthesis parent
   => [Attribute Tags.RubyParenthesis]
   -> [ChildHTML Tags.RubyParenthesis grandparent]
   -> ChildHTML parent grandparent
rp = Tag_RubyParenthesis . buildAttrMap

rt :: ValidChild Tags.RubyText parent
   => [Attribute Tags.RubyText]
   -> [ChildHTML Tags.RubyText grandparent]
   -> ChildHTML parent grandparent
rt = Tag_RubyText . buildAttrMap

ruby :: ValidChild Tags.Ruby parent
     => [Attribute Tags.Ruby]
     -> [ChildHTML Tags.Ruby grandparent]
     -> ChildHTML parent grandparent
ruby = Tag_Ruby . buildAttrMap

s :: ValidChild Tags.Strikethrough parent
  => [Attribute Tags.Strikethrough]
  -> [ChildHTML Tags.Strikethrough grandparent]
  -> ChildHTML parent grandparent
s = Tag_Strikethrough . buildAttrMap

sample :: ValidChild Tags.Sample parent
       => [Attribute Tags.Sample]
       -> [ChildHTML Tags.Sample grandparent]
       -> ChildHTML parent grandparent
sample = Tag_Sample . buildAttrMap

-- This should be changed to take script content instead of children.
--
script :: ValidChild Tags.Script parent
       => [Attribute Tags.Script]
       -> T.Text
       -> ChildHTML parent grandparent
script = Tag_Script . buildAttrMap

search :: ValidChild Tags.Search parent
       => [Attribute Tags.Search]
       -> [ChildHTML Tags.Search grandparent]
       -> ChildHTML parent grandparent
search = Tag_Search . buildAttrMap

section :: ValidChild Tags.Section parent
        => [Attribute Tags.Section]
        -> [ChildHTML Tags.Section grandparent]
        -> ChildHTML parent grandparent
section = Tag_Section . buildAttrMap

select :: ValidChild Tags.Select parent
       => [Attribute Tags.Select]
       -> [ChildHTML Tags.Select grandparent]
       -> ChildHTML parent grandparent
select = Tag_Select . buildAttrMap

slot :: ValidChild Tags.Slot parent
     => [Attribute Tags.Slot]
     -> [ChildHTML parent grandparent]
     -> ChildHTML parent grandparent
slot = Tag_Slot . buildAttrMap

small :: ValidChild Tags.SideComment parent
      => [Attribute Tags.SideComment]
      -> [ChildHTML Tags.SideComment grandparent]
      -> ChildHTML parent grandparent
small = Tag_SideComment . buildAttrMap

source :: ValidChild Tags.Source parent
       => [Attribute Tags.Source]
       -> ChildHTML parent grandparent
source = Tag_Source . buildAttrMap

span :: ValidChild Tags.Span parent
     => [Attribute Tags.Span]
     -> [ChildHTML Tags.Span grandparent]
     -> ChildHTML parent grandparent
span = Tag_Span . buildAttrMap

strong :: ValidChild Tags.Strong parent
       => [Attribute Tags.Strong]
       -> [ChildHTML Tags.Strong grandparent]
       -> ChildHTML parent grandparent
strong = Tag_Strong . buildAttrMap

-- This should be changed to take CSS content instead of children.
--
style :: ValidChild Tags.Style parent
      => [Attribute Tags.Style]
      -> T.Text
      -> ChildHTML parent grandparent
style = Tag_Style . buildAttrMap

sub :: ValidChild Tags.Subscript parent
    => [Attribute Tags.Subscript]
    -> [ChildHTML Tags.Subscript grandparent]
    -> ChildHTML parent grandparent
sub = Tag_Subscript . buildAttrMap

-- This is a candidate to receive safe constructor(s).
--
-- 	Phrasing content or one element of Heading content
--
summary :: ValidChild Tags.Summary parent
        => [Attribute Tags.Summary]
        -> [ChildHTML Tags.Summary grandparent]
        -> ChildHTML parent grandparent
summary = Tag_Summary . buildAttrMap

sup :: ValidChild Tags.Superscript parent
    => [Attribute Tags.Superscript]
    -> [ChildHTML Tags.Superscript grandparent]
    -> ChildHTML parent grandparent
sup = Tag_Superscript . buildAttrMap

table :: ValidChild Tags.Table parent
      => [Attribute Tags.Table]
      -> [ChildHTML Tags.Table grandparent]
      -> ChildHTML parent grandparent
table = Tag_Table . buildAttrMap

tbody :: ValidChild Tags.TableBody parent
      => [Attribute Tags.TableBody]
      -> [ChildHTML Tags.TableBody grandparent]
      -> ChildHTML parent grandparent
tbody = Tag_TableBody . buildAttrMap

td :: ValidChild Tags.TableDataCell parent
   => [Attribute Tags.TableDataCell]
   -> [ChildHTML Tags.TableDataCell grandparent]
   -> ChildHTML parent grandparent
td = Tag_TableDataCell . buildAttrMap

-- No content restrictions - remove the constraint?
--
template :: ValidChild Tags.ContentTemplate parent
         => [Attribute Tags.ContentTemplate]
         -> [ChildHTML Tags.ContentTemplate grandparent]
         -> ChildHTML parent grandparent
template = Tag_ContentTemplate . buildAttrMap

textarea :: ValidChild Tags.TextArea parent
         => [Attribute Tags.TextArea]
         -> [ChildHTML Tags.TextArea grandparent]
         -> ChildHTML parent grandparent
textarea = Tag_TextArea . buildAttrMap

tfoot :: ValidChild Tags.TableFoot parent
      => [Attribute Tags.TableFoot]
      -> [ChildHTML Tags.TableFoot grandparent]
      -> ChildHTML parent grandparent
tfoot = Tag_TableFoot . buildAttrMap

th :: ValidChild Tags.TableHeader parent
   => [Attribute Tags.TableHeader]
   -> [ChildHTML Tags.TableHeader grandparent]
   -> ChildHTML parent grandparent
th = Tag_TableHeader . buildAttrMap

thead :: ValidChild Tags.TableHead parent
      => [Attribute Tags.TableHead]
      -> [ChildHTML Tags.TableHead grandparent]
      -> ChildHTML parent grandparent
thead = Tag_TableHead . buildAttrMap

time :: ValidChild Tags.Time parent
     => [Attribute Tags.Time]
     -> [ChildHTML Tags.Time grandparent]
     -> ChildHTML parent grandparent
time = Tag_Time . buildAttrMap

title :: ValidChild Tags.Title parent
      => [Attribute Tags.Title]
      -> [ChildHTML Tags.Title grandparent]
      -> ChildHTML parent grandparent
title = Tag_Title . buildAttrMap

tr :: ValidChild Tags.TableRow parent
   => [Attribute Tags.TableRow]
   -> [ChildHTML Tags.TableRow grandparent]
   -> ChildHTML parent grandparent
tr = Tag_TableRow . buildAttrMap

track :: ValidChild Tags.Track parent
      => [Attribute Tags.Track]
      -> ChildHTML parent grandparent
track = Tag_Track . buildAttrMap

u :: ValidChild Tags.Underline parent
  => [Attribute Tags.Underline]
  -> [ChildHTML Tags.Underline grandparent]
  -> ChildHTML parent grandparent
u = Tag_Underline . buildAttrMap

ul :: ValidChild Tags.UnorderedList parent
   => [Attribute Tags.UnorderedList]
   -> [ChildHTML Tags.UnorderedList grandparent]
   -> ChildHTML parent grandparent
ul = Tag_UnorderedList . buildAttrMap

var :: ValidChild Tags.Variable parent
    => [Attribute Tags.Variable]
    -> [ChildHTML Tags.Variable grandparent]
    -> ChildHTML parent grandparent
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
      -> [ChildHTML Tags.Video grandparent]
      -> ChildHTML parent grandparent
video = Tag_Video . buildAttrMap

wbr :: ValidChild Tags.WordBreakOpportunity parent
    => [Attribute Tags.WordBreakOpportunity]
    -> ChildHTML parent grandparent
wbr = Tag_WordBreakOpportunity . buildAttrMap

wbrs :: ( ValidChild Tags.WordBreakOpportunity parent
        , ValidChild Tags.Text parent
        )
     => [T.Text]
     -> [ChildHTML parent grandparent]
wbrs = L.intersperse (wbr []) . fmap text

-- wbrURL: Add wbr around a URL.
