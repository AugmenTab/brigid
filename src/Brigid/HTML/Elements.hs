{-# LANGUAGE DataKinds #-}

module Brigid.HTML.Elements
  ( HTML
  , AnyHTML
  , ChildHTML
  , Tags.NoElement, noElement
  , Tags.Comment, comment
  , Tags.Text, text, texts
  , Tags.RawHTML, rawHTML
  , Tags.CustomHTML, customHTML
  , ValidChild
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
  , Tags.Sample, samp
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
import Data.Containers.ListUtils (nubOrdOn)
import Data.List qualified as L
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T

import Brigid.HTML.Attributes.Internal (Attribute, attributeText)
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Internal (ChildHTML (..))
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.Types qualified as Types

type HTML = ChildHTML Tags.Document Tags.NoElement

type AnyHTML = ChildHTML Tags.CustomHTML Tags.CustomHTML

noElement :: ChildHTML parent grandparent
noElement = Tag_NoElement

comment :: T.Text -> ChildHTML parent grandparent
comment = Tag_Comment

text :: ValidChild Tags.Text parent grandparent
     => T.Text
     -> ChildHTML parent grandparent
text = Tag_Text

texts :: ValidChild Tags.Text parent grandparent
      => [T.Text]
      -> ChildHTML parent grandparent
texts = text . T.unwords

rawHTML :: T.Text -> ChildHTML parent grandparent
rawHTML = Tag_RawHTML

customHTML :: T.Text
           -> [Attribute Tags.CustomHTML]
           -> Either Types.NoContent [ChildHTML Tags.CustomHTML parent]
           -> ChildHTML parent grandparent
customHTML element =
  Tag_CustomHTML element . nubOrdOn attributeText

a :: ValidChild Tags.Anchor parent grandparent
  => [Attribute Tags.Anchor]
  -> [ChildHTML Tags.Anchor parent]
  -> ChildHTML parent grandparent
a = Tag_Anchor . nubOrdOn attributeText

abbr :: ValidChild Tags.Abbreviation parent grandparent
     => [Attribute Tags.Abbreviation]
     -> [ChildHTML Tags.Abbreviation parent]
     -> ChildHTML parent grandparent
abbr = Tag_Abbreviation . nubOrdOn attributeText

address :: ValidChild Tags.ContactAddress parent grandparent
        => [Attribute Tags.ContactAddress]
        -> [ChildHTML Tags.ContactAddress parent]
        -> ChildHTML parent grandparent
address = Tag_ContactAddress . nubOrdOn attributeText

-- | The <area> tag must be a descendent of a <map> tag, but it need not be a
-- direct descendent. Checking for this isn't possible within Brigid, so the
-- user is responsible for ensuring the correct use of this tag.
--
area :: ValidChild Tags.Area parent grandparent
     => [Attribute Tags.Area]
     -> ChildHTML parent grandparent
area = Tag_Area . nubOrdOn attributeText

article :: ValidChild Tags.Article parent grandparent
        => [Attribute Tags.Article]
        -> [ChildHTML Tags.Article parent]
        -> ChildHTML parent grandparent
article = Tag_Article . nubOrdOn attributeText

aside :: ValidChild Tags.Aside parent grandparent
      => [Attribute Tags.Aside]
      -> [ChildHTML Tags.Aside parent]
      -> ChildHTML parent grandparent
aside = Tag_Aside . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- If the element has a src attribute: zero or more <track> elements followed
-- by transparent content that contains no <audio> or <video> media elements.
--
-- Else: zero or more <source> elements followed by zero or more <track>
-- elements followed by transparent content that contains no <audio> or <video>
-- media elements.
--
audio :: ValidChild Tags.Audio parent grandparent
      => [Attribute Tags.Audio]
      -> [ChildHTML Tags.Audio parent]
      -> ChildHTML parent grandparent
audio = Tag_Audio . nubOrdOn attributeText

b :: ValidChild Tags.BringAttentionTo parent grandparent
  => [Attribute Tags.BringAttentionTo]
  -> [ChildHTML Tags.BringAttentionTo parent]
  -> ChildHTML parent grandparent
b = Tag_BringAttentionTo . nubOrdOn attributeText

base :: ValidChild Tags.Base parent grandparent
     => [Attribute Tags.Base]
     -> ChildHTML parent grandparent
base = Tag_Base . nubOrdOn attributeText

bdi :: ValidChild Tags.BidirectionalIsolation parent grandparent
    => [Attribute Tags.BidirectionalIsolation]
    -> [ChildHTML Tags.BidirectionalIsolation parent]
    -> ChildHTML parent grandparent
bdi = Tag_BidirectionalIsolation . nubOrdOn attributeText

bdo :: ValidChild Tags.BidirectionalOverride parent grandparent
    => [Attribute Tags.BidirectionalOverride]
    -> [ChildHTML Tags.BidirectionalOverride parent]
    -> ChildHTML parent grandparent
bdo = Tag_BidirectionalOverride . nubOrdOn attributeText

blockquote :: ValidChild Tags.Blockquote parent grandparent
           => [Attribute Tags.Blockquote]
           -> [ChildHTML Tags.Blockquote parent]
           -> ChildHTML parent grandparent
blockquote = Tag_Blockquote . nubOrdOn attributeText

body :: ValidChild Tags.Body parent grandparent
     => [Attribute Tags.Body]
     -> [ChildHTML Tags.Body parent]
     -> ChildHTML parent grandparent
body = Tag_Body . nubOrdOn attributeText

br :: ValidChild Tags.LineBreak parent grandparent
   => [Attribute Tags.LineBreak]
   -> ChildHTML parent grandparent
br = Tag_LineBreak . nubOrdOn attributeText

button :: ValidChild Tags.Button parent grandparent
       => [Attribute Tags.Button]
       -> [ChildHTML Tags.Button parent]
       -> ChildHTML parent grandparent
button = Tag_Button . nubOrdOn attributeText

canvas :: ValidChild Tags.Canvas parent grandparent
       => [Attribute Tags.Canvas]
       -> [ChildHTML Tags.Canvas parent]
       -> ChildHTML parent grandparent
canvas = Tag_Canvas . nubOrdOn attributeText

caption :: ValidChild Tags.TableCaption parent grandparent
        => [Attribute Tags.TableCaption]
        -> [ChildHTML Tags.TableCaption parent]
        -> ChildHTML parent grandparent
caption = Tag_TableCaption . nubOrdOn attributeText

cite :: ValidChild Tags.Citation parent grandparent
     => [Attribute Tags.Citation]
     -> [ChildHTML Tags.Citation parent]
     -> ChildHTML parent grandparent
cite = Tag_Citation . nubOrdOn attributeText

code :: ValidChild Tags.Code parent grandparent
     => [Attribute Tags.Code]
     -> [ChildHTML Tags.Code parent]
     -> ChildHTML parent grandparent
code = Tag_Code . nubOrdOn attributeText

col :: ValidChild Tags.TableColumn parent grandparent
    => [Attribute Tags.TableColumn]
    -> ChildHTML parent grandparent
col = Tag_TableColumn . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- If the span attribute is present: none.
-- If the attribute is not present: zero or more <col> element
--
colgroup :: ValidChild Tags.TableColumnGroup parent grandparent
         => [Attribute Tags.TableColumnGroup]
         -> [ChildHTML Tags.TableColumnGroup parent]
         -> ChildHTML parent grandparent
colgroup = Tag_TableColumnGroup . nubOrdOn attributeText

data_ :: ValidChild Tags.Data parent grandparent
      => [Attribute Tags.Data]
      -> [ChildHTML Tags.Data parent]
      -> ChildHTML parent grandparent
data_ = Tag_Data . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- Either phrasing content or zero or more <option> elements.
--
datalist :: ValidChild Tags.DataList parent grandparent
         => [Attribute Tags.DataList]
         -> [ChildHTML Tags.DataList parent]
         -> ChildHTML parent grandparent
datalist = Tag_DataList . nubOrdOn attributeText

dd :: ValidChild Tags.DescriptionDetails parent grandparent
   => [Attribute Tags.DescriptionDetails]
   -> [ChildHTML Tags.DescriptionDetails parent]
   -> ChildHTML parent grandparent
dd = Tag_DescriptionDetails . nubOrdOn attributeText

del :: ValidChild Tags.DeletedText parent grandparent
    => [Attribute Tags.DeletedText]
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent
del = Tag_DeletedText . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- One <summary> element followed by flow content.
--
details :: ValidChild Tags.Details parent grandparent
        => [Attribute Tags.Details]
        -> [ChildHTML Tags.Details parent]
        -> ChildHTML parent grandparent
details = Tag_Details . nubOrdOn attributeText

dfn :: ValidChild Tags.Definition parent grandparent
    => [Attribute Tags.Definition]
    -> [ChildHTML Tags.Definition parent]
    -> ChildHTML parent grandparent
dfn = Tag_Definition . nubOrdOn attributeText

dialog :: ValidChild Tags.Dialog parent grandparent
       => [Attribute Tags.Dialog]
       -> [ChildHTML Tags.Dialog parent]
       -> ChildHTML parent grandparent
dialog = Tag_Dialog . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- Flow content.
--
-- Or (in WHATWG HTML): If the parent is a <dl> element: one or more <dt>
-- elements followed by one or more <dd> elements, optionally intermixed with
-- <script> and <template> elements.
--
div :: ValidChild Tags.Division parent grandparent
    => [Attribute Tags.Division]
    -> [ChildHTML Tags.Division parent]
    -> ChildHTML parent grandparent
div = Tag_Division . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- Either: Zero or more groups each consisting of one or more <dt> elements
-- followed by one or more <dd> elements, optionally intermixed with <script>
-- and <template> elements.
--
-- Or: (in WHATWG HTML, W3C HTML 5.2 and later) One or more <div> elements,
-- optionally intermixed with <script> and <template> elements.
--
dl :: ValidChild Tags.DescriptionList parent grandparent
   => [Attribute Tags.DescriptionList]
   -> [ChildHTML Tags.DescriptionList parent]
   -> ChildHTML parent grandparent
dl = Tag_DescriptionList . nubOrdOn attributeText

dt :: ValidChild Tags.DescriptionTerm parent grandparent
   => [Attribute Tags.DescriptionTerm]
   -> [ChildHTML Tags.DescriptionTerm parent]
   -> ChildHTML parent grandparent
dt = Tag_DescriptionTerm . nubOrdOn attributeText

em :: ValidChild Tags.Emphasis parent grandparent
   => [Attribute Tags.Emphasis]
   -> [ChildHTML Tags.Emphasis parent]
   -> ChildHTML parent grandparent
em = Tag_Emphasis . nubOrdOn attributeText

embed :: ValidChild Tags.Embed parent grandparent
      => [Attribute Tags.Embed]
      -> ChildHTML parent grandparent
embed = Tag_Embed . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- An optional <legend> element, followed by flow content.
--
fieldset :: ValidChild Tags.Fieldset parent grandparent
         => [Attribute Tags.Fieldset]
         -> [ChildHTML Tags.Fieldset parent]
         -> ChildHTML parent grandparent
fieldset = Tag_Fieldset . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- A <figcaption> element, followed by flow content; or flow content followed
-- by a <figcaption> element; or flow content.
--
figcaption :: ValidChild Tags.FigureCaption parent grandparent
           => [Attribute Tags.FigureCaption]
           -> [ChildHTML Tags.FigureCaption parent]
           -> ChildHTML parent grandparent
figcaption = Tag_FigureCaption . nubOrdOn attributeText

figure :: ValidChild Tags.Figure parent grandparent
       => [Attribute Tags.Figure]
       -> [ChildHTML Tags.Figure parent]
       -> ChildHTML parent grandparent
figure = Tag_Figure . nubOrdOn attributeText

footer :: ValidChild Tags.Footer parent grandparent
       => [Attribute Tags.Footer]
       -> [ChildHTML Tags.Footer parent]
       -> ChildHTML parent grandparent
footer = Tag_Footer . nubOrdOn attributeText

-- This would be a good candidate for a safe constructor, particularly in
-- relation to <input> elements.
--
form :: ValidChild Tags.Form parent grandparent
     => [Attribute Tags.Form]
     -> [ChildHTML Tags.Form parent]
     -> ChildHTML parent grandparent
form = Tag_Form . nubOrdOn attributeText

h1 :: ValidChild Tags.H1 parent grandparent
   => [Attribute Tags.H1]
   -> [ChildHTML Tags.H1 parent]
   -> ChildHTML parent grandparent
h1 = Tag_H1 . nubOrdOn attributeText

h2 :: ValidChild Tags.H2 parent grandparent
   => [Attribute Tags.H2]
   -> [ChildHTML Tags.H2 parent]
   -> ChildHTML parent grandparent
h2 = Tag_H2 . nubOrdOn attributeText

h3 :: ValidChild Tags.H3 parent grandparent
   => [Attribute Tags.H3]
   -> [ChildHTML Tags.H3 parent]
   -> ChildHTML parent grandparent
h3 = Tag_H3 . nubOrdOn attributeText

h4 :: ValidChild Tags.H4 parent grandparent
   => [Attribute Tags.H4]
   -> [ChildHTML Tags.H4 parent]
   -> ChildHTML parent grandparent
h4 = Tag_H4 . nubOrdOn attributeText

h5 :: ValidChild Tags.H5 parent grandparent
   => [Attribute Tags.H5]
   -> [ChildHTML Tags.H5 parent]
   -> ChildHTML parent grandparent
h5 = Tag_H5 . nubOrdOn attributeText

h6 :: ValidChild Tags.H6 parent grandparent
   => [Attribute Tags.H6]
   -> [ChildHTML Tags.H6 parent]
   -> ChildHTML parent grandparent
h6 = Tag_H6 . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- If the document is an <iframe> srcdoc document, or if title information is
-- available from a higher level protocol (like the subject line in HTML
-- email), zero or more elements of metadata content.
--
-- Otherwise, one or more elements of metadata content where exactly one is a
-- <title> element.
--
head :: ValidChild Tags.Head parent grandparent
     => [Attribute Tags.Head]
     -> [ChildHTML Tags.Head parent]
     -> ChildHTML parent grandparent
head = Tag_Head . nubOrdOn attributeText

header :: ValidChild Tags.Header parent grandparent
       => [Attribute Tags.Header]
       -> [ChildHTML Tags.Header parent]
       -> ChildHTML parent grandparent
header = Tag_Header . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <p> elements, followed by one h1, h2, h3, h4, h5, or h6
-- element, followed by zero or more <p> elements.
--
hgroup :: ValidChild Tags.HeadingGroup parent grandparent
       => [Attribute Tags.HeadingGroup]
       -> [ChildHTML Tags.HeadingGroup parent]
       -> ChildHTML parent grandparent
hgroup = Tag_HeadingGroup . nubOrdOn attributeText

hr :: ValidChild Tags.HorizontalRule parent grandparent
   => [Attribute Tags.HorizontalRule]
   -> ChildHTML parent grandparent
hr = Tag_HorizontalRule . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <p> elements, followed by one h1, h2, h3, h4, h5, or h6
-- element, followed by zero or more <p> elements.
--
html :: [Attribute Tags.Html]
     -> [ChildHTML Tags.Html Tags.Document]
     -> ChildHTML Tags.Document Tags.NoElement
html = Tag_Html . nubOrdOn attributeText

i :: ValidChild Tags.IdiomaticText parent grandparent
  => [Attribute Tags.IdiomaticText]
  -> [ChildHTML Tags.IdiomaticText parent]
  -> ChildHTML parent grandparent
i = Tag_IdiomaticText . nubOrdOn attributeText

iframe :: ValidChild Tags.IFrame parent grandparent
       => [Attribute Tags.IFrame]
       -> ChildHTML parent grandparent
iframe = Tag_IFrame . nubOrdOn attributeText

img :: ValidChild Tags.Image parent grandparent
    => [Attribute Tags.Image]
    -> ChildHTML parent grandparent
img = Tag_Image . nubOrdOn attributeText

input :: ValidChild Tags.Input parent grandparent
      => [Attribute Tags.Input]
      -> ChildHTML parent grandparent
input = Tag_Input . nubOrdOn attributeText

ins :: ValidChild Tags.InsertedText parent grandparent
    => [Attribute Tags.InsertedText]
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent
ins = Tag_InsertedText . nubOrdOn attributeText

kbd :: ValidChild Tags.KeyboardInput parent grandparent
    => [Attribute Tags.KeyboardInput]
    -> [ChildHTML Tags.KeyboardInput parent]
    -> ChildHTML parent grandparent
kbd = Tag_KeyboardInput . nubOrdOn attributeText

-- Phrasing content, but no descendant label elements. No labelable elements
-- other than the labeled control are allowed.
--
-- labeledInput :: ValidChild Tags.Label parent grandparent
--              => [Attribute Tags.Label]
--              -> T.Text
--              -> ChildHTML Tags.Label
--              -> ChildHTML parent grandparent
-- labeledInput attrs name =
--   label (A.for name : attrs)
--     [ addAttribute child $ A.name name
--     ]

label :: ValidChild Tags.Label parent grandparent
      => [Attribute Tags.Label]
      -> [ChildHTML Tags.Label parent]
      -> ChildHTML parent grandparent
label = Tag_Label . nubOrdOn attributeText

legend :: ValidChild Tags.Legend parent grandparent
       => [Attribute Tags.Legend]
       -> [ChildHTML Tags.Legend parent]
       -> ChildHTML parent grandparent
legend = Tag_Legend . nubOrdOn attributeText

li :: ValidChild Tags.ListItem parent grandparent
   => [Attribute Tags.ListItem]
   -> [ChildHTML Tags.ListItem parent]
   -> ChildHTML parent grandparent
li = Tag_ListItem . nubOrdOn attributeText

link :: ValidChild Tags.Link parent grandparent
     => [Attribute Tags.Link]
     -> ChildHTML parent grandparent
link = Tag_Link . nubOrdOn attributeText

main :: ValidChild Tags.Main parent grandparent
     => [Attribute Tags.Main]
     -> [ChildHTML Tags.Main parent]
     -> ChildHTML parent grandparent
main = Tag_Main . nubOrdOn attributeText

map :: ValidChild Tags.Map parent grandparent
    => [Attribute Tags.Map]
    -> [ChildHTML Tags.Map parent]
    -> ChildHTML parent grandparent
map = Tag_Map . nubOrdOn attributeText

mark :: ValidChild Tags.Mark parent grandparent
     => [Attribute Tags.Mark]
     -> [ChildHTML Tags.Mark parent]
     -> ChildHTML parent grandparent
mark = Tag_Mark . nubOrdOn attributeText

menu :: ValidChild Tags.Menu parent grandparent
     => [Attribute Tags.Menu]
     -> [ChildHTML Tags.Menu parent]
     -> ChildHTML parent grandparent
menu = Tag_Menu . nubOrdOn attributeText

meta :: ValidChild Tags.Meta parent grandparent
     => [Attribute Tags.Meta]
     -> ChildHTML parent grandparent
meta = Tag_Meta . nubOrdOn attributeText

meter :: ValidChild Tags.Meter parent grandparent
      => [Attribute Tags.Meter]
      -> [ChildHTML Tags.Meter parent]
      -> ChildHTML parent grandparent
meter = Tag_Meter . nubOrdOn attributeText

nav :: ValidChild Tags.Nav parent grandparent
    => [Attribute Tags.Nav]
    -> [ChildHTML Tags.Nav parent]
    -> ChildHTML parent grandparent
nav = Tag_Nav . nubOrdOn attributeText

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
noscript :: ValidChild Tags.NoScript parent grandparent
         => [Attribute Tags.NoScript]
         -> [ChildHTML Tags.NoScript parent]
         -> ChildHTML parent grandparent
noscript = Tag_NoScript . nubOrdOn attributeText

object :: ValidChild Tags.Object parent grandparent
       => [Attribute Tags.Object]
       -> [ChildHTML parent parent]
       -> ChildHTML parent grandparent
object = Tag_Object . nubOrdOn attributeText

ol :: ValidChild Tags.OrderedList parent grandparent
   => [Attribute Tags.OrderedList]
   -> [ChildHTML Tags.OrderedList parent]
   -> ChildHTML parent grandparent
ol = Tag_OrderedList . nubOrdOn attributeText

optgroup :: ValidChild Tags.OptionGroup parent grandparent
         => [Attribute Tags.OptionGroup]
         -> [ChildHTML Tags.OptionGroup parent]
         -> ChildHTML parent grandparent
optgroup = Tag_OptionGroup . nubOrdOn attributeText

option :: ValidChild Tags.Option parent grandparent
       => [Attribute Tags.Option]
       -> [ChildHTML Tags.Option parent]
       -> ChildHTML parent grandparent
option = Tag_Option . nubOrdOn attributeText

output :: ValidChild Tags.Output parent grandparent
       => [Attribute Tags.Output]
       -> [ChildHTML Tags.Output parent]
       -> ChildHTML parent grandparent
output = Tag_Output . nubOrdOn attributeText

p :: ValidChild Tags.Paragraph parent grandparent
  => [Attribute Tags.Paragraph]
  -> [ChildHTML Tags.Paragraph parent]
  -> ChildHTML parent grandparent
p = Tag_Paragraph . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- Zero or more <source> elements, followed by one <img> element,
-- optionally intermixed with script-supporting elements.
--
picture :: ValidChild Tags.Picture parent grandparent
        => [Attribute Tags.Picture]
        -> [ChildHTML Tags.Picture parent]
        -> ChildHTML parent grandparent
picture = Tag_Picture . nubOrdOn attributeText

pre :: ValidChild Tags.PreformattedText parent grandparent
    => [Attribute Tags.PreformattedText]
    -> [ChildHTML Tags.PreformattedText parent]
    -> ChildHTML parent grandparent
pre = Tag_PreformattedText . nubOrdOn attributeText

progress :: ValidChild Tags.Progress parent grandparent
         => [Attribute Tags.Progress]
         -> [ChildHTML Tags.Progress parent]
         -> ChildHTML parent grandparent
progress = Tag_Progress . nubOrdOn attributeText

q :: ValidChild Tags.Quotation parent grandparent
  => [Attribute Tags.Quotation]
  -> [ChildHTML Tags.Quotation parent]
  -> ChildHTML parent grandparent
q = Tag_Quotation . nubOrdOn attributeText

rp :: ValidChild Tags.RubyParenthesis parent grandparent
   => [Attribute Tags.RubyParenthesis]
   -> [ChildHTML Tags.RubyParenthesis parent]
   -> ChildHTML parent grandparent
rp = Tag_RubyParenthesis . nubOrdOn attributeText

rt :: ValidChild Tags.RubyText parent grandparent
   => [Attribute Tags.RubyText]
   -> [ChildHTML Tags.RubyText parent]
   -> ChildHTML parent grandparent
rt = Tag_RubyText . nubOrdOn attributeText

ruby :: ValidChild Tags.Ruby parent grandparent
     => [Attribute Tags.Ruby]
     -> [ChildHTML Tags.Ruby parent]
     -> ChildHTML parent grandparent
ruby = Tag_Ruby . nubOrdOn attributeText

s :: ValidChild Tags.Strikethrough parent grandparent
  => [Attribute Tags.Strikethrough]
  -> [ChildHTML Tags.Strikethrough parent]
  -> ChildHTML parent grandparent
s = Tag_Strikethrough . nubOrdOn attributeText

samp :: ValidChild Tags.Sample parent grandparent
     => [Attribute Tags.Sample]
     -> [ChildHTML Tags.Sample parent]
     -> ChildHTML parent grandparent
samp = Tag_Sample . nubOrdOn attributeText

-- This should be changed to take script content instead of children.
--
script :: ValidChild Tags.Script parent grandparent
       => [Attribute Tags.Script]
       -> Maybe NET.NonEmptyText
       -> ChildHTML parent grandparent
script = Tag_Script . nubOrdOn attributeText

search :: ValidChild Tags.Search parent grandparent
       => [Attribute Tags.Search]
       -> [ChildHTML Tags.Search parent]
       -> ChildHTML parent grandparent
search = Tag_Search . nubOrdOn attributeText

section :: ValidChild Tags.Section parent grandparent
        => [Attribute Tags.Section]
        -> [ChildHTML Tags.Section parent]
        -> ChildHTML parent grandparent
section = Tag_Section . nubOrdOn attributeText

select :: ValidChild Tags.Select parent grandparent
       => [Attribute Tags.Select]
       -> [ChildHTML Tags.Select parent]
       -> ChildHTML parent grandparent
select = Tag_Select . nubOrdOn attributeText

slot :: ValidChild Tags.Slot parent grandparent
     => [Attribute Tags.Slot]
     -> [ChildHTML parent parent]
     -> ChildHTML parent grandparent
slot = Tag_Slot . nubOrdOn attributeText

small :: ValidChild Tags.SideComment parent grandparent
      => [Attribute Tags.SideComment]
      -> [ChildHTML Tags.SideComment parent]
      -> ChildHTML parent grandparent
small = Tag_SideComment . nubOrdOn attributeText

source :: ValidChild Tags.Source parent grandparent
       => [Attribute Tags.Source]
       -> ChildHTML parent grandparent
source = Tag_Source . nubOrdOn attributeText

span :: ValidChild Tags.Span parent grandparent
     => [Attribute Tags.Span]
     -> [ChildHTML Tags.Span parent]
     -> ChildHTML parent grandparent
span = Tag_Span . nubOrdOn attributeText

strong :: ValidChild Tags.Strong parent grandparent
       => [Attribute Tags.Strong]
       -> [ChildHTML Tags.Strong parent]
       -> ChildHTML parent grandparent
strong = Tag_Strong . nubOrdOn attributeText

-- This should be changed to take CSS content instead of children.
--
style :: ValidChild Tags.Style parent grandparent
      => [Attribute Tags.Style]
      -> T.Text
      -> ChildHTML parent grandparent
style = Tag_Style . nubOrdOn attributeText

sub :: ValidChild Tags.Subscript parent grandparent
    => [Attribute Tags.Subscript]
    -> [ChildHTML Tags.Subscript parent]
    -> ChildHTML parent grandparent
sub = Tag_Subscript . nubOrdOn attributeText

-- This is a candidate to receive safe constructor(s).
--
-- 	Phrasing content or one element of Heading content
--
summary :: ValidChild Tags.Summary parent grandparent
        => [Attribute Tags.Summary]
        -> [ChildHTML Tags.Summary parent]
        -> ChildHTML parent grandparent
summary = Tag_Summary . nubOrdOn attributeText

sup :: ValidChild Tags.Superscript parent grandparent
    => [Attribute Tags.Superscript]
    -> [ChildHTML Tags.Superscript parent]
    -> ChildHTML parent grandparent
sup = Tag_Superscript . nubOrdOn attributeText

table :: ValidChild Tags.Table parent grandparent
      => [Attribute Tags.Table]
      -> [ChildHTML Tags.Table parent]
      -> ChildHTML parent grandparent
table = Tag_Table . nubOrdOn attributeText

tbody :: ValidChild Tags.TableBody parent grandparent
      => [Attribute Tags.TableBody]
      -> [ChildHTML Tags.TableBody parent]
      -> ChildHTML parent grandparent
tbody = Tag_TableBody . nubOrdOn attributeText

td :: ValidChild Tags.TableDataCell parent grandparent
   => [Attribute Tags.TableDataCell]
   -> [ChildHTML Tags.TableDataCell parent]
   -> ChildHTML parent grandparent
td = Tag_TableDataCell . nubOrdOn attributeText

-- No content restrictions - remove the constraint?
--
template :: ValidChild Tags.ContentTemplate parent grandparent
         => [Attribute Tags.ContentTemplate]
         -> [ChildHTML Tags.ContentTemplate parent]
         -> ChildHTML parent grandparent
template = Tag_ContentTemplate . nubOrdOn attributeText

textarea :: ValidChild Tags.TextArea parent grandparent
         => [Attribute Tags.TextArea]
         -> T.Text
         -> ChildHTML parent grandparent
textarea attrs =
  Tag_TextArea (nubOrdOn attributeText attrs) . L.singleton . Tag_Text

tfoot :: ValidChild Tags.TableFoot parent grandparent
      => [Attribute Tags.TableFoot]
      -> [ChildHTML Tags.TableFoot parent]
      -> ChildHTML parent grandparent
tfoot = Tag_TableFoot . nubOrdOn attributeText

th :: ValidChild Tags.TableHeader parent grandparent
   => [Attribute Tags.TableHeader]
   -> [ChildHTML Tags.TableHeader parent]
   -> ChildHTML parent grandparent
th = Tag_TableHeader . nubOrdOn attributeText

thead :: ValidChild Tags.TableHead parent grandparent
      => [Attribute Tags.TableHead]
      -> [ChildHTML Tags.TableHead parent]
      -> ChildHTML parent grandparent
thead = Tag_TableHead . nubOrdOn attributeText

time :: ValidChild Tags.Time parent grandparent
     => [Attribute Tags.Time]
     -> [ChildHTML Tags.Time parent]
     -> ChildHTML parent grandparent
time = Tag_Time . nubOrdOn attributeText

title :: ValidChild Tags.Title parent grandparent
      => [Attribute Tags.Title]
      -> [ChildHTML Tags.Title parent]
      -> ChildHTML parent grandparent
title = Tag_Title . nubOrdOn attributeText

tr :: ValidChild Tags.TableRow parent grandparent
   => [Attribute Tags.TableRow]
   -> [ChildHTML Tags.TableRow parent]
   -> ChildHTML parent grandparent
tr = Tag_TableRow . nubOrdOn attributeText

track :: ValidChild Tags.Track parent grandparent
      => [Attribute Tags.Track]
      -> ChildHTML parent grandparent
track = Tag_Track . nubOrdOn attributeText

u :: ValidChild Tags.Underline parent grandparent
  => [Attribute Tags.Underline]
  -> [ChildHTML Tags.Underline parent]
  -> ChildHTML parent grandparent
u = Tag_Underline . nubOrdOn attributeText

ul :: ValidChild Tags.UnorderedList parent grandparent
   => [Attribute Tags.UnorderedList]
   -> [ChildHTML Tags.UnorderedList parent]
   -> ChildHTML parent grandparent
ul = Tag_UnorderedList . nubOrdOn attributeText

var :: ValidChild Tags.Variable parent grandparent
    => [Attribute Tags.Variable]
    -> [ChildHTML Tags.Variable parent]
    -> ChildHTML parent grandparent
var = Tag_Variable . nubOrdOn attributeText

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
video :: ValidChild Tags.Video parent grandparent
      => [Attribute Tags.Video]
      -> [ChildHTML Tags.Video parent]
      -> ChildHTML parent grandparent
video = Tag_Video . nubOrdOn attributeText

wbr :: ValidChild Tags.WordBreakOpportunity parent grandparent
    => [Attribute Tags.WordBreakOpportunity]
    -> ChildHTML parent grandparent
wbr = Tag_WordBreakOpportunity . nubOrdOn attributeText

-- | This is a convenience function that takes a 'Text' and replaces
-- whitespaces with `<wbr>` to give the browser ample opportunities to break
-- the text without splitting words.
--
wbrs :: ( ValidChild Tags.WordBreakOpportunity parent grandparent
        , ValidChild Tags.Text parent grandparent
        )
     => T.Text
     -> [ChildHTML parent grandparent]
wbrs = L.intersperse (wbr []) . fmap text . T.words

-- wbrURL: Add wbr around a URL.
