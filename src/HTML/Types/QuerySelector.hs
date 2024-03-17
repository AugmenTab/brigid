{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Types.QuerySelector
  ( QuerySelector
  , QuerySelectorTypes
  , mkQuerySelector
  , unQuerySelector
  , querySelectorToBytes
  , querySelectorToText
  , RawQuerySelector (RawQuerySelector)
  , rawQuerySelectorToBytes
  , rawQuerySelectorToText
  , ElementSelector
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
  , AttributeSelector
  , attributeSelectorToBytes
  , attributeSelectorToText
  , customAttribute
  , accesskey
  , autocapitalize
  , autofocus
  , class_
  , contenteditable
  , customData
  , dir
  , draggable
  , enterkeyhint
  , exportparts
  , hidden
  , id
  , inert
  , inputmode
  , is
  , itemid
  , itemprop
  , itemref
  , itemscope
  , itemtype
  , lang
  , nonce
  , part
  , popover
  , role
  , slot
  , spellcheck
  , style
  , tabindex
  , title
  , translate
  , accept
  , acceptCharset
  , action
  , allow
  , alt
  , async
  , autocomplete
  , autoplay
  , background
  , bgcolor
  , border
  , capture
  , charset
  , checked
  , cite
  , color
  , cols
  , colspan
  , content
  , controls
  , coords
  , crossorigin
  , data_
  , datetime
  , decoding
  , default_
  , defer
  , dirname
  , disabled
  , download
  , enctype
  , for
  , form
  , formaction
  , formenctype
  , formmethod
  , formnovalidate
  , formtarget
  , headers
  , height
  , high
  , href
  , hreflang
  , httpEquiv
  , integrity
  , ismap
  , kind
  , label
  , list
  , loop
  , low
  , max
  , maxlength
  , minlength
  , media
  , method
  , min
  , multiple
  , muted
  , name
  , novalidate
  , open
  , optimum
  , pattern
  , ping
  , placeholder
  , playsinline
  , poster
  , preload
  , readonly
  , referrerpolicy
  , rel
  , required
  , reversed
  , rows
  , rowspan
  , sandbox
  , scope
  , selected
  , shape
  , size
  , sizes
  , span
  , src
  , srcdoc
  , srclang
  , srcset
  , start
  , step
  , target
  , type_
  , usemap
  , value
  , width
  , wrap
  , hxGet
  , hxPost
  , hxOn
  , hxPushURL
  , hxSelect
  , hxSelectOOB
  , hxSwap
  , hxSwapOOB
  , hxTarget
  , hxTrigger
  , hxVals
  , hxBoost
  , hxConfirm
  , hxDelete
  , hxDisable
  , hxDisabledElt
  , hxDisinherit
  , hxEncoding
  , hxExt
  , hxHeaders
  , hxHistory
  , hxHistoryElt
  , hxInclude
  , hxIndicator
  , hxParams
  , hxPatch
  , hxPreserve
  , hxPrompt
  , hxPut
  , hxReplaceURL
  , hxRequest
  , hxSync
  , hxValidate
  ) where

import Prelude hiding (div, head, id, map, max, min, span)
import Data.Bool qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import HTML.Types.Autocapitalize (AutocapitalizeOption, autocapitalizeOptionToText)
import HTML.Types.Class qualified as Class
import HTML.Types.ClassSelector qualified as CS
import HTML.Types.ContentEditable (ContentEditableOption, contentEditableOptionToText)
import HTML.Types.CrossOrigin (CrossOriginFetch, crossoriginFetchToText)
import HTML.Types.Directionality (Directionality, directionalityToText)
import HTML.Types.Disinherit (DisinheritTypes, disinheritToText, mkDisinherit)
import HTML.Types.Extension (Extension, extensionToText)
import HTML.Types.Href (HrefSelectorTypes, hrefSelectorToText, mkHrefSelector)
import HTML.Types.Id qualified as Id
import HTML.Types.KeyHint (KeyHintOption, keyHintOptionToText)
import HTML.Types.Method (Get, Post, Delete, Put, Patch)
import HTML.Types.NoContent (NoContent)
import HTML.Types.Part (ExportPart, Part, exportPartToText, partToText)
import HTML.Types.PopoverState (PopoverState, popoverStateToText)
import HTML.Types.PushURL (PushURLTypes, mkPushURL, pushURLToText)
import HTML.Types.URL (RelativeURL, relativeURLToText)

newtype QuerySelector =
  QuerySelector
    { unQuerySelector :: Shrubbery.Union QuerySelectorTypes
    }

type QuerySelectorTypes =
  [ Id.Id
  , Class.Class
  , ElementSelector
  , RawQuerySelector
  ]

mkQuerySelector :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                   )
                => querySelector -> QuerySelector
mkQuerySelector =
  QuerySelector . Shrubbery.unify

querySelectorToBytes :: QuerySelector -> LBS.ByteString
querySelectorToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Id.Id Id.idToBytes
      . Shrubbery.branch @Class.Class Class.classToBytes
      . Shrubbery.branch @ElementSelector elementSelectorToBytes
      . Shrubbery.branch @RawQuerySelector rawQuerySelectorToBytes
      $ Shrubbery.branchEnd
  ) . unQuerySelector

querySelectorToText :: QuerySelector -> T.Text
querySelectorToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Id.Id Id.idToText
      . Shrubbery.branch @Class.Class Class.classToText
      . Shrubbery.branch @ElementSelector elementSelectorToText
      . Shrubbery.branch @RawQuerySelector rawQuerySelectorToText
      $ Shrubbery.branchEnd
  ) . unQuerySelector

newtype RawQuerySelector =
  RawQuerySelector
    { rawQuerySelectorToText :: T.Text
    }

rawQuerySelectorToBytes :: RawQuerySelector -> LBS.ByteString
rawQuerySelectorToBytes =
  LBS.fromStrict . TE.encodeUtf8 . rawQuerySelectorToText

data ElementSelector =
  ElementSelector
    { elementSelectorType :: ElementType
    , elementSelectorAttr :: Maybe AttributeSelector
    , elementSelectorClasses :: [CS.ClassSelector]
    , elementSelectorChild :: Maybe ElementSelector
    }

elementSelectorToBytes :: ElementSelector -> LBS.ByteString
elementSelectorToBytes element =
  LBS.concat
    [ elementTypeToBytes $ elementSelectorType element
    , maybe "" attributeSelectorToBytes $ elementSelectorAttr element
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
    , maybe "" attributeSelectorToText $ elementSelectorAttr element
    , foldMap CS.classSelectorToText $ elementSelectorClasses element
    , maybe
        ""
        (T.cons ' ' . elementSelectorToText)
        (elementSelectorChild element)
    ]

customTag :: T.Text
          -> Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Either NoContent ElementSelector
          -> ElementSelector
customTag elementName mbAttr classes eiNoContentOrElement =
  ElementSelector
    (Tag_CustomElement elementName)
    mbAttr
    classes
    (either (const Nothing) Just eiNoContentOrElement)

a :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
a = ElementSelector Tag_Anchor

abbr :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
abbr = ElementSelector Tag_Abbreviation

address :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
address = ElementSelector Tag_ContactAddress

area :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
area mbAttr classes = ElementSelector Tag_Area mbAttr classes Nothing

article :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
article = ElementSelector Tag_Article

aside :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
aside = ElementSelector Tag_Aside

audio :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
audio = ElementSelector Tag_Audio

b :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
b = ElementSelector Tag_BringAttentionTo

base :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
base mbAttr classes = ElementSelector Tag_Base mbAttr classes Nothing

bdi :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
bdi = ElementSelector Tag_BidirectionalIsolation

bdo :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
bdo = ElementSelector Tag_BidirectionalOverride

blockquote :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
blockquote = ElementSelector Tag_Blockquote

body :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
body = ElementSelector Tag_Body

br :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
br mbAttr classes = ElementSelector Tag_LineBreak mbAttr classes Nothing

button :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
button = ElementSelector Tag_Button

canvas :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
canvas = ElementSelector Tag_Canvas

caption :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
caption = ElementSelector Tag_TableCaption

citeTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
citeTag = ElementSelector Tag_Citation

code :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
code = ElementSelector Tag_Code

col :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
col mbAttr classes = ElementSelector Tag_TableColumn mbAttr classes Nothing

colgroup :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
colgroup = ElementSelector Tag_TableColumnGroup

dataTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
dataTag = ElementSelector Tag_Data

datalist :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
datalist = ElementSelector Tag_DataList

dd :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dd = ElementSelector Tag_DescriptionDetails

del :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
del = ElementSelector Tag_DeletedText

details :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
details = ElementSelector Tag_Details

dfn :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
dfn = ElementSelector Tag_Definition

dialog :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
dialog = ElementSelector Tag_Dialog

div :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
div = ElementSelector Tag_Division

dl :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dl = ElementSelector Tag_DescriptionList

dt :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dt = ElementSelector Tag_DescriptionTerm

em :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
em = ElementSelector Tag_Emphasis

embed :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
embed mbAttr classes = ElementSelector Tag_Embed mbAttr classes Nothing

fieldset :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
fieldset = ElementSelector Tag_Fieldset

figcaption :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
figcaption = ElementSelector Tag_FigureCaption

figure :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
figure = ElementSelector Tag_Figure

footer :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
footer = ElementSelector Tag_Footer

formTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
formTag = ElementSelector Tag_Form

h1 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h1 = ElementSelector Tag_H1

h2 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h2 = ElementSelector Tag_H1

h3 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h3 = ElementSelector Tag_H3

h4 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h4 = ElementSelector Tag_H4

h5 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h5 = ElementSelector Tag_H5

h6 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h6 = ElementSelector Tag_H6

head :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
head = ElementSelector Tag_Head

header :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
header = ElementSelector Tag_Header

hgroup :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
hgroup = ElementSelector Tag_HeadingGroup

hr :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
hr mbAttr classes = ElementSelector Tag_HorizontalRule mbAttr classes Nothing

html :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
html = ElementSelector Tag_Html

i :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
i = ElementSelector Tag_IdiomaticText

iframe :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
iframe mbAttr classes = ElementSelector Tag_IFrame mbAttr classes Nothing

img :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
img mbAttr classes = ElementSelector Tag_Image mbAttr classes Nothing

input :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
input mbAttr classes = ElementSelector Tag_Input mbAttr classes Nothing

ins :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
ins = ElementSelector Tag_InsertedText

kbd :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
kbd = ElementSelector Tag_KeyboardInput

labelTag :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
labelTag = ElementSelector Tag_Label

legend :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
legend = ElementSelector Tag_Legend

li :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
li = ElementSelector Tag_ListItem

link :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
link mbAttr classes = ElementSelector Tag_Link mbAttr classes Nothing

main :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
main = ElementSelector Tag_Main

map :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
map = ElementSelector Tag_Map

mark :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
mark = ElementSelector Tag_Mark

menu :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
menu = ElementSelector Tag_Menu

meta :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
meta mbAttr classes = ElementSelector Tag_Meta mbAttr classes Nothing

meter :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
meter = ElementSelector Tag_Meter

nav :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
nav = ElementSelector Tag_Nav

noscript :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
noscript = ElementSelector Tag_NoScript

object :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
object = ElementSelector Tag_Object

ol :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
ol = ElementSelector Tag_OrderedList

optgroup :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
optgroup = ElementSelector Tag_OptionGroup

-- option does not take a child argument, since it only holds text values,
-- which cannot be targeted as part of a CSS query.
option :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
option mbAttr classes = ElementSelector Tag_Option mbAttr classes Nothing

output :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
output = ElementSelector Tag_Output

p :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
p = ElementSelector Tag_Paragraph

picture :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
picture = ElementSelector Tag_Picture

pre :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
pre = ElementSelector Tag_PreformattedText

progress :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
progress = ElementSelector Tag_Progress

q :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
q = ElementSelector Tag_Quotation

-- rp does not take a child argument, since it only holds text values, which
-- cannot be targeted as part of a CSS query.
rp :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
rp mbAttr classes = ElementSelector Tag_RubyParenthesis mbAttr classes Nothing

rt :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
rt = ElementSelector Tag_RubyText

ruby :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
ruby = ElementSelector Tag_Ruby

s :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
s = ElementSelector Tag_Strikethrough

sample :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
sample = ElementSelector Tag_Sample

script :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
script mbAttr classes = ElementSelector Tag_Script mbAttr classes Nothing

search :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
search = ElementSelector Tag_Search

section :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
section = ElementSelector Tag_Section

select :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
select = ElementSelector Tag_Select

slotTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
slotTag = ElementSelector Tag_Slot

small :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
small = ElementSelector Tag_SideComment

source :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
source mbAttr classes = ElementSelector Tag_Source mbAttr classes Nothing

spanTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
spanTag = ElementSelector Tag_Span

strong :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
strong = ElementSelector Tag_Strong

styleTag :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
styleTag mbAttr classes = ElementSelector Tag_Style mbAttr classes Nothing

sub :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
sub = ElementSelector Tag_Subscript

summary :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
summary = ElementSelector Tag_Summary

sup :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
sup = ElementSelector Tag_Superscript

table :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
table = ElementSelector Tag_Table

tbody :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tbody = ElementSelector Tag_TableBody

td :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
td = ElementSelector Tag_TableDataCell

template :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
template = ElementSelector Tag_ContentTemplate

-- textarea does not take a child argument, since it only holds text values,
-- which cannot be targeted as part of a CSS query.
textarea :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
textarea mbAttr classes = ElementSelector Tag_TextArea mbAttr classes Nothing

tfoot :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tfoot = ElementSelector Tag_TableFoot

th :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
th = ElementSelector Tag_TableHeader

thead :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
thead = ElementSelector Tag_TableHead

time :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
time = ElementSelector Tag_Time

-- title does not take a child argument, since it only holds text values, which
-- cannot be targeted as part of a CSS query.
titleTag :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
titleTag mbAttr classes = ElementSelector Tag_Title mbAttr classes Nothing

tr :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
tr = ElementSelector Tag_TableRow

track :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
track mbAttr classes = ElementSelector Tag_Track mbAttr classes Nothing

u :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
u = ElementSelector Tag_Underline

ul :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
ul = ElementSelector Tag_UnorderedList

var :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
var = ElementSelector Tag_Variable

video :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
video = ElementSelector Tag_Video

wbr :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
wbr mbAttr classes =
  ElementSelector Tag_WordBreakOpportunity mbAttr classes Nothing

data ElementType
  = Tag_CustomElement T.Text
  | Tag_Anchor
  | Tag_Abbreviation
  | Tag_ContactAddress
  | Tag_Area
  | Tag_Article
  | Tag_Aside
  | Tag_Audio
  | Tag_BringAttentionTo
  | Tag_Base
  | Tag_BidirectionalIsolation
  | Tag_BidirectionalOverride
  | Tag_Blockquote
  | Tag_Body
  | Tag_LineBreak
  | Tag_Button
  | Tag_Canvas
  | Tag_TableCaption
  | Tag_Citation
  | Tag_Code
  | Tag_TableColumn
  | Tag_TableColumnGroup
  | Tag_Data
  | Tag_DataList
  | Tag_DescriptionDetails
  | Tag_DeletedText
  | Tag_Details
  | Tag_Definition
  | Tag_Dialog
  | Tag_Division
  | Tag_DescriptionList
  | Tag_DescriptionTerm
  | Tag_Emphasis
  | Tag_Embed
  | Tag_Fieldset
  | Tag_FigureCaption
  | Tag_Figure
  | Tag_Footer
  | Tag_Form
  | Tag_H1
  | Tag_H2
  | Tag_H3
  | Tag_H4
  | Tag_H5
  | Tag_H6
  | Tag_Head
  | Tag_Header
  | Tag_HeadingGroup
  | Tag_HorizontalRule
  | Tag_Html
  | Tag_IdiomaticText
  | Tag_IFrame
  | Tag_Image
  | Tag_Input
  | Tag_InsertedText
  | Tag_KeyboardInput
  | Tag_Label
  | Tag_Legend
  | Tag_ListItem
  | Tag_Link
  | Tag_Main
  | Tag_Map
  | Tag_Mark
  | Tag_Menu
  | Tag_Meta
  | Tag_Meter
  | Tag_Nav
  | Tag_NoScript
  | Tag_Object
  | Tag_OrderedList
  | Tag_OptionGroup
  | Tag_Option
  | Tag_Output
  | Tag_Paragraph
  | Tag_Picture
  | Tag_PreformattedText
  | Tag_Progress
  | Tag_Quotation
  | Tag_RubyParenthesis
  | Tag_RubyText
  | Tag_Ruby
  | Tag_Strikethrough
  | Tag_Sample
  | Tag_Script
  | Tag_Search
  | Tag_Section
  | Tag_Select
  | Tag_Slot
  | Tag_SideComment
  | Tag_Source
  | Tag_Span
  | Tag_Strong
  | Tag_Style
  | Tag_Subscript
  | Tag_Summary
  | Tag_Superscript
  | Tag_Table
  | Tag_TableBody
  | Tag_TableDataCell
  | Tag_ContentTemplate
  | Tag_TextArea
  | Tag_TableFoot
  | Tag_TableHeader
  | Tag_TableHead
  | Tag_Time
  | Tag_Title
  | Tag_TableRow
  | Tag_Track
  | Tag_Underline
  | Tag_UnorderedList
  | Tag_Variable
  | Tag_Video
  | Tag_WordBreakOpportunity

elementTypeToBytes :: ElementType -> LBS.ByteString
elementTypeToBytes element =
  case element of
    Tag_CustomElement tagName  -> LBS.fromStrict $ TE.encodeUtf8 tagName
    Tag_Anchor                 -> "a"
    Tag_Abbreviation           -> "abbr"
    Tag_ContactAddress         -> "address"
    Tag_Area                   -> "area"
    Tag_Article                -> "article"
    Tag_Aside                  -> "aside"
    Tag_Audio                  -> "audio"
    Tag_BringAttentionTo       -> "b"
    Tag_Base                   -> "base"
    Tag_BidirectionalIsolation -> "bdi"
    Tag_BidirectionalOverride  -> "bdo"
    Tag_Blockquote             -> "blockquote"
    Tag_Body                   -> "body"
    Tag_LineBreak              -> "br"
    Tag_Button                 -> "button"
    Tag_Canvas                 -> "canvas"
    Tag_TableCaption           -> "caption"
    Tag_Citation               -> "cite"
    Tag_Code                   -> "code"
    Tag_TableColumn            -> "col"
    Tag_TableColumnGroup       -> "colgroup"
    Tag_Data                   -> "data"
    Tag_DataList               -> "datalist"
    Tag_DescriptionDetails     -> "dd"
    Tag_DeletedText            -> "del"
    Tag_Details                -> "details"
    Tag_Definition             -> "dfn"
    Tag_Dialog                 -> "dialog"
    Tag_Division               -> "div"
    Tag_DescriptionList        -> "dl"
    Tag_DescriptionTerm        -> "dt"
    Tag_Emphasis               -> "em"
    Tag_Embed                  -> "embed"
    Tag_Fieldset               -> "fieldset"
    Tag_FigureCaption          -> "figcaption"
    Tag_Figure                 -> "figure"
    Tag_Footer                 -> "footer"
    Tag_Form                   -> "form"
    Tag_H1                     -> "h1"
    Tag_H2                     -> "h2"
    Tag_H3                     -> "h3"
    Tag_H4                     -> "h4"
    Tag_H5                     -> "h5"
    Tag_H6                     -> "h6"
    Tag_Head                   -> "head"
    Tag_Header                 -> "header"
    Tag_HeadingGroup           -> "hgroup"
    Tag_HorizontalRule         -> "hr"
    Tag_Html                   -> "html"
    Tag_IdiomaticText          -> "i"
    Tag_IFrame                 -> "iframe"
    Tag_Image                  -> "img"
    Tag_Input                  -> "input"
    Tag_InsertedText           -> "ins"
    Tag_KeyboardInput          -> "kbd"
    Tag_Label                  -> "label"
    Tag_Legend                 -> "legend"
    Tag_ListItem               -> "li"
    Tag_Link                   -> "link"
    Tag_Main                   -> "main"
    Tag_Map                    -> "map"
    Tag_Mark                   -> "mark"
    Tag_Menu                   -> "menu"
    Tag_Meta                   -> "meta"
    Tag_Meter                  -> "metere"
    Tag_Nav                    -> "nav"
    Tag_NoScript               -> "noscript"
    Tag_Object                 -> "object"
    Tag_OrderedList            -> "ol"
    Tag_OptionGroup            -> "optgroup"
    Tag_Option                 -> "option"
    Tag_Output                 -> "output"
    Tag_Paragraph              -> "p"
    Tag_Picture                -> "picture"
    Tag_PreformattedText       -> "pre"
    Tag_Progress               -> "progress"
    Tag_Quotation              -> "q"
    Tag_RubyParenthesis        -> "rp"
    Tag_RubyText               -> "rt"
    Tag_Ruby                   -> "ruby"
    Tag_Strikethrough          -> "s"
    Tag_Sample                 -> "sample"
    Tag_Script                 -> "script"
    Tag_Search                 -> "search"
    Tag_Section                -> "section"
    Tag_Select                 -> "select"
    Tag_Slot                   -> "slot"
    Tag_SideComment            -> "small"
    Tag_Source                 -> "source"
    Tag_Span                   -> "span"
    Tag_Strong                 -> "strong"
    Tag_Style                  -> "style"
    Tag_Subscript              -> "sub"
    Tag_Summary                -> "summary"
    Tag_Superscript            -> "sup"
    Tag_Table                  -> "table"
    Tag_TableBody              -> "tbody"
    Tag_TableDataCell          -> "td"
    Tag_ContentTemplate        -> "template"
    Tag_TextArea               -> "textarea"
    Tag_TableFoot              -> "tfoot"
    Tag_TableHeader            -> "th"
    Tag_TableHead              -> "thead"
    Tag_Time                   -> "time"
    Tag_Title                  -> "title"
    Tag_TableRow               -> "tr"
    Tag_Track                  -> "track"
    Tag_Underline              -> "u"
    Tag_UnorderedList          -> "ul"
    Tag_Variable               -> "var"
    Tag_Video                  -> "video"
    Tag_WordBreakOpportunity   -> "wbr"

elementTypeToText :: ElementType -> T.Text
elementTypeToText element =
  case element of
    Tag_CustomElement tagName  -> tagName
    Tag_Anchor                 -> "a"
    Tag_Abbreviation           -> "abbr"
    Tag_ContactAddress         -> "address"
    Tag_Area                   -> "area"
    Tag_Article                -> "article"
    Tag_Aside                  -> "aside"
    Tag_Audio                  -> "audio"
    Tag_BringAttentionTo       -> "b"
    Tag_Base                   -> "base"
    Tag_BidirectionalIsolation -> "bdi"
    Tag_BidirectionalOverride  -> "bdo"
    Tag_Blockquote             -> "blockquote"
    Tag_Body                   -> "body"
    Tag_LineBreak              -> "br"
    Tag_Button                 -> "button"
    Tag_Canvas                 -> "canvas"
    Tag_TableCaption           -> "caption"
    Tag_Citation               -> "cite"
    Tag_Code                   -> "code"
    Tag_TableColumn            -> "col"
    Tag_TableColumnGroup       -> "colgroup"
    Tag_Data                   -> "data"
    Tag_DataList               -> "datalist"
    Tag_DescriptionDetails     -> "dd"
    Tag_DeletedText            -> "del"
    Tag_Details                -> "details"
    Tag_Definition             -> "dfn"
    Tag_Dialog                 -> "dialog"
    Tag_Division               -> "div"
    Tag_DescriptionList        -> "dl"
    Tag_DescriptionTerm        -> "dt"
    Tag_Emphasis               -> "em"
    Tag_Embed                  -> "embed"
    Tag_Fieldset               -> "fieldset"
    Tag_FigureCaption          -> "figcaption"
    Tag_Figure                 -> "figure"
    Tag_Footer                 -> "footer"
    Tag_Form                   -> "form"
    Tag_H1                     -> "h1"
    Tag_H2                     -> "h2"
    Tag_H3                     -> "h3"
    Tag_H4                     -> "h4"
    Tag_H5                     -> "h5"
    Tag_H6                     -> "h6"
    Tag_Head                   -> "head"
    Tag_Header                 -> "header"
    Tag_HeadingGroup           -> "hgroup"
    Tag_HorizontalRule         -> "hr"
    Tag_Html                   -> "html"
    Tag_IdiomaticText          -> "i"
    Tag_IFrame                 -> "iframe"
    Tag_Image                  -> "img"
    Tag_Input                  -> "input"
    Tag_InsertedText           -> "ins"
    Tag_KeyboardInput          -> "kbd"
    Tag_Label                  -> "label"
    Tag_Legend                 -> "legend"
    Tag_ListItem               -> "li"
    Tag_Link                   -> "link"
    Tag_Main                   -> "main"
    Tag_Map                    -> "map"
    Tag_Mark                   -> "mark"
    Tag_Menu                   -> "menu"
    Tag_Meta                   -> "meta"
    Tag_Meter                  -> "metere"
    Tag_Nav                    -> "nav"
    Tag_NoScript               -> "noscript"
    Tag_Object                 -> "object"
    Tag_OrderedList            -> "ol"
    Tag_OptionGroup            -> "optgroup"
    Tag_Option                 -> "option"
    Tag_Output                 -> "output"
    Tag_Paragraph              -> "p"
    Tag_Picture                -> "picture"
    Tag_PreformattedText       -> "pre"
    Tag_Progress               -> "progress"
    Tag_Quotation              -> "q"
    Tag_RubyParenthesis        -> "rp"
    Tag_RubyText               -> "rt"
    Tag_Ruby                   -> "ruby"
    Tag_Strikethrough          -> "s"
    Tag_Sample                 -> "sample"
    Tag_Script                 -> "script"
    Tag_Search                 -> "search"
    Tag_Section                -> "section"
    Tag_Select                 -> "select"
    Tag_Slot                   -> "slot"
    Tag_SideComment            -> "small"
    Tag_Source                 -> "source"
    Tag_Span                   -> "span"
    Tag_Strong                 -> "strong"
    Tag_Style                  -> "style"
    Tag_Subscript              -> "sub"
    Tag_Summary                -> "summary"
    Tag_Superscript            -> "sup"
    Tag_Table                  -> "table"
    Tag_TableBody              -> "tbody"
    Tag_TableDataCell          -> "td"
    Tag_ContentTemplate        -> "template"
    Tag_TextArea               -> "textarea"
    Tag_TableFoot              -> "tfoot"
    Tag_TableHeader            -> "th"
    Tag_TableHead              -> "thead"
    Tag_Time                   -> "time"
    Tag_Title                  -> "title"
    Tag_TableRow               -> "tr"
    Tag_Track                  -> "track"
    Tag_Underline              -> "u"
    Tag_UnorderedList          -> "ul"
    Tag_Variable               -> "var"
    Tag_Video                  -> "video"
    Tag_WordBreakOpportunity   -> "wbr"

type AttributeSelector = (AttributeType, Maybe T.Text)

attributeSelectorToBytes :: AttributeSelector -> LBS.ByteString
attributeSelectorToBytes (attr, mbVal) =
  LBS.concat
    [ "["
    , attributeTypeToBytes attr
    , maybe "" (\v -> "='" <> LBS.fromStrict (TE.encodeUtf8 v) <> "'") mbVal
    , "]"
    ]

attributeSelectorToText :: AttributeSelector -> T.Text
attributeSelectorToText (attr, mbVal) =
  let attrVal = maybe "" (\v -> "='" <> v <> "'") mbVal
   in "[" <> attributeTypeToText attr <> attrVal <> "]"

data AttributeType
  -- Custom Attribute
  = CustomAttribute T.Text

  -- Global Attributes
  --
  | AccessKey
  | Autocapitalize
  | Autofocus
  | Class
  | ContentEditable
  | CustomData T.Text
  | Dir
  | Draggable
  | EnterKeyHint
  | ExportParts
  | Hidden
  | Id
  | Inert
  | InputMode
  | Is
  | ItemId
  | ItemProp
  | ItemRef
  | ItemScope
  | ItemType
  | Lang
  | Nonce
  | Part
  | Popover
  | Role
  | Slot
  | Spellcheck
  | Style
  | TabIndex
  | Title
  | Translate

  -- Scoped Attributes
  --
  | Accept
  | AcceptCharset
  | Action
  | Allow
  | Alt
  | Async
  | Autocomplete
  | Autoplay
  | Background
  | BackgroundColor
  | Border
  | Capture
  | Charset
  | Checked
  | Cite
  | Color
  | Cols
  | Colspan
  | Content
  | Controls
  | Coords
  | CrossOrigin
  | Data
  | Datetime
  | Decoding
  | Default
  | Defer
  | Dirname
  | Disabled
  | Download
  | Enctype
  | For
  | Form
  | FormAction
  | FormEnctype
  | FormMethod
  | FormNoValidate
  | FormTarget
  | Headers
  | Height
  | High
  | Href
  | HrefLang
  | HttpEquiv
  | Integrity
  | IsMap
  | Kind
  | Label
  | List
  | Loop
  | Low
  | Max
  | MaxLength
  | MinLength
  | Media
  | Method
  | Min
  | Multiple
  | Muted
  | Name
  | NoValidate
  | Open
  | Optimum
  | Pattern
  | Ping
  | Placeholder
  | PlaysInline
  | Poster
  | Preload
  | ReadOnly
  | ReferrerPolicy
  | Rel
  | Required
  | Reversed
  | Rows
  | Rowspan
  | Sandbox
  | Scope
  | Selected
  | Shape
  | Size
  | Sizes
  | Span
  | Src
  | SrcDoc
  | SrcLang
  | SrcSet
  | Start
  | Step
  | Target
  | Type
  | UseMap
  | Value
  | Width
  | Wrap

  -- HTMX Attributes
  --
  | Htmx_HxGet
  | Htmx_HxPost
  | Htmx_HxOn
  | Htmx_HxPushURL
  | Htmx_HxSelect
  | Htmx_HxSelectOOB
  | Htmx_HxSwap
  | Htmx_HxSwapOOB
  | Htmx_HxTarget
  | Htmx_HxTrigger
  | Htmx_HxVals
  | Htmx_HxBoost
  | Htmx_HxConfirm
  | Htmx_HxDelete
  | Htmx_HxDisable
  | Htmx_HxDisabledElt
  | Htmx_HxDisinherit
  | Htmx_HxEncoding
  | Htmx_HxExt
  | Htmx_HxHeaders
  | Htmx_HxHistory
  | Htmx_HxHistoryElt
  | Htmx_HxInclude
  | Htmx_HxIndicator
  | Htmx_HxParams
  | Htmx_HxPatch
  | Htmx_HxPreserve
  | Htmx_HxPrompt
  | Htmx_HxPut
  | Htmx_HxReplaceURL
  | Htmx_HxRequest
  | Htmx_HxSync
  | Htmx_HxValidate

attributeTypeToBytes :: AttributeType -> LBS.ByteString
attributeTypeToBytes attr =
  case attr of
    -- Custom Attribute
    CustomAttribute attrName -> LBS.fromStrict $ TE.encodeUtf8 attrName

    -- Global Attributes
    --
    AccessKey           -> "accesskey"
    Autocapitalize      -> "autocapitalize"
    Autofocus           -> "autofocus"
    Class               -> "class"
    ContentEditable     -> "contenteditable"
    CustomData attrName -> "data-" <> LBS.fromStrict (TE.encodeUtf8 attrName)
    Dir                 -> "dir"
    Draggable           -> "draggable"
    EnterKeyHint        -> "enterkeyhint"
    ExportParts         -> "exportparts"
    Hidden              -> "hidden"
    Id                  -> "id"
    Inert               -> "inert"
    InputMode           -> "inputmode"
    Is                  -> "is"
    ItemId              -> "itemid"
    ItemProp            -> "itemprop"
    ItemRef             -> "itemref"
    ItemScope           -> "itemscope"
    ItemType            -> "itemtype"
    Lang                -> "lang"
    Nonce               -> "nonce"
    Part                -> "part"
    Popover             -> "popover"
    Role                -> "role"
    Slot                -> "slot"
    Spellcheck          -> "spellcheck"
    Style               -> "style"
    TabIndex            -> "tabindex"
    Title               -> "title"
    Translate           -> "translate"

    -- Scoped Attributes
    --
    Accept          -> "accept"
    AcceptCharset   -> "accept-charset"
    Action          -> "action"
    Allow           -> "allow"
    Alt             -> "alt"
    Async           -> "async"
    Autocomplete    -> "autocomplete"
    Autoplay        -> "autoplay"
    Background      -> "background"
    BackgroundColor -> "bgcolor"
    Border          -> "border"
    Capture         -> "capture"
    Charset         -> "charset"
    Checked         -> "checked"
    Cite            -> "cite"
    Color           -> "color"
    Cols            -> "cols"
    Colspan         -> "colspan"
    Content         -> "content"
    Controls        -> "controls"
    Coords          -> "coords"
    CrossOrigin     -> "crossorigin"
    Data            -> "data"
    Datetime        -> "datetime"
    Decoding        -> "decoding"
    Default         -> "default"
    Defer           -> "defer"
    Dirname         -> "dirname"
    Disabled        -> "disabled"
    Download        -> "download"
    Enctype         -> "enctype"
    For             -> "for"
    Form            -> "form"
    FormAction      -> "formaction"
    FormEnctype     -> "formenctype"
    FormMethod      -> "formmethod"
    FormNoValidate  -> "formnovalidate"
    FormTarget      -> "formtarget"
    Headers         -> "headers"
    Height          -> "height"
    High            -> "high"
    Href            -> "href"
    HrefLang        -> "hreflang"
    HttpEquiv       -> "http-equiv"
    Integrity       -> "integrity"
    IsMap           -> "ismap"
    Kind            -> "kind"
    Label           -> "label"
    List            -> "list"
    Loop            -> "loop"
    Low             -> "low"
    Max             -> "max"
    MaxLength       -> "maxlength"
    MinLength       -> "minlength"
    Media           -> "media"
    Method          -> "method"
    Min             -> "min"
    Multiple        -> "multiple"
    Muted           -> "muted"
    Name            -> "name"
    NoValidate      -> "novalidate"
    Open            -> "open"
    Optimum         -> "optimum"
    Pattern         -> "pattern"
    Ping            -> "ping"
    Placeholder     -> "placeholder"
    PlaysInline     -> "playsinline"
    Poster          -> "poster"
    Preload         -> "preload"
    ReadOnly        -> "readonly"
    ReferrerPolicy  -> "referrerpolicy"
    Rel             -> "rel"
    Required        -> "required"
    Reversed        -> "reversed"
    Rows            -> "rows"
    Rowspan         -> "rowspan"
    Sandbox         -> "sandbox"
    Scope           -> "scope"
    Selected        -> "selected"
    Shape           -> "shape"
    Size            -> "size"
    Sizes           -> "sizes"
    Span            -> "span"
    Src             -> "src"
    SrcDoc          -> "srcdoc"
    SrcLang         -> "srclang"
    SrcSet          -> "srcset"
    Start           -> "start"
    Step            -> "step"
    Target          -> "target"
    Type            -> "type"
    UseMap          -> "usemap"
    Value           -> "value"
    Width           -> "width"
    Wrap            -> "wrap"

    -- HTMX Attributes
    --
    Htmx_HxGet         -> "hx-get"
    Htmx_HxPost        -> "hx-post"
    Htmx_HxOn          -> "hx-on" -- TODO
    Htmx_HxPushURL     -> "hx-push-url"
    Htmx_HxSelect      -> "hx-select"
    Htmx_HxSelectOOB   -> "hx-select-oob"
    Htmx_HxSwap        -> "hx-swap"
    Htmx_HxSwapOOB     -> "hx-swap-oob"
    Htmx_HxTarget      -> "hx-target"
    Htmx_HxTrigger     -> "hx-trigger"
    Htmx_HxVals        -> "hx-vals"
    Htmx_HxBoost       -> "hx-boost"
    Htmx_HxConfirm     -> "hx-confirm"
    Htmx_HxDelete      -> "hx-delete"
    Htmx_HxDisable     -> "hx-disable"
    Htmx_HxDisabledElt -> "hx-disabled-elt"
    Htmx_HxDisinherit  -> "hx-disinherit"
    Htmx_HxEncoding    -> "hx-encoding"
    Htmx_HxExt         -> "hx-ext"
    Htmx_HxHeaders     -> "hx-headers"
    Htmx_HxHistory     -> "hx-history"
    Htmx_HxHistoryElt  -> "hx-historyElt"
    Htmx_HxInclude     -> "hx-include"
    Htmx_HxIndicator   -> "hx-indicator"
    Htmx_HxParams      -> "hx-params"
    Htmx_HxPatch       -> "hx-patch"
    Htmx_HxPreserve    -> "hx-preserve"
    Htmx_HxPrompt      -> "hx-prompt"
    Htmx_HxPut         -> "hx-put"
    Htmx_HxReplaceURL  -> "hx-replace-url"
    Htmx_HxRequest     -> "hx-request"
    Htmx_HxSync        -> "hx-sync"
    Htmx_HxValidate    -> "hx-validate"

attributeTypeToText :: AttributeType -> T.Text
attributeTypeToText attr =
  case attr of
    -- Custom Attribute
    CustomAttribute attrName -> attrName

    -- Global Attributes
    --
    AccessKey           -> "accesskey"
    Autocapitalize      -> "autocapitalize"
    Autofocus           -> "autofocus"
    Class               -> "class"
    ContentEditable     -> "contenteditable"
    CustomData attrName -> "data-" <> attrName
    Dir                 -> "dir"
    Draggable           -> "draggable"
    EnterKeyHint        -> "enterkeyhint"
    ExportParts         -> "exportparts"
    Hidden              -> "hidden"
    Id                  -> "id"
    Inert               -> "inert"
    InputMode           -> "inputmode"
    Is                  -> "is"
    ItemId              -> "itemid"
    ItemProp            -> "itemprop"
    ItemRef             -> "itemref"
    ItemScope           -> "itemscope"
    ItemType            -> "itemtype"
    Lang                -> "lang"
    Nonce               -> "nonce"
    Part                -> "part"
    Popover             -> "popover"
    Role                -> "role"
    Slot                -> "slot"
    Spellcheck          -> "spellcheck"
    Style               -> "style"
    TabIndex            -> "tabindex"
    Title               -> "title"
    Translate           -> "translate"

    -- Scoped Attributes
    --
    Accept          -> "accept"
    AcceptCharset   -> "accept-charset"
    Action          -> "action"
    Allow           -> "allow"
    Alt             -> "alt"
    Async           -> "async"
    Autocomplete    -> "autocomplete"
    Autoplay        -> "autoplay"
    Background      -> "background"
    BackgroundColor -> "bgcolor"
    Border          -> "border"
    Capture         -> "capture"
    Charset         -> "charset"
    Checked         -> "checked"
    Cite            -> "cite"
    Color           -> "color"
    Cols            -> "cols"
    Colspan         -> "colspan"
    Content         -> "content"
    Controls        -> "controls"
    Coords          -> "coords"
    CrossOrigin     -> "crossorigin"
    Data            -> "data"
    Datetime        -> "datetime"
    Decoding        -> "decoding"
    Default         -> "default"
    Defer           -> "defer"
    Dirname         -> "dirname"
    Disabled        -> "disabled"
    Download        -> "download"
    Enctype         -> "enctype"
    For             -> "for"
    Form            -> "form"
    FormAction      -> "formaction"
    FormEnctype     -> "formenctype"
    FormMethod      -> "formmethod"
    FormNoValidate  -> "formnovalidate"
    FormTarget      -> "formtarget"
    Headers         -> "headers"
    Height          -> "height"
    High            -> "high"
    Href            -> "href"
    HrefLang        -> "hreflang"
    HttpEquiv       -> "http-equiv"
    Integrity       -> "integrity"
    IsMap           -> "ismap"
    Kind            -> "kind"
    Label           -> "label"
    List            -> "list"
    Loop            -> "loop"
    Low             -> "low"
    Max             -> "max"
    MaxLength       -> "maxlength"
    MinLength       -> "minlength"
    Media           -> "media"
    Method          -> "method"
    Min             -> "min"
    Multiple        -> "multiple"
    Muted           -> "muted"
    Name            -> "name"
    NoValidate      -> "novalidate"
    Open            -> "open"
    Optimum         -> "optimum"
    Pattern         -> "pattern"
    Ping            -> "ping"
    Placeholder     -> "placeholder"
    PlaysInline     -> "playsinline"
    Poster          -> "poster"
    Preload         -> "preload"
    ReadOnly        -> "readonly"
    ReferrerPolicy  -> "referrerpolicy"
    Rel             -> "rel"
    Required        -> "required"
    Reversed        -> "reversed"
    Rows            -> "rows"
    Rowspan         -> "rowspan"
    Sandbox         -> "sandbox"
    Scope           -> "scope"
    Selected        -> "selected"
    Shape           -> "shape"
    Size            -> "size"
    Sizes           -> "sizes"
    Span            -> "span"
    Src             -> "src"
    SrcDoc          -> "srcdoc"
    SrcLang         -> "srclang"
    SrcSet          -> "srcset"
    Start           -> "start"
    Step            -> "step"
    Target          -> "target"
    Type            -> "type"
    UseMap          -> "usemap"
    Value           -> "value"
    Width           -> "width"
    Wrap            -> "wrap"

    -- HTMX Attributes
    --
    Htmx_HxGet         -> "hx-get"
    Htmx_HxPost        -> "hx-post"
    Htmx_HxOn          -> "hx-on" -- TODO
    Htmx_HxPushURL     -> "hx-push-url"
    Htmx_HxSelect      -> "hx-select"
    Htmx_HxSelectOOB   -> "hx-select-oob"
    Htmx_HxSwap        -> "hx-swap"
    Htmx_HxSwapOOB     -> "hx-swap-oob"
    Htmx_HxTarget      -> "hx-target"
    Htmx_HxTrigger     -> "hx-trigger"
    Htmx_HxVals        -> "hx-vals"
    Htmx_HxBoost       -> "hx-boost"
    Htmx_HxConfirm     -> "hx-confirm"
    Htmx_HxDelete      -> "hx-delete"
    Htmx_HxDisable     -> "hx-disable"
    Htmx_HxDisabledElt -> "hx-disabled-elt"
    Htmx_HxDisinherit  -> "hx-disinherit"
    Htmx_HxEncoding    -> "hx-encoding"
    Htmx_HxExt         -> "hx-ext"
    Htmx_HxHeaders     -> "hx-headers"
    Htmx_HxHistory     -> "hx-history"
    Htmx_HxHistoryElt  -> "hx-historyElt"
    Htmx_HxInclude     -> "hx-include"
    Htmx_HxIndicator   -> "hx-indicator"
    Htmx_HxParams      -> "hx-params"
    Htmx_HxPatch       -> "hx-patch"
    Htmx_HxPreserve    -> "hx-preserve"
    Htmx_HxPrompt      -> "hx-prompt"
    Htmx_HxPut         -> "hx-put"
    Htmx_HxReplaceURL  -> "hx-replace-url"
    Htmx_HxRequest     -> "hx-request"
    Htmx_HxSync        -> "hx-sync"
    Htmx_HxValidate    -> "hx-validate"

customAttribute :: T.Text -> T.Text -> AttributeSelector
customAttribute attrName val = (CustomAttribute attrName, Just val)

-- Global Attributes
--

accesskey :: Char -> AttributeSelector
accesskey = (,) AccessKey . Just . T.singleton

autocapitalize :: AutocapitalizeOption -> AttributeSelector
autocapitalize = (,) Autocapitalize . Just . autocapitalizeOptionToText

autofocus :: T.Text -> AttributeSelector
autofocus = (,) Autofocus . Just

class_ :: Class.Class -> AttributeSelector
class_ = (,) Class . Just . Class.classToText

contenteditable :: ContentEditableOption -> AttributeSelector
contenteditable = (,) ContentEditable . Just . contentEditableOptionToText

customData :: T.Text -> T.Text -> AttributeSelector
customData dataName val = (CustomData dataName, Just val)

dir :: Directionality -> AttributeSelector
dir = (,) Dir . Just . directionalityToText

draggable :: Bool -> AttributeSelector
draggable = (,) Draggable . Just . enumBoolToText

enterkeyhint :: KeyHintOption -> AttributeSelector
enterkeyhint = (,) EnterKeyHint . Just . keyHintOptionToText

exportparts :: NEL.NonEmpty ExportPart -> AttributeSelector
exportparts =
  (,) ExportParts
    . Just
    . T.intercalate ", "
    . fmap exportPartToText
    . NEL.toList

hidden :: AttributeSelector
hidden = (Hidden, Nothing)

id :: Id.Id -> AttributeSelector
id = (,) Id . Just . Id.idToText

inert :: AttributeSelector
inert = (Inert, Nothing)

-- TODO
inputmode :: T.Text -> AttributeSelector
inputmode = (,) InputMode . Just

is :: T.Text -> AttributeSelector
is = (,) Is . Just

-- TODO
itemid :: T.Text -> AttributeSelector
itemid = (,) ItemId . Just

-- TODO
itemprop :: T.Text -> AttributeSelector
itemprop = (,) ItemProp . Just

-- TODO
itemref :: T.Text -> AttributeSelector
itemref = (,) ItemRef . Just

-- TODO
itemscope :: T.Text -> AttributeSelector
itemscope = (,) ItemScope . Just

-- TODO
itemtype :: T.Text -> AttributeSelector
itemtype = (,) ItemType . Just

-- TODO
lang :: T.Text -> AttributeSelector
lang = (,) Lang . Just

-- TODO
nonce :: T.Text -> AttributeSelector
nonce = (,) Nonce . Just

part :: NEL.NonEmpty Part -> AttributeSelector
part = (,) Part . Just . T.unwords . fmap partToText . NEL.toList

popover :: PopoverState -> AttributeSelector
popover = (,) Popover . Just . popoverStateToText

-- TODO
role :: T.Text -> AttributeSelector
role = (,) Role . Just

-- TODO
slot :: T.Text -> AttributeSelector
slot = (,) Slot . Just

spellcheck :: Bool -> AttributeSelector
spellcheck = (,) Spellcheck . Just . enumBoolToText

style :: T.Text -> AttributeSelector
style = (,) Style . Just

tabindex :: Int -> AttributeSelector
tabindex = (,) TabIndex . Just . showText

title :: T.Text -> AttributeSelector
title = (,) Title . Just

translate :: Bool -> AttributeSelector
translate = (,) Translate . Just . enumBoolToText

-- Scoped Attributes
--

-- TODO
accept :: T.Text -> AttributeSelector
accept = (,) Accept . Just

-- TODO
acceptCharset :: T.Text -> AttributeSelector
acceptCharset = (,) AcceptCharset . Just

-- TODO
action :: T.Text -> AttributeSelector
action = (,) Action . Just

-- TODO
allow :: T.Text -> AttributeSelector
allow = (,) Allow . Just

-- TODO
alt :: T.Text -> AttributeSelector
alt = (,) Alt . Just

-- TODO
async :: T.Text -> AttributeSelector
async = (,) Async . Just

-- TODO
autocomplete :: T.Text -> AttributeSelector
autocomplete = (,) Autocomplete . Just

-- TODO
autoplay :: T.Text -> AttributeSelector
autoplay = (,) Autoplay . Just

-- TODO
background :: T.Text -> AttributeSelector
background = (,) Background . Just

-- TODO
bgcolor :: T.Text -> AttributeSelector
bgcolor = (,) BackgroundColor . Just

-- TODO
border :: T.Text -> AttributeSelector
border = (,) Border . Just

-- TODO
capture :: T.Text -> AttributeSelector
capture = (,) Capture . Just

-- TODO
charset :: T.Text -> AttributeSelector
charset = (,) Charset . Just

-- TODO
checked :: T.Text -> AttributeSelector
checked = (,) Checked . Just

-- TODO
cite :: T.Text -> AttributeSelector
cite = (,) Cite . Just

-- TODO
color :: T.Text -> AttributeSelector
color = (,) Color . Just

-- TODO
cols :: T.Text -> AttributeSelector
cols = (,) Cols . Just

-- TODO
colspan :: T.Text -> AttributeSelector
colspan = (,) Colspan . Just

-- TODO
content :: T.Text -> AttributeSelector
content = (,) Content . Just

-- TODO
controls :: T.Text -> AttributeSelector
controls = (,) Controls . Just

-- TODO
coords :: T.Text -> AttributeSelector
coords = (,) Coords . Just

crossorigin :: CrossOriginFetch -> AttributeSelector
crossorigin = (,) CrossOrigin . Just . crossoriginFetchToText

-- TODO
data_ :: T.Text -> AttributeSelector
data_ = (,) Data . Just

-- TODO
datetime :: T.Text -> AttributeSelector
datetime = (,) Datetime . Just

-- TODO
decoding :: T.Text -> AttributeSelector
decoding = (,) Decoding . Just

-- TODO
default_ :: T.Text -> AttributeSelector
default_ = (,) Default . Just

-- TODO
defer :: T.Text -> AttributeSelector
defer = (,) Defer . Just

-- TODO
dirname :: T.Text -> AttributeSelector
dirname = (,) Dirname . Just

disabled :: AttributeSelector
disabled = (Disabled, Nothing)

-- TODO
download :: T.Text -> AttributeSelector
download = (,) Download . Just

-- TODO
enctype :: T.Text -> AttributeSelector
enctype = (,) Enctype . Just

-- TODO
for :: T.Text -> AttributeSelector
for = (,) For . Just

-- TODO
form :: T.Text -> AttributeSelector
form = (,) Form . Just

-- TODO
formaction :: T.Text -> AttributeSelector
formaction = (,) FormAction . Just

-- TODO
formenctype :: T.Text -> AttributeSelector
formenctype = (,) FormEnctype . Just

-- TODO
formmethod :: T.Text -> AttributeSelector
formmethod = (,) FormMethod . Just

-- TODO
formnovalidate :: T.Text -> AttributeSelector
formnovalidate = (,) FormNoValidate . Just

-- TODO
formtarget :: T.Text -> AttributeSelector
formtarget = (,) FormTarget . Just

-- TODO
headers :: T.Text -> AttributeSelector
headers = (,) Headers . Just

-- TODO
height :: T.Text -> AttributeSelector
height = (,) Height . Just

-- TODO
high :: T.Text -> AttributeSelector
high = (,) High . Just

href :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf href HrefSelectorTypes
        )
     => href -> AttributeSelector
href = (,) Href . Just . hrefSelectorToText . mkHrefSelector

-- TODO
hreflang :: T.Text -> AttributeSelector
hreflang = (,) HrefLang . Just

-- TODO
httpEquiv :: T.Text -> AttributeSelector
httpEquiv = (,) HttpEquiv . Just

-- TODO
integrity :: T.Text -> AttributeSelector
integrity = (,) Integrity . Just

-- TODO
ismap :: T.Text -> AttributeSelector
ismap = (,) IsMap . Just

-- TODO
kind :: T.Text -> AttributeSelector
kind = (,) Kind . Just

-- TODO
label :: T.Text -> AttributeSelector
label = (,) Label . Just

-- TODO
list :: T.Text -> AttributeSelector
list = (,) List . Just

-- TODO
loop :: T.Text -> AttributeSelector
loop = (,) Loop . Just

-- TODO
low :: T.Text -> AttributeSelector
low = (,) Low . Just

-- TODO
max :: T.Text -> AttributeSelector
max = (,) Max . Just

-- TODO
maxlength :: T.Text -> AttributeSelector
maxlength = (,) MaxLength . Just

-- TODO
minlength :: T.Text -> AttributeSelector
minlength = (,) MinLength . Just

-- TODO
media :: T.Text -> AttributeSelector
media = (,) Media . Just

-- TODO
method :: T.Text -> AttributeSelector
method = (,) Method . Just

-- TODO
min :: T.Text -> AttributeSelector
min = (,) Min . Just

-- TODO
multiple :: T.Text -> AttributeSelector
multiple = (,) Multiple . Just

-- TODO
muted :: T.Text -> AttributeSelector
muted = (,) Muted . Just

-- TODO
name :: T.Text -> AttributeSelector
name = (,) Name . Just

-- TODO
novalidate :: T.Text -> AttributeSelector
novalidate = (,) NoValidate . Just

-- TODO
open :: T.Text -> AttributeSelector
open = (,) Open . Just

-- TODO
optimum :: T.Text -> AttributeSelector
optimum = (,) Optimum . Just

-- TODO
pattern :: T.Text -> AttributeSelector
pattern = (,) Pattern . Just

-- TODO
ping :: T.Text -> AttributeSelector
ping = (,) Ping . Just

-- TODO
placeholder :: T.Text -> AttributeSelector
placeholder = (,) Placeholder . Just

-- TODO
playsinline :: T.Text -> AttributeSelector
playsinline = (,) PlaysInline . Just

-- TODO
poster :: T.Text -> AttributeSelector
poster = (,) Poster . Just

-- TODO
preload :: T.Text -> AttributeSelector
preload = (,) Preload . Just

-- TODO
readonly :: T.Text -> AttributeSelector
readonly = (,) ReadOnly . Just

-- TODO
referrerpolicy :: T.Text -> AttributeSelector
referrerpolicy = (,) ReferrerPolicy . Just

-- TODO
rel :: T.Text -> AttributeSelector
rel = (,) Rel . Just

-- TODO
required :: T.Text -> AttributeSelector
required = (,) Required . Just

-- TODO
reversed :: T.Text -> AttributeSelector
reversed = (,) Reversed . Just

-- TODO
rows :: T.Text -> AttributeSelector
rows = (,) Rows . Just

-- TODO
rowspan :: T.Text -> AttributeSelector
rowspan = (,) Rowspan . Just

-- TODO
sandbox :: T.Text -> AttributeSelector
sandbox = (,) Sandbox . Just

-- TODO
scope :: T.Text -> AttributeSelector
scope = (,) Scope . Just

-- TODO
selected :: T.Text -> AttributeSelector
selected = (,) Selected . Just

-- TODO
shape :: T.Text -> AttributeSelector
shape = (,) Shape . Just

-- TODO
size :: T.Text -> AttributeSelector
size = (,) Size . Just

-- TODO
sizes :: T.Text -> AttributeSelector
sizes = (,) Sizes . Just

-- TODO
span :: T.Text -> AttributeSelector
span = (,) Span . Just

-- TODO
src :: T.Text -> AttributeSelector
src = (,) Src . Just

-- TODO
srcdoc :: T.Text -> AttributeSelector
srcdoc = (,) SrcDoc . Just

-- TODO
srclang :: T.Text -> AttributeSelector
srclang = (,) SrcLang . Just

-- TODO
srcset :: T.Text -> AttributeSelector
srcset = (,) SrcSet . Just

-- TODO
start :: T.Text -> AttributeSelector
start = (,) Start . Just

-- TODO
step :: T.Text -> AttributeSelector
step = (,) Step . Just

-- TODO
target :: T.Text -> AttributeSelector
target = (,) Target . Just

-- TODO
type_ :: T.Text -> AttributeSelector
type_ = (,) Type . Just

-- TODO
usemap :: T.Text -> AttributeSelector
usemap = (,) UseMap . Just

-- TODO
value :: T.Text -> AttributeSelector
value = (,) Value . Just

-- TODO
width :: T.Text -> AttributeSelector
width = (,) Width . Just

-- TODO
wrap :: T.Text -> AttributeSelector
wrap = (,) Wrap . Just

-- HTMX Attributes
--

hxGet :: RelativeURL Get -> AttributeSelector
hxGet = (,) Htmx_HxGet . Just . relativeURLToText

hxPost :: RelativeURL Post -> AttributeSelector
hxPost = (,) Htmx_HxPost . Just . relativeURLToText

-- TODO
hxOn :: T.Text -> AttributeSelector
hxOn = (,) Htmx_HxOn . Just

hxPushURL :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf url PushURLTypes
             )
          => url -> AttributeSelector
hxPushURL = (,) Htmx_HxPushURL . Just . pushURLToText . mkPushURL

-- TODO
hxSelect :: T.Text -> AttributeSelector
hxSelect = (,) Htmx_HxSelect . Just

-- TODO
hxSelectOOB :: T.Text -> AttributeSelector
hxSelectOOB = (,) Htmx_HxSelectOOB . Just

-- TODO
hxSwap :: T.Text -> AttributeSelector
hxSwap = (,) Htmx_HxSwap . Just

-- TODO
hxSwapOOB :: T.Text -> AttributeSelector
hxSwapOOB = (,) Htmx_HxSwapOOB . Just

-- TODO
hxTarget :: T.Text -> AttributeSelector
hxTarget = (,) Htmx_HxTarget . Just

-- TODO
hxTrigger :: T.Text -> AttributeSelector
hxTrigger = (,) Htmx_HxTrigger . Just

-- TODO
hxVals :: T.Text -> AttributeSelector
hxVals = (,) Htmx_HxVals . Just

hxBoost :: Bool -> AttributeSelector
hxBoost = (,) Htmx_HxBoost . Just . enumBoolToText

hxConfirm :: T.Text -> AttributeSelector
hxConfirm = (,) Htmx_HxConfirm . Just

hxDelete :: RelativeURL Delete -> AttributeSelector
hxDelete = (,) Htmx_HxDelete . Just . relativeURLToText

hxDisable :: AttributeSelector
hxDisable = (Htmx_HxDisable, Nothing)

-- TODO
hxDisabledElt :: T.Text -> AttributeSelector
hxDisabledElt = (,) Htmx_HxDisabledElt . Just

hxDisinherit :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf disinherit DisinheritTypes
                )
             => disinherit -> AttributeSelector
hxDisinherit = (,) Htmx_HxDisinherit . Just . disinheritToText . mkDisinherit

hxEncoding :: AttributeSelector
hxEncoding = (Htmx_HxEncoding, Just "multipart/form-data")

hxExt :: NEL.NonEmpty Extension -> AttributeSelector
hxExt =
  (,) Htmx_HxExt
    . Just
    . T.intercalate ","
    . fmap extensionToText
    . NEL.toList

-- TODO
hxHeaders :: T.Text -> AttributeSelector
hxHeaders = (,) Htmx_HxHeaders . Just

hxHistory :: AttributeSelector
hxHistory = (Htmx_HxHistory, Just "false")

hxHistoryElt :: AttributeSelector
hxHistoryElt = (Htmx_HxHistoryElt, Nothing)

-- TODO
hxInclude :: T.Text -> AttributeSelector
hxInclude = (,) Htmx_HxInclude . Just

-- TODO
hxIndicator :: T.Text -> AttributeSelector
hxIndicator = (,) Htmx_HxIndicator . Just

-- TODO
hxParams :: T.Text -> AttributeSelector
hxParams = (,) Htmx_HxParams . Just

hxPatch :: RelativeURL Patch -> AttributeSelector
hxPatch = (,) Htmx_HxPatch . Just . relativeURLToText

-- TODO
hxPreserve :: T.Text -> AttributeSelector
hxPreserve = (,) Htmx_HxPreserve . Just

hxPrompt :: T.Text -> AttributeSelector
hxPrompt = (,) Htmx_HxPrompt . Just

hxPut :: RelativeURL Put -> AttributeSelector
hxPut = (,) Htmx_HxPut . Just . relativeURLToText

hxReplaceURL :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf url PushURLTypes
                )
             => url -> AttributeSelector
hxReplaceURL = (,) Htmx_HxReplaceURL . Just . pushURLToText . mkPushURL

-- TODO
hxRequest :: T.Text -> AttributeSelector
hxRequest = (,) Htmx_HxRequest . Just

-- TODO
hxSync :: T.Text -> AttributeSelector
hxSync = (,) Htmx_HxSync . Just

hxValidate :: AttributeSelector
hxValidate = (Htmx_HxValidate, Nothing)

-- Helpers
--
enumBoolToText :: Bool -> T.Text
enumBoolToText = B.bool "false" "true"

showText :: Show s => s -> T.Text
showText = T.pack . show
