module HTML.Types.AttributeSelector
  ( AttributeSelector
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
  , hxDelete
  , hxPatch
  , hxPut
  , hxOn
  , hxPushURL
  , hxBoost
  , hxConfirm
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
  , hxPreserve
  , hxPrompt
  , hxReplaceURL
  , hxRequest
  , hxSync
  , hxValidate
  , AttributeType
  ) where

import Prelude hiding (id, max, min, span)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

type AttributeSelector = (AttributeType, T.Text)

attributeSelectorToBytes :: AttributeSelector -> LBS.ByteString
attributeSelectorToBytes (attr, val) =
  LBS.concat
    [ "["
    , attributeTypeToBytes attr
    , "='"
    , LBS.fromStrict (TE.encodeUtf8 val)
    , "']"
    ]

attributeSelectorToText :: AttributeSelector -> T.Text
attributeSelectorToText (attr, val) =
  "[" <> attributeTypeToText attr <> "='" <> val <> "']"

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
  | Htmx_HxDelete
  | Htmx_HxPatch
  | Htmx_HxPut
  | Htmx_HxOn
  | Htmx_HxPushURL
  | Htmx_HxBoost
  | Htmx_HxConfirm
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
  | Htmx_HxPreserve
  | Htmx_HxPrompt
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
    Htmx_HxDelete      -> "hx-delete"
    Htmx_HxPatch       -> "hx-patch"
    Htmx_HxPut         -> "hx-put"
    Htmx_HxOn          -> "hx-on" -- TODO: Revisit this
    Htmx_HxPushURL     -> "hx-push-url"
    Htmx_HxBoost       -> "hx-boost"
    Htmx_HxConfirm     -> "hx-confirm"
    Htmx_HxDisable     -> "hx-disable"
    Htmx_HxDisabledElt -> "hx-disabled-elt"
    Htmx_HxDisinherit  -> "hx-disinherit"
    Htmx_HxEncoding    -> "hx-encoding"
    Htmx_HxExt         -> "hx-ext"
    Htmx_HxHeaders     -> "hx-headers"
    Htmx_HxHistory     -> "hx-history"
    Htmx_HxHistoryElt  -> "hx-history-elt"
    Htmx_HxInclude     -> "hx-include"
    Htmx_HxIndicator   -> "hx-indicator"
    Htmx_HxParams      -> "hx-params"
    Htmx_HxPreserve    -> "hx-preserve"
    Htmx_HxPrompt      -> "hx-prompt"
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
    Htmx_HxDelete      -> "hx-delete"
    Htmx_HxPatch       -> "hx-patch"
    Htmx_HxPut         -> "hx-put"
    Htmx_HxOn          -> "hx-on" -- TODO: Revisit this
    Htmx_HxPushURL     -> "hx-push-url"
    Htmx_HxBoost       -> "hx-boost"
    Htmx_HxConfirm     -> "hx-confirm"
    Htmx_HxDisable     -> "hx-disable"
    Htmx_HxDisabledElt -> "hx-disabled-elt"
    Htmx_HxDisinherit  -> "hx-disinherit"
    Htmx_HxEncoding    -> "hx-encoding"
    Htmx_HxExt         -> "hx-ext"
    Htmx_HxHeaders     -> "hx-headers"
    Htmx_HxHistory     -> "hx-history"
    Htmx_HxHistoryElt  -> "hx-history-elt"
    Htmx_HxInclude     -> "hx-include"
    Htmx_HxIndicator   -> "hx-indicator"
    Htmx_HxParams      -> "hx-params"
    Htmx_HxPreserve    -> "hx-preserve"
    Htmx_HxPrompt      -> "hx-prompt"
    Htmx_HxReplaceURL  -> "hx-replace-url"
    Htmx_HxRequest     -> "hx-request"
    Htmx_HxSync        -> "hx-sync"
    Htmx_HxValidate    -> "hx-validate"

customAttribute :: T.Text -> T.Text -> AttributeSelector
customAttribute = (,) . CustomAttribute

accesskey :: T.Text -> AttributeSelector
accesskey = (,) AccessKey

autocapitalize :: T.Text -> AttributeSelector
autocapitalize = (,) Autocapitalize

autofocus :: T.Text -> AttributeSelector
autofocus = (,) Autofocus

class_ :: T.Text -> AttributeSelector
class_ = (,) Class

contenteditable :: T.Text -> AttributeSelector
contenteditable = (,) ContentEditable

customData :: T.Text -> T.Text -> AttributeSelector
customData = (,) . CustomData

dir :: T.Text -> AttributeSelector
dir = (,) Dir

draggable :: T.Text -> AttributeSelector
draggable = (,) Draggable

enterkeyhint :: T.Text -> AttributeSelector
enterkeyhint = (,) EnterKeyHint

exportparts :: T.Text -> AttributeSelector
exportparts = (,) ExportParts

hidden :: T.Text -> AttributeSelector
hidden = (,) Hidden

id :: T.Text -> AttributeSelector
id = (,) Id

inert :: T.Text -> AttributeSelector
inert = (,) Inert

inputmode :: T.Text -> AttributeSelector
inputmode = (,) InputMode

is :: T.Text -> AttributeSelector
is = (,) Is

itemid :: T.Text -> AttributeSelector
itemid = (,) ItemId

itemprop :: T.Text -> AttributeSelector
itemprop = (,) ItemProp

itemref :: T.Text -> AttributeSelector
itemref = (,) ItemRef

itemscope :: T.Text -> AttributeSelector
itemscope = (,) ItemScope

itemtype :: T.Text -> AttributeSelector
itemtype = (,) ItemType

lang :: T.Text -> AttributeSelector
lang = (,) Lang

nonce :: T.Text -> AttributeSelector
nonce = (,) Nonce

part :: T.Text -> AttributeSelector
part = (,) Part

popover :: T.Text -> AttributeSelector
popover = (,) Popover

role :: T.Text -> AttributeSelector
role = (,) Role

slot :: T.Text -> AttributeSelector
slot = (,) Slot

spellcheck :: T.Text -> AttributeSelector
spellcheck = (,) Spellcheck

style :: T.Text -> AttributeSelector
style = (,) Style

tabindex :: T.Text -> AttributeSelector
tabindex = (,) TabIndex

title :: T.Text -> AttributeSelector
title = (,) Title

translate :: T.Text -> AttributeSelector
translate = (,) Translate

accept :: T.Text -> AttributeSelector
accept = (,) Accept

acceptCharset :: T.Text -> AttributeSelector
acceptCharset = (,) AcceptCharset

action :: T.Text -> AttributeSelector
action = (,) Action

allow :: T.Text -> AttributeSelector
allow = (,) Allow

alt :: T.Text -> AttributeSelector
alt = (,) Alt

async :: T.Text -> AttributeSelector
async = (,) Async

autocomplete :: T.Text -> AttributeSelector
autocomplete = (,) Autocomplete

autoplay :: T.Text -> AttributeSelector
autoplay = (,) Autoplay

background :: T.Text -> AttributeSelector
background = (,) Background

bgcolor :: T.Text -> AttributeSelector
bgcolor = (,) BackgroundColor

border :: T.Text -> AttributeSelector
border = (,) Border

capture :: T.Text -> AttributeSelector
capture = (,) Capture

charset :: T.Text -> AttributeSelector
charset = (,) Charset

checked :: T.Text -> AttributeSelector
checked = (,) Checked

cite :: T.Text -> AttributeSelector
cite = (,) Cite

color :: T.Text -> AttributeSelector
color = (,) Color

cols :: T.Text -> AttributeSelector
cols = (,) Cols

colspan :: T.Text -> AttributeSelector
colspan = (,) Colspan

content :: T.Text -> AttributeSelector
content = (,) Content

controls :: T.Text -> AttributeSelector
controls = (,) Controls

coords :: T.Text -> AttributeSelector
coords = (,) Coords

crossorigin :: T.Text -> AttributeSelector
crossorigin = (,) CrossOrigin

data_ :: T.Text -> AttributeSelector
data_ = (,) Data

datetime :: T.Text -> AttributeSelector
datetime = (,) Datetime

decoding :: T.Text -> AttributeSelector
decoding = (,) Decoding

default_ :: T.Text -> AttributeSelector
default_ = (,) Default

defer :: T.Text -> AttributeSelector
defer = (,) Defer

dirname :: T.Text -> AttributeSelector
dirname = (,) Dirname

disabled :: T.Text -> AttributeSelector
disabled = (,) Disabled

download :: T.Text -> AttributeSelector
download = (,) Download

enctype :: T.Text -> AttributeSelector
enctype = (,) Enctype

for :: T.Text -> AttributeSelector
for = (,) For

form :: T.Text -> AttributeSelector
form = (,) Form

formaction :: T.Text -> AttributeSelector
formaction = (,) FormAction

formenctype :: T.Text -> AttributeSelector
formenctype = (,) FormEnctype

formmethod :: T.Text -> AttributeSelector
formmethod = (,) FormMethod

formnovalidate :: T.Text -> AttributeSelector
formnovalidate = (,) FormNoValidate

formtarget :: T.Text -> AttributeSelector
formtarget = (,) FormTarget

headers :: T.Text -> AttributeSelector
headers = (,) Headers

height :: T.Text -> AttributeSelector
height = (,) Height

high :: T.Text -> AttributeSelector
high = (,) High

href :: T.Text -> AttributeSelector
href = (,) Href

hreflang :: T.Text -> AttributeSelector
hreflang = (,) HrefLang

httpEquiv :: T.Text -> AttributeSelector
httpEquiv = (,) HttpEquiv

integrity :: T.Text -> AttributeSelector
integrity = (,) Integrity

ismap :: T.Text -> AttributeSelector
ismap = (,) IsMap

kind :: T.Text -> AttributeSelector
kind = (,) Kind

label :: T.Text -> AttributeSelector
label = (,) Label

list :: T.Text -> AttributeSelector
list = (,) List

loop :: T.Text -> AttributeSelector
loop = (,) Loop

low :: T.Text -> AttributeSelector
low = (,) Low

max :: T.Text -> AttributeSelector
max = (,) Max

maxlength :: T.Text -> AttributeSelector
maxlength = (,) MaxLength

minlength :: T.Text -> AttributeSelector
minlength = (,) MinLength

media :: T.Text -> AttributeSelector
media = (,) Media

method :: T.Text -> AttributeSelector
method = (,) Method

min :: T.Text -> AttributeSelector
min = (,) Min

multiple :: T.Text -> AttributeSelector
multiple = (,) Multiple

muted :: T.Text -> AttributeSelector
muted = (,) Muted

name :: T.Text -> AttributeSelector
name = (,) Name

novalidate :: T.Text -> AttributeSelector
novalidate = (,) NoValidate

open :: T.Text -> AttributeSelector
open = (,) Open

optimum :: T.Text -> AttributeSelector
optimum = (,) Optimum

pattern :: T.Text -> AttributeSelector
pattern = (,) Pattern

ping :: T.Text -> AttributeSelector
ping = (,) Ping

placeholder :: T.Text -> AttributeSelector
placeholder = (,) Placeholder

playsinline :: T.Text -> AttributeSelector
playsinline = (,) PlaysInline

poster :: T.Text -> AttributeSelector
poster = (,) Poster

preload :: T.Text -> AttributeSelector
preload = (,) Preload

readonly :: T.Text -> AttributeSelector
readonly = (,) ReadOnly

referrerpolicy :: T.Text -> AttributeSelector
referrerpolicy = (,) ReferrerPolicy

rel :: T.Text -> AttributeSelector
rel = (,) Rel

required :: T.Text -> AttributeSelector
required = (,) Required

reversed :: T.Text -> AttributeSelector
reversed = (,) Reversed

rows :: T.Text -> AttributeSelector
rows = (,) Rows

rowspan :: T.Text -> AttributeSelector
rowspan = (,) Rowspan

sandbox :: T.Text -> AttributeSelector
sandbox = (,) Sandbox

scope :: T.Text -> AttributeSelector
scope = (,) Scope

selected :: T.Text -> AttributeSelector
selected = (,) Selected

shape :: T.Text -> AttributeSelector
shape = (,) Shape

size :: T.Text -> AttributeSelector
size = (,) Size

sizes :: T.Text -> AttributeSelector
sizes = (,) Sizes

span :: T.Text -> AttributeSelector
span = (,) Span

src :: T.Text -> AttributeSelector
src = (,) Src

srcdoc :: T.Text -> AttributeSelector
srcdoc = (,) SrcDoc

srclang :: T.Text -> AttributeSelector
srclang = (,) SrcLang

srcset :: T.Text -> AttributeSelector
srcset = (,) SrcSet

start :: T.Text -> AttributeSelector
start = (,) Start

step :: T.Text -> AttributeSelector
step = (,) Step

target :: T.Text -> AttributeSelector
target = (,) Target

type_ :: T.Text -> AttributeSelector
type_ = (,) Type

usemap :: T.Text -> AttributeSelector
usemap = (,) UseMap

value :: T.Text -> AttributeSelector
value = (,) Value

width :: T.Text -> AttributeSelector
width = (,) Width

wrap :: T.Text -> AttributeSelector
wrap = (,) Wrap

hxGet :: T.Text -> AttributeSelector
hxGet = (,) Htmx_HxGet

hxPost :: T.Text -> AttributeSelector
hxPost = (,) Htmx_HxPost

hxDelete :: T.Text -> AttributeSelector
hxDelete = (,) Htmx_HxDelete

hxPatch :: T.Text -> AttributeSelector
hxPatch = (,) Htmx_HxPatch

hxPut :: T.Text -> AttributeSelector
hxPut = (,) Htmx_HxPut

hxOn :: T.Text -> AttributeSelector
hxOn = (,) Htmx_HxOn

hxPushURL :: T.Text -> AttributeSelector
hxPushURL = (,) Htmx_HxPushURL

hxBoost :: T.Text -> AttributeSelector
hxBoost = (,) Htmx_HxBoost

hxConfirm :: T.Text -> AttributeSelector
hxConfirm = (,) Htmx_HxConfirm

hxDisable :: T.Text -> AttributeSelector
hxDisable = (,) Htmx_HxDisable

hxDisabledElt :: T.Text -> AttributeSelector
hxDisabledElt = (,) Htmx_HxDisabledElt

hxDisinherit :: T.Text -> AttributeSelector
hxDisinherit = (,) Htmx_HxDisinherit

hxEncoding :: T.Text -> AttributeSelector
hxEncoding = (,) Htmx_HxEncoding

hxExt :: T.Text -> AttributeSelector
hxExt = (,) Htmx_HxExt

hxHeaders :: T.Text -> AttributeSelector
hxHeaders = (,) Htmx_HxHeaders

hxHistory :: T.Text -> AttributeSelector
hxHistory = (,) Htmx_HxHistory

hxHistoryElt :: T.Text -> AttributeSelector
hxHistoryElt = (,) Htmx_HxHistoryElt

hxInclude :: T.Text -> AttributeSelector
hxInclude = (,) Htmx_HxInclude

hxIndicator :: T.Text -> AttributeSelector
hxIndicator = (,) Htmx_HxIndicator

hxParams :: T.Text -> AttributeSelector
hxParams = (,) Htmx_HxParams

hxPreserve :: T.Text -> AttributeSelector
hxPreserve = (,) Htmx_HxPreserve

hxPrompt :: T.Text -> AttributeSelector
hxPrompt = (,) Htmx_HxPrompt

hxReplaceURL :: T.Text -> AttributeSelector
hxReplaceURL = (,) Htmx_HxReplaceURL

hxRequest :: T.Text -> AttributeSelector
hxRequest = (,) Htmx_HxRequest

hxSync :: T.Text -> AttributeSelector
hxSync = (,) Htmx_HxSync

hxValidate :: T.Text -> AttributeSelector
hxValidate = (,) Htmx_HxValidate
