{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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
import Data.Bool qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import HTML.Types.Autocapitalize (AutocapitalizeOption, autocapitalizeOptionToText)
import HTML.Types.Class qualified as C
import HTML.Types.ContentEditable (ContentEditableOption, contentEditableOptionToText)
import HTML.Types.CrossOrigin (CrossOriginFetch, crossoriginFetchToText)
import HTML.Types.Directionality (Directionality, directionalityToText)
import HTML.Types.Disinherit (DisinheritTypes, disinheritToText, mkDisinherit)
import HTML.Types.Extension (Extension, extensionToText)
import HTML.Types.Id qualified as Id
import HTML.Types.KeyHint (KeyHintOption, keyHintOptionToText)
import HTML.Types.Method (Get, Post, Delete, Put, Patch)
import HTML.Types.Part (ExportPart, Part, exportPartToText, partToText)
import HTML.Types.PopoverState (PopoverState, popoverStateToText)
import HTML.Types.PushURL (PushURLTypes, mkPushURL, pushURLToText)
import HTML.Types.URL (RelativeURL, relativeURLToText)

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
customAttribute attrName val = (CustomAttribute attrName, Just val)

-- Global Attributes
--

accesskey :: Char -> AttributeSelector
accesskey = (,) AccessKey . Just . T.singleton

autocapitalize :: AutocapitalizeOption -> AttributeSelector
autocapitalize = (,) Autocapitalize . Just . autocapitalizeOptionToText

autofocus :: T.Text -> AttributeSelector
autofocus = (,) Autofocus . Just

class_ :: C.Class -> AttributeSelector
class_ = (,) Class . Just . C.classToText

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

-- TODO
href :: T.Text -> AttributeSelector
href = (,) Href . Just

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

hxDelete :: RelativeURL Delete -> AttributeSelector
hxDelete = (,) Htmx_HxDelete . Just . relativeURLToText

hxPatch :: RelativeURL Patch -> AttributeSelector
hxPatch = (,) Htmx_HxPatch . Just . relativeURLToText

hxPut :: RelativeURL Put -> AttributeSelector
hxPut = (,) Htmx_HxPut . Just . relativeURLToText

-- TODO
hxOn :: T.Text -> AttributeSelector
hxOn = (,) Htmx_HxOn . Just

hxPushURL :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf url PushURLTypes
             )
          => url -> AttributeSelector
hxPushURL = (,) Htmx_HxPushURL . Just . pushURLToText . mkPushURL

hxBoost :: Bool -> AttributeSelector
hxBoost = (,) Htmx_HxBoost . Just . enumBoolToText

hxConfirm :: T.Text -> AttributeSelector
hxConfirm = (,) Htmx_HxConfirm . Just

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

-- TODO
hxPreserve :: T.Text -> AttributeSelector
hxPreserve = (,) Htmx_HxPreserve . Just

hxPrompt :: T.Text -> AttributeSelector
hxPrompt = (,) Htmx_HxPrompt . Just

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
