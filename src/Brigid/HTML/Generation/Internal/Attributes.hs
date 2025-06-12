module Brigid.HTML.Generation.Internal.Attributes
  ( Attribute (..)
  , attributeText
  , accesskey
  , autocapitalize
  , autocorrect
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
  , writingsuggestions
  , abbr
  , accept
  , acceptCharset
  , action
  , allow
  , alt
  , as
  , async
  , autocomplete
  , autoplay
  , blocking
  , capture
  , charset
  , checked
  , cite
  , cols
  , colspan
  , command
  , commandfor
  , content
  , controls
  , controlslist
  , coords
  , crossorigin
  , data_
  , datetime
  , decoding
  , default_
  , defer
  , dirname
  , disabled
  , disablepictureinpicture
  , disableremoteplayback
  , download
  , elementtiming
  , enctype
  , fetchpriority
  , forLabel
  , forOutput
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
  , imagesizes
  , imagesrcset
  , integrity
  , ismap
  , kind
  , label
  , list
  , loading
  , loop
  , low
  , max
  , maxlength
  , media
  , method
  , min
  , minlength
  , multiple
  , muted
  , name
  , nameMeta
  , nomodule
  , novalidate
  , open
  , optimum
  , pattern
  , ping
  , placeholder
  , playsinline
  , popovertarget
  , popovertargetaction
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
  , shadowrootmode
  , shadowrootdelegatesfocus
  , shadowrootclonable
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
  , xmlns
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Integer (Positive)
import Numeric.Natural (Natural)
import Ogma qualified
import Prelude hiding (div, id, map, max, min, reverse, span)

import Brigid.HTML.Generation.Internal.Generators qualified as Generators
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

-- This is effectively just `Brigid.HTML.Attributes.AttributeType`, but we
-- don't want to expose that ADT, and this has fewer constructors because we
-- don't particularly care to generate HTMX or HyperScript for benchmarking.
--
data Attribute
  -- Global Attributes
  --
  = AccessKey Char
  | Autocapitalize Types.AutocapitalizeOption
  | Autocorrect Types.OnOff
  | Autofocus Bool
  | Class Types.Class
  | ContentEditable Types.ContentEditableOption
  | CustomData T.Text T.Text
  | Dir Types.Directionality
  | Draggable Bool
  | EnterKeyHint Types.KeyHintOption
  | ExportParts [Types.ExportPart]
  | Hidden Bool
  | Id Types.Id
  | Inert Bool
  | InputMode Types.InputMode
  | Is T.Text
  | ItemId T.Text
  | ItemProp T.Text
  | ItemRef (NEL.NonEmpty Types.Id)
  | ItemScope
  | ItemType Types.RawURL
  | Lang (Maybe Ogma.BCP_47)
  | Nonce T.Text
  | Part [Types.Part]
  | Popover Types.PopoverState
  | Role Types.Role
  | Slot Types.Name
  | Spellcheck Bool
  | Style T.Text
  | TabIndex Types.Reachability
  | Title T.Text
  | Translate Types.YesNo
  | WritingSuggestions Bool

  -- Scoped Attributes
  --
  | Abbreviation T.Text
  | Accept BS.ByteString
  | AcceptCharset
  | Action Types.RawURL
  | Allow [Types.FeaturePolicyDirective]
  | As Types.As
  | Alt T.Text
  | Async
  | Autocomplete Types.OnOff
  | Autoplay
  | Blocking Types.BlockOption
  | Capture (Maybe Types.CaptureMethod)
  | Charset
  | Checked Bool
  | Cite Types.RawURL
  | Cols Natural
  | Colspan Positive
  | Command Types.CommandOption
  | CommandFor Types.Id
  | Content T.Text
  | Controls
  | ControlsList Types.ControlsList
  | Coords (NEL.NonEmpty Integer)
  | CrossOrigin Types.CrossOriginFetch
  | Data Types.RawURL
  | Datetime String
  | Decoding Types.Decoding
  | Default
  | Defer
  | Dirname T.Text
  | Disabled Bool
  | DisablePictureInPicture
  | DisableRemotePlayback
  | Download (Maybe NET.NonEmptyText)
  | ElementTiming T.Text
  | Enctype BS.ByteString
  | FetchPriority Types.FetchPriority
  | ForLabel Types.Id
  | ForOutput (NEL.NonEmpty Types.Id)
  | Form Types.Id
  | FormAction Types.RawURL
  | FormEnctype BS.ByteString
  | FormMethod Types.FormMethod
  | FormNoValidate
  | FormTarget Types.Target
  | Headers [Types.Id]
  | Height Positive
  | High Types.Number
  | Href Types.RawURL
  | HrefLang Ogma.BCP_47
  | HttpEquiv Types.HttpEquivToken
  | ImageSizes (NEL.NonEmpty Types.Size)
  | ImageSrcset (NEL.NonEmpty Types.SrcsetCandidate)
  | Integrity BS.ByteString
  | IsMap
  | Kind Types.TrackKind
  | Label T.Text
  | List Types.Id
  | Loading Types.LoadOption
  | Loop
  | Low Types.Number
  | Max Types.RawRangeBound
  | MaxLength Natural
  | Media (NEL.NonEmpty Types.MediaQuery)
  | Method Types.FormMethod
  | Min Types.RawRangeBound
  | MinLength Natural
  | Multiple
  | Muted Bool
  | Name Types.Name
  | NameMeta Types.MetadataName
  | NoModule Bool
  | NoValidate Bool
  | Open
  | Optimum Types.Number
  | Pattern T.Text
  | Ping (NEL.NonEmpty Types.RawURL)
  | Placeholder T.Text
  | PlaysInline Bool
  | PopoverTarget Types.Id
  | PopoverTargetAction Types.PopoverTargetAction
  | Poster Types.RawURL
  | Preload Types.Preload
  | ReadOnly
  | ReferrerPolicy Types.ReferrerPolicy
  | Rel Types.Rel_Help
  | Required Bool
  | Reversed Bool
  | Rows Natural
  | Rowspan Positive
  | Sandbox [Types.SandboxToken]
  | Scope Types.Scope
  | Selected Bool
  | ShadowRootMode Types.OpenClosed
  | ShadowRootDelegatesFocus
  | ShadowRootClonable
  | Shape Types.Shape
  | Size Positive
  | Sizes (NEL.NonEmpty Types.Size)
  | Span Positive
  | Src Types.RawURL
  | SrcDoc LBS.ByteString
  | SrcLang Ogma.BCP_47
  | SrcSet (NEL.NonEmpty Types.SrcsetCandidate)
  | Start Integer
  | Step Types.Step
  | Target Types.Target
  | Type Types.RawTypeOption
  | UseMap Types.Name
  | Value T.Text
  | Width Positive
  | Wrap Types.Wrap
  | XMLNS Types.RawURL
  deriving Show

attributeText :: Attribute -> T.Text
attributeText attr =
  case attr of
    -- Global Attributes
    --
    AccessKey _ -> "accesskey"
    Autocapitalize _ -> "autocapitalize"
    Autocorrect _ -> "autocorrect"
    Autofocus _ -> "autofocus"
    Class _ -> "class"
    ContentEditable _ -> "contenteditable"
    CustomData d _ -> "data-" <> d
    Dir _ -> "dir"
    Draggable _ -> "draggable"
    EnterKeyHint _ -> "enterkeyhint"
    ExportParts _ -> "exportparts"
    Hidden _ -> "hidden"
    Id _ -> "id"
    Inert _ -> "inert"
    InputMode _ -> "inputmode"
    Is _ -> "is"
    ItemId _ -> "itemid"
    ItemProp _ -> "itemprop"
    ItemRef _ -> "itemref"
    ItemScope -> "itemscope"
    ItemType _ -> "itemtype"
    Lang _ -> "lang"
    Nonce _ -> "nonce"
    Part _ -> "part"
    Popover _ -> "popover"
    Role _ -> "role"
    Slot _ -> "slot"
    Spellcheck _ -> "spellcheck"
    Style _ -> "style"
    TabIndex _ -> "tabindex"
    Title _ -> "title"
    Translate _ -> "translate"
    WritingSuggestions _ -> "writingsuggestions"

    -- Scoped Attributes
    --
    Abbreviation _ -> "abbreviation"
    Accept _ -> "accept"
    AcceptCharset -> "accept-charset"
    Action _ -> "action"
    Allow _ -> "allow"
    As _ -> "as"
    Alt _ -> "alt"
    Async -> "async"
    Autocomplete _ -> "autocomplete"
    Autoplay -> "autoplay"
    Blocking _ -> "blocking"
    Capture _ -> "capture"
    Charset -> "charset"
    Checked _ -> "checked"
    Cite _ -> "cite"
    Cols _ -> "cols"
    Colspan _ -> "colspan"
    Command _ -> "command"
    CommandFor _ -> "commandfor"
    Content _ -> "content"
    Controls -> "controls"
    ControlsList _ -> "constrolslist"
    Coords _ -> "coords"
    CrossOrigin _ -> "crossorigin"
    Data _ -> "data"
    Datetime _ -> "datetime"
    Decoding _ -> "decoding"
    Default -> "default"
    Defer -> "defer"
    Dirname _ -> "dirname"
    Disabled _ -> "disabled"
    DisablePictureInPicture -> "disablepictureinpicture"
    DisableRemotePlayback -> "disableremoteplayback"
    Download _ -> "download"
    ElementTiming _ -> "elementtiming"
    Enctype _ -> "enctype"
    FetchPriority _ -> "fetchpriority"
    ForLabel _ -> "for"
    ForOutput _ -> "for"
    Form _ -> "form"
    FormAction _ -> "formaction"
    FormEnctype _ -> "formenctype"
    FormMethod _ -> "formmethod"
    FormNoValidate -> "formnovalidate"
    FormTarget _ -> "formtarget"
    Headers _ -> "headers"
    Height _ -> "height"
    High _ -> "high"
    Href _ -> "href"
    HrefLang _ -> "hreflang"
    HttpEquiv _ -> "http-equiv"
    ImageSizes _ -> "imagesizes"
    ImageSrcset _ -> "imagesrcset"
    Integrity _ -> "integrity"
    IsMap -> "ismap"
    Kind _ -> "kind"
    Label _ -> "label"
    List _ -> "list"
    Loading _ -> "loading"
    Loop -> "loop"
    Low _ -> "low"
    Max _ -> "max"
    MaxLength _ -> "maxlength"
    Media _ -> "media"
    Method _ -> "method"
    Min _ -> "min"
    MinLength _ -> "minlength"
    Multiple -> "multiple"
    Muted _ -> "muted"
    Name _ -> "name"
    NameMeta _ -> "name"
    NoModule _ -> "nomodule"
    NoValidate _ -> "novalidate"
    Open -> "open"
    Optimum _ -> "optimum"
    Pattern _ -> "pattern"
    Ping _ -> "ping"
    Placeholder _ -> "placeholder"
    PlaysInline _ -> "playsinline"
    PopoverTarget _ -> "popovertarget"
    PopoverTargetAction _ -> "popovertargetaction"
    Poster _ -> "poster"
    Preload _ -> "preload"
    ReadOnly -> "readonly"
    ReferrerPolicy _ -> "referrerpolicy"
    Rel _ -> "rel"
    Required _ -> "required"
    Reversed _ -> "reversed"
    Rows _ -> "rows"
    Rowspan _ -> "rowspan"
    Sandbox _ -> "sandbox"
    Scope _ -> "scope"
    Selected _ -> "selected"
    ShadowRootMode _ -> "shadowrootmode"
    ShadowRootDelegatesFocus -> "shadowrootdelegatesfocus"
    ShadowRootClonable -> "shadowrootclonable"
    Shape _ -> "shape"
    Size _ -> "size"
    Sizes _ -> "sizes"
    Span _ -> "span"
    Src _ -> "src"
    SrcDoc _ -> "srcdoc"
    SrcLang _ -> "srclang"
    SrcSet _ -> "srcset"
    Start _ -> "start"
    Step _ -> "step"
    Target _ -> "target"
    Type _ -> "type"
    UseMap _ -> "usemap"
    Value _ -> "value"
    Width _ -> "width"
    Wrap _ -> "wrap"
    XMLNS _ -> "xmlns"

accesskey :: MonadGen m => m Attribute
accesskey =
  AccessKey <$> Generators.char

autocapitalize :: MonadGen m => m Attribute
autocapitalize =
  Autocapitalize <$> Generators.autocapitalizeOption

autocorrect :: MonadGen m => m Attribute
autocorrect =
  Autocorrect <$> Generators.onOff

autofocus :: MonadGen m => m Attribute
autofocus =
  Autofocus <$> Gen.bool

class_ :: MonadGen m => m Attribute
class_ =
  Class <$> Generators.class_

contenteditable :: MonadGen m => m Attribute
contenteditable =
  ContentEditable <$> Generators.contentEditableOption

customData :: MonadGen m => m Attribute
customData =
  CustomData
    <$> Generators.text
    <*> Generators.text

dir :: MonadGen m => m Attribute
dir =
  Dir <$> Generators.directionality

draggable :: MonadGen m => m Attribute
draggable =
  Draggable <$> Gen.bool

enterkeyhint :: MonadGen m => m Attribute
enterkeyhint =
  EnterKeyHint <$> Generators.keyHintOption

exportparts :: MonadGen m => m Attribute
exportparts =
  ExportParts <$> Gen.list (Range.linear 0 6) Generators.exportPart

hidden :: MonadGen m => m Attribute
hidden =
  Hidden <$> Gen.bool

id :: MonadGen m => m Attribute
id =
  Id <$> Generators.id

inert :: MonadGen m => m Attribute
inert =
  Inert <$> Gen.bool

inputmode :: MonadGen m => m Attribute
inputmode =
  InputMode <$> Generators.inputMode

is :: MonadGen m => m Attribute
is =
  Is <$> Generators.text

itemid :: MonadGen m => m Attribute
itemid =
  ItemId <$> Generators.text

itemprop :: MonadGen m => m Attribute
itemprop =
  ItemProp <$> Generators.text

itemref :: MonadGen m => m Attribute
itemref =
  ItemRef <$> Gen.nonEmpty (Range.linear 1 6) Generators.id

itemscope :: MonadGen m => m Attribute
itemscope =
  pure ItemScope

itemtype :: MonadGen m => m Attribute
itemtype =
  ItemType <$> Generators.url

lang :: MonadGen m => m Attribute
lang =
  Lang <$> Gen.maybe Generators.bcp47

nonce :: MonadGen m => m Attribute
nonce =
  Nonce <$> Generators.text

part :: MonadGen m => m Attribute
part =
  Part <$> Gen.list (Range.linear 0 5) Generators.part

popover :: MonadGen m => m Attribute
popover =
  Popover <$> Generators.popoverState

role :: MonadGen m => m Attribute
role =
  Role <$> Generators.role

slot :: MonadGen m => m Attribute
slot =
  Slot <$> Generators.name

spellcheck :: MonadGen m => m Attribute
spellcheck =
  Spellcheck <$> Gen.bool

style :: MonadGen m => m Attribute
style =
  Style <$> Generators.text

tabindex :: MonadGen m => m Attribute
tabindex =
  TabIndex <$> Gen.enumBounded

title :: MonadGen m => m Attribute
title =
  Title <$> Generators.text

translate :: MonadGen m => m Attribute
translate =
  Translate <$> Generators.yesNo

writingsuggestions :: MonadGen m => m Attribute
writingsuggestions =
  WritingSuggestions <$> Gen.bool

abbr :: MonadGen m => m Attribute
abbr =
  Abbreviation <$> Generators.text

accept :: MonadGen m => m Attribute
accept =
  Accept <$> Generators.byteString

acceptCharset :: MonadGen m => m Attribute
acceptCharset =
  pure AcceptCharset

action :: MonadGen m => m Attribute
action =
  Action <$> Generators.url

allow :: MonadGen m => m Attribute
allow =
  Allow <$> Gen.list (Range.linear 0 6) Generators.featurePolicyDirective

alt :: MonadGen m => m Attribute
alt =
  Alt <$> Generators.text

as :: MonadGen m => m Attribute
as =
  As <$> Generators.as

async :: MonadGen m => m Attribute
async =
  pure Async

autocomplete :: MonadGen m => m Attribute
autocomplete =
  Autocomplete <$> Generators.onOff

autoplay :: MonadGen m => m Attribute
autoplay =
  pure Autoplay

blocking :: MonadGen m => m Attribute
blocking =
  Blocking <$> Gen.enumBounded

capture :: MonadGen m => m Attribute
capture =
  Capture <$> Gen.maybe Generators.captureMethod

charset :: MonadGen m => m Attribute
charset =
  pure Charset

checked :: MonadGen m => m Attribute
checked =
  Checked <$> Gen.bool

cite :: MonadGen m => m Attribute
cite =
  Cite <$> Generators.url

cols :: MonadGen m => m Attribute
cols =
  Cols <$> Generators.natural

colspan :: MonadGen m => m Attribute
colspan =
  Colspan <$> Generators.positive

command :: MonadGen m => m Attribute
command =
  Command <$> Generators.commandOption

commandfor :: MonadGen m => m Attribute
commandfor =
  CommandFor <$> Generators.id

content :: MonadGen m => m Attribute
content =
  Content <$> Generators.text

controls :: MonadGen m => m Attribute
controls =
  pure Controls

controlslist :: MonadGen m => m Attribute
controlslist =
  ControlsList <$> Generators.controlsList

coords :: MonadGen m => m Attribute
coords =
  Coords <$> Gen.nonEmpty (Range.linear 1 10) Generators.integer

crossorigin :: MonadGen m => m Attribute
crossorigin =
  CrossOrigin <$> Generators.crossOriginFetch

data_ :: MonadGen m => m Attribute
data_ =
  Data <$> Generators.url

datetime :: MonadGen m => m Attribute
datetime =
  Datetime <$> Generators.string

decoding :: MonadGen m => m Attribute
decoding =
  Decoding <$> Generators.decoding

default_ :: MonadGen m => m Attribute
default_ =
  pure Default

defer :: MonadGen m => m Attribute
defer =
  pure Defer

dirname :: MonadGen m => m Attribute
dirname =
  Dirname <$> Generators.text

disabled :: MonadGen m => m Attribute
disabled =
  Disabled <$> Gen.bool

disablepictureinpicture :: MonadGen m => m Attribute
disablepictureinpicture =
  pure DisablePictureInPicture

disableremoteplayback :: MonadGen m => m Attribute
disableremoteplayback =
  pure DisableRemotePlayback

download :: MonadGen m => m Attribute
download =
  Download <$> Gen.maybe Generators.nonEmptyText

elementtiming :: MonadGen m => m Attribute
elementtiming =
  ElementTiming <$> Generators.text

enctype :: MonadGen m => m Attribute
enctype =
  Enctype <$> Generators.byteString

fetchpriority :: MonadGen m => m Attribute
fetchpriority =
  FetchPriority <$> Generators.fetchPriority

forLabel :: MonadGen m => m Attribute
forLabel =
  ForLabel <$> Generators.id

forOutput :: MonadGen m => m Attribute
forOutput =
  ForOutput <$> Gen.nonEmpty (Range.linear 1 10) Generators.id

form :: MonadGen m => m Attribute
form =
  Form <$> Generators.id

formaction :: MonadGen m => m Attribute
formaction =
  FormAction <$> Generators.url

formenctype :: MonadGen m => m Attribute
formenctype =
  FormEnctype <$> Generators.byteString

formmethod :: MonadGen m => m Attribute
formmethod =
  FormMethod <$> Generators.formMethod

formnovalidate :: MonadGen m => m Attribute
formnovalidate =
  pure FormNoValidate

formtarget :: MonadGen m => m Attribute
formtarget =
  FormTarget <$> Generators.target

headers :: MonadGen m => m Attribute
headers =
  Headers <$> Gen.list (Range.linear 0 5) Generators.id

height :: MonadGen m => m Attribute
height =
  Height <$> Generators.positive

high :: MonadGen m => m Attribute
high =
  High <$> Generators.number

href :: MonadGen m => m Attribute
href =
  Href <$> Generators.url

hreflang :: MonadGen m => m Attribute
hreflang =
  HrefLang <$> Generators.bcp47

httpEquiv :: MonadGen m => m Attribute
httpEquiv =
  HttpEquiv <$> Generators.httpEquivToken

imagesizes :: MonadGen m => m Attribute
imagesizes =
  ImageSizes <$> Gen.nonEmpty (Range.linear 1 5) Generators.size

imagesrcset :: MonadGen m => m Attribute
imagesrcset =
  ImageSrcset <$> Gen.nonEmpty (Range.linear 1 6) Generators.srcsetCandidate

integrity :: MonadGen m => m Attribute
integrity =
  Integrity <$> Generators.byteString

ismap :: MonadGen m => m Attribute
ismap =
  pure IsMap

kind :: MonadGen m => m Attribute
kind =
  Kind <$> Generators.trackKind

label :: MonadGen m => m Attribute
label =
  Label <$> Generators.text

list :: MonadGen m => m Attribute
list =
  List <$> Generators.id

loading :: MonadGen m => m Attribute
loading =
  Loading <$> Generators.loadOption

loop :: MonadGen m => m Attribute
loop =
  pure Loop

low :: MonadGen m => m Attribute
low =
  Low <$> Generators.number

max :: MonadGen m => m Attribute
max =
  Max <$> Generators.rangeBound

maxlength :: MonadGen m => m Attribute
maxlength =
  MaxLength <$> Generators.natural

media :: MonadGen m => m Attribute
media =
  Media <$> Gen.nonEmpty (Range.linear 1 5) Generators.mediaQuery

method :: MonadGen m => m Attribute
method =
  Method <$> Generators.formMethod

min :: MonadGen m => m Attribute
min =
  Min <$> Generators.rangeBound

minlength :: MonadGen m => m Attribute
minlength =
  MinLength <$> Generators.natural

multiple :: MonadGen m => m Attribute
multiple =
  pure Multiple

muted :: MonadGen m => m Attribute
muted =
  Muted <$> Gen.bool

name :: MonadGen m => m Attribute
name =
  Name <$> Generators.name

nameMeta :: MonadGen m => m Attribute
nameMeta =
  NameMeta <$> Generators.metadataName

nomodule :: MonadGen m => m Attribute
nomodule =
  NoModule <$> Gen.bool

novalidate :: MonadGen m => m Attribute
novalidate =
  NoValidate <$> Gen.bool

open :: MonadGen m => m Attribute
open =
  pure Open

optimum :: MonadGen m => m Attribute
optimum =
  Optimum <$> Generators.number

pattern :: MonadGen m => m Attribute
pattern =
  Pattern <$> Generators.text

ping :: MonadGen m => m Attribute
ping =
  Ping <$> Gen.nonEmpty (Range.linear 1 5) Generators.url

placeholder :: MonadGen m => m Attribute
placeholder =
  Placeholder <$> Generators.text

playsinline :: MonadGen m => m Attribute
playsinline =
  PlaysInline <$> Gen.bool

popovertarget :: MonadGen m => m Attribute
popovertarget =
  PopoverTarget <$> Generators.id

popovertargetaction :: MonadGen m => m Attribute
popovertargetaction =
  PopoverTargetAction <$> Generators.popoverTargetAction

poster :: MonadGen m => m Attribute
poster =
  Poster <$> Generators.url

preload :: MonadGen m => m Attribute
preload =
  Preload <$> Generators.preload

readonly :: MonadGen m => m Attribute
readonly =
  pure ReadOnly

referrerpolicy :: MonadGen m => m Attribute
referrerpolicy =
  ReferrerPolicy <$> Generators.referrerPolicy

rel :: MonadGen m => m Attribute
rel =
  Rel <$> Generators.help

required :: MonadGen m => m Attribute
required =
  Required <$> Gen.bool

reversed :: MonadGen m => m Attribute
reversed =
  Reversed <$> Gen.bool

rows :: MonadGen m => m Attribute
rows =
  Rows <$> Generators.natural

rowspan :: MonadGen m => m Attribute
rowspan =
  Rowspan <$> Generators.positive

sandbox :: MonadGen m => m Attribute
sandbox =
  Sandbox <$> Gen.list (Range.linear 0 5) Generators.sandboxToken

scope :: MonadGen m => m Attribute
scope =
  Scope <$> Generators.scope

selected :: MonadGen m => m Attribute
selected =
  Selected <$> Gen.bool

shadowrootmode :: MonadGen m => m Attribute
shadowrootmode =
  ShadowRootMode <$> Generators.openClosed

shadowrootdelegatesfocus :: MonadGen m => m Attribute
shadowrootdelegatesfocus =
  pure ShadowRootDelegatesFocus

shadowrootclonable :: MonadGen m => m Attribute
shadowrootclonable =
  pure ShadowRootClonable

shape :: MonadGen m => m Attribute
shape =
  Shape <$> Generators.shape

size :: MonadGen m => m Attribute
size =
  Size <$> Generators.positive

sizes :: MonadGen m => m Attribute
sizes =
  Sizes <$> Gen.nonEmpty (Range.linear 1 5) Generators.size

span :: MonadGen m => m Attribute
span =
  Span <$> Generators.positive

src :: MonadGen m => m Attribute
src =
  Src <$> Generators.url

srcdoc :: MonadGen m => m Attribute
srcdoc =
  SrcDoc <$> Generators.lazyByteString

srclang :: MonadGen m => m Attribute
srclang =
  SrcLang <$> Generators.bcp47

srcset :: MonadGen m => m Attribute
srcset =
  SrcSet <$> Gen.nonEmpty (Range.linear 1 6) Generators.srcsetCandidate

start :: MonadGen m => m Attribute
start =
  Start <$> Generators.integer

step :: MonadGen m => m Attribute
step =
  Step <$> Generators.step

target :: MonadGen m => m Attribute
target =
  Target <$> Generators.target

type_ :: MonadGen m => m Attribute
type_ =
  Type <$> Generators.type_

usemap :: MonadGen m => m Attribute
usemap =
  UseMap <$> Generators.name

value :: MonadGen m => m Attribute
value =
  Value <$> Generators.text

width :: MonadGen m => m Attribute
width =
  Width <$> Generators.positive

wrap :: MonadGen m => m Attribute
wrap =
  Wrap <$> Generators.wrap

xmlns :: MonadGen m => m Attribute
xmlns =
  XMLNS <$> Generators.url
