module Generation.Attribute
  ( Attribute (..)
  , accessKey
  , autocapitalize
  , autofocus
  , class_
  , contentEditable
  , dir
  , draggable
  , enterKeyHint
  , exportParts
  , hidden
  , id
  , inert
  , inputMode
  , is
  , itemId
  , itemProp
  , itemRef
  , itemScope
  , itemType
  , lang
  , nonce
  , part
  , popover
  , role
  , slot
  , spellcheck
  , style
  , tabIndex
  , title
  , translate
  , writingSuggestions
  , abbr
  , accept
  , acceptcharset
  , action
  , allow
  , alt
  , as
  , async
  , autocomplete
  , autoplay
  , capture
  , charset
  , checked
  , cite
  , cols
  , colspan
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
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Integer (Positive)
import Numeric.Natural (Natural)
import Ogma qualified
import Prelude hiding (div, id, map, max, min, reverse, span)

import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types
import Generation.Generators qualified as Generators

-- This is effectively just `Brigid.HTML.Attributes.AttributeType`, but we
-- don't want to expose that ADT, and this has fewer constructors because we
-- don't particularly care to generate HTMX or HyperScript for benchmarking.
--
data Attribute
  -- Global Attributes
  --
  = AccessKey Char
  | Autocapitalize Types.AutocapitalizeOption
  | Autofocus Bool
  | Class Types.Class
  | ContentEditable Types.ContentEditableOption
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
  | TabIndex Integer
  | Title T.Text
  | Translate Bool
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
  | Capture (Maybe Types.CaptureMethod)
  | Charset
  | Checked Bool
  | Cite Types.RawURL
  | Cols Natural
  | Colspan Positive
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

accessKey :: Gen Attribute
accessKey =
  AccessKey <$> Generators.char

autocapitalize :: Gen Attribute
autocapitalize =
  Autocapitalize <$> Gen.enumBounded

autofocus :: Gen Attribute
autofocus =
  Autofocus <$> Gen.bool

class_ :: Gen Attribute
class_ =
  Class <$> Generators.class_

contentEditable :: Gen Attribute
contentEditable =
  ContentEditable <$> Gen.enumBounded

dir :: Gen Attribute
dir =
  Dir <$> Gen.enumBounded

draggable :: Gen Attribute
draggable =
  Draggable <$> Gen.bool

enterKeyHint :: Gen Attribute
enterKeyHint =
  EnterKeyHint <$> Gen.enumBounded

exportParts :: Gen Attribute
exportParts =
  ExportParts <$> Gen.list (Range.linear 0 6) Generators.exportPart

hidden :: Gen Attribute
hidden =
  Hidden <$> Gen.bool

id :: Gen Attribute
id =
  Id <$> Generators.id

inert :: Gen Attribute
inert =
  Inert <$> Gen.bool

inputMode :: Gen Attribute
inputMode =
  InputMode <$> Gen.enumBounded

is :: Gen Attribute
is =
  Is <$> Generators.text

itemId :: Gen Attribute
itemId =
  ItemId <$> Generators.text

itemProp :: Gen Attribute
itemProp =
  ItemProp <$> Generators.text

itemRef :: Gen Attribute
itemRef =
  ItemRef <$> Gen.nonEmpty (Range.linear 1 6) Generators.id

itemScope :: Gen Attribute
itemScope =
  pure ItemScope

itemType :: Gen Attribute
itemType =
  ItemType <$> Generators.url

lang :: Gen Attribute
lang =
  Lang <$> Gen.maybe Generators.bcp47

nonce :: Gen Attribute
nonce =
  Nonce <$> Generators.text

part :: Gen Attribute
part =
  Part <$> Gen.list (Range.linear 0 5) Generators.part

popover :: Gen Attribute
popover =
  Popover <$> Gen.enumBounded

role :: Gen Attribute
role =
  Role <$> Gen.enumBounded

slot :: Gen Attribute
slot =
  Slot <$> Generators.name

spellcheck :: Gen Attribute
spellcheck =
  Spellcheck <$> Gen.bool

style :: Gen Attribute
style =
  Style <$> Generators.text

tabIndex :: Gen Attribute
tabIndex =
  TabIndex <$> Generators.integer

title :: Gen Attribute
title =
  Title <$> Generators.text

translate :: Gen Attribute
translate =
  Translate <$> Gen.bool

writingSuggestions :: Gen Attribute
writingSuggestions =
  WritingSuggestions <$> Gen.bool

abbr :: Gen Attribute
abbr =
  Abbreviation <$> Generators.text

accept :: Gen Attribute
accept =
  Accept <$> Generators.byteString

acceptcharset :: Gen Attribute
acceptcharset =
  pure AcceptCharset

action :: Gen Attribute
action =
  Action <$> Generators.url

allow :: Gen Attribute
allow =
  Allow <$> Gen.list (Range.linear 0 6) Gen.enumBounded

alt :: Gen Attribute
alt =
  Alt <$> Generators.text

as :: Gen Attribute
as =
  As <$> Gen.enumBounded

async :: Gen Attribute
async =
  pure Async

autocomplete :: Gen Attribute
autocomplete =
  Autocomplete <$> Generators.onOff

autoplay :: Gen Attribute
autoplay =
  pure Autoplay

capture :: Gen Attribute
capture =
  Capture <$> Gen.maybe Gen.enumBounded

charset :: Gen Attribute
charset =
  pure Charset

checked :: Gen Attribute
checked =
  Checked <$> Gen.bool

cite :: Gen Attribute
cite =
  Cite <$> Generators.url

cols :: Gen Attribute
cols =
  Cols <$> Generators.natural

colspan :: Gen Attribute
colspan =
  Colspan <$> Generators.positive

content :: Gen Attribute
content =
  Content <$> Generators.text

controls :: Gen Attribute
controls =
  pure Controls

controlslist :: Gen Attribute
controlslist =
  ControlsList <$> Gen.enumBounded

coords :: Gen Attribute
coords =
  Coords <$> Gen.nonEmpty (Range.linear 1 10) Generators.integer

crossorigin :: Gen Attribute
crossorigin =
  CrossOrigin <$> Gen.enumBounded

data_ :: Gen Attribute
data_ =
  Data <$> Generators.url

datetime :: Gen Attribute
datetime =
  Datetime <$> Generators.string

decoding :: Gen Attribute
decoding =
  Decoding <$> Gen.enumBounded

default_ :: Gen Attribute
default_ =
  pure Default

defer :: Gen Attribute
defer =
  pure Defer

dirname :: Gen Attribute
dirname =
  Dirname <$> Generators.text

disabled :: Gen Attribute
disabled =
  Disabled <$> Gen.bool

disablepictureinpicture :: Gen Attribute
disablepictureinpicture =
  pure DisablePictureInPicture

disableremoteplayback :: Gen Attribute
disableremoteplayback =
  pure DisableRemotePlayback

download :: Gen Attribute
download =
  Download <$> Gen.maybe Generators.nonEmptyText

enctype :: Gen Attribute
enctype =
  Enctype <$> Generators.byteString

fetchpriority :: Gen Attribute
fetchpriority =
  FetchPriority <$> Gen.enumBounded

forLabel :: Gen Attribute
forLabel =
  ForLabel <$> Generators.id

forOutput :: Gen Attribute
forOutput =
  ForOutput <$> Gen.nonEmpty (Range.linear 1 10) Generators.id

form :: Gen Attribute
form =
  Form <$> Generators.id

formaction :: Gen Attribute
formaction =
  FormAction <$> Generators.url

formenctype :: Gen Attribute
formenctype =
  FormEnctype <$> Generators.byteString

formmethod :: Gen Attribute
formmethod =
  FormMethod <$> Gen.enumBounded

formnovalidate :: Gen Attribute
formnovalidate =
  pure FormNoValidate

formtarget :: Gen Attribute
formtarget =
  FormTarget <$> Generators.target

headers :: Gen Attribute
headers =
  Headers <$> Gen.list (Range.linear 0 5) Generators.id

height :: Gen Attribute
height =
  Height <$> Generators.positive

high :: Gen Attribute
high =
  High <$> Generators.number

href :: Gen Attribute
href =
  Href <$> Generators.url

hreflang :: Gen Attribute
hreflang =
  HrefLang <$> Generators.bcp47

httpEquiv :: Gen Attribute
httpEquiv =
  HttpEquiv <$> Gen.enumBounded

imagesizes :: Gen Attribute
imagesizes =
  ImageSizes <$> Gen.nonEmpty (Range.linear 1 5) Generators.size

imagesrcset :: Gen Attribute
imagesrcset =
  ImageSrcset <$> Gen.nonEmpty (Range.linear 1 6) Generators.srcsetCandidate

integrity :: Gen Attribute
integrity =
  Integrity <$> Generators.byteString

ismap :: Gen Attribute
ismap =
  pure IsMap

kind :: Gen Attribute
kind =
  Kind <$> Gen.enumBounded

label :: Gen Attribute
label =
  Label <$> Generators.text

list :: Gen Attribute
list =
  List <$> Generators.id

loading :: Gen Attribute
loading =
  Loading <$> Gen.enumBounded

loop :: Gen Attribute
loop =
  pure Loop

low :: Gen Attribute
low =
  Low <$> Generators.number

max :: Gen Attribute
max =
  Max <$> Generators.rangeBound

maxlength :: Gen Attribute
maxlength =
  MaxLength <$> Generators.natural

media :: Gen Attribute
media =
  Media <$> Gen.nonEmpty (Range.linear 1 5) Generators.mediaQuery

method :: Gen Attribute
method =
  Method <$> Gen.enumBounded

min :: Gen Attribute
min =
  Min <$> Generators.rangeBound

minlength :: Gen Attribute
minlength =
  MinLength <$> Generators.natural

multiple :: Gen Attribute
multiple =
  pure Multiple

muted :: Gen Attribute
muted =
  Muted <$> Gen.bool

name :: Gen Attribute
name =
  Name <$> Generators.name

nameMeta :: Gen Attribute
nameMeta =
  NameMeta <$> Gen.enumBounded

nomodule :: Gen Attribute
nomodule =
  NoModule <$> Gen.bool

novalidate :: Gen Attribute
novalidate =
  NoValidate <$> Gen.bool

open :: Gen Attribute
open =
  pure Open

optimum :: Gen Attribute
optimum =
  Optimum <$> Generators.number

pattern :: Gen Attribute
pattern =
  Pattern <$> Generators.text

ping :: Gen Attribute
ping =
  Ping <$> Gen.nonEmpty (Range.linear 1 5) Generators.url

placeholder :: Gen Attribute
placeholder =
  Placeholder <$> Generators.text

playsinline :: Gen Attribute
playsinline =
  PlaysInline <$> Gen.bool

popovertarget :: Gen Attribute
popovertarget =
  PopoverTarget <$> Generators.id

popovertargetaction :: Gen Attribute
popovertargetaction =
  PopoverTargetAction <$> Gen.enumBounded

poster :: Gen Attribute
poster =
  Poster <$> Generators.url

preload :: Gen Attribute
preload =
  Preload <$> Gen.enumBounded

readonly :: Gen Attribute
readonly =
  pure ReadOnly

referrerpolicy :: Gen Attribute
referrerpolicy =
  ReferrerPolicy <$> Gen.enumBounded

rel :: Gen Attribute
rel =
  Rel <$> Generators.help

required :: Gen Attribute
required =
  Required <$> Gen.bool

reversed :: Gen Attribute
reversed =
  Reversed <$> Gen.bool

rows :: Gen Attribute
rows =
  Rows <$> Generators.natural

rowspan :: Gen Attribute
rowspan =
  Rowspan <$> Generators.positive

sandbox :: Gen Attribute
sandbox =
  Sandbox <$> Gen.list (Range.linear 0 5) Gen.enumBounded

scope :: Gen Attribute
scope =
  Scope <$> Gen.enumBounded

selected :: Gen Attribute
selected =
  Selected <$> Gen.bool

shape :: Gen Attribute
shape =
  Shape <$> Gen.enumBounded

size :: Gen Attribute
size =
  Size <$> Generators.positive

sizes :: Gen Attribute
sizes =
  Sizes <$> Gen.nonEmpty (Range.linear 1 5) Generators.size

span :: Gen Attribute
span =
  Span <$> Generators.positive

src :: Gen Attribute
src =
  Src <$> Generators.url

srcdoc :: Gen Attribute
srcdoc =
  SrcDoc <$> Generators.lazyByteString

srclang :: Gen Attribute
srclang =
  SrcLang <$> Generators.bcp47

srcset :: Gen Attribute
srcset =
  SrcSet <$> Gen.nonEmpty (Range.linear 1 6) Generators.srcsetCandidate

start :: Gen Attribute
start =
  Start <$> Generators.integer

step :: Gen Attribute
step =
  Step <$> Generators.step

target :: Gen Attribute
target =
  Target <$> Generators.target

type_ :: Gen Attribute
type_ =
  Type <$> Generators.type_

usemap :: Gen Attribute
usemap =
  UseMap <$> Generators.name

value :: Gen Attribute
value =
  Value <$> Generators.text

width :: Gen Attribute
width =
  Width <$> Generators.positive

wrap :: Gen Attribute
wrap =
  Wrap <$> Gen.enumBounded

xmlns :: Gen Attribute
xmlns =
  XMLNS <$> Generators.url
