{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Attributes.Scoped
  ( acceptCharset
  , action
  , allow
  , alt
  , async
  , autocomplete
  , autoplay
  , capture
  , charset
  , check
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
  , datetimeWithFormat
  , decoding
  , default_
  , defer
  , dirname
  , disable
  , disabled
  , disablepictureinpicture
  , disableremoteplayback
  , download
  , for
  , form
  , formaction
  , formmethod
  , formnovalidate
  , formtarget
  , headers
  , height
  , high
  , href
  , hreflang
  , httpEquiv
  , ismap
  , kind
  , label
  , list
  , loop
  , low
  , max
  , maxlength
  , method
  , min
  , minlength
  , multiple
  , mute
  , muted
  , name
  , nomodule
  , novalidate
  , optimum
  , ping
  , placeholder
  , playInline
  , playsinline
  , poster
  , preload
  , readonly
  , referrerpolicy
  , rel
  , require
  , required
  , reverse
  , reversed
  , rows
  , rowspan
  , sandbox
  , select
  , selected
  , shape
  , size
  , span
  , src
  , srclang
  , start
  , target
  , type_
  , usemap
  , validate
  , value
  , width
  , wrap
  , xmlns
  ) where

import Prelude hiding (max, min, reverse, span)
import Data.Containers.ListUtils (nubOrdOn)
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (ISO8601, iso8601Show)
import GHC.TypeLits (KnownNat)
import Integer (Positive)
import Ogma qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes.AttributeType (AttributeType (..))
import Brigid.HTML.Attributes.Autocomplete (ValidAutocomplete)
import Brigid.HTML.Attributes.Elements (ValidAttribute)
import Brigid.HTML.Attributes.For (ValidFor)
import Brigid.HTML.Attributes.Href (ValidHref)
import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Name (ValidName)
import Brigid.HTML.Attributes.RangeBound (ValidRangeBound)
import Brigid.HTML.Attributes.Relationship (ValidRelationship)
import Brigid.HTML.Attributes.Source (ValidSource)
import Brigid.HTML.Attributes.Type (ValidTypeOption)
import Brigid.HTML.Attributes.Value (ValidValue)
import Brigid.HTML.Types qualified as Types

-- Scoped Attributes
--

-- | This will default to using "UTF-8" as the only supported encoding.
--
-- Previously, this attribute accepted a list of valid encodings that the
-- server could accept. In the current HTML5 specification, UTF-8 is the
-- recommended and default character encoding for web applications. Specifying
-- multiple encodings is now considered unnecessary and non-standard practice.
--
-- If you must specify multiple encodings, create a custom attribute instead.
--
acceptCharset :: ValidAttribute 'AcceptCharset tag => Attribute tag
acceptCharset = Attr_AcceptCharset

action :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf action Types.ActionTypes
          , ValidAttribute 'Action tag
          )
       => action -> Attribute tag
action =
  Attr_Action . Types.mkAction

allow :: ValidAttribute 'Allow tag
      => [Types.FeaturePolicyDirective] -> Attribute tag
allow = Attr_Allow . nubOrdOn fromEnum

alt :: ValidAttribute 'Alt tag => T.Text -> Attribute tag
alt = Attr_Alt

async :: ValidAttribute 'Async tag => Attribute tag
async = Attr_Async

autocomplete :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf token Types.AutocompleteTokenTypes
                , ValidAutocomplete token tag
                , ValidAttribute 'Autocomplete tag
                )
             => token -> Attribute tag
autocomplete =
  Attr_Autocomplete . Types.mkAutocompleteToken

autoplay :: ValidAttribute 'Autoplay tag => Attribute tag
autoplay = Attr_Autoplay

capture :: ValidAttribute 'Capture tag
        => Maybe Types.CaptureMethod -> Attribute tag
capture = Attr_Capture

-- | Limited to UTF-8, since that is the only valid option for HTML5.
--
charset :: ValidAttribute 'Charset tag => Attribute tag
charset = Attr_Charset

check :: ValidAttribute 'Checked tag => Bool -> Attribute tag
check = Attr_Checked

checked :: ValidAttribute 'Checked tag => Attribute tag
checked = check True

cite :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf cite Types.URLTypes
        , ValidAttribute 'Cite tag
        )
     => cite -> Attribute tag
cite =
  Attr_Cite . Types.mkURL

cols :: ValidAttribute 'Cols tag => Word -> Attribute tag
cols = Attr_Cols

colspan :: ValidAttribute 'Colspan tag => Word -> Attribute tag
colspan = Attr_Colspan

-- | The `content` attribute is left as simple 'T.Text' because its value is
-- dependent on the `name` or `http-equiv` attributes, and managing the
-- inter-dependency across multiple attributes in the `meta` tag is too complex
-- to reconcile here, For safe construction of the `content` attributes
-- together with its dependencies on a `meta` tag, use
-- 'Brigid.HTML.Elements.Safe.Meta'.
--
content :: ValidAttribute 'Content tag => T.Text -> Attribute tag
content = Attr_Content

controls :: ValidAttribute 'Controls tag => Attribute tag
controls = Attr_Controls

controlslist :: ValidAttribute 'ControlsList tag
             => Types.ControlsList -> Attribute tag
controlslist = Attr_ControlsList

-- | The `coords` attribute is used in combination with the `shape` attribute
-- to specify the size and area of an <area> tag in an image <map>. For safe
-- construction of the `coords` and `shape` attributes on an `area` tag, use
-- `Brigid.HTML.Elements.Safe.Area`.
--
coords :: ValidAttribute 'Coords tag => NEL.NonEmpty Word -> Attribute tag
coords = Attr_Coords

{-|
   This enumerated attribute indicates whether CORS must be used when fetching
   the resource. CORS-enabled images can be reused in the @<canvas>@ element
   without being tainted. If the attribute is not present, the resource is
   fetched without a CORS request (i.e. without sending the @Origin@ HTTP
   header), preventing its non-tainted usage. If invalid, it is handled as if
   the enumerated keyword @anonymous@ was used.
-}
crossorigin :: ValidAttribute 'CrossOrigin tag
            => Types.CrossOriginFetch -> Attribute tag
crossorigin = Attr_CrossOrigin

data_ :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf _data Types.URLTypes
          , ValidAttribute 'Data tag
          )
       => _data -> Attribute tag
data_ =
  Attr_Data . Types.mkURL

datetime :: (ISO8601 t, ValidAttribute 'Datetime tag) => t -> Attribute tag
datetime = Attr_Datetime . iso8601Show

-- | This option is provided for users that want to supply their own 'datetime'
-- encodings besides the common standards, and are willing to accept the risks
-- of using a potentially unsupported encoding.
--
datetimeWithFormat :: (Time.FormatTime t, ValidAttribute 'Datetime tag)
                   => String -> t -> Attribute tag
datetimeWithFormat format =
  Attr_Datetime . Time.formatTime Time.defaultTimeLocale format

decoding :: ValidAttribute 'Decoding tag => Types.Decoding -> Attribute tag
decoding = Attr_Decoding

default_ :: ValidAttribute 'Default tag => Attribute tag
default_ = Attr_Default

defer :: ValidAttribute 'Defer tag => Attribute tag
defer = Attr_Defer

dirname :: ValidAttribute 'Dirname tag => T.Text -> Attribute tag
dirname = Attr_Dirname

disable :: ValidAttribute 'Disabled tag => Bool -> Attribute tag
disable = Attr_Disabled

disabled :: ValidAttribute 'Disabled tag => Attribute tag
disabled = disable True

disablepictureinpicture :: ValidAttribute 'DisablePictureInPicture tag
                        => Attribute tag
disablepictureinpicture = Attr_DisablePictureInPicture

disableremoteplayback :: ValidAttribute 'DisableRemotePlayback tag
                      => Attribute tag
disableremoteplayback = Attr_DisableRemotePlayback

download :: ValidAttribute 'Download tag
         => Maybe NET.NonEmptyText -> Attribute tag
download = Attr_Download

for :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf for Types.ForOptionTypes
       , ValidFor for tag
       , ValidAttribute 'For tag
       )
    => for -> Attribute tag
for =
  Attr_For . Types.mkForOption

form :: ValidAttribute 'Form tag => Types.Id -> Attribute tag
form = Attr_Form

formaction :: ( KnownNat branchIndex
              , branchIndex ~ FirstIndexOf action Types.ActionTypes
              , ValidAttribute 'FormAction tag
              )
           => action -> Attribute tag
formaction =
  Attr_FormAction . Types.mkAction

formmethod :: ValidAttribute 'FormMethod tag
           => Types.FormMethod -> Attribute tag
formmethod = Attr_FormMethod

formnovalidate :: ValidAttribute 'FormNoValidate tag => Attribute tag
formnovalidate = Attr_FormNoValidate

formtarget :: ValidAttribute 'FormTarget tag => Types.Target -> Attribute tag
formtarget = Attr_FormTarget

headers :: ValidAttribute 'Headers tag
        => NEL.NonEmpty Types.Id -> Attribute tag
headers = Attr_Headers

height :: ValidAttribute 'Height tag => Word -> Attribute tag
height = Attr_Height

high :: ValidAttribute 'High tag => Types.Number -> Attribute tag
high = Attr_High

href :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf href (Types.HrefTypes Types.Get)
        , ValidHref href tag
        , ValidAttribute 'Href tag
        )
     => href -> Attribute tag
href =
  Attr_Href . Types.mkHref

hreflang :: ValidAttribute 'HrefLang tag => Ogma.BCP_47 -> Attribute tag
hreflang = Attr_HrefLang

httpEquiv :: ValidAttribute 'HttpEquiv tag
          => Types.HttpEquivToken -> Attribute tag
httpEquiv = Attr_HttpEquiv

-- | The `ismap` attribute is only valid on <img> tags that are nested within
-- an <a> tag. For safe construction of the `ismap` attribute, use
-- 'Brigid.HTML.Elements.Safe.Image'.
--
ismap :: ValidAttribute 'IsMap tag => Attribute tag
ismap = Attr_IsMap

kind :: ValidAttribute 'Kind tag => Types.TrackKind -> Attribute tag
kind = Attr_Kind

label :: ValidAttribute 'Label tag => T.Text -> Attribute tag
label = Attr_Label

-- TODO: A Safe module could build inputs and datalists in tandom, taking an Id
-- to ensure proper list application and that the values are valid for the
-- input type.
--
list :: ValidAttribute 'List tag => Types.Id -> Attribute tag
list = Attr_List

loop :: ValidAttribute 'Loop tag => Attribute tag
loop = Attr_Loop

low :: ValidAttribute 'Low tag => Types.Number -> Attribute tag
low = Attr_Low

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/max should be done to ensure that the bounds
-- are applied appropriately.
--
max :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf max Types.RangeBoundTypes
       , ValidRangeBound max tag
       , ValidAttribute 'Max tag
       )
    => max -> Attribute tag
max =
  Attr_Max . Types.mkRangeBound

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/maxlength should be done to ensure that the
-- bounds are applied appropriately.
--
maxlength :: ValidAttribute 'MaxLength tag => Word -> Attribute tag
maxlength = Attr_MaxLength

-- TODO: This should be derived from the RelativeURL given in the action
-- attribute in the Safe.Form module.
--
method :: ValidAttribute 'Method tag => Types.FormMethod -> Attribute tag
method = Attr_Method

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/max should be done to ensure that the bounds
-- are applied appropriately.
--
min :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf min Types.RangeBoundTypes
       , ValidRangeBound min tag
       , ValidAttribute 'Max tag
       )
    => min -> Attribute tag
min =
  Attr_Max . Types.mkRangeBound

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/maxlength should be done to ensure that the
-- bounds are applied appropriately.
--
minlength :: ValidAttribute 'MinLength tag => Word -> Attribute tag
minlength = Attr_MinLength

multiple :: ValidAttribute 'Multiple tag => Attribute tag
multiple = Attr_Multiple

mute :: ValidAttribute 'Muted tag => Bool -> Attribute tag
mute = Attr_Muted

muted :: ValidAttribute 'Muted tag => Attribute tag
muted = mute True

name :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf name Types.NameOptionTypes
        , ValidName name tag
        , ValidAttribute 'Name tag
        )
     => name -> Attribute tag
name =
  Attr_Name . Types.mkNameOption

nomodule :: ValidAttribute 'NoModule tag => Bool -> Attribute tag
nomodule = Attr_NoModule

-- TODO: A Safe module could take a Maybe of form validation attributes, and
-- use this attribute otherwise.
--
novalidate :: ValidAttribute 'NoValidate tag => Attribute tag
novalidate = validate False

optimum :: ValidAttribute 'Optimum tag => Types.Number -> Attribute tag
optimum = Attr_Optimum

ping :: ValidAttribute 'Ping tag => NEL.NonEmpty Types.Ping -> Attribute tag
ping = Attr_Ping

placeholder :: ValidAttribute 'Placeholder tag => T.Text -> Attribute tag
placeholder = Attr_Placeholder

playInline :: ValidAttribute 'PlaysInline tag => Bool -> Attribute tag
playInline = Attr_PlaysInline

playsinline :: ValidAttribute 'PlaysInline tag => Attribute tag
playsinline = playInline True

poster :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf poster Types.URLTypes
          , ValidAttribute 'Poster tag
          )
       => poster -> Attribute tag
poster =
  Attr_Poster . Types.mkURL

preload :: ValidAttribute 'Preload tag => Types.Preload -> Attribute tag
preload = Attr_Preload

readonly :: ValidAttribute 'ReadOnly tag => Attribute tag
readonly = Attr_ReadOnly

referrerpolicy :: ValidAttribute 'ReferrerPolicy tag
               => Types.ReferrerPolicy -> Attribute tag
referrerpolicy = Attr_ReferrerPolicy

rel :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf rel Types.RelationshipTypes
       , ValidRelationship rel tag
       , ValidAttribute 'Rel tag
       )
    => rel -> Attribute tag
rel =
  Attr_Rel . Types.mkRelationship

require :: ValidAttribute 'Required tag => Bool -> Attribute tag
require = Attr_Required

required :: ValidAttribute 'Required tag => Attribute tag
required = require True

reverse :: ValidAttribute 'Reversed tag => Bool -> Attribute tag
reverse = Attr_Reversed

reversed :: ValidAttribute 'Reversed tag => Attribute tag
reversed = reverse True

rows :: ValidAttribute 'Rows tag => Word -> Attribute tag
rows = Attr_Rows

rowspan :: ValidAttribute 'Rowspan tag => Word -> Attribute tag
rowspan = Attr_Rowspan

sandbox :: ValidAttribute 'Sandbox tag => [Types.SandboxToken] -> Attribute tag
sandbox = Attr_Sandbox

select :: ValidAttribute 'Selected tag => Bool -> Attribute tag
select = Attr_Selected

selected :: ValidAttribute 'Selected tag => Attribute tag
selected = select True

shape :: ValidAttribute 'Shape tag => Types.Shape -> Attribute tag
shape = Attr_Shape

size :: ValidAttribute 'Size tag => Positive -> Attribute tag
size = Attr_Size

span :: ValidAttribute 'Span tag => Positive -> Attribute tag
span = Attr_Span

src :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf url Types.URLTypes
       , ValidSource url tag
       , ValidAttribute 'Src tag
       )
    => url -> Attribute tag
src =
  Attr_Src . Types.mkURL

srclang :: ValidAttribute 'SrcLang tag => Ogma.BCP_47 -> Attribute tag
srclang = Attr_SrcLang

start :: ValidAttribute 'Start tag => Int -> Attribute tag
start = Attr_Start

target :: ValidAttribute 'Target tag => Types.Target -> Attribute tag
target = Attr_Target

type_ :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf type_ Types.TypeOptionTypes
         , ValidTypeOption type_ tag
         , ValidAttribute 'Type tag
         )
      => type_ -> Attribute tag
type_ =
  Attr_Type . Types.mkTypeOption

-- TODO: For broader compatibility and clarity, it's common to include both id
-- and name on the <map> element with the same value. When making a `Safe`
-- module, the id and name attributes will be the same.
--
usemap :: ValidAttribute 'UseMap tag => Types.Name -> Attribute tag
usemap = Attr_UseMap

-- | This function permits an inline argument to determine whether validation
-- should be enabled. If 'False', the @novalidate@ attribute will be added.
--
validate :: ValidAttribute 'NoValidate tag => Bool -> Attribute tag
validate = Attr_NoValidate . not

value :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf value Types.ValueTypes
         , ValidValue value tag
         , ValidAttribute 'Value tag
         )
      => value -> Attribute tag
value =
  Attr_Value . Types.mkValue

width :: ValidAttribute 'Width tag => Word -> Attribute tag
width = Attr_Width

wrap :: ValidAttribute 'Wrap tag => Types.Wrap -> Attribute tag
wrap = Attr_Wrap

xmlns :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf url Types.URLTypes
         , ValidAttribute 'XMLNS tag
         )
      => url -> Attribute tag
xmlns =
  Attr_XMLNS . Types.mkURL
