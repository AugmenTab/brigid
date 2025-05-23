{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Attributes.Scoped
  ( ValidAttribute
  , AttributeTags.Abbreviation, abbr
  , AttributeTags.Accept, accept
  , AttributeTags.AcceptCharset, acceptCharset
  , AttributeTags.Action, action
  , AttributeTags.Allow, allow
  , AttributeTags.Alt, alt
  , AttributeTags.As, as
  , AttributeTags.Async, async
  , ValidAutocomplete
  , AttributeTags.Autocomplete, autocomplete
  , AttributeTags.Autoplay, autoplay
  , AttributeTags.Capture, capture
  , AttributeTags.Charset, charset
  , AttributeTags.Checked, check
  , checked
  , AttributeTags.Cite, cite
  , AttributeTags.Cols, cols
  , AttributeTags.Colspan, colspan
  , AttributeTags.Content, content
  , AttributeTags.Controls, controls
  , AttributeTags.ControlsList, controlslist
  , AttributeTags.Coords, coords
  , AttributeTags.CrossOrigin, crossorigin
  , AttributeTags.Data, data_
  , AttributeTags.Datetime, datetime
  , datetimeWithFormat
  , AttributeTags.Decoding, decoding
  , AttributeTags.Default, default_
  , AttributeTags.Defer, defer
  , AttributeTags.Dirname, dirname
  , AttributeTags.Disabled, disable
  , disabled
  , AttributeTags.DisablePictureInPicture, disablepictureinpicture
  , AttributeTags.DisableRemotePlayback, disableremoteplayback
  , AttributeTags.Download, download
  , AttributeTags.Enctype, enctype
  , AttributeTags.FetchPriority, fetchpriority
  , ValidFor
  , AttributeTags.For, for
  , AttributeTags.Form, form
  , AttributeTags.FormAction, formaction
  , AttributeTags.FormEnctype, formenctype
  , AttributeTags.FormMethod, formmethod
  , AttributeTags.FormNoValidate, formnovalidate
  , AttributeTags.FormTarget, formtarget
  , AttributeTags.Headers, headers
  , AttributeTags.Height, height
  , AttributeTags.High, high
  , ValidHref
  , AttributeTags.Href, href
  , AttributeTags.HrefLang, hreflang
  , AttributeTags.HttpEquiv, httpEquiv
  , AttributeTags.ImageSizes, imagesizes
  , AttributeTags.ImageSrcset, imagesrcset
  , AttributeTags.Integrity, integrity
  , AttributeTags.IsMap, ismap
  , AttributeTags.Kind, kind
  , AttributeTags.Label, label
  , AttributeTags.List, list
  , AttributeTags.Loading, loading
  , AttributeTags.Loop, loop
  , AttributeTags.Low, low
  , ValidRangeBound
  , AttributeTags.Max, max
  , AttributeTags.MaxLength, maxlength
  , AttributeTags.Media, media
  , AttributeTags.Method, method
  , AttributeTags.Min, min
  , AttributeTags.MinLength, minlength
  , AttributeTags.Multiple, multiple
  , AttributeTags.Muted, mute
  , muted
  , ValidName
  , AttributeTags.Name, name
  , AttributeTags.NoModule, nomodule
  , AttributeTags.NoValidate, novalidate
  , validate
  , AttributeTags.Open, open
  , AttributeTags.Optimum, optimum
  , AttributeTags.Pattern, pattern
  , AttributeTags.Ping, ping
  , AttributeTags.Placeholder, placeholder
  , AttributeTags.PlaysInline, playInline
  , playsinline
  , AttributeTags.PopoverTarget, popovertarget
  , AttributeTags.PopoverTargetAction, popovertargetaction
  , AttributeTags.Poster, poster
  , AttributeTags.Preload, preload
  , AttributeTags.ReadOnly, readonly
  , AttributeTags.ReferrerPolicy, referrerpolicy
  , ValidRelationship
  , AttributeTags.Rel, rel
  , AttributeTags.Required, require
  , required
  , AttributeTags.Reversed, reverse
  , reversed
  , AttributeTags.Rows, rows
  , AttributeTags.Rowspan, rowspan
  , AttributeTags.Sandbox, sandbox
  , AttributeTags.Scope, scope
  , AttributeTags.Selected, select
  , selected
  , AttributeTags.Shape, shape
  , AttributeTags.Size, size
  , AttributeTags.Sizes, sizes
  , AttributeTags.Span, span
  , ValidSource
  , AttributeTags.Src, src
  , AttributeTags.SrcDoc, srcdoc
  , AttributeTags.SrcLang, srclang
  , AttributeTags.SrcSet, srcset
  , AttributeTags.Start, start
  , AttributeTags.Step, step
  , AttributeTags.Target, target
  , ValidTypeOption
  , AttributeTags.Type, type_
  , AttributeTags.UseMap, usemap
  , ValidValue
  , AttributeTags.Value, value
  , AttributeTags.Width, width
  , AttributeTags.Wrap, wrap
  , AttributeTags.XMLNS, xmlns
  ) where

import Prelude hiding (max, min, reverse, span)
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrdOn)
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (ISO8601, iso8601Show)
import GHC.TypeLits (KnownNat)
import Integer (Positive)
import Numeric.Natural (Natural)
import Ogma qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes.Autocomplete (ValidAutocomplete)
import Brigid.HTML.Attributes.Elements (ValidAttribute)
import Brigid.HTML.Attributes.For (ValidFor)
import Brigid.HTML.Attributes.Href (ValidHref)
import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Name (ValidName)
import Brigid.HTML.Attributes.RangeBound (ValidRangeBound)
import Brigid.HTML.Attributes.Relationship (ValidRelationship)
import Brigid.HTML.Attributes.Source (ValidSource)
import Brigid.HTML.Attributes.Tags qualified as AttributeTags
import Brigid.HTML.Attributes.Type (ValidTypeOption)
import Brigid.HTML.Attributes.Value (ValidValue)
import Brigid.HTML.Elements.Internal (ChildHTML)
import Brigid.HTML.Render.ByteString (renderLazyHTML)
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

-- Scoped Attributes
--

abbr :: ValidAttribute AttributeTags.Abbreviation tag
     => T.Text -> Attribute tag
abbr = Attr_Abbreviation

accept :: ValidAttribute AttributeTags.Accept tag
       => BS.ByteString -> Attribute tag
accept = Attr_Accept

-- | This will default to using "UTF-8" as the only supported encoding.
--
-- Previously, this attribute accepted a list of valid encodings that the
-- server could accept. In the current HTML5 specification, UTF-8 is the
-- recommended and default character encoding for web applications. Specifying
-- multiple encodings is now considered unnecessary and non-standard practice.
--
-- If you must specify multiple encodings, create a custom attribute instead.
--
acceptCharset :: ValidAttribute AttributeTags.AcceptCharset tag
              => Attribute tag
acceptCharset = Attr_AcceptCharset

action :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf action Types.ActionTypes
          , ValidAttribute AttributeTags.Action tag
          )
       => action -> Attribute tag
action =
  Attr_Action . Types.mkAction

allow :: ValidAttribute AttributeTags.Allow tag
      => [Types.FeaturePolicyDirective] -> Attribute tag
allow = Attr_Allow . nubOrdOn fromEnum

alt :: ValidAttribute AttributeTags.Alt tag => T.Text -> Attribute tag
alt = Attr_Alt

as :: ValidAttribute AttributeTags.As tag => Types.As -> Attribute tag
as = Attr_As

async :: ValidAttribute AttributeTags.Async tag => Attribute tag
async = Attr_Async

autocomplete :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf token Types.AutocompleteTokenTypes
                , ValidAutocomplete token tag
                , ValidAttribute AttributeTags.Autocomplete tag
                )
             => token -> Attribute tag
autocomplete =
  Attr_Autocomplete . Types.mkAutocompleteToken

autoplay :: ValidAttribute AttributeTags.Autoplay tag => Attribute tag
autoplay = Attr_Autoplay

capture :: ValidAttribute AttributeTags.Capture tag
        => Maybe Types.CaptureMethod -> Attribute tag
capture = Attr_Capture

-- | Limited to UTF-8, since that is the only valid option for HTML5.
--
charset :: ValidAttribute AttributeTags.Charset tag => Attribute tag
charset = Attr_Charset

check :: ValidAttribute AttributeTags.Checked tag => Bool -> Attribute tag
check = Attr_Checked

checked :: ValidAttribute AttributeTags.Checked tag => Attribute tag
checked = check True

cite :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf cite Types.URLTypes
        , ValidAttribute AttributeTags.Cite tag
        )
     => cite -> Attribute tag
cite =
  Attr_Cite . Types.mkURL

cols :: ValidAttribute AttributeTags.Cols tag => Natural -> Attribute tag
cols = Attr_Cols

colspan :: ValidAttribute AttributeTags.Colspan tag
        => Positive -> Attribute tag
colspan = Attr_Colspan

-- | The `content` attribute is left as simple 'T.Text' because its value is
-- dependent on the `name` or `http-equiv` attributes, and managing the
-- inter-dependency across multiple attributes in the `meta` tag is too complex
-- to reconcile here, For safe construction of the `content` attributes
-- together with its dependencies on a `meta` tag, use
-- 'Brigid.HTML.Elements.Safe.Meta'.
--
content :: ValidAttribute AttributeTags.Content tag => T.Text -> Attribute tag
content = Attr_Content

controls :: ValidAttribute AttributeTags.Controls tag => Attribute tag
controls = Attr_Controls

controlslist :: ValidAttribute AttributeTags.ControlsList tag
             => Types.ControlsList -> Attribute tag
controlslist = Attr_ControlsList

-- | The `coords` attribute is used in combination with the `shape` attribute
-- to specify the size and area of an <area> tag in an image <map>. For safe
-- construction of the `coords` and `shape` attributes on an `area` tag, use
-- `Brigid.HTML.Elements.Safe.Area`.
--
coords :: ValidAttribute AttributeTags.Coords tag
       => NEL.NonEmpty Integer -> Attribute tag
coords = Attr_Coords

{-|
   This enumerated attribute indicates whether CORS must be used when fetching
   the resource. CORS-enabled images can be reused in the @<canvas>@ element
   without being tainted. If the attribute is not present, the resource is
   fetched without a CORS request (i.e. without sending the @Origin@ HTTP
   header), preventing its non-tainted usage. If invalid, it is handled as if
   the enumerated keyword @anonymous@ was used.
-}
crossorigin :: ValidAttribute AttributeTags.CrossOrigin tag
            => Types.CrossOriginFetch -> Attribute tag
crossorigin = Attr_CrossOrigin

data_ :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf _data Types.URLTypes
          , ValidAttribute AttributeTags.Data tag
          )
       => _data -> Attribute tag
data_ =
  Attr_Data . Types.mkURL

datetime :: (ISO8601 t, ValidAttribute AttributeTags.Datetime tag)
         => t -> Attribute tag
datetime = Attr_Datetime . iso8601Show

-- | This option is provided for users that want to supply their own 'datetime'
-- encodings besides the common standards, and are willing to accept the risks
-- of using a potentially unsupported encoding.
--
datetimeWithFormat :: ( Time.FormatTime t
                      , ValidAttribute AttributeTags.Datetime tag
                      )
                   => String -> t -> Attribute tag
datetimeWithFormat format =
  Attr_Datetime . Time.formatTime Time.defaultTimeLocale format

decoding :: ValidAttribute AttributeTags.Decoding tag
         => Types.Decoding -> Attribute tag
decoding = Attr_Decoding

default_ :: ValidAttribute AttributeTags.Default tag => Attribute tag
default_ = Attr_Default

defer :: ValidAttribute AttributeTags.Defer tag => Attribute tag
defer = Attr_Defer

dirname :: ValidAttribute AttributeTags.Dirname tag => T.Text -> Attribute tag
dirname = Attr_Dirname

disable :: ValidAttribute AttributeTags.Disabled tag => Bool -> Attribute tag
disable = Attr_Disabled

disabled :: ValidAttribute AttributeTags.Disabled tag => Attribute tag
disabled = disable True

disablepictureinpicture :: ValidAttribute AttributeTags.DisablePictureInPicture tag
                        => Attribute tag
disablepictureinpicture = Attr_DisablePictureInPicture

disableremoteplayback :: ValidAttribute AttributeTags.DisableRemotePlayback tag
                      => Attribute tag
disableremoteplayback = Attr_DisableRemotePlayback

download :: ValidAttribute AttributeTags.Download tag
         => Maybe NET.NonEmptyText -> Attribute tag
download = Attr_Download

enctype :: ValidAttribute AttributeTags.Enctype tag
        => BS.ByteString -> Attribute tag
enctype = Attr_Enctype

fetchpriority :: ValidAttribute AttributeTags.FetchPriority tag
              => Types.FetchPriority -> Attribute tag
fetchpriority = Attr_FetchPriority

for :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf for Types.ForOptionTypes
       , ValidFor for tag
       , ValidAttribute AttributeTags.For tag
       )
    => for -> Attribute tag
for =
  Attr_For . Types.mkForOption

form :: ValidAttribute AttributeTags.Form tag => Types.Id -> Attribute tag
form = Attr_Form

formaction :: ( KnownNat branchIndex
              , branchIndex ~ FirstIndexOf action Types.ActionTypes
              , ValidAttribute AttributeTags.FormAction tag
              )
           => action -> Attribute tag
formaction =
  Attr_FormAction . Types.mkAction

formenctype :: ValidAttribute AttributeTags.FormEnctype tag
            => BS.ByteString -> Attribute tag
formenctype = Attr_FormEnctype

formmethod :: ValidAttribute AttributeTags.FormMethod tag
           => Types.FormMethod -> Attribute tag
formmethod = Attr_FormMethod

formnovalidate :: ValidAttribute AttributeTags.FormNoValidate tag
               => Attribute tag
formnovalidate = Attr_FormNoValidate

formtarget :: ValidAttribute AttributeTags.FormTarget tag
           => Types.Target -> Attribute tag
formtarget = Attr_FormTarget

headers :: ValidAttribute AttributeTags.Headers tag
        => [Types.Id] -> Attribute tag
headers = Attr_Headers

height :: ValidAttribute AttributeTags.Height tag => Positive -> Attribute tag
height = Attr_Height

high :: ValidAttribute AttributeTags.High tag => Types.Number -> Attribute tag
high = Attr_High

href :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf href (Types.HrefTypes Types.Get)
        , ValidHref href tag
        , ValidAttribute AttributeTags.Href tag
        )
     => href -> Attribute tag
href =
  Attr_Href . Types.mkHref

hreflang :: ValidAttribute AttributeTags.HrefLang tag
         => Ogma.BCP_47 -> Attribute tag
hreflang = Attr_HrefLang

httpEquiv :: ValidAttribute AttributeTags.HttpEquiv tag
          => Types.HttpEquivToken -> Attribute tag
httpEquiv = Attr_HttpEquiv

imagesizes :: ValidAttribute AttributeTags.ImageSizes tag
           => NEL.NonEmpty Types.Size -> Attribute tag
imagesizes = Attr_ImageSizes

imagesrcset :: ValidAttribute AttributeTags.ImageSrcset tag
            => NEL.NonEmpty Types.SrcsetCandidate -> Attribute tag
imagesrcset = Attr_ImageSrcset

integrity :: ValidAttribute AttributeTags.Integrity tag
          => Types.IntegrityEncoding -> BS.ByteString -> Attribute tag
integrity = Attr_Integrity

-- | The `ismap` attribute is only valid on <img> tags that are nested within
-- an <a> tag. For safe construction of the `ismap` attribute, use
-- 'Brigid.HTML.Elements.Safe.Image'.
--
ismap :: ValidAttribute AttributeTags.IsMap tag => Attribute tag
ismap = Attr_IsMap

kind :: ValidAttribute AttributeTags.Kind tag
     => Types.TrackKind -> Attribute tag
kind = Attr_Kind

label :: ValidAttribute AttributeTags.Label tag => T.Text -> Attribute tag
label = Attr_Label

-- TODO: A Safe module could build inputs and datalists in tandom, taking an Id
-- to ensure proper list application and that the values are valid for the
-- input type.
--
list :: ValidAttribute AttributeTags.List tag => Types.Id -> Attribute tag
list = Attr_List

loading :: ValidAttribute AttributeTags.Loading tag
        => Types.LoadOption -> Attribute tag
loading = Attr_Loading

loop :: ValidAttribute AttributeTags.Loop tag => Attribute tag
loop = Attr_Loop

low :: ValidAttribute AttributeTags.Low tag => Types.Number -> Attribute tag
low = Attr_Low

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/max should be done to ensure that the bounds
-- are applied appropriately.
--
max :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf max Types.RangeBoundTypes
       , ValidRangeBound max tag
       , ValidAttribute AttributeTags.Max tag
       )
    => max -> Attribute tag
max =
  Attr_Max . Types.mkRangeBound

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/maxlength should be done to ensure that the
-- bounds are applied appropriately.
--
maxlength :: ValidAttribute AttributeTags.MaxLength tag
          => Natural -> Attribute tag
maxlength = Attr_MaxLength

media :: ValidAttribute AttributeTags.Media tag
      => NEL.NonEmpty Types.MediaQuery -> Attribute tag
media = Attr_Media

-- TODO: This should be derived from the RelativeURL given in the action
-- attribute in the Safe.Form module.
--
method :: ValidAttribute AttributeTags.Method tag
       => Types.FormMethod -> Attribute tag
method = Attr_Method

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/max should be done to ensure that the bounds
-- are applied appropriately.
--
min :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf min Types.RangeBoundTypes
       , ValidRangeBound min tag
       , ValidAttribute AttributeTags.Min tag
       )
    => min -> Attribute tag
min =
  Attr_Min . Types.mkRangeBound

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/maxlength should be done to ensure that the
-- bounds are applied appropriately.
--
minlength :: ValidAttribute AttributeTags.MinLength tag
          => Natural -> Attribute tag
minlength = Attr_MinLength

multiple :: ValidAttribute AttributeTags.Multiple tag => Attribute tag
multiple = Attr_Multiple

mute :: ValidAttribute AttributeTags.Muted tag => Bool -> Attribute tag
mute = Attr_Muted

muted :: ValidAttribute AttributeTags.Muted tag => Attribute tag
muted = mute True

name :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf name Types.NameOptionTypes
        , ValidName name tag
        , ValidAttribute AttributeTags.Name tag
        )
     => name -> Attribute tag
name =
  Attr_Name . Types.mkNameOption

nomodule :: ValidAttribute AttributeTags.NoModule tag => Bool -> Attribute tag
nomodule = Attr_NoModule

-- TODO: A Safe module could take a Maybe of form validation attributes, and
-- use this attribute otherwise.
--
novalidate :: ValidAttribute AttributeTags.NoValidate tag => Attribute tag
novalidate = validate False

open :: ValidAttribute AttributeTags.Open tag => Attribute tag
open = Attr_Open

optimum :: ValidAttribute AttributeTags.Optimum tag
        => Types.Number -> Attribute tag
optimum = Attr_Optimum

-- | This attribute takes a simple 'T.Text' because there's no way to validate
-- that the provided regular expression is valid. We can check for the pattern
-- to be a valid JavaScript regular expression, but not all browsers support
-- every feature of JavaScript regular expressions. The user must accept the
-- risk that the provided pattern may still be invalid in actual practice when
-- using this function.
--
pattern :: ValidAttribute AttributeTags.Pattern tag => T.Text -> Attribute tag
pattern = Attr_Pattern

ping :: ValidAttribute AttributeTags.Ping tag
     => NEL.NonEmpty Types.Ping -> Attribute tag
ping = Attr_Ping

placeholder :: ValidAttribute AttributeTags.Placeholder tag
            => T.Text -> Attribute tag
placeholder = Attr_Placeholder

playInline :: ValidAttribute AttributeTags.PlaysInline tag
           => Bool -> Attribute tag
playInline = Attr_PlaysInline

playsinline :: ValidAttribute AttributeTags.PlaysInline tag => Attribute tag
playsinline = playInline True

popovertarget :: ValidAttribute AttributeTags.PopoverTarget tag
              => Types.Id -> Attribute tag
popovertarget = Attr_PopoverTarget

popovertargetaction :: ValidAttribute AttributeTags.PopoverTargetAction tag
                    => Types.PopoverTargetAction -> Attribute tag
popovertargetaction = Attr_PopoverTargetAction

poster :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf poster Types.URLTypes
          , ValidAttribute AttributeTags.Poster tag
          )
       => poster -> Attribute tag
poster =
  Attr_Poster . Types.mkURL

preload :: ValidAttribute AttributeTags.Preload tag
        => Types.Preload -> Attribute tag
preload = Attr_Preload

readonly :: ValidAttribute AttributeTags.ReadOnly tag => Attribute tag
readonly = Attr_ReadOnly

referrerpolicy :: ValidAttribute AttributeTags.ReferrerPolicy tag
               => Types.ReferrerPolicy -> Attribute tag
referrerpolicy = Attr_ReferrerPolicy

rel :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf rel Types.RelationshipTypes
       , ValidRelationship rel tag
       , ValidAttribute AttributeTags.Rel tag
       )
    => rel -> Attribute tag
rel =
  Attr_Rel . Types.mkRelationship

require :: ValidAttribute AttributeTags.Required tag => Bool -> Attribute tag
require = Attr_Required

required :: ValidAttribute AttributeTags.Required tag => Attribute tag
required = require True

reverse :: ValidAttribute AttributeTags.Reversed tag => Bool -> Attribute tag
reverse = Attr_Reversed

reversed :: ValidAttribute AttributeTags.Reversed tag => Attribute tag
reversed = reverse True

rows :: ValidAttribute AttributeTags.Rows tag => Natural -> Attribute tag
rows = Attr_Rows

rowspan :: ValidAttribute AttributeTags.Rowspan tag
        => Positive -> Attribute tag
rowspan = Attr_Rowspan

sandbox :: ValidAttribute AttributeTags.Sandbox tag
        => [Types.SandboxToken] -> Attribute tag
sandbox = Attr_Sandbox

scope :: ValidAttribute AttributeTags.Scope tag => Types.Scope -> Attribute tag
scope = Attr_Scope

select :: ValidAttribute AttributeTags.Selected tag => Bool -> Attribute tag
select = Attr_Selected

selected :: ValidAttribute AttributeTags.Selected tag => Attribute tag
selected = select True

shape :: ValidAttribute AttributeTags.Shape tag => Types.Shape -> Attribute tag
shape = Attr_Shape

size :: ValidAttribute AttributeTags.Size tag => Positive -> Attribute tag
size = Attr_Size

sizes :: ValidAttribute AttributeTags.Sizes tag
      => NEL.NonEmpty Types.Size -> Attribute tag
sizes = Attr_Sizes

span :: ValidAttribute AttributeTags.Span tag => Positive -> Attribute tag
span = Attr_Span

src :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf url Types.URLTypes
       , ValidSource url tag
       , ValidAttribute AttributeTags.Src tag
       )
    => url -> Attribute tag
src =
  Attr_Src . Types.mkURL

srcdoc :: ValidAttribute AttributeTags.SrcDoc tag
       => ChildHTML parent grandparent -> Attribute tag
srcdoc = Attr_SrcDoc . renderLazyHTML

srclang :: ValidAttribute AttributeTags.SrcLang tag
        => Ogma.BCP_47 -> Attribute tag
srclang = Attr_SrcLang

srcset :: ValidAttribute AttributeTags.SrcSet tag
       => NEL.NonEmpty Types.SrcsetCandidate -> Attribute tag
srcset = Attr_SrcSet

start :: ValidAttribute AttributeTags.Start tag => Integer -> Attribute tag
start = Attr_Start

step :: ValidAttribute AttributeTags.Step tag => Types.Step -> Attribute tag
step = Attr_Step

target :: ValidAttribute AttributeTags.Target tag
       => Types.Target -> Attribute tag
target = Attr_Target

type_ :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf type_ Types.TypeOptionTypes
         , ValidTypeOption type_ tag
         , ValidAttribute AttributeTags.Type tag
         )
      => type_ -> Attribute tag
type_ =
  Attr_Type . Types.mkTypeOption

-- TODO: For broader compatibility and clarity, it's common to include both id
-- and name on the <map> element with the same value. When making a `Safe`
-- module, the id and name attributes will be the same.
--
usemap :: ValidAttribute AttributeTags.UseMap tag
       => Types.Name -> Attribute tag
usemap = Attr_UseMap

-- | This function permits an inline argument to determine whether validation
-- should be enabled. If 'False', the @novalidate@ attribute will be added.
--
validate :: ValidAttribute AttributeTags.NoValidate tag
         => Bool -> Attribute tag
validate = Attr_NoValidate . not

value :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf value Types.ValueTypes
         , ValidValue value tag
         , ValidAttribute AttributeTags.Value tag
         )
      => value -> Attribute tag
value =
  Attr_Value . Types.mkValue

width :: ValidAttribute AttributeTags.Width tag => Positive -> Attribute tag
width = Attr_Width

wrap :: ValidAttribute AttributeTags.Wrap tag => Types.Wrap -> Attribute tag
wrap = Attr_Wrap

xmlns :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf url Types.URLTypes
         , ValidAttribute AttributeTags.XMLNS tag
         )
      => url -> Attribute tag
xmlns =
  Attr_XMLNS . Types.mkURL
