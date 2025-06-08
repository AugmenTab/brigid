module Generation.Generators
  ( as
  , autocapitalizeOption
  , bcp47
  , byteString
  , captureMethod
  , char
  , class_
  , contentEditableOption
  , controlsList
  , crossOriginFetch
  , decoding
  , directionality
  , exportPart
  , featurePolicyDirective
  , fetchPriority
  , formMethod
  , help
  , httpEquivToken
  , id
  , inputMode
  , int
  , integer
  , keyHintOption
  , lazyByteString
  , loadOption
  , mediaFeature
  , mediaQuery
  , metadataName
  , name
  , natural
  , nonEmptyText
  , number
  , onOff
  , part
  , popoverState
  , popoverTargetAction
  , positive
  , preload
  , rangeBound
  , referrerPolicy
  , role
  , sandboxToken
  , scope
  , shape
  , size
  , srcsetCandidate
  , step
  , string
  , target
  , text
  , trackKind
  , type_
  , url
  , uuid
  , wrap
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Ratio (Ratio, (%))
import Data.Text qualified as T
import Data.UUID (UUID, fromWords64)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Integer (Positive)
import Numeric.Natural (Natural)
import Prelude hiding (id)

import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types
import Ogma qualified

as :: MonadGen m => m Types.As
as =
  Gen.enumBounded

autocapitalizeOption :: MonadGen m => m Types.AutocapitalizeOption
autocapitalizeOption =
  Gen.enumBounded

bcp47  :: MonadGen m => m Ogma.BCP_47
bcp47 =
  Ogma.simpleBCP_47 <$> Gen.enumBounded

byteString  :: MonadGen m => m BS.ByteString
byteString =
  Gen.utf8 (Range.linear 1 100) Gen.alphaNum

captureMethod :: MonadGen m => m Types.CaptureMethod
captureMethod =
  Gen.enumBounded

char  :: MonadGen m => m Char
char =
  Gen.enumBounded

class_  :: MonadGen m => m Types.Class
class_ =
  Types.Class <$> text

contentEditableOption :: MonadGen m => m Types.ContentEditableOption
contentEditableOption =
  Gen.enumBounded

controlsList :: MonadGen m => m Types.ControlsList
controlsList =
  Gen.enumBounded

crossOriginFetch :: MonadGen m => m Types.CrossOriginFetch
crossOriginFetch =
  Gen.enumBounded

decoding :: MonadGen m => m Types.Decoding
decoding =
  Gen.enumBounded

directionality :: MonadGen m => m Types.Directionality
directionality =
  Gen.enumBounded

exportPart  :: MonadGen m => m Types.ExportPart
exportPart =
  Types.ExportPart
    <$> part
    <*> Gen.maybe text

featurePolicyDirective :: MonadGen m => m Types.FeaturePolicyDirective
featurePolicyDirective =
  Gen.enumBounded

fetchPriority :: MonadGen m => m Types.FetchPriority
fetchPriority =
  Gen.enumBounded

formMethod :: MonadGen m => m Types.FormMethod
formMethod =
  Gen.enumBounded

help  :: MonadGen m => m Types.Rel_Help
help =
  pure Types.Rel_Help

httpEquivToken :: MonadGen m => m Types.HttpEquivToken
httpEquivToken =
  Gen.enumBounded

id  :: MonadGen m => m Types.Id
id =
  Types.Id <$> text

inputMode :: MonadGen m => m Types.InputMode
inputMode =
  Gen.enumBounded

int  :: MonadGen m => m Int
int =
  Gen.integral (Range.linear 1 1000)

integer  :: MonadGen m => m Integer
integer =
  Gen.integral (Range.linear 1 1000)

keyHintOption :: MonadGen m => m Types.KeyHintOption
keyHintOption =
  Gen.enumBounded

lazyByteString  :: MonadGen m => m LBS.ByteString
lazyByteString =
  LBS.fromStrict <$> byteString

loadOption :: MonadGen m => m Types.LoadOption
loadOption =
  Gen.enumBounded

mediaFeature  :: MonadGen m => m Types.MediaFeature
mediaFeature =
  Gen.choice
    [ Types.Width
        <$> mediaFeatureType
        <*> mediaLength
    , Types.Height
        <$> mediaFeatureType
        <*> mediaLength
    , Types.DeviceWidth
        <$> mediaFeatureType
        <*> mediaLength
    , Types.DeviceHeight
        <$> mediaFeatureType
        <*> mediaLength
    , Types.AspectRatio
        <$> mediaFeatureType
        <*> ratio
    , Types.DeviceAspectRatio
        <$> mediaFeatureType
        <*> ratio
    , Types.Orientation <$> mediaOrientation
    , Types.Resolution
        <$> mediaFeatureType
        <*> mediaResolution
    , Types.Scan <$> mediaScanType
    , pure Types.Grid
    , Types.Color <$> Gen.maybe mediaColor
    , Types.ColorIndex <$> Gen.maybe mediaColor
    , Types.Monochrome <$> Gen.maybe mediaColor
    , Types.InvertedColors <$> Gen.bool
    , Types.ForcedColors <$> Gen.bool
    , Types.PrefersColorScheme <$> mediaColorScheme
    , Types.PrefersReducedMotion <$> Gen.bool
    , Types.PrefersReducedTransparency <$> Gen.bool
    , Types.PrefersContrast <$> mediaPrefersContrast
    , Types.PrefersReducedData <$> Gen.bool
    , Types.Hover <$> Gen.bool
    , Types.Pointer <$> mediaPointerAccuracy
    , Types.AnyHover <$> Gen.bool
    , Types.AnyPointer <$> mediaPointerAccuracy
    , Types.Update <$> mediaUpdateFrequency
    ]

mediaFeatureType  :: MonadGen m => m Types.MediaFeatureType
mediaFeatureType =
  Gen.enumBounded

mediaLength  :: MonadGen m => m Types.MediaLength
mediaLength =
  Gen.choice
    [ Types.MediaPx <$> number
    , Types.MediaIn <$> number
    , Types.MediaCm <$> number
    , Types.MediaMm <$> number
    , Types.MediaPt <$> number
    , Types.MediaPc <$> number
    ]

mediaOrientation  :: MonadGen m => m Types.MediaOrientation
mediaOrientation =
  Gen.enumBounded

mediaResolution  :: MonadGen m => m Types.MediaResolution
mediaResolution =
  Gen.choice
    [ Types.MediaDpi <$> number
    , Types.MediaDpcm <$> number
    , Types.MediaDppx <$> number
    ]

mediaScanType  :: MonadGen m => m Types.MediaScanType
mediaScanType =
  Gen.enumBounded

mediaColor  :: MonadGen m => m (Types.MediaFeatureType, Positive)
mediaColor =
  (,)
    <$> mediaFeatureType
    <*> Gen.integral (Range.linear 1 65536)

mediaColorScheme  :: MonadGen m => m Types.MediaColorScheme
mediaColorScheme =
  Gen.enumBounded

mediaPrefersContrast  :: MonadGen m => m Types.MediaPrefersContrast
mediaPrefersContrast =
  Gen.enumBounded

mediaPointerAccuracy  :: MonadGen m => m Types.MediaPointerAccuracy
mediaPointerAccuracy =
  Gen.enumBounded

mediaUpdateFrequency  :: MonadGen m => m Types.MediaUpdateFrequency
mediaUpdateFrequency =
  Gen.enumBounded

mediaQuery  :: MonadGen m => m Types.MediaQuery
mediaQuery =
  Types.MediaQuery
    <$> Gen.maybe Gen.enumBounded
    <*> Gen.maybe Gen.enumBounded
    <*> Gen.list (Range.linear 0 4) mediaFeature

metadataName :: MonadGen m => m Types.MetadataName
metadataName =
  Gen.enumBounded

name  :: MonadGen m => m Types.Name
name =
  Types.Name <$> text

natural  :: MonadGen m => m Natural
natural =
  Gen.integral (Range.linear 1 1000)

nonEmptyText  :: MonadGen m => m NET.NonEmptyText
nonEmptyText =
  NET.new
    <$> char
    <*> text

number  :: MonadGen m => m Types.Number
number =
  Types.numberFromFractional
    <$> Gen.double (Range.linearFrac 0.1 100.0)
    <*> Gen.int (Range.linear (negate 4) 4)

onOff  :: MonadGen m => m Types.OnOff
onOff =
  Gen.enumBounded

part  :: MonadGen m => m Types.Part
part =
  Types.Part <$> text

popoverState :: MonadGen m => m Types.PopoverState
popoverState =
  Gen.enumBounded

popoverTargetAction :: MonadGen m => m Types.PopoverTargetAction
popoverTargetAction =
  Gen.enumBounded

positive  :: MonadGen m => m Positive
positive =
  Gen.integral (Range.linear 1 1000)

preload :: MonadGen m => m Types.Preload
preload =
  Gen.enumBounded

rangeBound  :: MonadGen m => m Types.RawRangeBound
rangeBound =
  Types.mkRawRangeBound <$> text

ratio :: (Integral a, MonadGen m) => m (Ratio a)
ratio =
  (%)
    <$> Gen.integral (Range.linear 0 1000)
    <*> Gen.integral (Range.linear 1 1000)

referrerPolicy :: MonadGen m => m Types.ReferrerPolicy
referrerPolicy =
  Gen.enumBounded

role :: MonadGen m => m Types.Role
role =
  Gen.enumBounded

sandboxToken :: MonadGen m => m Types.SandboxToken
sandboxToken =
  Gen.enumBounded

scope :: MonadGen m => m Types.Scope
scope =
  Gen.enumBounded

shape :: MonadGen m => m Types.Shape
shape =
  Gen.enumBounded

size  :: MonadGen m => m Types.Size
size =
  Types.Size
    <$> Gen.maybe mediaFeature
    <*> sizeLength

sizeLength  :: MonadGen m => m Types.SizeLength
sizeLength =
  Gen.choice
    [ Types.SizePx <$> int
    , Types.SizeVw <$> int
    , Types.SizeVh <$> int
    , Types.SizePercent <$> int
    , Types.SizeEm <$> number
    , Types.SizeRem <$> number
    , Types.SizeCh <$> number
    , Types.SizeEx <$> number
    , Types.SizeVmin <$> number
    , Types.SizeVmax <$> number
    , Types.SizeCalc <$> text
    ]

srcsetCandidate  :: MonadGen m => m Types.SrcsetCandidate
srcsetCandidate =
  Types.SrcsetCandidate
    <$> fmap Types.mkURL url
    <*> srcsetDescriptor

srcsetDescriptor  :: MonadGen m => m Types.SrcsetDescriptor
srcsetDescriptor =
  Gen.choice
    [ Types.SrcsetWidth <$> positive
    , Types.SrcsetDensity <$> number
    ]

step  :: MonadGen m => m Types.Step
step =
  Gen.choice
    [ pure Types.Any
    , Types.Step <$> number
    ]

string  :: MonadGen m => m String
string =
  Gen.string (Range.linear 1 100) Gen.alphaNum

target  :: MonadGen m => m Types.Target
target =
  Gen.choice
    [ pure Types.Self
    , pure Types.Blank
    , pure Types.Parent
    , pure Types.Top
    , Types.CustomTarget <$> text
    ]

text  :: MonadGen m => m T.Text
text =
  Gen.text (Range.linear 1 100) Gen.alphaNum

trackKind :: MonadGen m => m Types.TrackKind
trackKind =
  Gen.enumBounded

type_  :: MonadGen m => m Types.RawTypeOption
type_ =
  Types.mkRawTypeOption <$> text

url  :: MonadGen m => m Types.RawURL
url =
  Types.mkRawURL <$> text

uuid :: MonadGen m => m UUID
uuid =
  fromWords64
    <$> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded

wrap :: MonadGen m => m Types.Wrap
wrap =
  Gen.enumBounded
