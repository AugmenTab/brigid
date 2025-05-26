module Generation.Generators
  ( bcp47
  , byteString
  , char
  , class_
  , exportPart
  , help
  , id
  , int
  , integer
  , lazyByteString
  , mediaFeature
  , mediaQuery
  , name
  , natural
  , nonEmptyText
  , number
  , onOff
  , part
  , positive
  , rangeBound
  , size
  , srcsetCandidate
  , step
  , string
  , target
  , text
  , type_
  , url
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Ratio (Ratio, (%))
import Data.Text qualified as T
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Integer (Positive)
import Numeric.Natural (Natural)
import Prelude hiding (id)

import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types
import Ogma qualified

bcp47  :: MonadGen m => m Ogma.BCP_47
bcp47 =
  Ogma.simpleBCP_47 <$> Gen.enumBounded

byteString  :: MonadGen m => m BS.ByteString
byteString =
  Gen.utf8 (Range.linear 1 100) Gen.ascii

char  :: MonadGen m => m Char
char =
  Gen.enumBounded

class_  :: MonadGen m => m Types.Class
class_ =
  Types.Class <$> text

exportPart  :: MonadGen m => m Types.ExportPart
exportPart =
  Types.ExportPart
    <$> part
    <*> Gen.maybe text

help  :: MonadGen m => m Types.Rel_Help
help =
  pure Types.Rel_Help

id  :: MonadGen m => m Types.Id
id =
  Types.Id <$> text

int  :: MonadGen m => m Int
int =
  Gen.integral (Range.linear 1 1000)

integer  :: MonadGen m => m Integer
integer =
  Gen.integral (Range.linear 1 1000)

lazyByteString  :: MonadGen m => m LBS.ByteString
lazyByteString =
  LBS.fromStrict <$> byteString

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
    <*> Gen.integral (Range.linear 0 65536)

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
    <$> Gen.double (Range.linearFrac 0.0 100.0)
    <*> Gen.word16 (Range.linear 0 4)

onOff  :: MonadGen m => m Types.OnOff
onOff =
  Gen.enumBounded

part  :: MonadGen m => m Types.Part
part =
  Types.Part <$> text

positive  :: MonadGen m => m Positive
positive =
  Gen.integral (Range.linear 1 1000)

rangeBound  :: MonadGen m => m Types.RawRangeBound
rangeBound =
  Types.mkRawRangeBound <$> text

ratio :: (Integral a, MonadGen m) => m (Ratio a)
ratio =
  (%)
    <$> Gen.integral (Range.linear 0 1000)
    <*> Gen.integral (Range.linear 0 1000)

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
  Gen.string (Range.linear 1 100) Gen.ascii

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
  Gen.text (Range.linear 1 100) Gen.ascii

type_  :: MonadGen m => m Types.RawTypeOption
type_ =
  Types.mkRawTypeOption <$> text

url  :: MonadGen m => m Types.RawURL
url =
  Types.mkRawURL <$> text
