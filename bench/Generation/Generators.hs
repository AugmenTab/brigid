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
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Integer (Positive)
import Numeric.Natural (Natural)
import Prelude hiding (id)

import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types
import Ogma qualified

bcp47 :: Gen Ogma.BCP_47
bcp47 =
  Ogma.simpleBCP_47 <$> Gen.enumBounded

byteString :: Gen BS.ByteString
byteString =
  Gen.utf8 (Range.linear 1 100) Gen.ascii

char :: Gen Char
char =
  Gen.enumBounded

class_ :: Gen Types.Class
class_ =
  Types.Class <$> text

exportPart :: Gen Types.ExportPart
exportPart =
  Types.ExportPart
    <$> part
    <*> Gen.maybe text

help :: Gen Types.Rel_Help
help =
  pure Types.Rel_Help

id :: Gen Types.Id
id =
  Types.Id <$> text

int :: Gen Int
int =
  Gen.integral (Range.linear 1 1000)

integer :: Gen Integer
integer =
  Gen.integral (Range.linear 1 1000)

lazyByteString :: Gen LBS.ByteString
lazyByteString =
  LBS.fromStrict <$> byteString

mediaFeature :: Gen Types.MediaFeature
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

mediaFeatureType :: Gen Types.MediaFeatureType
mediaFeatureType =
  Gen.enumBounded

mediaLength :: Gen Types.MediaLength
mediaLength =
  Gen.choice
    [ Types.MediaPx <$> number
    , Types.MediaIn <$> number
    , Types.MediaCm <$> number
    , Types.MediaMm <$> number
    , Types.MediaPt <$> number
    , Types.MediaPc <$> number
    ]

mediaOrientation :: Gen Types.MediaOrientation
mediaOrientation =
  Gen.enumBounded

mediaResolution :: Gen Types.MediaResolution
mediaResolution =
  Gen.choice
    [ Types.MediaDpi <$> number
    , Types.MediaDpcm <$> number
    , Types.MediaDppx <$> number
    ]

mediaScanType :: Gen Types.MediaScanType
mediaScanType =
  Gen.enumBounded

mediaColor :: Gen (Types.MediaFeatureType, Positive)
mediaColor =
  (,)
    <$> mediaFeatureType
    <*> Gen.integral (Range.linear 0 65536)

mediaColorScheme :: Gen Types.MediaColorScheme
mediaColorScheme =
  Gen.enumBounded

mediaPrefersContrast :: Gen Types.MediaPrefersContrast
mediaPrefersContrast =
  Gen.enumBounded

mediaPointerAccuracy :: Gen Types.MediaPointerAccuracy
mediaPointerAccuracy =
  Gen.enumBounded

mediaUpdateFrequency :: Gen Types.MediaUpdateFrequency
mediaUpdateFrequency =
  Gen.enumBounded

mediaQuery :: Gen Types.MediaQuery
mediaQuery =
  Types.MediaQuery
    <$> Gen.maybe Gen.enumBounded
    <*> Gen.maybe Gen.enumBounded
    <*> Gen.list (Range.linear 0 4) mediaFeature

name :: Gen Types.Name
name =
  Types.Name <$> text

natural :: Gen Natural
natural =
  Gen.integral (Range.linear 1 1000)

nonEmptyText :: Gen NET.NonEmptyText
nonEmptyText =
  NET.new
    <$> char
    <*> text

number :: Gen Types.Number
number =
  Types.numberFromFractional
    <$> Gen.double (Range.linearFrac 0.0 100.0)
    <*> Gen.word16 (Range.linear 0 4)

onOff :: Gen Types.OnOff
onOff =
  Gen.enumBounded

part :: Gen Types.Part
part =
  Types.Part <$> text

positive :: Gen Positive
positive =
  Gen.integral (Range.linear 1 1000)

rangeBound :: Gen Types.RawRangeBound
rangeBound =
  Types.mkRawRangeBound <$> text

ratio :: Integral a => Gen (Ratio a)
ratio =
  (%)
    <$> Gen.integral (Range.linear 0 1000)
    <*> Gen.integral (Range.linear 0 1000)

size :: Gen Types.Size
size =
  Types.Size
    <$> Gen.maybe mediaFeature
    <*> sizeLength

sizeLength :: Gen Types.SizeLength
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

srcsetCandidate :: Gen Types.SrcsetCandidate
srcsetCandidate =
  Types.SrcsetCandidate
    <$> fmap Types.mkURL url
    <*> srcsetDescriptor

srcsetDescriptor :: Gen Types.SrcsetDescriptor
srcsetDescriptor =
  Gen.choice
    [ Types.SrcsetWidth <$> positive
    , Types.SrcsetDensity <$> number
    ]

step :: Gen Types.Step
step =
  Gen.choice
    [ pure Types.Any
    , Types.Step <$> number
    ]

string :: Gen String
string =
  Gen.string (Range.linear 1 100) Gen.ascii

target :: Gen Types.Target
target =
  Gen.choice
    [ pure Types.Self
    , pure Types.Blank
    , pure Types.Parent
    , pure Types.Top
    , Types.CustomTarget <$> text
    ]

text :: Gen T.Text
text =
  Gen.text (Range.linear 1 100) Gen.ascii

type_ :: Gen Types.RawTypeOption
type_ =
  Types.mkRawTypeOption <$> text

url :: Gen Types.RawURL
url =
  Types.mkRawURL <$> text
