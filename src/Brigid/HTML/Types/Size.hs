module Brigid.HTML.Types.Size
  ( Size (..)
  , sizeToBytes
  , sizeToBytesBuilder
  , sizeToText
  , sizeToTextBuilder
  , SizeLength
      ( SizePx
      , SizeVw
      , SizeVh
      , SizePercent
      , SizeEm
      , SizeRem
      , SizeCh
      , SizeEx
      , SizeVmin
      , SizeVmax
      , SizeCalc
      )
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Encoding qualified as TE

import Brigid.HTML.Types.MediaQuery (MediaFeature, mediaFeatureToBytes, mediaFeatureToBytesBuilder, mediaFeatureToText)
import Brigid.HTML.Types.Number (Number, numberToBytes, numberToBytesBuilder, numberToText)
import Brigid.Internal.Render qualified as Render

data Size =
  Size
    { sizeCondition :: Maybe MediaFeature
    , sizeLength :: SizeLength
    } deriving (Eq, Show)

sizeToBytes :: Size -> LBS.ByteString
sizeToBytes size =
  LBS8.unwords $
    catMaybes
      [ mediaFeatureToBytes <$> sizeCondition size
      , Just . sizeLengthToBytes $ sizeLength size
      ]

sizeToBytesBuilder :: Size -> Builder
{-# INLINE sizeToBytesBuilder #-}
sizeToBytesBuilder size =
  Render.foldToBytesBuilderWithSeparator id " " $
    catMaybes
      [ mediaFeatureToBytesBuilder <$> sizeCondition size
      , Just . sizeLengthToBytesBuilder $ sizeLength size
      ]

sizeToText :: Size -> T.Text
sizeToText size =
  T.unwords $
    catMaybes
      [ mediaFeatureToText <$> sizeCondition size
      , Just . sizeLengthToText $ sizeLength size
      ]

sizeToTextBuilder :: Size -> TBL.Builder
sizeToTextBuilder = TBL.fromText . sizeToText

data SizeLength
  = SizePx Int
  | SizeVw Int
  | SizeVh Int
  | SizePercent Int
  | SizeEm Number
  | SizeRem Number
  | SizeCh Number
  | SizeEx Number
  | SizeVmin Number
  | SizeVmax Number
  | SizeCalc T.Text
  deriving (Eq, Show)

sizeLengthToBytes :: SizeLength -> LBS.ByteString
sizeLengthToBytes sl =
  case sl of
    SizePx n -> Render.showBytes n <> "px"
    SizeVw n -> Render.showBytes n <> "vw"
    SizeVh n -> Render.showBytes n <> "vh"
    SizePercent n -> Render.showBytes n <> "%"
    SizeEm n -> numberToBytes n <> "em"
    SizeRem n -> numberToBytes n <> "rem"
    SizeCh n -> numberToBytes n <> "ch"
    SizeEx n -> numberToBytes n <> "ex"
    SizeVmin n -> numberToBytes n <> "vmin"
    SizeVmax n -> numberToBytes n <> "vmax"
    SizeCalc n -> "calc(" <> Render.textToLazyBytes n <> ")"

sizeLengthToBytesBuilder :: SizeLength -> Builder
{-# INLINE sizeLengthToBytesBuilder #-}
sizeLengthToBytesBuilder sl =
  case sl of
    SizePx n      -> Render.showIntegerBytesBuilder n <> string8 "px"
    SizeVw n      -> Render.showIntegerBytesBuilder n <> string8 "vw"
    SizeVh n      -> Render.showIntegerBytesBuilder n <> string8 "vh"
    SizePercent n -> Render.showIntegerBytesBuilder n <> string8 "%"
    SizeEm n      -> numberToBytesBuilder n <> string8 "em"
    SizeRem n     -> numberToBytesBuilder n <> string8 "rem"
    SizeCh n      -> numberToBytesBuilder n <> string8 "ch"
    SizeEx n      -> numberToBytesBuilder n <> string8 "ex"
    SizeVmin n    -> numberToBytesBuilder n <> string8 "vmin"
    SizeVmax n    -> numberToBytesBuilder n <> string8 "vmax"
    SizeCalc n    -> string8 "calc(" <> TE.encodeUtf8Builder n <> string8 ")"

sizeLengthToText :: SizeLength -> T.Text
sizeLengthToText sl =
  case sl of
    SizePx n -> Render.showText n <> "px"
    SizeVw n -> Render.showText n <> "vw"
    SizeVh n -> Render.showText n <> "vh"
    SizePercent n -> Render.showText n <> "%"
    SizeEm n -> numberToText n <> "em"
    SizeRem n -> numberToText n <> "rem"
    SizeCh n -> numberToText n <> "ch"
    SizeEx n -> numberToText n <> "ex"
    SizeVmin n -> numberToText n <> "vmin"
    SizeVmax n -> numberToText n <> "vmax"
    SizeCalc n -> "calc(" <> n <> ")"
