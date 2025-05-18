module Brigid.HTML.Types.Size
  ( Size (..)
  , sizeToBytes
  , sizeToText
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

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (catMaybes)
import Data.Text qualified as T

import Brigid.HTML.Types.MediaQuery (MediaFeature, mediaFeatureToBytes, mediaFeatureToText)
import Brigid.HTML.Types.Number (Number, numberToBytes, numberToText)
import Brigid.Internal.Render qualified as Render

data Size =
  Size
    { sizeCondition :: Maybe MediaFeature
    , sizeLength :: SizeLength
    }

sizeToBytes :: Size -> LBS.ByteString
sizeToBytes size =
  LBS8.unwords $
    catMaybes
      [ mediaFeatureToBytes <$> sizeCondition size
      , Just . sizeLengthToBytes $ sizeLength size
      ]

sizeToText :: Size -> T.Text
sizeToText size =
  T.unwords $
    catMaybes
      [ mediaFeatureToText <$> sizeCondition size
      , Just . sizeLengthToText $ sizeLength size
      ]

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
