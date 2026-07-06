module Brigid.HTML.Types.Shape
  ( Shape
      ( Default
      , Rect
      , Circle
      , Poly
      )
  , shapeToBytes
  , shapeToBytesBuilder
  , shapeToText
  , shapeToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data Shape
  = Default
  | Rect
  | Circle
  | Poly
  deriving (Bounded, Enum, Eq, Show)

shapeToBytes :: Shape -> LBS.ByteString
shapeToBytes shape =
  case shape of
    Default -> "default"
    Rect    -> "rect"
    Circle  -> "circle"
    Poly    -> "poly"

shapeToBytesBuilder :: Shape -> Builder
{-# INLINE shapeToBytesBuilder #-}
shapeToBytesBuilder = lazyByteString . shapeToBytes

shapeToText :: Shape -> T.Text
shapeToText shape =
  case shape of
    Default -> "default"
    Rect    -> "rect"
    Circle  -> "circle"
    Poly    -> "poly"

shapeToTextBuilder :: Shape -> TBL.Builder
shapeToTextBuilder = TBL.fromText . shapeToText
