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
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
shapeToBytesBuilder shape =
  case shape of
    Default -> string8 "default"
    Rect    -> string8 "rect"
    Circle  -> string8 "circle"
    Poly    -> string8 "poly"

shapeToText :: Shape -> T.Text
shapeToText shape =
  case shape of
    Default -> "default"
    Rect    -> "rect"
    Circle  -> "circle"
    Poly    -> "poly"
