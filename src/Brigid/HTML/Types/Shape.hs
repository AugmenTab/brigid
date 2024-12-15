module Brigid.HTML.Types.Shape
  ( Shape
      ( Default
      , Rect
      , Circle
      , Poly
      )
  , shapeToBytes
  , shapeToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Shape
  = Default
  | Rect
  | Circle
  | Poly

shapeToBytes :: Shape -> LBS.ByteString
shapeToBytes shape =
  case shape of
    Default -> "default"
    Rect    -> "rect"
    Circle  -> "circle"
    Poly    -> "poly"

shapeToText :: Shape -> T.Text
shapeToText shape =
  case shape of
    Default -> "default"
    Rect    -> "rect"
    Circle  -> "circle"
    Poly    -> "poly"
