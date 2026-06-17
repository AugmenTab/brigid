module Brigid.HXML.Types.ScrollOrientation
  ( ScrollOrientation
      ( Vertical
      , Horizontal
      )
  , scrollOrientationToBytes
  , scrollOrientationToBytesBuilder
  , scrollOrientationToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data ScrollOrientation
  = Vertical
  | Horizontal
  deriving (Bounded, Enum, Eq, Show)

scrollOrientationToBytes :: ScrollOrientation -> LBS.ByteString
scrollOrientationToBytes orientation =
  case orientation of
    Vertical   -> "vertical"
    Horizontal -> "horizontal"

scrollOrientationToBytesBuilder :: ScrollOrientation -> Builder
scrollOrientationToBytesBuilder orientation =
  case orientation of
    Vertical   -> string8 "vertical"
    Horizontal -> string8 "horizontal"

scrollOrientationToText :: ScrollOrientation -> T.Text
scrollOrientationToText orientation =
  case orientation of
    Vertical   -> "vertical"
    Horizontal -> "horizontal"
