module Brigid.HXML.Types.ScrollOrientation
  ( ScrollOrientation
      ( Vertical
      , Horizontal
      )
  , scrollOrientationToBytes
  , scrollOrientationToText
  ) where

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

scrollOrientationToText :: ScrollOrientation -> T.Text
scrollOrientationToText orientation =
  case orientation of
    Vertical   -> "vertical"
    Horizontal -> "horizontal"
