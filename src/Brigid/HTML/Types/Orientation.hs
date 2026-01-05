module Brigid.HTML.Types.Orientation
  ( Orientation
      ( Horizontal
      , Vertical
      )
  , orientationToBytes
  , orientationToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

data Orientation
  = Horizontal
  | Vertical
  deriving (Bounded, Enum, Eq, Show)

orientationToBytes :: Orientation -> LBS.ByteString
orientationToBytes o =
  LBS8.pack $
    case o of
      Horizontal -> "horizontal"
      Vertical -> "vertical"

orientationToText :: Orientation -> T.Text
orientationToText o =
  T.pack $
    case o of
      Horizontal -> "horizontal"
      Vertical -> "vertical"
