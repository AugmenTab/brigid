module Brigid.HTML.Types.Orientation
  ( Orientation
      ( Horizontal
      , Vertical
      )
  , orientationToBytes
  , orientationToBytesBuilder
  , orientationToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Orientation
  = Horizontal
  | Vertical
  deriving (Bounded, Enum, Eq, Show)

orientationToBytes :: Orientation -> LBS.ByteString
orientationToBytes o =
  case o of
    Horizontal -> "horizontal"
    Vertical   -> "vertical"

orientationToBytesBuilder :: Orientation -> Builder
{-# INLINE orientationToBytesBuilder #-}
orientationToBytesBuilder = lazyByteString . orientationToBytes

orientationToText :: Orientation -> T.Text
orientationToText o =
  T.pack $
    case o of
      Horizontal -> "horizontal"
      Vertical -> "vertical"
