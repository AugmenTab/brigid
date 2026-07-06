module Brigid.HXML.Types.ScrollOrientation
  ( ScrollOrientation
      ( Vertical
      , Horizontal
      )
  , scrollOrientationToBytes
  , scrollOrientationToBytesBuilder
  , scrollOrientationToText
  , scrollOrientationToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
{-# INLINE scrollOrientationToBytesBuilder #-}
scrollOrientationToBytesBuilder = lazyByteString . scrollOrientationToBytes

scrollOrientationToText :: ScrollOrientation -> T.Text
scrollOrientationToText orientation =
  case orientation of
    Vertical   -> "vertical"
    Horizontal -> "horizontal"

scrollOrientationToTextBuilder :: ScrollOrientation -> TBL.Builder
scrollOrientationToTextBuilder = TBL.fromText . scrollOrientationToText
