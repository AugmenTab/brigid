module Brigid.HTML.Types.TargetType
  ( TargetType
      ( TargetNext
      , TargetPrevious
      )
  , targetTypeToBytes
  , targetTypeToBytesBuilder
  , targetTypeToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data TargetType
  = TargetNext
  | TargetPrevious
  deriving (Bounded, Enum, Eq, Show)

targetTypeToBytes :: TargetType -> LBS.ByteString
targetTypeToBytes targetType =
  case targetType of
    TargetNext     -> "next"
    TargetPrevious -> "previous"

targetTypeToBytesBuilder :: TargetType -> Builder
{-# INLINE targetTypeToBytesBuilder #-}
targetTypeToBytesBuilder = lazyByteString . targetTypeToBytes

targetTypeToText :: TargetType -> T.Text
targetTypeToText targetType =
  case targetType of
    TargetNext     -> "next"
    TargetPrevious -> "previous"
