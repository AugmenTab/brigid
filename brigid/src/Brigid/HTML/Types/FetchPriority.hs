module Brigid.HTML.Types.FetchPriority
  ( FetchPriority
      ( FetchHigh
      , FetchLow
      , FetchAuto
      )
  , fetchPriorityToBytes
  , fetchPriorityToBytesBuilder
  , fetchPriorityToText
  , fetchPriorityToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data FetchPriority
  = FetchHigh
  | FetchLow
  | FetchAuto
  deriving (Bounded, Enum, Eq, Show)

fetchPriorityToBytes :: FetchPriority -> LBS.ByteString
fetchPriorityToBytes fp =
  case fp of
    FetchHigh -> "high"
    FetchLow -> "low"
    FetchAuto -> "auto"

fetchPriorityToBytesBuilder :: FetchPriority -> Builder
{-# INLINE fetchPriorityToBytesBuilder #-}
fetchPriorityToBytesBuilder = lazyByteString . fetchPriorityToBytes

fetchPriorityToText :: FetchPriority -> T.Text
fetchPriorityToText fp =
  case fp of
    FetchHigh -> "high"
    FetchLow -> "low"
    FetchAuto -> "auto"

fetchPriorityToTextBuilder :: FetchPriority -> TBL.Builder
fetchPriorityToTextBuilder = TBL.fromText . fetchPriorityToText
