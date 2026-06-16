module Brigid.HTML.Types.FetchPriority
  ( FetchPriority
      ( FetchHigh
      , FetchLow
      , FetchAuto
      )
  , fetchPriorityToBytes
  , fetchPriorityToBytesBuilder
  , fetchPriorityToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
fetchPriorityToBytesBuilder fp =
  case fp of
    FetchHigh -> string8 "high"
    FetchLow  -> string8 "low"
    FetchAuto -> string8 "auto"

fetchPriorityToText :: FetchPriority -> T.Text
fetchPriorityToText fp =
  case fp of
    FetchHigh -> "high"
    FetchLow -> "low"
    FetchAuto -> "auto"
