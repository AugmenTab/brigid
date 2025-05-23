module Brigid.HTML.Types.FetchPriority
  ( FetchPriority
      ( FetchHigh
      , FetchLow
      , FetchAuto
      )
  , fetchPriorityToBytes
  , fetchPriorityToText
  ) where

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

fetchPriorityToText :: FetchPriority -> T.Text
fetchPriorityToText fp =
  case fp of
    FetchHigh -> "high"
    FetchLow -> "low"
    FetchAuto -> "auto"
