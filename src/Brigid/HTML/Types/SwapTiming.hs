module Brigid.HTML.Types.SwapTiming
  ( SwapTiming
  , swapTimingToBytes
  , swapTimingToBytesBuilder
  , swapTimingToText
  , swap
  , settle
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Numeric.Natural (Natural)
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

data SwapTiming =
  SwapTiming
    { swapTimingType    :: SwapTimingType
    , swapTimingSeconds :: Natural
    } deriving (Eq, Show)

swapTimingToBytes :: SwapTiming -> LBS.ByteString
swapTimingToBytes swapTiming =
  LBS.concat
    [ swapTimingTypeToBytes $ swapTimingType swapTiming
    , ":"
    , LBS8.pack . show $ swapTimingSeconds swapTiming
    , "s"
    ]

swapTimingToBytesBuilder :: SwapTiming -> Builder
{-# INLINE swapTimingToBytesBuilder #-}
swapTimingToBytesBuilder swapTiming =
  mconcat
    [ swapTimingTypeToBytesBuilder $ swapTimingType swapTiming
    , ":"
    , Render.showBytesBuilder $ swapTimingSeconds swapTiming
    , "s"
    ]

swapTimingToText :: SwapTiming -> T.Text
swapTimingToText swapTiming =
  T.concat
    [ swapTimingTypeToText $ swapTimingType swapTiming
    , ":"
    , T.pack . show $ swapTimingSeconds swapTiming
    , "s"
    ]

swap :: Natural -> SwapTiming
swap = SwapTiming Swap

settle :: Natural -> SwapTiming
settle = SwapTiming Settle

data SwapTimingType
  = Swap
  | Settle
  deriving (Bounded, Enum, Eq, Show)

swapTimingTypeToBytes :: SwapTimingType -> LBS.ByteString
swapTimingTypeToBytes timingType =
  case timingType of
    Swap   -> "swap"
    Settle -> "settle"

swapTimingTypeToBytesBuilder :: SwapTimingType -> Builder
{-# INLINE swapTimingTypeToBytesBuilder #-}
swapTimingTypeToBytesBuilder timingType =
  case timingType of
    Swap   -> "swap"
    Settle -> "settle"

swapTimingTypeToText :: SwapTimingType -> T.Text
swapTimingTypeToText timingType =
  case timingType of
    Swap   -> "swap"
    Settle -> "settle"
