module Brigid.HTML.Types.SwapTransition
  ( SwapTransition (SwapTransition)
  , swapTransitionToBytes
  , swapTransitionToBytesBuilder
  , swapTransitionToText
  ) where

import Data.Bool qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

newtype SwapTransition =
  SwapTransition
    { unSwapTransition :: Bool
    } deriving (Eq, Show)

swapTransitionToBytes :: SwapTransition -> LBS.ByteString
swapTransitionToBytes =
  ("transition:" <>) . B.bool "false" "true" . unSwapTransition

swapTransitionToBytesBuilder :: SwapTransition -> Builder
{-# INLINE swapTransitionToBytesBuilder #-}
swapTransitionToBytesBuilder =
  ("transition:" <>) . B.bool "false" "true" . unSwapTransition

swapTransitionToText :: SwapTransition -> T.Text
swapTransitionToText =
  ("transition:" <>) . B.bool "false" "true" . unSwapTransition
