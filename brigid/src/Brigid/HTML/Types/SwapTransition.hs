module Brigid.HTML.Types.SwapTransition
  ( SwapTransition (SwapTransition)
  , swapTransitionToBytes
  , swapTransitionToBytesBuilder
  , swapTransitionToText
  ) where

import Data.Bool qualified as B
import Data.ByteString.Builder (Builder, lazyByteString)
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
swapTransitionToBytesBuilder = lazyByteString . swapTransitionToBytes

swapTransitionToText :: SwapTransition -> T.Text
swapTransitionToText =
  ("transition:" <>) . B.bool "false" "true" . unSwapTransition
