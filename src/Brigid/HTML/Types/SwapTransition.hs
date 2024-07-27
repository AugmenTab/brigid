module Brigid.HTML.Types.SwapTransition
  ( SwapTransition (SwapTransition)
  , swapTransitionToBytes
  , swapTransitionToText
  ) where

import Data.Bool qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

newtype SwapTransition =
  SwapTransition
    { unSwapTransition :: Bool
    }

swapTransitionToBytes :: SwapTransition -> LBS.ByteString
swapTransitionToBytes =
  ("transition:" <>) . B.bool "false" "true" . unSwapTransition

swapTransitionToText :: SwapTransition -> T.Text
swapTransitionToText =
  ("transition:" <>) . B.bool "false" "true" . unSwapTransition
