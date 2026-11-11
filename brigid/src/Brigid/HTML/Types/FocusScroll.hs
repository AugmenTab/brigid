module Brigid.HTML.Types.FocusScroll
  ( FocusScroll (FocusScroll)
  , focusScrollToBytes
  , focusScrollToBytesBuilder
  , focusScrollToText
  ) where

import Data.Bool qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

newtype FocusScroll =
  FocusScroll
    { unFocusScroll :: Bool
    } deriving (Eq, Show)

focusScrollToBytes :: FocusScroll -> LBS.ByteString
focusScrollToBytes =
  ("focus-scroll:" <>) . B.bool "false" "true" . unFocusScroll

focusScrollToBytesBuilder :: FocusScroll -> Builder
{-# INLINE focusScrollToBytesBuilder #-}
focusScrollToBytesBuilder =
  ("focus-scroll:" <>) . B.bool "false" "true" . unFocusScroll

focusScrollToText :: FocusScroll -> T.Text
focusScrollToText =
  ("focus-scroll:" <>) . B.bool "false" "true" . unFocusScroll
