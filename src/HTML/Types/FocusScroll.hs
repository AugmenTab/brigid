module HTML.Types.FocusScroll
  ( FocusScroll (FocusScroll)
  , focusScrollToBytes
  , focusScrollToText
  ) where

import Data.Bool qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

newtype FocusScroll =
  FocusScroll
    { unFocusScroll :: Bool
    }

focusScrollToBytes :: FocusScroll -> LBS.ByteString
focusScrollToBytes =
  ("focus-scroll:" <>) . B.bool "false" "true" . unFocusScroll

focusScrollToText :: FocusScroll -> T.Text
focusScrollToText =
  ("focus-scroll:" <>) . B.bool "false" "true" . unFocusScroll
