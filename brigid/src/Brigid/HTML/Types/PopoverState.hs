module Brigid.HTML.Types.PopoverState
  ( PopoverState
      ( AutoPopover
      , ManualPopover
      )
  , popoverStateToBytes
  , popoverStateToBytesBuilder
  , popoverStateToText
  , popoverStateToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data PopoverState
  = AutoPopover
  | ManualPopover
  deriving (Bounded, Enum, Eq, Show)

popoverStateToBytes :: PopoverState -> LBS.ByteString
popoverStateToBytes pos =
  case pos of
    AutoPopover   -> "auto"
    ManualPopover -> "manual"

popoverStateToBytesBuilder :: PopoverState -> Builder
{-# INLINE popoverStateToBytesBuilder #-}
popoverStateToBytesBuilder = lazyByteString . popoverStateToBytes

popoverStateToText :: PopoverState -> T.Text
popoverStateToText pos =
  case pos of
    AutoPopover   -> "auto"
    ManualPopover -> "manual"

popoverStateToTextBuilder :: PopoverState -> TBL.Builder
popoverStateToTextBuilder = TBL.fromText . popoverStateToText
