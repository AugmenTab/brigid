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

import Data.ByteString.Builder (Builder, string8)
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
popoverStateToBytesBuilder pos =
  case pos of
    AutoPopover   -> string8 "auto"
    ManualPopover -> string8 "manual"

popoverStateToText :: PopoverState -> T.Text
popoverStateToText pos =
  case pos of
    AutoPopover   -> "auto"
    ManualPopover -> "manual"

popoverStateToTextBuilder :: PopoverState -> TBL.Builder
popoverStateToTextBuilder = TBL.fromText . popoverStateToText
