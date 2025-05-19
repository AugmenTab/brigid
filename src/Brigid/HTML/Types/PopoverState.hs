module Brigid.HTML.Types.PopoverState
  ( PopoverState
      ( AutoPopover
      , ManualPopover
      )
  , popoverStateToBytes
  , popoverStateToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data PopoverState
  = AutoPopover
  | ManualPopover
  deriving (Bounded, Enum, Eq, Show)

popoverStateToBytes :: PopoverState -> LBS.ByteString
popoverStateToBytes pos =
  case pos of
    AutoPopover   -> "auto"
    ManualPopover -> "manual"

popoverStateToText :: PopoverState -> T.Text
popoverStateToText pos =
  case pos of
    AutoPopover   -> "auto"
    ManualPopover -> "manual"
