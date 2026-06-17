module Brigid.HTML.Types.PopoverTargetAction
  ( PopoverTargetAction
      ( PopoverShow
      , PopoverHide
      , PopoverToggle
      )
  , popoverTargetActionToBytes
  , popoverTargetActionToBytesBuilder
  , popoverTargetActionToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data PopoverTargetAction
  = PopoverShow
  | PopoverHide
  | PopoverToggle
  deriving (Bounded, Enum, Eq, Show)

popoverTargetActionToBytes :: PopoverTargetAction -> LBS.ByteString
popoverTargetActionToBytes pta =
  case pta of
    PopoverShow -> "show"
    PopoverHide -> "hide"
    PopoverToggle -> "toggle"

popoverTargetActionToBytesBuilder :: PopoverTargetAction -> Builder
{-# INLINE popoverTargetActionToBytesBuilder #-}
popoverTargetActionToBytesBuilder pta =
  case pta of
    PopoverShow   -> string8 "show"
    PopoverHide   -> string8 "hide"
    PopoverToggle -> string8 "toggle"

popoverTargetActionToText :: PopoverTargetAction -> T.Text
popoverTargetActionToText pta =
  case pta of
    PopoverShow -> "show"
    PopoverHide -> "hide"
    PopoverToggle -> "toggle"
