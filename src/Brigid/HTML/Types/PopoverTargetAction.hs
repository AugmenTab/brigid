module Brigid.HTML.Types.PopoverTargetAction
  ( PopoverTargetAction
      ( PopoverShow
      , PopoverHide
      , PopoverToggle
      )
  , popoverTargetActionToBytes
  , popoverTargetActionToText
  ) where

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

popoverTargetActionToText :: PopoverTargetAction -> T.Text
popoverTargetActionToText pta =
  case pta of
    PopoverShow -> "show"
    PopoverHide -> "hide"
    PopoverToggle -> "toggle"
