module Brigid.HTML.Types.PopoverTargetAction
  ( PopoverTargetAction
      ( PopoverShow
      , PopoverHide
      , PopoverToggle
      )
  , popoverTargetActionToBytes
  , popoverTargetActionToBytesBuilder
  , popoverTargetActionToText
  , popoverTargetActionToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
popoverTargetActionToBytesBuilder = lazyByteString . popoverTargetActionToBytes

popoverTargetActionToText :: PopoverTargetAction -> T.Text
popoverTargetActionToText pta =
  case pta of
    PopoverShow -> "show"
    PopoverHide -> "hide"
    PopoverToggle -> "toggle"

popoverTargetActionToTextBuilder :: PopoverTargetAction -> TBL.Builder
popoverTargetActionToTextBuilder = TBL.fromText . popoverTargetActionToText
