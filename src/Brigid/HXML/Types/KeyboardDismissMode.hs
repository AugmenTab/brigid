module Brigid.HXML.Types.KeyboardDismissMode
  ( KeyboardDismissMode
      ( None
      , OnDrag
      , Interactive
      )
  , keyboardDismissModeToBytes
  , keyboardDismissModeToBytesBuilder
  , keyboardDismissModeToText
  , keyboardDismissModeToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data KeyboardDismissMode
  = None
  | OnDrag
  | Interactive
  deriving (Bounded, Enum, Eq, Show)

keyboardDismissModeToBytes :: KeyboardDismissMode -> LBS.ByteString
keyboardDismissModeToBytes mode =
  case mode of
    None        -> "none"
    OnDrag      -> "on-drag"
    Interactive -> "interactive"

keyboardDismissModeToBytesBuilder :: KeyboardDismissMode -> Builder
{-# INLINE keyboardDismissModeToBytesBuilder #-}
keyboardDismissModeToBytesBuilder = lazyByteString . keyboardDismissModeToBytes

keyboardDismissModeToText :: KeyboardDismissMode -> T.Text
keyboardDismissModeToText mode =
  case mode of
    None        -> "none"
    OnDrag      -> "on-drag"
    Interactive -> "interactive"

keyboardDismissModeToTextBuilder :: KeyboardDismissMode -> TBL.Builder
keyboardDismissModeToTextBuilder = TBL.fromText . keyboardDismissModeToText
