module Brigid.HXML.Types.KeyboardShouldPersistTaps
  ( KeyboardShouldPersistTaps
      ( Never
      , Always
      , Handled
      )
  , keyboardShouldPersistTapsToBytes
  , keyboardShouldPersistTapsToBytesBuilder
  , keyboardShouldPersistTapsToText
  , keyboardShouldPersistTapsToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data KeyboardShouldPersistTaps
  = Never
  | Always
  | Handled
  deriving (Bounded, Enum, Eq, Show)

keyboardShouldPersistTapsToBytes :: KeyboardShouldPersistTaps -> LBS.ByteString
keyboardShouldPersistTapsToBytes option =
  case option of
    Never   -> "never"
    Always  -> "always"
    Handled -> "handled"

keyboardShouldPersistTapsToBytesBuilder :: KeyboardShouldPersistTaps -> Builder
{-# INLINE keyboardShouldPersistTapsToBytesBuilder #-}
keyboardShouldPersistTapsToBytesBuilder = lazyByteString . keyboardShouldPersistTapsToBytes

keyboardShouldPersistTapsToText :: KeyboardShouldPersistTaps -> T.Text
keyboardShouldPersistTapsToText option =
  case option of
    Never   -> "never"
    Always  -> "always"
    Handled -> "handled"

keyboardShouldPersistTapsToTextBuilder :: KeyboardShouldPersistTaps -> TBL.Builder
keyboardShouldPersistTapsToTextBuilder = TBL.fromText . keyboardShouldPersistTapsToText
