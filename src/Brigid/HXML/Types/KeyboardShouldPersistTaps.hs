module Brigid.HXML.Types.KeyboardShouldPersistTaps
  ( KeyboardShouldPersistTaps
      ( Never
      , Always
      , Handled
      )
  , keyboardShouldPersistTapsToBytes
  , keyboardShouldPersistTapsToBytesBuilder
  , keyboardShouldPersistTapsToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
keyboardShouldPersistTapsToBytesBuilder option =
  case option of
    Never   -> string8 "never"
    Always  -> string8 "always"
    Handled -> string8 "handled"

keyboardShouldPersistTapsToText :: KeyboardShouldPersistTaps -> T.Text
keyboardShouldPersistTapsToText option =
  case option of
    Never   -> "never"
    Always  -> "always"
    Handled -> "handled"
