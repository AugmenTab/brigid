module Brigid.HXML.Types.KeyboardShouldPersistTaps
  ( KeyboardShouldPersistTaps
      ( Never
      , Always
      , Handled
      )
  , keyboardShouldPersistTapsToBytes
  , keyboardShouldPersistTapsToText
  ) where

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

keyboardShouldPersistTapsToText :: KeyboardShouldPersistTaps -> T.Text
keyboardShouldPersistTapsToText option =
  case option of
    Never   -> "never"
    Always  -> "always"
    Handled -> "handled"
