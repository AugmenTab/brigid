module Brigid.HXML.Types.KeyboardDismissMode
  ( KeyboardDismissMode
      ( None
      , OnDrag
      , Interactive
      )
  , keyboardDismissModeToBytes
  , keyboardDismissModeToBytesBuilder
  , keyboardDismissModeToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
keyboardDismissModeToBytesBuilder mode =
  case mode of
    None        -> string8 "none"
    OnDrag      -> string8 "on-drag"
    Interactive -> string8 "interactive"

keyboardDismissModeToText :: KeyboardDismissMode -> T.Text
keyboardDismissModeToText mode =
  case mode of
    None        -> "none"
    OnDrag      -> "on-drag"
    Interactive -> "interactive"
