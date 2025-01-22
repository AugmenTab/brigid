module Brigid.HXML.Types.KeyboardDismissMode
  ( KeyboardDismissMode
      ( None
      , OnDrag
      , Interactive
      )
  , keyboardDismissModeToBytes
  , keyboardDismissModeToText
  ) where

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

keyboardDismissModeToText :: KeyboardDismissMode -> T.Text
keyboardDismissModeToText mode =
  case mode of
    None        -> "none"
    OnDrag      -> "on-drag"
    Interactive -> "interactive"
