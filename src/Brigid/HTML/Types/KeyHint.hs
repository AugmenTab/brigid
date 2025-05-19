module Brigid.HTML.Types.KeyHint
  ( KeyHintOption
      ( KeyHintEnter
      , KeyHintDone
      , KeyHintGo
      , KeyHintNext
      , KeyHintPrevious
      , KeyHintSearch
      , KeyHintSend
      )
  , keyHintOptionToBytes
  , keyHintOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data KeyHintOption
  = KeyHintEnter
  | KeyHintDone
  | KeyHintGo
  | KeyHintNext
  | KeyHintPrevious
  | KeyHintSearch
  | KeyHintSend
  deriving (Bounded, Enum, Eq, Show)

keyHintOptionToBytes :: KeyHintOption -> LBS.ByteString
keyHintOptionToBytes option =
  case option of
    KeyHintEnter    -> "enter"
    KeyHintDone     -> "done"
    KeyHintGo       -> "go"
    KeyHintNext     -> "next"
    KeyHintPrevious -> "previous"
    KeyHintSearch   -> "search"
    KeyHintSend     -> "send"

keyHintOptionToText :: KeyHintOption -> T.Text
keyHintOptionToText option =
  case option of
    KeyHintEnter    -> "enter"
    KeyHintDone     -> "done"
    KeyHintGo       -> "go"
    KeyHintNext     -> "next"
    KeyHintPrevious -> "previous"
    KeyHintSearch   -> "search"
    KeyHintSend     -> "send"
