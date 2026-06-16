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
  , keyHintOptionToBytesBuilder
  , keyHintOptionToText
  ) where

import Data.ByteString.Builder (Builder, string8)
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

keyHintOptionToBytesBuilder :: KeyHintOption -> Builder
keyHintOptionToBytesBuilder option =
  case option of
    KeyHintEnter    -> string8 "enter"
    KeyHintDone     -> string8 "done"
    KeyHintGo       -> string8 "go"
    KeyHintNext     -> string8 "next"
    KeyHintPrevious -> string8 "previous"
    KeyHintSearch   -> string8 "search"
    KeyHintSend     -> string8 "send"

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
