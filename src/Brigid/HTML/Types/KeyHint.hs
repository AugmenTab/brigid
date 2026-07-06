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
  , keyHintOptionToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
{-# INLINE keyHintOptionToBytesBuilder #-}
keyHintOptionToBytesBuilder = lazyByteString . keyHintOptionToBytes

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

keyHintOptionToTextBuilder :: KeyHintOption -> TBL.Builder
keyHintOptionToTextBuilder = TBL.fromText . keyHintOptionToText
