module HTML.Types.KeyHint
  ( KeyHintOption
      ( Enter
      , Done
      , Go
      , Next
      , Previous
      , Search
      , Send
      )
  , keyHintOptionToBytes
  , keyHintOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data KeyHintOption
  = Enter
  | Done
  | Go
  | Next
  | Previous
  | Search
  | Send

keyHintOptionToBytes :: KeyHintOption -> LBS.ByteString
keyHintOptionToBytes option =
  case option of
    Enter    -> "enter"
    Done     -> "done"
    Go       -> "go"
    Next     -> "next"
    Previous -> "previous"
    Search   -> "search"
    Send     -> "send"

keyHintOptionToText :: KeyHintOption -> T.Text
keyHintOptionToText option =
  case option of
    Enter    -> "enter"
    Done     -> "done"
    Go       -> "go"
    Next     -> "next"
    Previous -> "previous"
    Search   -> "search"
    Send     -> "send"
