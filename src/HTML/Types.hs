module HTML.Types
  ( AutocapitalizeOption
      ( NoAutocapitalization
      , Sentences
      , Words
      , Characters
      )
  , autocapitalizeOptionToBytes
  , autocapitalizeOptionToText
  , ContentEditableOption
      ( Editable
      , NotEditable
      , PlaintextOnly
      )
  , contentEditableOptionToBytes
  , contentEditableOptionToText
  , Directionality
      ( LeftToRight
      , RightToLeft
      , Auto
      )
  , directionalityToBytes
  , directionalityToText
  , KeyHintOption
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
  , InputMode
      ( NoInputMode
      , TextMode
      , DecimalMode
      , NumericMode
      , TelephoneMode
      , SearchMode
      , EmailMode
      , URLMode
      )
  , inputModeToBytes
  , inputModeToText
  , Reachability
      ( Reachable
      , NotReachable
      )
  , reachabilityToInt
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data AutocapitalizeOption
  = NoAutocapitalization
  | Sentences
  | Words
  | Characters

autocapitalizeOptionToBytes :: AutocapitalizeOption -> LBS.ByteString
autocapitalizeOptionToBytes option =
  case option of
    NoAutocapitalization -> "none"
    Sentences            -> "sentences"
    Words                -> "words"
    Characters           -> "characters"

autocapitalizeOptionToText :: AutocapitalizeOption -> T.Text
autocapitalizeOptionToText option =
  case option of
    NoAutocapitalization -> "none"
    Sentences            -> "sentences"
    Words                -> "words"
    Characters           -> "characters"

data ContentEditableOption
  = Editable
  | NotEditable
  | PlaintextOnly

contentEditableOptionToBytes :: ContentEditableOption -> LBS.ByteString
contentEditableOptionToBytes option =
  case option of
    Editable      -> "true"
    NotEditable   -> "false"
    PlaintextOnly -> "plaintext-only"

contentEditableOptionToText :: ContentEditableOption -> T.Text
contentEditableOptionToText option =
  case option of
    Editable      -> "true"
    NotEditable   -> "false"
    PlaintextOnly -> "plaintext-only"

data Directionality
  = LeftToRight
  | RightToLeft
  | Auto

directionalityToBytes :: Directionality -> LBS.ByteString
directionalityToBytes option =
  case option of
    LeftToRight -> "ltr"
    RightToLeft -> "rtl"
    Auto        -> "auto"

directionalityToText :: Directionality -> T.Text
directionalityToText option =
  case option of
    LeftToRight -> "ltr"
    RightToLeft -> "rtl"
    Auto        -> "auto"

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

data InputMode
  = NoInputMode
  | TextMode
  | DecimalMode
  | NumericMode
  | TelephoneMode
  | SearchMode
  | EmailMode
  | URLMode

inputModeToBytes :: InputMode -> LBS.ByteString
inputModeToBytes mode =
  case mode of
    NoInputMode   -> "none"
    TextMode      -> "text"
    DecimalMode   -> "decimal"
    NumericMode   -> "numeric"
    TelephoneMode -> "tel"
    SearchMode    -> "search"
    EmailMode     -> "email"
    URLMode       -> "url"

inputModeToText :: InputMode -> T.Text
inputModeToText mode =
  case mode of
    NoInputMode   -> "none"
    TextMode      -> "text"
    DecimalMode   -> "decimal"
    NumericMode   -> "numeric"
    TelephoneMode -> "tel"
    SearchMode    -> "search"
    EmailMode     -> "email"
    URLMode       -> "url"

data Reachability
  = Reachable
  | NotReachable

reachabilityToInt :: Reachability -> Int
reachabilityToInt option =
  case option of
    Reachable    -> 0
    NotReachable -> negate 1
