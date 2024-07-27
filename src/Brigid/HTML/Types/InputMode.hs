module Brigid.HTML.Types.InputMode
  ( InputMode
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
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
