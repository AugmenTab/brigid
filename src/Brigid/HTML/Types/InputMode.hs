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
  , inputModeToBytesBuilder
  , inputModeToText
  ) where

import Data.ByteString.Builder (Builder, string8)
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
  deriving (Bounded, Enum, Eq, Show)

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

inputModeToBytesBuilder :: InputMode -> Builder
{-# INLINE inputModeToBytesBuilder #-}
inputModeToBytesBuilder mode =
  case mode of
    NoInputMode   -> string8 "none"
    TextMode      -> string8 "text"
    DecimalMode   -> string8 "decimal"
    NumericMode   -> string8 "numeric"
    TelephoneMode -> string8 "tel"
    SearchMode    -> string8 "search"
    EmailMode     -> string8 "email"
    URLMode       -> string8 "url"

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
