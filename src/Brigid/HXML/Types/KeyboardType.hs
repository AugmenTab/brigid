module Brigid.HXML.Types.KeyboardType
  ( KeyboardType
      ( AlphaNumeric
      , NumberPad
      , DecimalPad
      , PhonePad
      , EmailAddress
      , Url
      , ASCIICapable
      , NumbersAndPunctuation
      , NamePhonePad
      , Twitter
      , WebSearch
      )
  , keyboardTypeToBytes
  , keyboardTypeToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data KeyboardType
  = AlphaNumeric -- ^ Standard alpha-numeric keyboard
  | NumberPad -- ^ Keyboard restricted to 0-9 digits plus decimals
  | DecimalPad -- ^ Keyboard restricted to 0-9 digits with no decimals
  | PhonePad -- ^ Keyboard restricted to digits and symbols that appear in phone numbers
  | EmailAddress -- ^ Keyboard adapted for easier email address input (handy @ symbol)
  | Url -- ^ Keyboard adapted for easier URL input (., / and .com in place of space bar)
  | ASCIICapable -- ^ Similar to the default keyboard, without emojis (iOS only)
  | NumbersAndPunctuation -- ^ Keyboard that opens by default on the page with numbers and punctuation. User can switch back to first page showing alphabetical characters. (iOS only)
  | NamePhonePad -- ^ Keyboard that opens by default on the page with alphabetical characters, where user can switch to second page showing a phone pad (useful to search for contacts, either by their name, or phone number). (iOS only)
  | Twitter -- ^ Keyboard with @ and # keys in place of "Return" key (iOS only)
  | WebSearch -- ^ Keyboard with "Go" key im place of "Return" key (iOS only)
  deriving (Bounded, Enum, Eq, Show)

keyboardTypeToBytes :: KeyboardType -> LBS.ByteString
keyboardTypeToBytes keyboardType =
  case keyboardType of
    AlphaNumeric          -> "default"
    NumberPad             -> "number-pad"
    DecimalPad            -> "decimal-pad"
    PhonePad              -> "phone-pad"
    EmailAddress          -> "email-address"
    Url                   -> "url"
    ASCIICapable          -> "ascii-capable"
    NumbersAndPunctuation -> "numbers-and-punctuation"
    NamePhonePad          -> "name-phone-pad"
    Twitter               -> "twitter"
    WebSearch             -> "web-search"

keyboardTypeToText :: KeyboardType -> T.Text
keyboardTypeToText keyboardType =
  case keyboardType of
    AlphaNumeric          -> "default"
    NumberPad             -> "number-pad"
    DecimalPad            -> "decimal-pad"
    PhonePad              -> "phone-pad"
    EmailAddress          -> "email-address"
    Url                   -> "url"
    ASCIICapable          -> "ascii-capable"
    NumbersAndPunctuation -> "numbers-and-punctuation"
    NamePhonePad          -> "name-phone-pad"
    Twitter               -> "twitter"
    WebSearch             -> "web-search"
