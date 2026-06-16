module Brigid.HTML.Types.NumberingType
  ( NumberingType
      ( DecimalNumbers
      , UppercaseLatinLetters
      , LowercaseLatinLetters
      , UppercaseRomanNumerals
      , LowercaseRomanNumerals
      )
  , numberingTypeToBytes
  , numberingTypeToBytesBuilder
  , numberingTypeToText
  ) where

import Data.ByteString.Builder (Builder, char8)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

data NumberingType
  = DecimalNumbers
  | UppercaseLatinLetters
  | LowercaseLatinLetters
  | UppercaseRomanNumerals
  | LowercaseRomanNumerals
  deriving (Bounded, Enum, Eq, Show)

numberingTypeToBytes :: NumberingType -> LBS8.ByteString
numberingTypeToBytes nt =
  case nt of
    DecimalNumbers         -> LBS8.singleton '1'
    UppercaseLatinLetters  -> LBS8.singleton 'A'
    LowercaseLatinLetters  -> LBS8.singleton 'a'
    UppercaseRomanNumerals -> LBS8.singleton 'I'
    LowercaseRomanNumerals -> LBS8.singleton 'i'

numberingTypeToBytesBuilder :: NumberingType -> Builder
numberingTypeToBytesBuilder nt =
  case nt of
    DecimalNumbers         -> char8 '1'
    UppercaseLatinLetters  -> char8 'A'
    LowercaseLatinLetters  -> char8 'a'
    UppercaseRomanNumerals -> char8 'I'
    LowercaseRomanNumerals -> char8 'i'

numberingTypeToText :: NumberingType -> T.Text
numberingTypeToText nt =
  case nt of
    DecimalNumbers         -> T.singleton '1'
    UppercaseLatinLetters  -> T.singleton 'A'
    LowercaseLatinLetters  -> T.singleton 'a'
    UppercaseRomanNumerals -> T.singleton 'I'
    LowercaseRomanNumerals -> T.singleton 'i'
