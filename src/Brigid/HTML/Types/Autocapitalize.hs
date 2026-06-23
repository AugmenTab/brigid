module Brigid.HTML.Types.Autocapitalize
  ( AutocapitalizeOption
      ( NoAutocapitalization
      , Sentences
      , Words
      , Characters
      )
  , autocapitalizeOptionToBytes
  , autocapitalizeOptionToBytesBuilder
  , autocapitalizeOptionToText
  , autocapitalizeOptionToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data AutocapitalizeOption
  = NoAutocapitalization
  | Sentences
  | Words
  | Characters
  deriving (Bounded, Enum, Eq, Show)

autocapitalizeOptionToBytes :: AutocapitalizeOption -> LBS.ByteString
autocapitalizeOptionToBytes option =
  case option of
    NoAutocapitalization -> "none"
    Sentences            -> "sentences"
    Words                -> "words"
    Characters           -> "characters"

autocapitalizeOptionToBytesBuilder :: AutocapitalizeOption -> Builder
{-# INLINE autocapitalizeOptionToBytesBuilder #-}
autocapitalizeOptionToBytesBuilder option =
  case option of
    NoAutocapitalization -> string8 "none"
    Sentences            -> string8 "sentences"
    Words                -> string8 "words"
    Characters           -> string8 "characters"

autocapitalizeOptionToText :: AutocapitalizeOption -> T.Text
autocapitalizeOptionToText option =
  case option of
    NoAutocapitalization -> "none"
    Sentences            -> "sentences"
    Words                -> "words"
    Characters           -> "characters"

autocapitalizeOptionToTextBuilder :: AutocapitalizeOption -> TBL.Builder
autocapitalizeOptionToTextBuilder = TBL.fromText . autocapitalizeOptionToText
