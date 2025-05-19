module Brigid.HTML.Types.Autocapitalize
  ( AutocapitalizeOption
      ( NoAutocapitalization
      , Sentences
      , Words
      , Characters
      )
  , autocapitalizeOptionToBytes
  , autocapitalizeOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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

autocapitalizeOptionToText :: AutocapitalizeOption -> T.Text
autocapitalizeOptionToText option =
  case option of
    NoAutocapitalization -> "none"
    Sentences            -> "sentences"
    Words                -> "words"
    Characters           -> "characters"
