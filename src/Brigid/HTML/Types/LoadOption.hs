module Brigid.HTML.Types.LoadOption
  ( LoadOption
      ( Eager
      , Lazy
      )
  , loadOptionToBytes
  , loadOptionToBytesBuilder
  , loadOptionToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data LoadOption
  = Eager
  | Lazy
  deriving (Bounded, Enum, Eq, Show)

loadOptionToBytes :: LoadOption -> LBS.ByteString
loadOptionToBytes pta =
  case pta of
    Eager -> "eager"
    Lazy -> "lazy"

loadOptionToBytesBuilder :: LoadOption -> Builder
loadOptionToBytesBuilder pta =
  case pta of
    Eager -> string8 "eager"
    Lazy  -> string8 "lazy"

loadOptionToText :: LoadOption -> T.Text
loadOptionToText pta =
  case pta of
    Eager -> "eager"
    Lazy -> "lazy"
