module Brigid.HTML.Types.LoadOption
  ( LoadOption
      ( Eager
      , Lazy
      )
  , loadOptionToBytes
  , loadOptionToBytesBuilder
  , loadOptionToText
  , loadOptionToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
{-# INLINE loadOptionToBytesBuilder #-}
loadOptionToBytesBuilder pta =
  case pta of
    Eager -> string8 "eager"
    Lazy  -> string8 "lazy"

loadOptionToText :: LoadOption -> T.Text
loadOptionToText pta =
  case pta of
    Eager -> "eager"
    Lazy -> "lazy"

loadOptionToTextBuilder :: LoadOption -> TBL.Builder
loadOptionToTextBuilder = TBL.fromText . loadOptionToText
