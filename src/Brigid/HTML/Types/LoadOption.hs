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

import Data.ByteString.Builder (Builder, lazyByteString)
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
loadOptionToBytesBuilder = lazyByteString . loadOptionToBytes

loadOptionToText :: LoadOption -> T.Text
loadOptionToText pta =
  case pta of
    Eager -> "eager"
    Lazy -> "lazy"

loadOptionToTextBuilder :: LoadOption -> TBL.Builder
loadOptionToTextBuilder = TBL.fromText . loadOptionToText
