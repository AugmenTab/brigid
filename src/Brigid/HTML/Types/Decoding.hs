module Brigid.HTML.Types.Decoding
  ( Decoding
      ( DecodeAuto
      , DecodeSync
      , DecodeAsync
      )
  , decodingToBytes
  , decodingToBytesBuilder
  , decodingToText
  , decodingToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data Decoding
  = DecodeAuto
  | DecodeSync
  | DecodeAsync
  deriving (Bounded, Enum, Eq, Show)

decodingToBytes :: Decoding -> LBS.ByteString
decodingToBytes decoding =
  case decoding of
    DecodeAuto  -> "auto"
    DecodeSync  -> "sync"
    DecodeAsync -> "async"

decodingToBytesBuilder :: Decoding -> Builder
{-# INLINE decodingToBytesBuilder #-}
decodingToBytesBuilder decoding =
  case decoding of
    DecodeAuto  -> string8 "auto"
    DecodeSync  -> string8 "sync"
    DecodeAsync -> string8 "async"

decodingToText :: Decoding -> T.Text
decodingToText decoding =
  case decoding of
    DecodeAuto  -> "auto"
    DecodeSync  -> "sync"
    DecodeAsync -> "async"

decodingToTextBuilder :: Decoding -> TBL.Builder
decodingToTextBuilder = TBL.fromText . decodingToText
