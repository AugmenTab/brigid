module Brigid.HTML.Types.Decoding
  ( Decoding
      ( DecodeAuto
      , DecodeSync
      , DecodeAsync
      )
  , decodingToBytes
  , decodingToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Decoding
  = DecodeAuto
  | DecodeSync
  | DecodeAsync

decodingToBytes :: Decoding -> LBS.ByteString
decodingToBytes decoding =
  case decoding of
    DecodeAuto  -> "auto"
    DecodeSync  -> "sync"
    DecodeAsync -> "async"

decodingToText :: Decoding -> T.Text
decodingToText decoding =
  case decoding of
    DecodeAuto  -> "auto"
    DecodeSync  -> "sync"
    DecodeAsync -> "async"
