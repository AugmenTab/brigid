module Brigid.HTML.Types.Step
  ( Step
      ( Any
      , Step
      )
  , stepToBytes
  , stepToBytesBuilder
  , stepToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.HTML.Types.Number (Number, numberToBytes, numberToBytesBuilder, numberToText)

data Step
  = Any
  | Step Number
  deriving (Eq, Show)

stepToBytes :: Step -> LBS.ByteString
stepToBytes step =
  case step of
    Any    -> "any"
    Step n -> numberToBytes n

stepToBytesBuilder :: Step -> Builder
{-# INLINE stepToBytesBuilder #-}
stepToBytesBuilder step =
  case step of
    Any    -> string8 "any"
    Step n -> numberToBytesBuilder n

stepToText :: Step -> T.Text
stepToText step =
  case step of
    Any    -> "any"
    Step n -> numberToText n
