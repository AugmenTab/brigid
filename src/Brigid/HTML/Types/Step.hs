module Brigid.HTML.Types.Step
  ( Step
      ( Any
      , Step
      )
  , stepToBytes
  , stepToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.HTML.Types.Number (Number, numberToBytes, numberToText)

data Step
  = Any
  | Step Number
  deriving (Eq, Show)

stepToBytes :: Step -> LBS.ByteString
stepToBytes step =
  case step of
    Any    -> "any"
    Step n -> numberToBytes n

stepToText :: Step -> T.Text
stepToText step =
  case step of
    Any    -> "any"
    Step n -> numberToText n
