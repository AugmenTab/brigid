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

stepToBytes :: Step -> LBS.ByteString
stepToBytes step =
  case step of
    Any -> "any"
    Step number -> numberToBytes number

stepToText :: Step -> T.Text
stepToText step =
  case step of
    Any -> "any"
    Step number -> numberToText number
