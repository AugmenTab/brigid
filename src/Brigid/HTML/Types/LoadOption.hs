module Brigid.HTML.Types.LoadOption
  ( LoadOption
      ( Eager
      , Lazy
      )
  , loadOptionToBytes
  , loadOptionToText
  ) where

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

loadOptionToText :: LoadOption -> T.Text
loadOptionToText pta =
  case pta of
    Eager -> "eager"
    Lazy -> "lazy"
