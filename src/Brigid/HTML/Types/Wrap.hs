module Brigid.HTML.Types.Wrap
  ( Wrap
      ( WrapHard
      , WrapSoft
      , WrapOff
      )
  , wrapToBytes
  , wrapToBytesBuilder
  , wrapToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Wrap
  = WrapHard
  | WrapSoft
  | WrapOff
  deriving (Bounded, Enum, Eq, Show)

wrapToBytes :: Wrap -> LBS.ByteString
wrapToBytes wrap =
  case wrap of
    WrapHard -> "hard"
    WrapSoft -> "soft"
    WrapOff  -> "off"

wrapToBytesBuilder :: Wrap -> Builder
wrapToBytesBuilder wrap =
  case wrap of
    WrapHard -> string8 "hard"
    WrapSoft -> string8 "soft"
    WrapOff  -> string8 "off"

wrapToText :: Wrap -> T.Text
wrapToText wrap =
  case wrap of
    WrapHard -> "hard"
    WrapSoft -> "soft"
    WrapOff  -> "off"
