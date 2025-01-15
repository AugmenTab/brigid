module Brigid.HTML.Types.Wrap
  ( Wrap
      ( WrapHard
      , WrapSoft
      , WrapOff
      )
  , wrapToBytes
  , wrapToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Wrap
  = WrapHard
  | WrapSoft
  | WrapOff

wrapToBytes :: Wrap -> LBS.ByteString
wrapToBytes wrap =
  case wrap of
    WrapHard -> "hard"
    WrapSoft -> "soft"
    WrapOff  -> "off"

wrapToText :: Wrap -> T.Text
wrapToText wrap =
  case wrap of
    WrapHard -> "hard"
    WrapSoft -> "soft"
    WrapOff  -> "off"
