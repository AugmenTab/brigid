module Brigid.HTML.Types.Wrap
  ( Wrap
      ( Hard
      , Soft
      , Off
      )
  , wrapToBytes
  , wrapToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Wrap
  = Hard
  | Soft
  | Off

wrapToBytes :: Wrap -> LBS.ByteString
wrapToBytes wrap =
  case wrap of
    Hard -> "hard"
    Soft -> "soft"
    Off  -> "off"

wrapToText :: Wrap -> T.Text
wrapToText wrap =
  case wrap of
    Hard -> "hard"
    Soft -> "soft"
    Off  -> "off"
