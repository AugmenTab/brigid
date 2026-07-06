module Brigid.HTML.Types.Wrap
  ( Wrap
      ( WrapHard
      , WrapSoft
      , WrapOff
      )
  , wrapToBytes
  , wrapToBytesBuilder
  , wrapToText
  , wrapToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
{-# INLINE wrapToBytesBuilder #-}
wrapToBytesBuilder = lazyByteString . wrapToBytes

wrapToText :: Wrap -> T.Text
wrapToText wrap =
  case wrap of
    WrapHard -> "hard"
    WrapSoft -> "soft"
    WrapOff  -> "off"

wrapToTextBuilder :: Wrap -> TBL.Builder
wrapToTextBuilder = TBL.fromText . wrapToText
