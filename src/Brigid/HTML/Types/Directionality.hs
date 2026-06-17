module Brigid.HTML.Types.Directionality
  ( Directionality
      ( LeftToRight
      , RightToLeft
      , AutoDirection
      )
  , directionalityToBytes
  , directionalityToBytesBuilder
  , directionalityToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Directionality
  = LeftToRight
  | RightToLeft
  | AutoDirection
  deriving (Bounded, Enum, Eq, Show)

directionalityToBytes :: Directionality -> LBS.ByteString
directionalityToBytes option =
  case option of
    LeftToRight   -> "ltr"
    RightToLeft   -> "rtl"
    AutoDirection -> "auto"

directionalityToBytesBuilder :: Directionality -> Builder
{-# INLINE directionalityToBytesBuilder #-}
directionalityToBytesBuilder option =
  case option of
    LeftToRight   -> string8 "ltr"
    RightToLeft   -> string8 "rtl"
    AutoDirection -> string8 "auto"

directionalityToText :: Directionality -> T.Text
directionalityToText option =
  case option of
    LeftToRight   -> "ltr"
    RightToLeft   -> "rtl"
    AutoDirection -> "auto"
