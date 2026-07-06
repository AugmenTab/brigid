module Brigid.HTML.Types.Directionality
  ( Directionality
      ( LeftToRight
      , RightToLeft
      , AutoDirection
      )
  , directionalityToBytes
  , directionalityToBytesBuilder
  , directionalityToText
  , directionalityToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
directionalityToBytesBuilder = lazyByteString . directionalityToBytes

directionalityToText :: Directionality -> T.Text
directionalityToText option =
  case option of
    LeftToRight   -> "ltr"
    RightToLeft   -> "rtl"
    AutoDirection -> "auto"

directionalityToTextBuilder :: Directionality -> TBL.Builder
directionalityToTextBuilder = TBL.fromText . directionalityToText
