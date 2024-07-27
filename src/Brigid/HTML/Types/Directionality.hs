module Brigid.HTML.Types.Directionality
  ( Directionality
      ( LeftToRight
      , RightToLeft
      , Auto
      )
  , directionalityToBytes
  , directionalityToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Directionality
  = LeftToRight
  | RightToLeft
  | Auto

directionalityToBytes :: Directionality -> LBS.ByteString
directionalityToBytes option =
  case option of
    LeftToRight -> "ltr"
    RightToLeft -> "rtl"
    Auto        -> "auto"

directionalityToText :: Directionality -> T.Text
directionalityToText option =
  case option of
    LeftToRight -> "ltr"
    RightToLeft -> "rtl"
    Auto        -> "auto"
