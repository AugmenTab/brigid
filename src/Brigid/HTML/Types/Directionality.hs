module Brigid.HTML.Types.Directionality
  ( Directionality
      ( LeftToRight
      , RightToLeft
      , AutoDirection
      )
  , directionalityToBytes
  , directionalityToText
  ) where

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

directionalityToText :: Directionality -> T.Text
directionalityToText option =
  case option of
    LeftToRight   -> "ltr"
    RightToLeft   -> "rtl"
    AutoDirection -> "auto"
