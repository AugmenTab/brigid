module HTML.Types.Target
  ( TargetType
      ( TargetNext
      , TargetPrevious
      )
  , targetTypeToBytes
  , targetTypeToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data TargetType
  = TargetNext
  | TargetPrevious

targetTypeToBytes :: TargetType -> LBS.ByteString
targetTypeToBytes targetType =
  case targetType of
    TargetNext     -> "next"
    TargetPrevious -> "previous"

targetTypeToText :: TargetType -> T.Text
targetTypeToText targetType =
  case targetType of
    TargetNext     -> "next"
    TargetPrevious -> "previous"
