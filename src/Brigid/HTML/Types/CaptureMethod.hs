module Brigid.HTML.Types.CaptureMethod
  ( CaptureMethod
      ( User
      , Environment
      )
  , captureMethodToBytes
  , captureMethodToBytesBuilder
  , captureMethodToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data CaptureMethod
  = User
  | Environment
  deriving (Bounded, Enum, Eq, Show)

captureMethodToBytes :: CaptureMethod -> LBS.ByteString
captureMethodToBytes capture =
  case capture of
    User        -> "user"
    Environment -> "environment"

captureMethodToBytesBuilder :: CaptureMethod -> Builder
{-# INLINE captureMethodToBytesBuilder #-}
captureMethodToBytesBuilder capture =
  case capture of
    User        -> string8 "user"
    Environment -> string8 "environment"

captureMethodToText :: CaptureMethod -> T.Text
captureMethodToText capture =
  case capture of
    User        -> "user"
    Environment -> "environment"
