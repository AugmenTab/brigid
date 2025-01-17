module Brigid.HTML.Types.CaptureMethod
  ( CaptureMethod
      ( User
      , Environment
      )
  , captureMethodToBytes
  , captureMethodToText
  ) where

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

captureMethodToText :: CaptureMethod -> T.Text
captureMethodToText capture =
  case capture of
    User        -> "user"
    Environment -> "environment"
