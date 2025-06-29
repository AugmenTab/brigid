module Brigid.HTML.Types.Throttle
  ( Throttle
  , throttle
  , throttleToBytes
  , throttleToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Numeric.Natural (Natural)

newtype Throttle = Throttle Natural
  deriving (Eq, Show)

throttle :: Natural -> Throttle
throttle = Throttle

throttleToBytes :: Throttle -> LBS.ByteString
throttleToBytes (Throttle n) =
  LBS.concat
    [ "throttle:"
    , LBS8.pack $ show n
    , "s"
    ]

throttleToText :: Throttle -> T.Text
throttleToText (Throttle n) =
  T.concat
    [ "throttle:"
    , T.pack $ show n
    , "s"
    ]
