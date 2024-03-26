module HTML.Types.Delay
  ( Delay
  , delay
  , delayToBytes
  , delayToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Numeric.Natural (Natural)

newtype Delay = Delay Natural

delay :: Natural -> Delay
delay = Delay

delayToBytes :: Delay -> LBS.ByteString
delayToBytes (Delay n) =
  LBS.concat
    [ "delay:"
    , LBS8.pack $ show n
    , "s"
    ]

delayToText :: Delay -> T.Text
delayToText (Delay n) =
  T.concat
    [ "delay:"
    , T.pack $ show n
    , "s"
    ]
