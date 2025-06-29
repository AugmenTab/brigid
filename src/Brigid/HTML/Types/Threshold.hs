module Brigid.HTML.Types.Threshold
  ( Threshold
  , mkThreshold
  , thresholdToBytes
  , thresholdToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Fixed (Milli)
import Data.Ratio (Ratio, (%))
import Data.Text qualified as T
import Numeric.Natural (Natural)

newtype Threshold =
  Threshold
    { unThreshold :: Ratio Natural
    } deriving (Eq, Show)

mkThreshold :: Natural -> Either String Threshold
mkThreshold n =
  if n % 100 > 1
     then
       Left $
         "Invalid Threshold "
           <> show n
           <> ". Thresholds must be between 1 and 100."
     else
       Right . Threshold $ n % 100

thresholdToBytes :: Threshold -> LBS.ByteString
thresholdToBytes =
  ("threshold:" <>) . LBS8.pack . show . toMilli . unThreshold

thresholdToText :: Threshold -> T.Text
thresholdToText =
  ("threshold:" <>) . T.pack . show . toMilli . unThreshold

-- Helpers
--
toMilli :: Ratio Natural -> Milli
toMilli = fromRational . toRational
