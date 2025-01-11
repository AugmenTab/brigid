module Brigid.HTML.Types.Number
  ( Number
  , numberFromIntegral
  , numberFromFractional
  , numberFromReal
  , numberFromScientific
  , numberToBytes
  , numberToText
  , number
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function (on)
import Data.Word (Word16)
import Data.Scientific qualified as Sci
import Data.Text qualified as T

data Number =
  Number
    { number :: Sci.Scientific
    , decimalPlaces :: Word16
    }

instance Eq Number where
  (==) = (==) `on` number

instance Show Number where
  show = formatNumber

numberFromIntegral :: Integral n => n -> Number
numberFromIntegral n =
  Number
    { number = fromIntegral n
    , decimalPlaces = 0
    }

numberFromFractional :: RealFloat n => n -> Word16 -> Number
numberFromFractional n d =
  Number
    { number = Sci.fromFloatDigits n
    , decimalPlaces = d
    }

numberFromReal :: Real n => n -> Word16 -> Number
numberFromReal n d =
  Number
    { number = either fst fst . Sci.fromRationalRepetend Nothing $ toRational n
    , decimalPlaces = d
    }

numberFromScientific :: Sci.Scientific -> Word16 -> Number
numberFromScientific = Number

numberToBytes :: Number -> LBS.ByteString
numberToBytes = LBS8.pack . formatNumber

numberToText :: Number -> T.Text
numberToText = T.pack . formatNumber

formatNumber :: Number -> String
formatNumber num =
  let s = number num
      e = fromIntegral $ decimalPlaces num
   in if Sci.isInteger s
         then Sci.formatScientific Sci.Fixed (Just e) s
         else
           if Sci.base10Exponent s >= (negate e)
              then Sci.formatScientific Sci.Fixed (Just e) s
              else Sci.formatScientific Sci.Generic Nothing s
