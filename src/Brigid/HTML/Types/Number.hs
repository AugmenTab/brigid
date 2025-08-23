{-# LANGUAGE ScopedTypeVariables #-}

module Brigid.HTML.Types.Number
  ( Number
  , numberFromIntegral
  , numberFromFractional
  , numberFromReal
  , numberFromScientific
  , numberFromText
  , numberToBytes
  , numberToText
  , number
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function (on)
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Text.Read (readMaybe)

data Number =
  Number
    { number :: Sci.Scientific
    , decimalPlaces :: Int
    }

instance Eq Number where
  (==) = (==) `on` number

instance Ord Number where
  compare = compare `on` number

instance Show Number where
  show = formatNumber

numberFromIntegral :: Integral n => n -> Number
numberFromIntegral n =
  Number
    { number = fromIntegral n
    , decimalPlaces = 0
    }

numberFromFractional :: RealFloat n => n -> Int -> Number
numberFromFractional n d =
  Number
    { number = Sci.fromFloatDigits n
    , decimalPlaces = d
    }

numberFromReal :: Real n => n -> Int -> Number
numberFromReal n d =
  Number
    { number = either fst fst . Sci.fromRationalRepetend Nothing $ toRational n
    , decimalPlaces = d
    }

numberFromScientific :: Sci.Scientific -> Int -> Number
numberFromScientific = Number

numberFromText :: T.Text -> Either String Number
numberFromText txt =
  case T.split (== '.') txt of
    [ n ] ->
      case readMaybe $ T.unpack n of
        Just (i :: Int) -> Right $ numberFromIntegral i
        Nothing -> Left $ "Could not parse integral Number: " <> T.unpack n

    [ _n, d ] ->
      case readMaybe $ T.unpack txt of
        Just (f :: Double) -> Right . numberFromFractional f $ T.length d
        Nothing -> Left $ "Could not parse fractional Number: " <> T.unpack txt

    _otherwise ->
      Left $ "Invalid Number: " <> T.unpack txt

numberToBytes :: Number -> LBS.ByteString
numberToBytes = LBS8.pack . formatNumber

numberToText :: Number -> T.Text
numberToText = T.pack . formatNumber

formatNumber :: Number -> String
formatNumber num =
  let s = number num
      e = decimalPlaces num
   in if Sci.isInteger s
         then Sci.formatScientific Sci.Fixed (Just e) s
         else
           if Sci.base10Exponent s >= (negate e)
              then Sci.formatScientific Sci.Fixed (Just e) s
              else Sci.formatScientific Sci.Generic Nothing s
