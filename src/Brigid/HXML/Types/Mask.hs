module Brigid.HXML.Types.Mask
  ( Mask
  , mkMask
  , maskToBytes
  , maskToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype Mask =
  Mask
    { maskToText :: T.Text
    } deriving (Eq, Show)

mkMask :: T.Text -> Either String Mask
mkMask txt =
  let isValid c =
        if C.isAlphaNum c
           then c `elem` [ '9', 'A', 'S' ]
           else c /= '\t'
   in if T.all isValid txt
         then Right $ Mask txt
         else Left $ "Not a valid mask string: " <> T.unpack txt

maskToBytes :: Mask -> LBS.ByteString
maskToBytes = Render.textToBytes . maskToText
