module Brigid.HXML.Types.Mask
  ( Mask
  , mkMask
  , maskToBytes
  , maskToBytesBuilder
  , maskToText
  , maskToTextBuilder
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

import Brigid.Internal.Render qualified as Render

newtype Mask =
  Mask
    { maskToText :: T.Text
    } deriving (Eq)

instance Show Mask where
  show = mappend "Mask " . T.unpack . maskToText

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
maskToBytes = Render.textToLazyBytes . maskToText

maskToBytesBuilder :: Mask -> Builder
maskToBytesBuilder = Render.textToBytesBuilder . maskToText

maskToTextBuilder :: Mask -> TBL.Builder
maskToTextBuilder = TBL.fromText . maskToText
