module Brigid.HTML.Types.Throttle
  ( Throttle
  , throttle
  , throttleToBytes
  , throttleToBytesBuilder
  , throttleToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Integer (Positive)

import Brigid.HTML.Types.TimingDeclaration qualified as TD

newtype Throttle = Throttle TD.TimingDeclaration
  deriving (Eq, Show)

throttle :: Positive -> TD.TimingUnits -> Throttle
throttle n = Throttle . TD.TimingDeclaration n

throttleToBytes :: Throttle -> LBS.ByteString
throttleToBytes (Throttle td) =
  "throttle:" <> TD.timingDeclarationToBytes td

throttleToBytesBuilder :: Throttle -> Builder
throttleToBytesBuilder (Throttle td) =
  "throttle:" <> TD.timingDeclarationToBytesBuilder td

throttleToText :: Throttle -> T.Text
throttleToText (Throttle td) =
  "throttle:" <> TD.timingDeclarationToText td
