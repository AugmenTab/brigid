module Brigid.HTML.Types.Throttle
  ( Throttle
  , throttle
  , throttleToBytes
  , throttleToText
  ) where

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

throttleToText :: Throttle -> T.Text
throttleToText (Throttle td) =
  "throttle:" <> TD.timingDeclarationToText td
