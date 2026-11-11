module Brigid.HTML.Types.Delay
  ( Delay
  , delay
  , delayToBytes
  , delayToBytesBuilder
  , delayToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Integer (Positive)

import Brigid.HTML.Types.TimingDeclaration qualified as TD

newtype Delay = Delay TD.TimingDeclaration
  deriving (Eq, Show)

delay :: Positive -> TD.TimingUnits -> Delay
delay n = Delay . TD.TimingDeclaration n

delayToBytes :: Delay -> LBS.ByteString
delayToBytes (Delay td) =
  "delay:" <> TD.timingDeclarationToBytes td

delayToBytesBuilder :: Delay -> Builder
{-# INLINE delayToBytesBuilder #-}
delayToBytesBuilder (Delay td) =
  "delay:" <> TD.timingDeclarationToBytesBuilder td

delayToText :: Delay -> T.Text
delayToText (Delay td) =
  "delay:" <> TD.timingDeclarationToText td
