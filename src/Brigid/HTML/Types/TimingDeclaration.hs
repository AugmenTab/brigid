module Brigid.HTML.Types.TimingDeclaration
  ( TimingDeclaration (..)
  , timingDeclarationMilliseconds
  , timingDeclarationToBytes
  , timingDeclarationToText
  , TimingUnits
      ( Milliseconds
      , Seconds
      , Minutes
      , Hours
      )
  , timingUnitsToBytes
  , timingUnitsToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function (on)
import Data.Text qualified as T
import Integer (Positive)

data TimingDeclaration = TimingDeclaration Positive TimingUnits
  deriving (Eq, Show)

instance Ord TimingDeclaration where
  compare = compare `on` timingDeclarationMilliseconds

timingDeclarationMilliseconds :: TimingDeclaration -> Positive
timingDeclarationMilliseconds (TimingDeclaration p tu) =
  case tu of
    Milliseconds -> p
    Seconds -> timingDeclarationMilliseconds $ TimingDeclaration (1000 * p) Milliseconds
    Minutes -> timingDeclarationMilliseconds $ TimingDeclaration (60 * p) Seconds
    Hours -> timingDeclarationMilliseconds $ TimingDeclaration (60 * p) Minutes

timingDeclarationToBytes :: TimingDeclaration -> LBS.ByteString
timingDeclarationToBytes (TimingDeclaration n tu) =
  LBS8.pack (show n) <> timingUnitsToBytes tu

timingDeclarationToText :: TimingDeclaration -> T.Text
timingDeclarationToText (TimingDeclaration n tu) =
  T.pack (show n) <> timingUnitsToText tu

data TimingUnits
  = Milliseconds
  | Seconds
  | Minutes
  | Hours
  deriving (Bounded, Enum, Eq, Ord, Show)

timingUnitsToBytes :: TimingUnits -> LBS.ByteString
timingUnitsToBytes tu =
  case tu of
    Milliseconds -> "ms"
    Seconds -> "s"
    Minutes -> "m"
    Hours -> "h"

timingUnitsToText :: TimingUnits -> T.Text
timingUnitsToText tu =
  case tu of
    Milliseconds -> "ms"
    Seconds -> "s"
    Minutes -> "m"
    Hours -> "h"
