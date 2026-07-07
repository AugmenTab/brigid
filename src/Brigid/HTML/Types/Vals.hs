{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Vals
  ( HtmxVals
  , HtmxValsTypes
  , mkHtmxVals
  , htmxValsToBytes
  , htmxValsToBytesBuilder
  , htmxValsToText
  , htmxValsToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.InlineJSON (InlineJSON, inlineJSONToBytes, inlineJSONToText)
import Brigid.Types.RawJavaScript qualified as RawJS

newtype HtmxVals =
  HtmxVals (Shrubbery.Union HtmxValsTypes)
    deriving (Eq, Show)

type HtmxValsTypes =
  [ InlineJSON
  , RawJS.RawJavaScript
  ]

mkHtmxVals :: ( KnownNat branchIndex
              , branchIndex ~ FirstIndexOf vals HtmxValsTypes
              )
           => vals -> HtmxVals
mkHtmxVals =
  HtmxVals . Shrubbery.unify

htmxValsToBytes :: HtmxVals -> LBS.ByteString
htmxValsToBytes (HtmxVals vals) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InlineJSON          inlineJSONToBytes
      . Shrubbery.branch @RawJS.RawJavaScript RawJS.rawJavaScriptToBytes
      $ Shrubbery.branchEnd
  ) vals

htmxValsToBytesBuilder :: HtmxVals -> Builder
{-# INLINABLE htmxValsToBytesBuilder #-}
htmxValsToBytesBuilder = lazyByteString . htmxValsToBytes

htmxValsToText :: HtmxVals -> T.Text
htmxValsToText (HtmxVals vals) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InlineJSON          inlineJSONToText
      . Shrubbery.branch @RawJS.RawJavaScript RawJS.rawJavaScriptToText
      $ Shrubbery.branchEnd
  ) vals

htmxValsToTextBuilder :: HtmxVals -> TBL.Builder
htmxValsToTextBuilder = TBL.fromText . htmxValsToText
