{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Vals
  ( HtmxVals
  , HtmxValsTypes
  , mkHtmxVals
  , htmxValsToBytes
  , htmxValsToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.InlineJSON (InlineJSON, inlineJSONToBytes, inlineJSONToText)
import Brigid.Types.RawJavaScript qualified as RawJS

newtype HtmxVals = HtmxVals (Shrubbery.Union HtmxValsTypes)

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
      . Shrubbery.branch @RawJS.RawJavaScript (("js:" <>) . RawJS.rawJavaScriptToBytes)
      $ Shrubbery.branchEnd
  ) vals

htmxValsToText :: HtmxVals -> T.Text
htmxValsToText (HtmxVals vals) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InlineJSON          inlineJSONToText
      . Shrubbery.branch @RawJS.RawJavaScript (("js:" <>) . RawJS.rawJavaScriptToText)
      $ Shrubbery.branchEnd
  ) vals
