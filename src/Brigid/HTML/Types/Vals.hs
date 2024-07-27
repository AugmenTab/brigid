{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Vals
  ( HtmxVals
  , HtmxValsTypes
  , mkHtmxVals
  , unHtmxVals
  , htmxValsToBytes
  , htmxValsToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.InlineJSON (InlineJSON, inlineJSONToBytes, inlineJSONToText)
import Brigid.HTML.Types.RawJavaScript qualified as RawJS

newtype HtmxVals =
  HtmxVals
    { unHtmxVals :: Shrubbery.Union HtmxValsTypes
    }

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
htmxValsToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InlineJSON inlineJSONToBytes
      . Shrubbery.branch @RawJS.RawJavaScript RawJS.rawJavaScriptToBytes
      $ Shrubbery.branchEnd
  ) . unHtmxVals

htmxValsToText :: HtmxVals -> T.Text
htmxValsToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InlineJSON inlineJSONToText
      . Shrubbery.branch @RawJS.RawJavaScript RawJS.rawJavaScriptToText
      $ Shrubbery.branchEnd
  ) . unHtmxVals
