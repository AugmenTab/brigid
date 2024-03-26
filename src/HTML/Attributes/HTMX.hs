{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Attributes.HTMX
  ( htmx
  , hxGet
  , hxPost
  , hxPushURL
  , hxSelect
  , hxSelectOOB
  , hxSwap
  , hxSwapOOB
  , hxTarget
  , hxTrigger
  , hxVals
  , hxBoost
  , hxConfirm
  , hxDelete
  , hxDisable
  , hxDisabled
  , hxDisabledElt
  , hxDisinherit
  , hxEncoding
  , hxExt
  , hxHistory
  , hxHistoryElt
  , hxInclude
  , hxOn
  , hxParams
  , hxPatch
  , hxPreserve
  , hxPreserved
  , hxPrompt
  , hxPut
  , hxReplaceURL
  , hxValidate
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import HTML.Attributes.AttributeType (AttributeType(..))
import HTML.Attributes.Elements (ValidAttribute)
import HTML.Attributes.Internal (Attribute(..))
import HTML.Types qualified as Types

-- Core Attributes
--

htmx :: Types.RelativeURL method -> Attribute tag
htmx = Attr_Htmx

hxGet :: Types.RelativeURL Types.Get -> Attribute tag
hxGet = Attr_Htmx

hxPost :: Types.RelativeURL Types.Post -> Attribute tag
hxPost = Attr_Htmx

hxDelete :: Types.RelativeURL Types.Delete -> Attribute tag
hxDelete = Attr_Htmx

hxPatch :: Types.RelativeURL Types.Patch -> Attribute tag
hxPatch = Attr_Htmx

hxPut :: Types.RelativeURL Types.Put -> Attribute tag
hxPut = Attr_Htmx

hxOn :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf eventType Types.EventTypes
        )
     => eventType -> T.Text -> Attribute tag
hxOn = Attr_HxOn . Types.mkEvent

hxPushURL :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf url Types.PushURLTypes
             )
          => url -> Attribute tag
hxPushURL = Attr_HxPushURL . Types.mkPushURL

hxSelect :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf querySelector Types.QuerySelectorTypes
            )
         => querySelector -> Attribute tag
hxSelect = Attr_HxSelect . Types.mkQuerySelector

hxSelectOOB :: NEL.NonEmpty Types.OutOfBandSelect -> Attribute tag
hxSelectOOB = Attr_HxSelectOOB

hxSwap :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf swap Types.SwapTypes
          )
       => swap -> Attribute tag
hxSwap = Attr_HxSwap . Types.mkSwap

hxSwapOOB :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf swap Types.OutOfBandSwapTypes
             )
          => Maybe swap -> Attribute tag
hxSwapOOB = Attr_HxSwapOOB . fmap Types.mkOutOfBandSwap

hxTarget :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf target Types.TargetTypes
            )
         => target -> Attribute tag
hxTarget = Attr_HxTarget . Types.mkTarget

hxTrigger :: NEL.NonEmpty Types.Trigger -> Attribute tag
hxTrigger = Attr_HxTrigger

hxVals :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf vals Types.HtmxValsTypes
          )
       => vals -> Attribute tag
hxVals = Attr_HxVals . Types.mkHtmxVals

-- Additional Attributes
--

hxBoost :: Bool -> Attribute tag
hxBoost = Attr_HxBoost

hxConfirm :: T.Text -> Attribute tag
hxConfirm = Attr_HxConfirm

hxDisable :: Bool -> Attribute tag
hxDisable = Attr_HxDisable

hxDisabled :: Attribute tag
hxDisabled = hxDisable True

hxDisabledElt :: NEL.NonEmpty Types.DisabledSelector -> Attribute tag
hxDisabledElt = Attr_HxDisabledElt

hxDisinherit :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf disinherit Types.DisinheritTypes
                )
             => disinherit -> Attribute tag
hxDisinherit = Attr_HxDisinherit . Types.mkDisinherit

hxEncoding :: Attribute tag
hxEncoding = Attr_HxEncoding

hxExt :: NEL.NonEmpty Types.Extension -> Attribute tag
hxExt = Attr_HxExt

-- hx-headers

hxHistory :: Attribute tag
hxHistory = Attr_HxHistory

hxHistoryElt :: Attribute tag
hxHistoryElt = Attr_HxHistoryElt

hxInclude :: Types.IncludeSelector -> Attribute tag
hxInclude = Attr_HxInclude

-- hx-indicator

hxParams :: Types.RequestParams -> Attribute tag
hxParams = Attr_HxParams

hxPreserve :: Bool -> Attribute tag
hxPreserve = Attr_HxPreserve

hxPreserved :: Attribute tag
hxPreserved = hxPreserve True

hxPrompt :: T.Text -> Attribute tag
hxPrompt = Attr_HxPrompt

hxReplaceURL :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf url Types.PushURLTypes
                )
             => url -> Attribute tag
hxReplaceURL =
  Attr_HxReplaceURL . Types.mkPushURL

-- hx-request

-- hx-sync

hxValidate :: ValidAttribute 'HxValidate tag => Attribute tag
hxValidate = Attr_HxValidate
