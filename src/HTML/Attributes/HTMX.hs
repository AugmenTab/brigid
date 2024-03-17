{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Attributes.HTMX
  ( htmx
  , hxGet
  , hxPost
  , hxPushURL
  , hxSelect
  , hxBoost
  , hxConfirm
  , hxDelete
  , hxDisable
  , hxDisabled
  , hxDisinherit
  , hxEncoding
  , hxExt
  , hxHistory
  , hxHistoryElt
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

-- hxOn

hxPushURL :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf url Types.PushURLTypes
             )
          => url -> Attribute tag
hxPushURL =
  Attr_HxPushURL . Types.mkPushURL

hxSelect :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf querySelector Types.QuerySelectorTypes
            )
         => querySelector -> Attribute tag
hxSelect = Attr_HxSelect . Types.mkQuerySelector

-- hx-select-oob

-- hx-swap

-- hx-swap-oob

-- hx-target

-- hx-trigger

-- hx-vals

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

-- hx-disabled-elt

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

-- hx-include

-- hx-indicator

-- hx-params

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
