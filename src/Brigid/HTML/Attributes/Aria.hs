{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Attributes.Aria
  ( AttributeTags.Aria
  , aria
  , aria_activedescendant
  , aria_atomic
  , aria_autocomplete
  , aria_braillelabel
  , aria_brailleroledescription
  , aria_busy
  -- , aria_checked
  , aria_colcount
  , aria_colindex
  , aria_colindextext
  , aria_colspan
  , aria_controls
  , aria_current
  , aria_describedby
  , aria_description
  , aria_details
  , aria_disabled
  , aria_errormessage
  -- , aria_expanded
  , aria_flowto
  -- , aria_haspopup
  -- , aria_hidden
  -- , aria_invalid
  , aria_keyshortcuts
  , aria_label
  , aria_labelledby
  , aria_level
  , aria_live
  , aria_modal
  , aria_multiline
  , aria_multiselectable
  -- , aria_orientation
  , aria_owns
  , aria_placeholder
  , aria_posinset
  -- , aria_pressed
  , aria_readonly
  , aria_relevant
  , aria_required
  , aria_roledescription
  , aria_rowcount
  , aria_rowindex
  , aria_rowindextext
  , aria_rowspan
  -- , aria_selected
  , aria_setsize
  , aria_sort
  , aria_valuemax
  , aria_valuemin
  , aria_valuenow
  , aria_valuetext
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Integer (Positive)
import Numeric.Natural (Natural)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Tags qualified as AttributeTags
import Brigid.HTML.Types qualified as Types
import Brigid.HTML.Types.Aria qualified as Aria
import Brigid.Types qualified as Types

aria :: Types.RawAria -> Attribute tag
aria =
  Attr_Aria . Aria.mkAria

aria_activedescendant :: Types.Id -> Attribute tag
aria_activedescendant =
  Attr_Aria . Aria.mkAria . Aria.AriaActiveDescendant

aria_atomic :: Bool -> Attribute tag
aria_atomic =
  Attr_Aria . Aria.mkAria . Aria.AriaAtomic

aria_autocomplete :: Types.AriaAutocompleteOption -> Attribute tag
aria_autocomplete =
  Attr_Aria . Aria.mkAria . Aria.AriaAutocomplete

aria_braillelabel :: T.Text -> Attribute tag
aria_braillelabel =
  Attr_Aria . Aria.mkAria . Aria.AriaBrailleLabel

aria_brailleroledescription :: T.Text -> Attribute tag
aria_brailleroledescription =
  Attr_Aria . Aria.mkAria . Aria.AriaBrailleRoleDescription

aria_busy :: Bool -> Attribute tag
aria_busy =
  Attr_Aria . Aria.mkAria . Aria.AriaBusy

-- aria_checked

aria_colcount :: Natural -> Attribute tag
aria_colcount =
  Attr_Aria . Aria.mkAria . Aria.AriaColCount

aria_colindex :: Positive -> Attribute tag
aria_colindex =
  Attr_Aria . Aria.mkAria . Aria.AriaColIndex

aria_colindextext :: T.Text -> Attribute tag
aria_colindextext =
  Attr_Aria . Aria.mkAria . Aria.AriaColIndexText

aria_colspan :: Positive -> Attribute tag
aria_colspan =
  Attr_Aria . Aria.mkAria . Aria.AriaColspan

aria_controls :: [Types.Id] -> Attribute tag
aria_controls =
  Attr_Aria . Aria.mkAria . Aria.AriaControls

aria_current :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf aria Types.AriaCurrentTypes
                )
             => aria -> Attribute tag
aria_current =
  Attr_Aria . Aria.mkAria . Aria.AriaCurrent . Shrubbery.unify

aria_describedby :: [Types.Id] -> Attribute tag
aria_describedby =
  Attr_Aria . Aria.mkAria . Aria.AriaDescribedBy

aria_description :: T.Text -> Attribute tag
aria_description =
  Attr_Aria . Aria.mkAria . Aria.AriaDescription

aria_details :: [Types.Id] -> Attribute tag
aria_details =
  Attr_Aria . Aria.mkAria . Aria.AriaDetails

aria_disabled :: Bool -> Attribute tag
aria_disabled =
  Attr_Aria . Aria.mkAria . Aria.AriaDisabled

aria_errormessage :: NEL.NonEmpty Types.Id -> Attribute tag
aria_errormessage =
  Attr_Aria . Aria.mkAria . Aria.AriaErrorMessage

-- aria_expanded

aria_flowto :: [Types.Id] -> Attribute tag
aria_flowto =
  Attr_Aria . Aria.mkAria . Aria.AriaFlowTo

-- aria_haspopup

-- aria_hidden

-- aria_invalid

aria_keyshortcuts :: T.Text -> Attribute tag
aria_keyshortcuts =
  Attr_Aria . Aria.mkAria . Aria.AriaKeyShortcuts

aria_label :: T.Text -> Attribute tag
aria_label =
  Attr_Aria . Aria.mkAria . Aria.AriaLabel

aria_labelledby :: NEL.NonEmpty Types.Id -> Attribute tag
aria_labelledby =
  Attr_Aria . Aria.mkAria . Aria.AriaLabelledBy

aria_level :: Positive -> Attribute tag
aria_level =
  Attr_Aria . Aria.mkAria . Aria.AriaLevel

aria_live :: Types.AriaLiveOption -> Attribute tag
aria_live =
  Attr_Aria . Aria.mkAria . Aria.AriaLive

aria_modal :: Bool -> Attribute tag
aria_modal =
  Attr_Aria . Aria.mkAria . Aria.AriaModal

aria_multiline :: Bool -> Attribute tag
aria_multiline =
  Attr_Aria . Aria.mkAria . Aria.AriaMultiline

aria_multiselectable :: Bool -> Attribute tag
aria_multiselectable =
  Attr_Aria . Aria.mkAria . Aria.AriaMultiselectable

-- aria_orientation

aria_owns :: [Types.Id] -> Attribute tag
aria_owns =
  Attr_Aria . Aria.mkAria . Aria.AriaOwns

aria_placeholder :: T.Text -> Attribute tag
aria_placeholder =
  Attr_Aria . Aria.mkAria . Aria.AriaPlaceholder

aria_posinset :: Positive -> Attribute tag
aria_posinset =
  Attr_Aria . Aria.mkAria . Aria.AriaPosInSet

-- aria_pressed

aria_readonly :: Bool -> Attribute tag
aria_readonly =
  Attr_Aria . Aria.mkAria . Aria.AriaReadOnly

aria_relevant :: Types.AriaRelevantOption -> Attribute tag
aria_relevant =
  Attr_Aria . Aria.mkAria . Aria.AriaRelevant

aria_required :: Bool -> Attribute tag
aria_required =
  Attr_Aria . Aria.mkAria . Aria.AriaRequired

aria_roledescription :: NET.NonEmptyText -> Attribute tag
aria_roledescription =
  Attr_Aria . Aria.mkAria . Aria.AriaRoleDescription

aria_rowcount :: Integer -> Attribute tag
aria_rowcount =
  Attr_Aria . Aria.mkAria . Aria.AriaRowCount

aria_rowindex :: Positive -> Attribute tag
aria_rowindex =
  Attr_Aria . Aria.mkAria . Aria.AriaRowIndex

aria_rowindextext :: T.Text -> Attribute tag
aria_rowindextext =
  Attr_Aria . Aria.mkAria . Aria.AriaRowIndexText

aria_rowspan :: Natural -> Attribute tag
aria_rowspan =
  Attr_Aria . Aria.mkAria . Aria.AriaRowspan

-- aria_selected

aria_setsize :: Integer -> Attribute tag
aria_setsize =
  Attr_Aria . Aria.mkAria . Aria.AriaSetSize

aria_sort :: Types.AriaSortOption -> Attribute tag
aria_sort =
  Attr_Aria . Aria.mkAria . Aria.AriaSort

aria_valuemax :: Types.Number -> Attribute tag
aria_valuemax =
  Attr_Aria . Aria.mkAria . Aria.AriaValueMax

aria_valuemin :: Types.Number -> Attribute tag
aria_valuemin =
  Attr_Aria . Aria.mkAria . Aria.AriaValueMin

aria_valuenow :: Types.Number -> Attribute tag
aria_valuenow =
  Attr_Aria . Aria.mkAria . Aria.AriaValueNow

aria_valuetext :: T.Text -> Attribute tag
aria_valuetext =
  Attr_Aria . Aria.mkAria . Aria.AriaValueText
