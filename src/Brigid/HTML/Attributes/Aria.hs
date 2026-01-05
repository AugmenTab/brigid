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
  , aria_checked
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
  , aria_expanded
  , aria_flowto
  , aria_haspopup
  , aria_hidden
  , aria_invalid
  , aria_keyshortcuts
  , aria_label
  , aria_labelledby
  , aria_level
  , aria_live
  , aria_modal
  , aria_multiline
  , aria_multiselectable
  , aria_orientation
  , aria_owns
  , aria_placeholder
  , aria_posinset
  , aria_pressed
  , aria_readonly
  , aria_relevant
  , aria_required
  , aria_roledescription
  , aria_rowcount
  , aria_rowindex
  , aria_rowindextext
  , aria_rowspan
  , aria_selected
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

aria :: NET.NonEmptyText -> T.Text -> Attribute tag
aria name =
  Attr_Aria
    . Aria.Aria
    . Shrubbery.unifyTaggedUnion @"raw"
    . Aria.RawAria name

aria_activedescendant :: Types.Id -> Attribute tag
aria_activedescendant =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"activedescendant"

aria_atomic :: Bool -> Attribute tag
aria_atomic =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"atomic"

aria_autocomplete :: Types.AriaAutocompleteOption -> Attribute tag
aria_autocomplete =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"autocomplete"

aria_braillelabel :: T.Text -> Attribute tag
aria_braillelabel =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"braillelabel"

aria_brailleroledescription :: T.Text -> Attribute tag
aria_brailleroledescription =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"brailleroledescription"

aria_busy :: Bool -> Attribute tag
aria_busy =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"busy"

aria_checked :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf aria Types.AriaMixedBoolTypes
                )
             => aria -> Attribute tag
aria_checked =
  Attr_Aria
    . Aria.Aria
    . Shrubbery.unifyTaggedUnion @"checked"
    . Aria.AriaMixedBool
    . Shrubbery.unify

aria_colcount :: Natural -> Attribute tag
aria_colcount =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"colcount"

aria_colindex :: Positive -> Attribute tag
aria_colindex =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"colindex"

aria_colindextext :: T.Text -> Attribute tag
aria_colindextext =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"colindextext"

aria_colspan :: Positive -> Attribute tag
aria_colspan =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"colspan"

aria_controls :: [Types.Id] -> Attribute tag
aria_controls =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"controls"

aria_current :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf aria Types.AriaCurrentTypes
                )
             => aria -> Attribute tag
aria_current =
  Attr_Aria
    . Aria.Aria
    . Shrubbery.unifyTaggedUnion @"current"
    . Aria.AriaCurrent
    . Shrubbery.unify

aria_describedby :: [Types.Id] -> Attribute tag
aria_describedby =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"describedby"

aria_description :: T.Text -> Attribute tag
aria_description =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"description"

aria_details :: [Types.Id] -> Attribute tag
aria_details =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"details"

aria_disabled :: Bool -> Attribute tag
aria_disabled =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"disabled"

aria_errormessage :: NEL.NonEmpty Types.Id -> Attribute tag
aria_errormessage =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"errormessage"

aria_expanded :: Bool -> Attribute tag
aria_expanded =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"expanded"

aria_flowto :: [Types.Id] -> Attribute tag
aria_flowto =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"flowto"

aria_haspopup :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf aria Types.AriaHasPopupTypes
                 )
              => aria -> Attribute tag
aria_haspopup =
  Attr_Aria
    . Aria.Aria
    . Shrubbery.unifyTaggedUnion @"haspopup"
    . Aria.AriaHasPopup
    . Shrubbery.unify

aria_hidden :: Bool -> Attribute tag
aria_hidden =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"hidden"

aria_invalid :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf aria Types.AriaInvalidTypes
                )
             => aria -> Attribute tag
aria_invalid =
  Attr_Aria
    . Aria.Aria
    . Shrubbery.unifyTaggedUnion @"invalid"
    . Aria.AriaInvalid
    . Shrubbery.unify

aria_keyshortcuts :: T.Text -> Attribute tag
aria_keyshortcuts =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"keyshortcuts"

aria_label :: T.Text -> Attribute tag
aria_label =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"label"

aria_labelledby :: NEL.NonEmpty Types.Id -> Attribute tag
aria_labelledby =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"labelledby"

aria_level :: Positive -> Attribute tag
aria_level =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"level"

aria_live :: Types.AriaLiveOption -> Attribute tag
aria_live =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"live"

aria_modal :: Bool -> Attribute tag
aria_modal =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"modal"

aria_multiline :: Bool -> Attribute tag
aria_multiline =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"multiline"

aria_multiselectable :: Bool -> Attribute tag
aria_multiselectable =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"multiselectable"

aria_orientation :: Types.Orientation -> Attribute tag
aria_orientation =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"orientation"

aria_owns :: [Types.Id] -> Attribute tag
aria_owns =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"owns"

aria_placeholder :: T.Text -> Attribute tag
aria_placeholder =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"placeholder"

aria_posinset :: Positive -> Attribute tag
aria_posinset =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"posinset"

aria_pressed :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf aria Types.AriaMixedBoolTypes
                )
             => aria -> Attribute tag
aria_pressed =
  Attr_Aria
    . Aria.Aria
    . Shrubbery.unifyTaggedUnion @"pressed"
    . Aria.AriaMixedBool
    . Shrubbery.unify

aria_readonly :: Bool -> Attribute tag
aria_readonly =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"readonly"

aria_relevant :: Types.AriaRelevantOption -> Attribute tag
aria_relevant =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"relevant"

aria_required :: Bool -> Attribute tag
aria_required =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"required"

aria_roledescription :: NET.NonEmptyText -> Attribute tag
aria_roledescription =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"roledescription"

aria_rowcount :: Integer -> Attribute tag
aria_rowcount =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"rowcount"

aria_rowindex :: Positive -> Attribute tag
aria_rowindex =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"rowindex"

aria_rowindextext :: T.Text -> Attribute tag
aria_rowindextext =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"rowindextext"

aria_rowspan :: Natural -> Attribute tag
aria_rowspan =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"rowspan"

aria_selected :: Bool -> Attribute tag
aria_selected =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"selected"

aria_setsize :: Integer -> Attribute tag
aria_setsize =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"setsize"

aria_sort :: Types.AriaSortOption -> Attribute tag
aria_sort =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"sort"

aria_valuemax :: Types.Number -> Attribute tag
aria_valuemax =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"valuemax"

aria_valuemin :: Types.Number -> Attribute tag
aria_valuemin =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"valuemin"

aria_valuenow :: Types.Number -> Attribute tag
aria_valuenow =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"valuenow"

aria_valuetext :: T.Text -> Attribute tag
aria_valuetext =
  Attr_Aria . Aria.Aria . Shrubbery.unifyTaggedUnion @"valuetext"
