{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Aria
  ( Aria
  , unAria
  , AriaTypes
  , mkAria
  , ariaAttributeToBytes
  , ariaAttributeToText
  , ariaValueToBytes
  , ariaValueToText
  , AriaActiveDescendant (..)
  , AriaAtomic (..)
  , AriaAutocomplete (..)
  , AriaBrailleLabel (..)
  , AriaBrailleRoleDescription (..)
  , AriaBusy (..)
  -- , AriaChecked (..)
  , AriaColCount (..)
  , AriaColIndex (..)
  , AriaColIndexText (..)
  , AriaColspan (..)
  , AriaControls (..)
  , AriaCurrent (..)
  , AriaCurrentTypes
  , AriaDescribedBy (..)
  , AriaDescription (..)
  , AriaDetails (..)
  , AriaDisabled (..)
  , AriaErrorMessage (..)
  -- , AriaExpanded (..)
  , AriaFlowTo (..)
  -- , AriaHasPopup (..)
  -- , AriaHidden (..)
  -- , AriaInvalid (..)
  , AriaKeyShortcuts (..)
  , AriaLabel (..)
  , AriaLabelledBy (..)
  , AriaLevel (..)
  , AriaLive (..)
  , AriaModal (..)
  , AriaMultiline (..)
  , AriaMultiselectable (..)
  -- , AriaOrientation (..)
  , AriaOwns (..)
  , AriaPlaceholder (..)
  , AriaPosInSet (..)
  -- , AriaPressed (..)
  , AriaReadOnly (..)
  , AriaRelevant (..)
  , AriaRequired (..)
  , AriaRoleDescription (..)
  , AriaRowCount (..)
  , AriaRowIndex (..)
  , AriaRowIndexText (..)
  , AriaRowspan (..)
  -- , AriaSelected (..)
  , AriaSetSize (..)
  , AriaSort (..)
  , AriaValueMax (..)
  , AriaValueMin (..)
  , AriaValueNow (..)
  , AriaValueText (..)
  , RawAria (..)
  , AriaPage (AriaPage)
  , AriaStep (AriaStep)
  , AriaLocation (AriaLocation)
  , AriaDate (AriaDate)
  , AriaTime (AriaTime)
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Integer (Positive)
import Numeric.Natural (Natural)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.AriaOption qualified as Option
import Brigid.HTML.Types.Number (Number)
import Brigid.Internal.Render qualified as Render
import Brigid.Types qualified as Types

newtype Aria =
  Aria
    { unAria :: Shrubbery.Union AriaTypes
    } deriving (Eq, Show)

type AriaTypes =
  [ AriaActiveDescendant
  , AriaAtomic
  , AriaAutocomplete
  , AriaBrailleLabel
  , AriaBrailleRoleDescription
  , AriaBusy
  -- , AriaChecked
  , AriaColCount
  , AriaColIndex
  , AriaColIndexText
  , AriaColspan
  , AriaControls
  , AriaCurrent
  , AriaDescribedBy
  , AriaDescription
  , AriaDetails
  , AriaDisabled
  , AriaErrorMessage
  -- , AriaExpanded
  , AriaFlowTo
  -- , AriaHasPopup
  -- , AriaHidden
  -- , AriaInvalid
  , AriaKeyShortcuts
  , AriaLabel
  , AriaLabelledBy
  , AriaLevel
  , AriaLive
  , AriaModal
  , AriaMultiline
  , AriaMultiselectable
  -- , AriaOrientation
  , AriaOwns
  , AriaPlaceholder
  , AriaPosInSet
  -- , AriaPressed
  , AriaReadOnly
  , AriaRelevant
  , AriaRequired
  , AriaRoleDescription
  , AriaRowCount
  , AriaRowIndex
  , AriaRowIndexText
  , AriaRowspan
  -- , AriaSelected
  , AriaSetSize
  , AriaSort
  , AriaValueMax
  , AriaValueMin
  , AriaValueNow
  , AriaValueText
  , RawAria
  ]

mkAria :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf aria AriaTypes)
       => aria -> Aria
mkAria =
  Aria . Shrubbery.unify

ariaAttributeToBytes :: Aria -> LBS.ByteString
ariaAttributeToBytes (Aria aria) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AriaActiveDescendant (const "aria-activedescendant")
      . Shrubbery.branch @AriaAtomic (const "aria-atomic")
      . Shrubbery.branch @AriaAutocomplete (const "aria-autocomplete")
      . Shrubbery.branch @AriaBrailleLabel (const "aria-braillelabel")
      . Shrubbery.branch @AriaBrailleRoleDescription (const "aria-brailleroledescription")
      . Shrubbery.branch @AriaBusy (const "aria-busy")
      -- . Shrubbery.branch @AriaChecked (const "aria-checked")
      . Shrubbery.branch @AriaColCount (const "aria-colcount")
      . Shrubbery.branch @AriaColIndex (const "aria-colindex")
      . Shrubbery.branch @AriaColIndexText (const "aria-colindextext")
      . Shrubbery.branch @AriaColspan (const "aria-colspan")
      . Shrubbery.branch @AriaControls (const "aria-controls")
      . Shrubbery.branch @AriaCurrent (const "aria-current")
      . Shrubbery.branch @AriaDescribedBy (const "aria-describedby")
      . Shrubbery.branch @AriaDescription (const "aria-description")
      . Shrubbery.branch @AriaDetails (const "aria-details")
      . Shrubbery.branch @AriaDisabled (const "aria-disabled")
      . Shrubbery.branch @AriaErrorMessage (const "aria-errormessage")
      -- . Shrubbery.branch @AriaExpanded (const "aria-expanded")
      . Shrubbery.branch @AriaFlowTo (const "aria-flowto")
      -- . Shrubbery.branch @AriaHasPopup (const "aria-haspopup")
      -- . Shrubbery.branch @AriaHidden (const "aria-hidden")
      -- . Shrubbery.branch @AriaInvalid (const "aria-invalid")
      . Shrubbery.branch @AriaKeyShortcuts (const "aria-keyshortcuts")
      . Shrubbery.branch @AriaLabel (const "aria-label")
      . Shrubbery.branch @AriaLabelledBy (const "aria-labelledby")
      . Shrubbery.branch @AriaLevel (const "aria-level")
      . Shrubbery.branch @AriaLive (const "aria-live")
      . Shrubbery.branch @AriaModal (const "aria-modal")
      . Shrubbery.branch @AriaMultiline (const "aria-multiline")
      . Shrubbery.branch @AriaMultiselectable (const "aria-multiselectable")
      -- . Shrubbery.branch @AriaOrientation (const "aria-orientation")
      . Shrubbery.branch @AriaOwns (const "aria-owns")
      . Shrubbery.branch @AriaPlaceholder (const "aria-placeholder")
      . Shrubbery.branch @AriaPosInSet (const "aria-posinset")
      -- . Shrubbery.branch @AriaPressed (const "aria-pressed")
      . Shrubbery.branch @AriaReadOnly (const "aria-readonly")
      . Shrubbery.branch @AriaRelevant (const "aria-relevant")
      . Shrubbery.branch @AriaRequired (const "aria-required")
      . Shrubbery.branch @AriaRoleDescription (const "aria-roledescription")
      . Shrubbery.branch @AriaRowCount (const "aria-rowcount")
      . Shrubbery.branch @AriaRowIndex (const "aria-rowindex")
      . Shrubbery.branch @AriaRowIndexText (const "aria-rowindextext")
      . Shrubbery.branch @AriaRowspan (const "aria-rowspan")
      -- . Shrubbery.branch @AriaSelected (const "aria-selected")
      . Shrubbery.branch @AriaSetSize (const "aria-setsize")
      . Shrubbery.branch @AriaSort (const "aria-sort")
      . Shrubbery.branch @AriaValueMax (const "aria-valuemax")
      . Shrubbery.branch @AriaValueMin (const "aria-valuemin")
      . Shrubbery.branch @AriaValueNow (const "aria-valuenow")
      . Shrubbery.branch @AriaValueText (const "aria-valuetext")
      . Shrubbery.branch @RawAria (("aria-" <>) . Render.textToLazyBytes . NET.toText . rawAriaAttribute)
      $ Shrubbery.branchEnd
  ) aria

ariaAttributeToText :: Aria -> T.Text
ariaAttributeToText (Aria aria) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AriaActiveDescendant (const "aria-activedescendant")
      . Shrubbery.branch @AriaAtomic (const "aria-atomic")
      . Shrubbery.branch @AriaAutocomplete (const "aria-autocomplete")
      . Shrubbery.branch @AriaBrailleLabel (const "aria-braillelabel")
      . Shrubbery.branch @AriaBrailleRoleDescription (const "aria-brailleroledescription")
      . Shrubbery.branch @AriaBusy (const "aria-busy")
      -- . Shrubbery.branch @AriaChecked (const "aria-checked")
      . Shrubbery.branch @AriaColCount (const "aria-colcount")
      . Shrubbery.branch @AriaColIndex (const "aria-colindex")
      . Shrubbery.branch @AriaColIndexText (const "aria-colindextext")
      . Shrubbery.branch @AriaColspan (const "aria-colspan")
      . Shrubbery.branch @AriaControls (const "aria-controls")
      . Shrubbery.branch @AriaCurrent (const "aria-current")
      . Shrubbery.branch @AriaDescribedBy (const "aria-describedby")
      . Shrubbery.branch @AriaDescription (const "aria-description")
      . Shrubbery.branch @AriaDetails (const "aria-details")
      . Shrubbery.branch @AriaDisabled (const "aria-disabled")
      . Shrubbery.branch @AriaErrorMessage (const "aria-errormessage")
      -- . Shrubbery.branch @AriaExpanded (const "aria-expanded")
      . Shrubbery.branch @AriaFlowTo (const "aria-flowto")
      -- . Shrubbery.branch @AriaHasPopup (const "aria-haspopup")
      -- . Shrubbery.branch @AriaHidden (const "aria-hidden")
      -- . Shrubbery.branch @AriaInvalid (const "aria-invalid")
      . Shrubbery.branch @AriaKeyShortcuts (const "aria-keyshortcuts")
      . Shrubbery.branch @AriaLabel (const "aria-label")
      . Shrubbery.branch @AriaLabelledBy (const "aria-labelledby")
      . Shrubbery.branch @AriaLevel (const "aria-level")
      . Shrubbery.branch @AriaLive (const "aria-live")
      . Shrubbery.branch @AriaModal (const "aria-modal")
      . Shrubbery.branch @AriaMultiline (const "aria-multiline")
      . Shrubbery.branch @AriaMultiselectable (const "aria-multiselectable")
      -- . Shrubbery.branch @AriaOrientation (const "aria-orientation")
      . Shrubbery.branch @AriaOwns (const "aria-owns")
      . Shrubbery.branch @AriaPlaceholder (const "aria-placeholder")
      . Shrubbery.branch @AriaPosInSet (const "aria-posinset")
      -- . Shrubbery.branch @AriaPressed (const "aria-pressed")
      . Shrubbery.branch @AriaReadOnly (const "aria-readonly")
      . Shrubbery.branch @AriaRelevant (const "aria-relevant")
      . Shrubbery.branch @AriaRequired (const "aria-required")
      . Shrubbery.branch @AriaRoleDescription (const "aria-roledescription")
      . Shrubbery.branch @AriaRowCount (const "aria-rowcount")
      . Shrubbery.branch @AriaRowIndex (const "aria-rowindex")
      . Shrubbery.branch @AriaRowIndexText (const "aria-rowindextext")
      . Shrubbery.branch @AriaRowspan (const "aria-rowspan")
      -- . Shrubbery.branch @AriaSelected (const "aria-selected")
      . Shrubbery.branch @AriaSetSize (const "aria-setsize")
      . Shrubbery.branch @AriaSort (const "aria-sort")
      . Shrubbery.branch @AriaValueMax (const "aria-valuemax")
      . Shrubbery.branch @AriaValueMin (const "aria-valuemin")
      . Shrubbery.branch @AriaValueNow (const "aria-valuenow")
      . Shrubbery.branch @AriaValueText (const "aria-valuetext")
      . Shrubbery.branch @RawAria (("aria-" <>) . NET.toText . rawAriaAttribute)
      $ Shrubbery.branchEnd
  ) aria

ariaValueToBytes :: Aria -> LBS.ByteString
ariaValueToBytes (Aria aria) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AriaActiveDescendant (Types.idToBytes . unAriaActiveDescendant)
      . Shrubbery.branch @AriaAtomic (Render.enumBoolToBytes . unAriaAtomic)
      . Shrubbery.branch @AriaAutocomplete (Option.ariaAutocompleteOptionToBytes . unAriaAutocomplete)
      . Shrubbery.branch @AriaBrailleLabel (Render.textToLazyBytes . unAriaBrailleLabel)
      . Shrubbery.branch @AriaBrailleRoleDescription (Render.textToLazyBytes . unAriaBrailleRoleDescription)
      . Shrubbery.branch @AriaBusy (Render.enumBoolToBytes . unAriaBusy)
      -- . Shrubbery.branch @AriaChecked _
      . Shrubbery.branch @AriaColCount (Render.showBytes . unAriaColCount)
      . Shrubbery.branch @AriaColIndex (Render.showBytes . unAriaColIndex)
      . Shrubbery.branch @AriaColIndexText (Render.textToLazyBytes . unAriaColIndexText)
      . Shrubbery.branch @AriaColspan (Render.showBytes . unAriaColspan)
      . Shrubbery.branch @AriaControls (Render.foldToBytesWithSeparator Types.idToBytes " " . unAriaControls)
      . Shrubbery.branch @AriaCurrent ariaCurrentToBytes
      . Shrubbery.branch @AriaDescribedBy (Render.foldToBytesWithSeparator Types.idToBytes " " . unAriaDescribedBy)
      . Shrubbery.branch @AriaDescription (Render.textToLazyBytes . unAriaDescription)
      . Shrubbery.branch @AriaDetails (Render.foldToBytesWithSeparator Types.idToBytes " " . unAriaDetails)
      . Shrubbery.branch @AriaDisabled (Render.enumBoolToBytes . unAriaDisabled)
      . Shrubbery.branch @AriaErrorMessage (Render.foldToBytesWithSeparator Types.idToBytes " " . NEL.toList . unAriaErrorMessage)
      -- . Shrubbery.branch @AriaExpanded _
      . Shrubbery.branch @AriaFlowTo (Render.foldToBytesWithSeparator Types.idToBytes " " . unAriaFlowTo)
      -- . Shrubbery.branch @AriaHasPopup _
      -- . Shrubbery.branch @AriaHidden _
      -- . Shrubbery.branch @AriaInvalid _
      . Shrubbery.branch @AriaKeyShortcuts (Render.textToLazyBytes . unAriaKeyShortcuts)
      . Shrubbery.branch @AriaLabel (Render.textToLazyBytes . unAriaLabel)
      . Shrubbery.branch @AriaLabelledBy (Render.foldToBytesWithSeparator Types.idToBytes " " . NEL.toList . unAriaLabelledBy)
      . Shrubbery.branch @AriaLevel (Render.showBytes . unAriaLevel)
      . Shrubbery.branch @AriaLive (Option.ariaLiveOptionToBytes . unAriaLive)
      . Shrubbery.branch @AriaModal (Render.enumBoolToBytes . unAriaModal)
      . Shrubbery.branch @AriaMultiline (Render.enumBoolToBytes . unAriaMultiline)
      . Shrubbery.branch @AriaMultiselectable (Render.enumBoolToBytes . unAriaMultiselectable)
      -- . Shrubbery.branch @AriaOrientation _
      . Shrubbery.branch @AriaOwns (Render.foldToBytesWithSeparator Types.idToBytes " " . unAriaOwns)
      . Shrubbery.branch @AriaPlaceholder (Render.textToLazyBytes . unAriaPlaceholder)
      . Shrubbery.branch @AriaPosInSet (Render.showBytes . unAriaPosInSet)
      -- . Shrubbery.branch @AriaPressed _
      . Shrubbery.branch @AriaReadOnly (Render.showBytes . unAriaReadOnly)
      . Shrubbery.branch @AriaRelevant (Option.ariaRelevantOptionToBytes . unAriaRelevant)
      . Shrubbery.branch @AriaRequired (Render.enumBoolToBytes . unAriaRequired)
      . Shrubbery.branch @AriaRoleDescription (Render.textToLazyBytes . NET.toText . unAriaRoleDescription)
      . Shrubbery.branch @AriaRowCount (Render.showBytes . unAriaRowCount)
      . Shrubbery.branch @AriaRowIndex (Render.showBytes . unAriaRowIndex)
      . Shrubbery.branch @AriaRowIndexText (Render.textToLazyBytes . unAriaRowIndexText)
      . Shrubbery.branch @AriaRowspan (Render.showBytes . unAriaRowspan)
      -- . Shrubbery.branch @AriaSelected _
      . Shrubbery.branch @AriaSetSize (Render.showBytes . unAriaSetSize)
      . Shrubbery.branch @AriaSort (Option.ariaSortOptionToBytes . unAriaSort)
      . Shrubbery.branch @AriaValueMax (Render.showBytes . unAriaValueMax)
      . Shrubbery.branch @AriaValueMin (Render.showBytes . unAriaValueMin)
      . Shrubbery.branch @AriaValueNow (Render.showBytes . unAriaValueNow)
      . Shrubbery.branch @AriaValueText (Render.textToLazyBytes . unAriaValueText)
      . Shrubbery.branch @RawAria (Render.textToLazyBytes . rawAriaValue)
      $ Shrubbery.branchEnd
  ) aria

ariaValueToText :: Aria -> T.Text
ariaValueToText (Aria aria) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AriaActiveDescendant (Types.idToText . unAriaActiveDescendant)
      . Shrubbery.branch @AriaAtomic (Render.enumBoolToText . unAriaAtomic)
      . Shrubbery.branch @AriaAutocomplete (Option.ariaAutocompleteOptionToText . unAriaAutocomplete)
      . Shrubbery.branch @AriaBrailleLabel unAriaBrailleLabel
      . Shrubbery.branch @AriaBrailleRoleDescription unAriaBrailleRoleDescription
      . Shrubbery.branch @AriaBusy (Render.enumBoolToText . unAriaBusy)
      -- . Shrubbery.branch @AriaChecked _
      . Shrubbery.branch @AriaColCount (Render.showText . unAriaColCount)
      . Shrubbery.branch @AriaColIndex (Render.showText . unAriaColIndex)
      . Shrubbery.branch @AriaColIndexText unAriaColIndexText
      . Shrubbery.branch @AriaColspan (Render.showText . unAriaColspan)
      . Shrubbery.branch @AriaControls (Render.foldToTextWithSeparator Types.idToText " " . unAriaControls)
      . Shrubbery.branch @AriaCurrent ariaCurrentToText
      . Shrubbery.branch @AriaDescribedBy (Render.foldToTextWithSeparator Types.idToText " " . unAriaDescribedBy)
      . Shrubbery.branch @AriaDescription unAriaDescription
      . Shrubbery.branch @AriaDetails (Render.foldToTextWithSeparator Types.idToText " " . unAriaDetails)
      . Shrubbery.branch @AriaDisabled (Render.enumBoolToText . unAriaDisabled)
      . Shrubbery.branch @AriaErrorMessage (Render.foldToTextWithSeparator Types.idToText " " . NEL.toList . unAriaErrorMessage)
      -- . Shrubbery.branch @AriaExpanded _
      . Shrubbery.branch @AriaFlowTo (Render.foldToTextWithSeparator Types.idToText " " . unAriaFlowTo)
      -- . Shrubbery.branch @AriaHasPopup _
      -- . Shrubbery.branch @AriaHidden _
      -- . Shrubbery.branch @AriaInvalid _
      . Shrubbery.branch @AriaKeyShortcuts unAriaKeyShortcuts
      . Shrubbery.branch @AriaLabel unAriaLabel
      . Shrubbery.branch @AriaLabelledBy (Render.foldToTextWithSeparator Types.idToText " " . NEL.toList . unAriaLabelledBy)
      . Shrubbery.branch @AriaLevel (Render.showText . unAriaLevel)
      . Shrubbery.branch @AriaLive (Option.ariaLiveOptionToText . unAriaLive)
      . Shrubbery.branch @AriaModal (Render.enumBoolToText . unAriaModal)
      . Shrubbery.branch @AriaMultiline (Render.enumBoolToText . unAriaMultiline)
      . Shrubbery.branch @AriaMultiselectable (Render.enumBoolToText . unAriaMultiselectable)
      -- . Shrubbery.branch @AriaOrientation _
      . Shrubbery.branch @AriaOwns (Render.foldToTextWithSeparator Types.idToText " " . unAriaOwns)
      . Shrubbery.branch @AriaPlaceholder unAriaPlaceholder
      . Shrubbery.branch @AriaPosInSet (Render.showText . unAriaPosInSet)
      -- . Shrubbery.branch @AriaPressed _
      . Shrubbery.branch @AriaReadOnly (Render.showText . unAriaReadOnly)
      . Shrubbery.branch @AriaRelevant (Option.ariaRelevantOptionToText . unAriaRelevant)
      . Shrubbery.branch @AriaRequired (Render.enumBoolToText . unAriaRequired)
      . Shrubbery.branch @AriaRoleDescription (NET.toText . unAriaRoleDescription)
      . Shrubbery.branch @AriaRowCount (Render.showText . unAriaRowCount)
      . Shrubbery.branch @AriaRowIndex (Render.showText . unAriaRowIndex)
      . Shrubbery.branch @AriaRowIndexText unAriaRowIndexText
      . Shrubbery.branch @AriaRowspan (Render.showText . unAriaRowspan)
      -- . Shrubbery.branch @AriaSelected _
      . Shrubbery.branch @AriaSetSize (Render.showText . unAriaSetSize)
      . Shrubbery.branch @AriaSort (Option.ariaSortOptionToText . unAriaSort)
      . Shrubbery.branch @AriaValueMax (Render.showText . unAriaValueMax)
      . Shrubbery.branch @AriaValueMin (Render.showText . unAriaValueMin)
      . Shrubbery.branch @AriaValueNow (Render.showText . unAriaValueNow)
      . Shrubbery.branch @AriaValueText unAriaValueText
      . Shrubbery.branch @RawAria rawAriaValue
      $ Shrubbery.branchEnd
  ) aria

newtype AriaActiveDescendant =
  AriaActiveDescendant
    { unAriaActiveDescendant :: Types.Id
    } deriving (Eq, Show)

newtype AriaAtomic =
  AriaAtomic
    { unAriaAtomic :: Bool
    } deriving (Eq, Show)

newtype AriaAutocomplete =
  AriaAutocomplete
    { unAriaAutocomplete :: Option.AriaAutocompleteOption
    } deriving (Eq, Show)

newtype AriaBrailleLabel =
  AriaBrailleLabel
    { unAriaBrailleLabel :: T.Text
    } deriving (Eq, Show)

newtype AriaBrailleRoleDescription =
  AriaBrailleRoleDescription
    { unAriaBrailleRoleDescription :: T.Text -- TODO: Restrict to Braille characters?
    } deriving (Eq, Show)

newtype AriaBusy =
  AriaBusy
    { unAriaBusy :: Bool
    } deriving (Eq, Show)

-- newtype AriaChecked = AriaChecked -- TODO
--   deriving (Eq, Show)

newtype AriaColCount =
  AriaColCount
    { unAriaColCount :: Natural
    } deriving (Eq, Show)

newtype AriaColIndex =
  AriaColIndex
    { unAriaColIndex :: Positive
    } deriving (Eq, Show)

newtype AriaColIndexText =
  AriaColIndexText
    { unAriaColIndexText :: T.Text
    } deriving (Eq, Show)

newtype AriaColspan =
  AriaColspan
    { unAriaColspan :: Positive
    } deriving (Eq, Show)

newtype AriaControls =
  AriaControls
    { unAriaControls :: [Types.Id]
    } deriving (Eq, Show)

newtype AriaCurrent =
  AriaCurrent
    { unAriaCurrent :: Shrubbery.Union AriaCurrentTypes
    } deriving (Eq, Show)

type AriaCurrentTypes =
  [ AriaPage
  , AriaStep
  , AriaLocation
  , AriaDate
  , AriaTime
  , Bool
  ]

ariaCurrentToBytes :: AriaCurrent -> LBS.ByteString
ariaCurrentToBytes (AriaCurrent aria) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AriaPage ariaPageToBytes
      . Shrubbery.branch @AriaStep ariaStepToBytes
      . Shrubbery.branch @AriaLocation ariaLocationToBytes
      . Shrubbery.branch @AriaDate ariaDateToBytes
      . Shrubbery.branch @AriaTime ariaTimeToBytes
      . Shrubbery.branch @Bool Render.enumBoolToBytes
      $ Shrubbery.branchEnd
  ) aria

ariaCurrentToText :: AriaCurrent -> T.Text
ariaCurrentToText (AriaCurrent aria) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AriaPage ariaPageToText
      . Shrubbery.branch @AriaStep ariaStepToText
      . Shrubbery.branch @AriaLocation ariaLocationToText
      . Shrubbery.branch @AriaDate ariaDateToText
      . Shrubbery.branch @AriaTime ariaTimeToText
      . Shrubbery.branch @Bool Render.enumBoolToText
      $ Shrubbery.branchEnd
  ) aria

newtype AriaDescribedBy =
  AriaDescribedBy
    { unAriaDescribedBy :: [Types.Id]
    } deriving (Eq, Show)

newtype AriaDescription =
  AriaDescription
    { unAriaDescription :: T.Text
    } deriving (Eq, Show)

newtype AriaDetails =
  AriaDetails
    { unAriaDetails :: [Types.Id]
    } deriving (Eq, Show)

newtype AriaDisabled =
  AriaDisabled
    { unAriaDisabled :: Bool
    } deriving (Eq, Show)

newtype AriaErrorMessage =
  AriaErrorMessage
    { unAriaErrorMessage :: NEL.NonEmpty Types.Id
    } deriving (Eq, Show)

-- newtype AriaExpanded = AriaExpanded -- TODO
--   deriving (Eq, Show)

newtype AriaFlowTo =
  AriaFlowTo
    { unAriaFlowTo :: [Types.Id]
    } deriving (Eq, Show)

-- newtype AriaHasPopup = AriaHasPopup -- TODO
--   deriving (Eq, Show)

-- newtype AriaHidden = AriaHidden -- TODO
--   deriving (Eq, Show)

-- newtype AriaInvalid = AriaInvalid -- TODO
--   deriving (Eq, Show)

newtype AriaKeyShortcuts =
  AriaKeyShortcuts
    { unAriaKeyShortcuts :: T.Text -- TODO: Add type-safe implementation?
    } deriving (Eq, Show)

newtype AriaLabel =
  AriaLabel
    { unAriaLabel :: T.Text
    } deriving (Eq, Show)

newtype AriaLabelledBy =
  AriaLabelledBy
    { unAriaLabelledBy :: NEL.NonEmpty Types.Id
    } deriving (Eq, Show)

newtype AriaLevel =
  AriaLevel
    { unAriaLevel :: Positive
    } deriving (Eq, Show)

newtype AriaLive =
  AriaLive
    { unAriaLive :: Option.AriaLiveOption
    } deriving (Eq, Show)

newtype AriaModal =
  AriaModal
    { unAriaModal :: Bool
    } deriving (Eq, Show)

newtype AriaMultiline =
  AriaMultiline
    { unAriaMultiline :: Bool
    } deriving (Eq, Show)

newtype AriaMultiselectable =
  AriaMultiselectable
    { unAriaMultiselectable :: Bool
    } deriving (Eq, Show)

-- newtype AriaOrientation = AriaOrientation -- TODO
--   deriving (Eq, Show)

newtype AriaOwns =
  AriaOwns
    { unAriaOwns :: [Types.Id]
    } deriving (Eq, Show)

newtype AriaPlaceholder =
  AriaPlaceholder
    { unAriaPlaceholder :: T.Text
    } deriving (Eq, Show)

newtype AriaPosInSet =
  AriaPosInSet
    { unAriaPosInSet :: Positive
    } deriving (Eq, Show)

-- newtype AriaPressed =
--   AriaPressed
--     { unAriaPressed :: _
--     } deriving (Eq, Show)

newtype AriaReadOnly =
  AriaReadOnly
    { unAriaReadOnly :: Bool
    } deriving (Eq, Show)

newtype AriaRelevant =
  AriaRelevant
    { unAriaRelevant :: Option.AriaRelevantOption
    } deriving (Eq, Show)

newtype AriaRequired =
  AriaRequired
    { unAriaRequired :: Bool
    } deriving (Eq, Show)

newtype AriaRoleDescription =
  AriaRoleDescription
    { unAriaRoleDescription :: NET.NonEmptyText
    } deriving (Eq, Show)

newtype AriaRowCount =
  AriaRowCount
    { unAriaRowCount :: Integer
    } deriving (Eq, Show)

newtype AriaRowIndex =
  AriaRowIndex
    { unAriaRowIndex :: Positive
    } deriving (Eq, Show)

newtype AriaRowIndexText =
  AriaRowIndexText
    { unAriaRowIndexText :: T.Text
    } deriving (Eq, Show)

newtype AriaRowspan =
  AriaRowspan
    { unAriaRowspan :: Natural
    } deriving (Eq, Show)

-- newtype AriaSelected = AriaSelected -- TODO
--   deriving (Eq, Show)

newtype AriaSetSize =
  AriaSetSize
    { unAriaSetSize :: Integer
    } deriving (Eq, Show)

newtype AriaSort =
  AriaSort
    { unAriaSort :: Option.AriaSortOption
    } deriving (Eq, Show)

newtype AriaValueMax =
  AriaValueMax
    { unAriaValueMax :: Number
    } deriving (Eq, Show)

newtype AriaValueMin =
  AriaValueMin
    { unAriaValueMin :: Number
    } deriving (Eq, Show)

newtype AriaValueNow =
  AriaValueNow
    { unAriaValueNow :: Number
    } deriving (Eq, Show)

newtype AriaValueText =
  AriaValueText
    { unAriaValueText :: T.Text
    } deriving (Eq, Show)

data RawAria =
  RawAria
    { rawAriaAttribute :: NET.NonEmptyText
    , rawAriaValue :: T.Text
    } deriving (Eq, Show)

data AriaDate = AriaDate
  deriving (Eq, Show)

ariaDateToBytes :: AriaDate -> LBS.ByteString
ariaDateToBytes AriaDate = "date"

ariaDateToText :: AriaDate -> T.Text
ariaDateToText AriaDate = "date"

data AriaLocation = AriaLocation
  deriving (Eq, Show)

ariaLocationToBytes :: AriaLocation -> LBS.ByteString
ariaLocationToBytes AriaLocation = "location"

ariaLocationToText :: AriaLocation -> T.Text
ariaLocationToText AriaLocation = "location"

data AriaPage = AriaPage
  deriving (Eq, Show)

ariaPageToBytes :: AriaPage -> LBS.ByteString
ariaPageToBytes AriaPage = "page"

ariaPageToText :: AriaPage -> T.Text
ariaPageToText AriaPage = "page"

data AriaStep = AriaStep
  deriving (Eq, Show)

ariaStepToBytes :: AriaStep -> LBS.ByteString
ariaStepToBytes AriaStep = "step"

ariaStepToText :: AriaStep -> T.Text
ariaStepToText AriaStep = "step"

data AriaTime = AriaTime
  deriving (Eq, Show)

ariaTimeToBytes :: AriaTime -> LBS.ByteString
ariaTimeToBytes AriaTime = "time"

ariaTimeToText :: AriaTime -> T.Text
ariaTimeToText AriaTime = "time"
