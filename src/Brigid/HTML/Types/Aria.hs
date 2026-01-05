{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Aria
  ( Aria (Aria)
  , AriaTypes
  , ariaAttributeToBytes
  , ariaAttributeToText
  , ariaValueToBytes
  , ariaValueToText
  , Current.AriaCurrent (..)
  , Current.AriaCurrentTypes
  , Current.CurrentPage (..)
  , Current.CurrentStep (..)
  , Current.CurrentLocation (..)
  , Current.CurrentDate (..)
  , Current.CurrentTime (..)
  , HasPopup.AriaHasPopup (..)
  , HasPopup.AriaHasPopupTypes
  , HasPopup.PopupMenu (..)
  , HasPopup.PopupListbox (..)
  , HasPopup.PopupTree (..)
  , HasPopup.PopupGrid (..)
  , HasPopup.PopupDialog (..)
  , Invalid.AriaInvalid (..)
  , Invalid.AriaInvalidTypes
  , Invalid.InvalidGrammar (..)
  , Invalid.InvalidSpelling (..)
  , MixedBool.AriaMixedBool (..)
  , MixedBool.AriaMixedBoolTypes
  , Option.AriaAutocompleteOption (..)
  , Option.AriaLiveOption (..)
  , Option.AriaRelevantOption (..)
  , Option.AriaSortOption (..)
  , RawAria (..)
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Integer (Positive)
import Numeric.Natural (Natural)
import Shrubbery (type (@=))
import Shrubbery qualified

import Brigid.HTML.Types.Aria.Current qualified as Current
import Brigid.HTML.Types.Aria.HasPopup qualified as HasPopup
import Brigid.HTML.Types.Aria.Invalid qualified as Invalid
import Brigid.HTML.Types.Aria.MixedBool qualified as MixedBool
import Brigid.HTML.Types.Aria.Option qualified as Option
import Brigid.HTML.Types.Number (Number)
import Brigid.HTML.Types.Orientation (Orientation, orientationToBytes, orientationToText)
import Brigid.Internal.Render qualified as Render
import Brigid.Types qualified as Types

newtype Aria = Aria (Shrubbery.TaggedUnion AriaTypes)
  deriving (Eq, Show)

type AriaTypes =
  [ "activedescendant" @= Types.Id
  , "atomic" @= Bool
  , "autocomplete" @= Option.AriaAutocompleteOption
  , "braillelabel" @= T.Text
  , "brailleroledescription" @= T.Text
  , "busy" @= Bool
  , "checked" @= MixedBool.AriaMixedBool
  , "colcount" @= Natural
  , "colindex" @= Positive
  , "colindextext" @= T.Text
  , "colspan" @= Positive
  , "controls" @= [Types.Id]
  , "current" @= Current.AriaCurrent
  , "describedby" @= [Types.Id]
  , "description" @= T.Text
  , "details" @= [Types.Id]
  , "disabled" @= Bool
  , "errormessage" @= NEL.NonEmpty Types.Id
  , "expanded" @= Bool
  , "flowto" @= [Types.Id]
  , "haspopup" @= HasPopup.AriaHasPopup
  , "hidden" @= Bool
  , "invalid" @= Invalid.AriaInvalid
  , "keyshortcuts" @= T.Text -- TODO: Add type-safe implementation?
  , "label" @= T.Text
  , "labelledby" @= NEL.NonEmpty Types.Id
  , "level" @= Positive
  , "live" @= Option.AriaLiveOption
  , "modal" @= Bool
  , "multiline" @= Bool
  , "multiselectable" @= Bool
  , "orientation" @= Orientation
  , "owns" @= [Types.Id]
  , "placeholder" @= T.Text
  , "posinset" @= Positive
  , "pressed" @= MixedBool.AriaMixedBool
  , "readonly" @= Bool
  , "relevant" @= Option.AriaRelevantOption
  , "required" @= Bool
  , "roledescription" @= NET.NonEmptyText
  , "rowcount" @= Integer
  , "rowindex" @= Positive
  , "rowindextext" @= T.Text
  , "rowspan" @= Natural
  , "selected" @= Bool
  , "setsize" @= Integer
  , "sort" @= Option.AriaSortOption
  , "valuemax" @= Number
  , "valuemin" @= Number
  , "valuenow" @= Number
  , "valuetext" @= T.Text
  , "raw" @= RawAria
  ]

ariaAttributeToBytes :: Aria -> LBS.ByteString
ariaAttributeToBytes (Aria aria) =
  ( Shrubbery.dissectTaggedUnion
      . Shrubbery.taggedBranchBuild
      . Shrubbery.taggedBranch @"activedescendant" (const "aria-activedescendant")
      . Shrubbery.taggedBranch @"atomic" (const "aria-atomic")
      . Shrubbery.taggedBranch @"autocomplete" (const "aria-autocomplete")
      . Shrubbery.taggedBranch @"braillelabel" (const "aria-braillelabel")
      . Shrubbery.taggedBranch @"brailleroledescription" (const "aria-brailleroledescription")
      . Shrubbery.taggedBranch @"busy" (const "aria-busy")
      . Shrubbery.taggedBranch @"checked" (const "aria-checked")
      . Shrubbery.taggedBranch @"colcount" (const "aria-colcount")
      . Shrubbery.taggedBranch @"colindex" (const "aria-colindex")
      . Shrubbery.taggedBranch @"colindextext" (const "aria-colindextext")
      . Shrubbery.taggedBranch @"colspan" (const "aria-colspan")
      . Shrubbery.taggedBranch @"controls" (const "aria-controls")
      . Shrubbery.taggedBranch @"current" (const "aria-current")
      . Shrubbery.taggedBranch @"describedby" (const "aria-describedby")
      . Shrubbery.taggedBranch @"description" (const "aria-description")
      . Shrubbery.taggedBranch @"details" (const "aria-details")
      . Shrubbery.taggedBranch @"disabled" (const "aria-disabled")
      . Shrubbery.taggedBranch @"errormessage" (const "aria-errormessage")
      . Shrubbery.taggedBranch @"expanded" (const "aria-expanded")
      . Shrubbery.taggedBranch @"flowto" (const "aria-flowto")
      . Shrubbery.taggedBranch @"haspopup" (const "aria-haspopup")
      . Shrubbery.taggedBranch @"hidden" (const "aria-hidden")
      . Shrubbery.taggedBranch @"invalid" (const "aria-invalid")
      . Shrubbery.taggedBranch @"keyshortcuts" (const "aria-keyshortcuts")
      . Shrubbery.taggedBranch @"label" (const "aria-label")
      . Shrubbery.taggedBranch @"labelledby" (const "aria-labelledby")
      . Shrubbery.taggedBranch @"level" (const "aria-level")
      . Shrubbery.taggedBranch @"live" (const "aria-live")
      . Shrubbery.taggedBranch @"modal" (const "aria-modal")
      . Shrubbery.taggedBranch @"multiline" (const "aria-multiline")
      . Shrubbery.taggedBranch @"multiselectable" (const "aria-multiselectable")
      . Shrubbery.taggedBranch @"orientation" (const "aria-orientation")
      . Shrubbery.taggedBranch @"owns" (const "aria-owns")
      . Shrubbery.taggedBranch @"placeholder" (const "aria-placeholder")
      . Shrubbery.taggedBranch @"posinset" (const "aria-posinset")
      . Shrubbery.taggedBranch @"pressed" (const "aria-pressed")
      . Shrubbery.taggedBranch @"readonly" (const "aria-readonly")
      . Shrubbery.taggedBranch @"relevant" (const "aria-relevant")
      . Shrubbery.taggedBranch @"required" (const "aria-required")
      . Shrubbery.taggedBranch @"roledescription" (const "aria-roledescription")
      . Shrubbery.taggedBranch @"rowcount" (const "aria-rowcount")
      . Shrubbery.taggedBranch @"rowindex" (const "aria-rowindex")
      . Shrubbery.taggedBranch @"rowindextext" (const "aria-rowindextext")
      . Shrubbery.taggedBranch @"rowspan" (const "aria-rowspan")
      . Shrubbery.taggedBranch @"selected" (const "aria-selected")
      . Shrubbery.taggedBranch @"setsize" (const "aria-setsize")
      . Shrubbery.taggedBranch @"sort" (const "aria-sort")
      . Shrubbery.taggedBranch @"valuemax" (const "aria-valuemax")
      . Shrubbery.taggedBranch @"valuemin" (const "aria-valuemin")
      . Shrubbery.taggedBranch @"valuenow" (const "aria-valuenow")
      . Shrubbery.taggedBranch @"valuetext" (const "aria-valuetext")
      . Shrubbery.taggedBranch @"raw" (("aria-" <>) . Render.textToLazyBytes . NET.toText . rawAriaAttribute)
      $ Shrubbery.taggedBranchEnd
  ) aria

ariaAttributeToText :: Aria -> T.Text
ariaAttributeToText (Aria aria) =
  ( Shrubbery.dissectTaggedUnion
      . Shrubbery.taggedBranchBuild
      . Shrubbery.taggedBranch @"activedescendant" (const "aria-activedescendant")
      . Shrubbery.taggedBranch @"atomic" (const "aria-atomic")
      . Shrubbery.taggedBranch @"autocomplete" (const "aria-autocomplete")
      . Shrubbery.taggedBranch @"braillelabel" (const "aria-braillelabel")
      . Shrubbery.taggedBranch @"brailleroledescription" (const "aria-brailleroledescription")
      . Shrubbery.taggedBranch @"busy" (const "aria-busy")
      . Shrubbery.taggedBranch @"checked" (const "aria-checked")
      . Shrubbery.taggedBranch @"colcount" (const "aria-colcount")
      . Shrubbery.taggedBranch @"colindex" (const "aria-colindex")
      . Shrubbery.taggedBranch @"colindextext" (const "aria-colindextext")
      . Shrubbery.taggedBranch @"colspan" (const "aria-colspan")
      . Shrubbery.taggedBranch @"controls" (const "aria-controls")
      . Shrubbery.taggedBranch @"current" (const "aria-current")
      . Shrubbery.taggedBranch @"describedby" (const "aria-describedby")
      . Shrubbery.taggedBranch @"description" (const "aria-description")
      . Shrubbery.taggedBranch @"details" (const "aria-details")
      . Shrubbery.taggedBranch @"disabled" (const "aria-disabled")
      . Shrubbery.taggedBranch @"errormessage" (const "aria-errormessage")
      . Shrubbery.taggedBranch @"expanded" (const "aria-expanded")
      . Shrubbery.taggedBranch @"flowto" (const "aria-flowto")
      . Shrubbery.taggedBranch @"haspopup" (const "aria-haspopup")
      . Shrubbery.taggedBranch @"hidden" (const "aria-hidden")
      . Shrubbery.taggedBranch @"invalid" (const "aria-invalid")
      . Shrubbery.taggedBranch @"keyshortcuts" (const "aria-keyshortcuts")
      . Shrubbery.taggedBranch @"label" (const "aria-label")
      . Shrubbery.taggedBranch @"labelledby" (const "aria-labelledby")
      . Shrubbery.taggedBranch @"level" (const "aria-level")
      . Shrubbery.taggedBranch @"live" (const "aria-live")
      . Shrubbery.taggedBranch @"modal" (const "aria-modal")
      . Shrubbery.taggedBranch @"multiline" (const "aria-multiline")
      . Shrubbery.taggedBranch @"multiselectable" (const "aria-multiselectable")
      . Shrubbery.taggedBranch @"orientation" (const "aria-orientation")
      . Shrubbery.taggedBranch @"owns" (const "aria-owns")
      . Shrubbery.taggedBranch @"placeholder" (const "aria-placeholder")
      . Shrubbery.taggedBranch @"posinset" (const "aria-posinset")
      . Shrubbery.taggedBranch @"pressed" (const "aria-pressed")
      . Shrubbery.taggedBranch @"readonly" (const "aria-readonly")
      . Shrubbery.taggedBranch @"relevant" (const "aria-relevant")
      . Shrubbery.taggedBranch @"required" (const "aria-required")
      . Shrubbery.taggedBranch @"roledescription" (const "aria-roledescription")
      . Shrubbery.taggedBranch @"rowcount" (const "aria-rowcount")
      . Shrubbery.taggedBranch @"rowindex" (const "aria-rowindex")
      . Shrubbery.taggedBranch @"rowindextext" (const "aria-rowindextext")
      . Shrubbery.taggedBranch @"rowspan" (const "aria-rowspan")
      . Shrubbery.taggedBranch @"selected" (const "aria-selected")
      . Shrubbery.taggedBranch @"setsize" (const "aria-setsize")
      . Shrubbery.taggedBranch @"sort" (const "aria-sort")
      . Shrubbery.taggedBranch @"valuemax" (const "aria-valuemax")
      . Shrubbery.taggedBranch @"valuemin" (const "aria-valuemin")
      . Shrubbery.taggedBranch @"valuenow" (const "aria-valuenow")
      . Shrubbery.taggedBranch @"valuetext" (const "aria-valuetext")
      . Shrubbery.taggedBranch @"raw" (("aria-" <>) . NET.toText . rawAriaAttribute)
      $ Shrubbery.taggedBranchEnd
  ) aria

ariaValueToBytes :: Aria -> LBS.ByteString
ariaValueToBytes (Aria aria) =
  ( Shrubbery.dissectTaggedUnion
      . Shrubbery.taggedBranchBuild
      . Shrubbery.taggedBranch @"activedescendant" Types.idToBytes
      . Shrubbery.taggedBranch @"atomic" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"autocomplete" Option.ariaAutocompleteOptionToBytes
      . Shrubbery.taggedBranch @"braillelabel" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"brailleroledescription" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"busy" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"checked" MixedBool.ariaMixedBoolToBytes
      . Shrubbery.taggedBranch @"colcount" Render.showBytes
      . Shrubbery.taggedBranch @"colindex" Render.showBytes
      . Shrubbery.taggedBranch @"colindextext" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"colspan" Render.showBytes
      . Shrubbery.taggedBranch @"controls" (Render.foldToBytesWithSeparator Types.idToBytes " ")
      . Shrubbery.taggedBranch @"current" Current.ariaCurrentToBytes
      . Shrubbery.taggedBranch @"describedby" (Render.foldToBytesWithSeparator Types.idToBytes " ")
      . Shrubbery.taggedBranch @"description" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"details" (Render.foldToBytesWithSeparator Types.idToBytes " ")
      . Shrubbery.taggedBranch @"disabled" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"errormessage" (Render.foldToBytesWithSeparator Types.idToBytes " " . NEL.toList)
      . Shrubbery.taggedBranch @"expanded" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"flowto" (Render.foldToBytesWithSeparator Types.idToBytes " ")
      . Shrubbery.taggedBranch @"haspopup" HasPopup.ariaHasPopupToBytes
      . Shrubbery.taggedBranch @"hidden" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"invalid" Invalid.ariaInvalidToBytes
      . Shrubbery.taggedBranch @"keyshortcuts" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"label" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"labelledby" (Render.foldToBytesWithSeparator Types.idToBytes " " . NEL.toList)
      . Shrubbery.taggedBranch @"level" Render.showBytes
      . Shrubbery.taggedBranch @"live" Option.ariaLiveOptionToBytes
      . Shrubbery.taggedBranch @"modal" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"multiline" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"multiselectable" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"orientation" orientationToBytes
      . Shrubbery.taggedBranch @"owns" (Render.foldToBytesWithSeparator Types.idToBytes " ")
      . Shrubbery.taggedBranch @"placeholder" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"posinset" Render.showBytes
      . Shrubbery.taggedBranch @"pressed" MixedBool.ariaMixedBoolToBytes
      . Shrubbery.taggedBranch @"readonly" Render.showBytes
      . Shrubbery.taggedBranch @"relevant" Option.ariaRelevantOptionToBytes
      . Shrubbery.taggedBranch @"required" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"roledescription" (Render.textToLazyBytes . NET.toText)
      . Shrubbery.taggedBranch @"rowcount" Render.showBytes
      . Shrubbery.taggedBranch @"rowindex" Render.showBytes
      . Shrubbery.taggedBranch @"rowindextext" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"rowspan" Render.showBytes
      . Shrubbery.taggedBranch @"selected" Render.enumBoolToBytes
      . Shrubbery.taggedBranch @"setsize" Render.showBytes
      . Shrubbery.taggedBranch @"sort" Option.ariaSortOptionToBytes
      . Shrubbery.taggedBranch @"valuemax" Render.showBytes
      . Shrubbery.taggedBranch @"valuemin" Render.showBytes
      . Shrubbery.taggedBranch @"valuenow" Render.showBytes
      . Shrubbery.taggedBranch @"valuetext" Render.textToLazyBytes
      . Shrubbery.taggedBranch @"raw" (Render.textToLazyBytes . rawAriaValue)
      $ Shrubbery.taggedBranchEnd
  ) aria

ariaValueToText :: Aria -> T.Text
ariaValueToText (Aria aria) =
  ( Shrubbery.dissectTaggedUnion
      . Shrubbery.taggedBranchBuild
      . Shrubbery.taggedBranch @"activedescendant" Types.idToText
      . Shrubbery.taggedBranch @"atomic" Render.enumBoolToText
      . Shrubbery.taggedBranch @"autocomplete" Option.ariaAutocompleteOptionToText
      . Shrubbery.taggedBranch @"braillelabel" id
      . Shrubbery.taggedBranch @"brailleroledescription" id
      . Shrubbery.taggedBranch @"busy" Render.enumBoolToText
      . Shrubbery.taggedBranch @"checked" MixedBool.ariaMixedBoolToText
      . Shrubbery.taggedBranch @"colcount" Render.showText
      . Shrubbery.taggedBranch @"colindex" Render.showText
      . Shrubbery.taggedBranch @"colindextext" id
      . Shrubbery.taggedBranch @"colspan" Render.showText
      . Shrubbery.taggedBranch @"controls" (Render.foldToTextWithSeparator Types.idToText " ")
      . Shrubbery.taggedBranch @"current" Current.ariaCurrentToText
      . Shrubbery.taggedBranch @"describedby" (Render.foldToTextWithSeparator Types.idToText " ")
      . Shrubbery.taggedBranch @"description" id
      . Shrubbery.taggedBranch @"details" (Render.foldToTextWithSeparator Types.idToText " ")
      . Shrubbery.taggedBranch @"disabled" Render.enumBoolToText
      . Shrubbery.taggedBranch @"errormessage" (Render.foldToTextWithSeparator Types.idToText " " . NEL.toList)
      . Shrubbery.taggedBranch @"expanded" Render.enumBoolToText
      . Shrubbery.taggedBranch @"flowto" (Render.foldToTextWithSeparator Types.idToText " ")
      . Shrubbery.taggedBranch @"haspopup" HasPopup.ariaHasPopupToText
      . Shrubbery.taggedBranch @"hidden" Render.enumBoolToText
      . Shrubbery.taggedBranch @"invalid" Invalid.ariaInvalidToText
      . Shrubbery.taggedBranch @"keyshortcuts" id
      . Shrubbery.taggedBranch @"label" id
      . Shrubbery.taggedBranch @"labelledby" (Render.foldToTextWithSeparator Types.idToText " " . NEL.toList)
      . Shrubbery.taggedBranch @"level" Render.showText
      . Shrubbery.taggedBranch @"live" Option.ariaLiveOptionToText
      . Shrubbery.taggedBranch @"modal" Render.enumBoolToText
      . Shrubbery.taggedBranch @"multiline" Render.enumBoolToText
      . Shrubbery.taggedBranch @"multiselectable" Render.enumBoolToText
      . Shrubbery.taggedBranch @"orientation" orientationToText
      . Shrubbery.taggedBranch @"owns" (Render.foldToTextWithSeparator Types.idToText " ")
      . Shrubbery.taggedBranch @"placeholder" id
      . Shrubbery.taggedBranch @"posinset" Render.showText
      . Shrubbery.taggedBranch @"pressed" MixedBool.ariaMixedBoolToText
      . Shrubbery.taggedBranch @"readonly" Render.showText
      . Shrubbery.taggedBranch @"relevant" Option.ariaRelevantOptionToText
      . Shrubbery.taggedBranch @"required" Render.enumBoolToText
      . Shrubbery.taggedBranch @"roledescription" NET.toText
      . Shrubbery.taggedBranch @"rowcount" Render.showText
      . Shrubbery.taggedBranch @"rowindex" Render.showText
      . Shrubbery.taggedBranch @"rowindextext" id
      . Shrubbery.taggedBranch @"rowspan" Render.showText
      . Shrubbery.taggedBranch @"selected" Render.enumBoolToText
      . Shrubbery.taggedBranch @"setsize" Render.showText
      . Shrubbery.taggedBranch @"sort" Option.ariaSortOptionToText
      . Shrubbery.taggedBranch @"valuemax" Render.showText
      . Shrubbery.taggedBranch @"valuemin" Render.showText
      . Shrubbery.taggedBranch @"valuenow" Render.showText
      . Shrubbery.taggedBranch @"valuetext" id
      . Shrubbery.taggedBranch @"raw" rawAriaValue
      $ Shrubbery.taggedBranchEnd
  ) aria

data RawAria =
  RawAria
    { rawAriaAttribute :: NET.NonEmptyText
    , rawAriaValue :: T.Text
    } deriving (Eq, Show)
