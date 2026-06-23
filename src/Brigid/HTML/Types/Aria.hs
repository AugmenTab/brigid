{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Aria
  ( Aria (Aria)
  , AriaTypes
  , ariaAttributeToBytes
  , ariaAttributeToBytesBuilder
  , ariaAttributeToText
  , ariaAttributeToTextBuilder
  , ariaValueToBytes
  , ariaValueToBytesBuilder
  , ariaValueToText
  , ariaValueToTextBuilder
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

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Encoding qualified as TE
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
import Brigid.HTML.Types.Orientation
  ( Orientation
  , orientationToBytes
  , orientationToBytesBuilder
  , orientationToText
  )
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

ariaAttributeToBytesBuilder :: Aria -> Builder
{-# INLINABLE ariaAttributeToBytesBuilder #-}
ariaAttributeToBytesBuilder (Aria aria) =
  ( Shrubbery.dissectTaggedUnion
      . Shrubbery.taggedBranchBuild
      . Shrubbery.taggedBranch @"activedescendant"      (const (string8 "aria-activedescendant"))
      . Shrubbery.taggedBranch @"atomic"                (const (string8 "aria-atomic"))
      . Shrubbery.taggedBranch @"autocomplete"          (const (string8 "aria-autocomplete"))
      . Shrubbery.taggedBranch @"braillelabel"          (const (string8 "aria-braillelabel"))
      . Shrubbery.taggedBranch @"brailleroledescription" (const (string8 "aria-brailleroledescription"))
      . Shrubbery.taggedBranch @"busy"                  (const (string8 "aria-busy"))
      . Shrubbery.taggedBranch @"checked"               (const (string8 "aria-checked"))
      . Shrubbery.taggedBranch @"colcount"              (const (string8 "aria-colcount"))
      . Shrubbery.taggedBranch @"colindex"              (const (string8 "aria-colindex"))
      . Shrubbery.taggedBranch @"colindextext"          (const (string8 "aria-colindextext"))
      . Shrubbery.taggedBranch @"colspan"               (const (string8 "aria-colspan"))
      . Shrubbery.taggedBranch @"controls"              (const (string8 "aria-controls"))
      . Shrubbery.taggedBranch @"current"               (const (string8 "aria-current"))
      . Shrubbery.taggedBranch @"describedby"           (const (string8 "aria-describedby"))
      . Shrubbery.taggedBranch @"description"           (const (string8 "aria-description"))
      . Shrubbery.taggedBranch @"details"               (const (string8 "aria-details"))
      . Shrubbery.taggedBranch @"disabled"              (const (string8 "aria-disabled"))
      . Shrubbery.taggedBranch @"errormessage"          (const (string8 "aria-errormessage"))
      . Shrubbery.taggedBranch @"expanded"              (const (string8 "aria-expanded"))
      . Shrubbery.taggedBranch @"flowto"                (const (string8 "aria-flowto"))
      . Shrubbery.taggedBranch @"haspopup"              (const (string8 "aria-haspopup"))
      . Shrubbery.taggedBranch @"hidden"                (const (string8 "aria-hidden"))
      . Shrubbery.taggedBranch @"invalid"               (const (string8 "aria-invalid"))
      . Shrubbery.taggedBranch @"keyshortcuts"          (const (string8 "aria-keyshortcuts"))
      . Shrubbery.taggedBranch @"label"                 (const (string8 "aria-label"))
      . Shrubbery.taggedBranch @"labelledby"            (const (string8 "aria-labelledby"))
      . Shrubbery.taggedBranch @"level"                 (const (string8 "aria-level"))
      . Shrubbery.taggedBranch @"live"                  (const (string8 "aria-live"))
      . Shrubbery.taggedBranch @"modal"                 (const (string8 "aria-modal"))
      . Shrubbery.taggedBranch @"multiline"             (const (string8 "aria-multiline"))
      . Shrubbery.taggedBranch @"multiselectable"       (const (string8 "aria-multiselectable"))
      . Shrubbery.taggedBranch @"orientation"           (const (string8 "aria-orientation"))
      . Shrubbery.taggedBranch @"owns"                  (const (string8 "aria-owns"))
      . Shrubbery.taggedBranch @"placeholder"           (const (string8 "aria-placeholder"))
      . Shrubbery.taggedBranch @"posinset"              (const (string8 "aria-posinset"))
      . Shrubbery.taggedBranch @"pressed"               (const (string8 "aria-pressed"))
      . Shrubbery.taggedBranch @"readonly"              (const (string8 "aria-readonly"))
      . Shrubbery.taggedBranch @"relevant"              (const (string8 "aria-relevant"))
      . Shrubbery.taggedBranch @"required"              (const (string8 "aria-required"))
      . Shrubbery.taggedBranch @"roledescription"       (const (string8 "aria-roledescription"))
      . Shrubbery.taggedBranch @"rowcount"              (const (string8 "aria-rowcount"))
      . Shrubbery.taggedBranch @"rowindex"              (const (string8 "aria-rowindex"))
      . Shrubbery.taggedBranch @"rowindextext"          (const (string8 "aria-rowindextext"))
      . Shrubbery.taggedBranch @"rowspan"               (const (string8 "aria-rowspan"))
      . Shrubbery.taggedBranch @"selected"              (const (string8 "aria-selected"))
      . Shrubbery.taggedBranch @"setsize"               (const (string8 "aria-setsize"))
      . Shrubbery.taggedBranch @"sort"                  (const (string8 "aria-sort"))
      . Shrubbery.taggedBranch @"valuemax"              (const (string8 "aria-valuemax"))
      . Shrubbery.taggedBranch @"valuemin"              (const (string8 "aria-valuemin"))
      . Shrubbery.taggedBranch @"valuenow"              (const (string8 "aria-valuenow"))
      . Shrubbery.taggedBranch @"valuetext"             (const (string8 "aria-valuetext"))
      . Shrubbery.taggedBranch @"raw"                   ((string8 "aria-" <>) . TE.encodeUtf8Builder . NET.toText . rawAriaAttribute)
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
      . Shrubbery.taggedBranch @"readonly" Render.enumBoolToBytes
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

ariaValueToBytesBuilder :: Aria -> Builder
{-# INLINABLE ariaValueToBytesBuilder #-}
ariaValueToBytesBuilder (Aria aria) =
  ( Shrubbery.dissectTaggedUnion
      . Shrubbery.taggedBranchBuild
      . Shrubbery.taggedBranch @"activedescendant"      Types.idToBytesBuilder
      . Shrubbery.taggedBranch @"atomic"                Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"autocomplete"          Option.ariaAutocompleteOptionToBytesBuilder
      . Shrubbery.taggedBranch @"braillelabel"          TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"brailleroledescription" TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"busy"                  Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"checked"               MixedBool.ariaMixedBoolToBytesBuilder
      . Shrubbery.taggedBranch @"colcount"              Render.showBytesBuilder
      . Shrubbery.taggedBranch @"colindex"              Render.showBytesBuilder
      . Shrubbery.taggedBranch @"colindextext"          TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"colspan"               Render.showBytesBuilder
      . Shrubbery.taggedBranch @"controls"              (Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " ")
      . Shrubbery.taggedBranch @"current"               Current.ariaCurrentToBytesBuilder
      . Shrubbery.taggedBranch @"describedby"           (Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " ")
      . Shrubbery.taggedBranch @"description"           TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"details"               (Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " ")
      . Shrubbery.taggedBranch @"disabled"              Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"errormessage"          (Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " " . NEL.toList)
      . Shrubbery.taggedBranch @"expanded"              Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"flowto"                (Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " ")
      . Shrubbery.taggedBranch @"haspopup"              HasPopup.ariaHasPopupToBytesBuilder
      . Shrubbery.taggedBranch @"hidden"                Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"invalid"               Invalid.ariaInvalidToBytesBuilder
      . Shrubbery.taggedBranch @"keyshortcuts"          TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"label"                 TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"labelledby"            (Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " " . NEL.toList)
      . Shrubbery.taggedBranch @"level"                 Render.showBytesBuilder
      . Shrubbery.taggedBranch @"live"                  Option.ariaLiveOptionToBytesBuilder
      . Shrubbery.taggedBranch @"modal"                 Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"multiline"             Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"multiselectable"       Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"orientation"           orientationToBytesBuilder
      . Shrubbery.taggedBranch @"owns"                  (Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " ")
      . Shrubbery.taggedBranch @"placeholder"           TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"posinset"              Render.showBytesBuilder
      . Shrubbery.taggedBranch @"pressed"               MixedBool.ariaMixedBoolToBytesBuilder
      . Shrubbery.taggedBranch @"readonly"              Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"relevant"              Option.ariaRelevantOptionToBytesBuilder
      . Shrubbery.taggedBranch @"required"              Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"roledescription"       (TE.encodeUtf8Builder . NET.toText)
      . Shrubbery.taggedBranch @"rowcount"              Render.showIntegerBytesBuilder
      . Shrubbery.taggedBranch @"rowindex"              Render.showBytesBuilder
      . Shrubbery.taggedBranch @"rowindextext"          TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"rowspan"               Render.showBytesBuilder
      . Shrubbery.taggedBranch @"selected"              Render.enumBoolToBytesBuilder
      . Shrubbery.taggedBranch @"setsize"               Render.showIntegerBytesBuilder
      . Shrubbery.taggedBranch @"sort"                  Option.ariaSortOptionToBytesBuilder
      . Shrubbery.taggedBranch @"valuemax"              Render.showBytesBuilder
      . Shrubbery.taggedBranch @"valuemin"              Render.showBytesBuilder
      . Shrubbery.taggedBranch @"valuenow"              Render.showBytesBuilder
      . Shrubbery.taggedBranch @"valuetext"             TE.encodeUtf8Builder
      . Shrubbery.taggedBranch @"raw"                   (Render.textToBytesBuilder . rawAriaValue)
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
      . Shrubbery.taggedBranch @"readonly" Render.enumBoolToText
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

ariaAttributeToTextBuilder :: Aria -> TBL.Builder
ariaAttributeToTextBuilder = TBL.fromText . ariaAttributeToText

ariaValueToTextBuilder :: Aria -> TBL.Builder
ariaValueToTextBuilder = TBL.fromText . ariaValueToText

data RawAria =
  RawAria
    { rawAriaAttribute :: NET.NonEmptyText
    , rawAriaValue :: T.Text
    } deriving (Eq, Show)
