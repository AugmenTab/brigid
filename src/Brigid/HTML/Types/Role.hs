module Brigid.HTML.Types.Role
  ( Role
      ( RoleAlert
      , RoleAlertDialog
      , RoleArticle
      , RoleBanner
      , RoleBlockquote
      , RoleButton
      , RoleCaption
      , RoleCell
      , RoleCheckbox
      , RoleCode
      , RoleColumnHeader
      , RoleCombobox
      , RoleComplementary
      , RoleContentInfo
      , RoleDefinition
      , RoleDeletion
      , RoleDialog
      , RoleDirectory
      , RoleDocument
      , RoleEmphasis
      , RoleFeed
      , RoleFigure
      , RoleForm
      , RoleGeneric
      , RoleGrid
      , RoleGridCell
      , RoleGroup
      , RoleHeading
      , RoleImage
      , RoleInsertion
      , RoleLink
      , RoleList
      , RoleListItem
      , RoleListbox
      , RoleLog
      , RoleMain
      , RoleMarquee
      , RoleMath
      , RoleMenu
      , RoleMenuItem
      , RoleMenuItemCheckbox
      , RoleMenuItemRadio
      , RoleMenubar
      , RoleNavigation
      , RoleNone
      , RoleNote
      , RoleOption
      , RoleParagraph
      , RolePresentation
      , RoleProgressBar
      , RoleRadio
      , RoleRadioGroup
      , RoleRegion
      , RoleRow
      , RoleRowGroup
      , RoleRowHeader
      , RoleScrollbar
      , RoleSearch
      , RoleSearchbox
      , RoleSeparator
      , RoleSlider
      , RoleSpinButton
      , RoleStatus
      , RoleStrong
      , RoleSubscript
      , RoleSuperscript
      , RoleSwitch
      , RoleTab
      , RoleTabList
      , RoleTabPanel
      , RoleTable
      , RoleTerm
      , RoleTime
      , RoleTimer
      , RoleToolbar
      , RoleTooltip
      )
  , roleToBytes
  , roleToBytesBuilder
  , roleToText
  , roleToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data Role
  = RoleAlert
  | RoleAlertDialog
  | RoleArticle
  | RoleBanner
  | RoleBlockquote
  | RoleButton
  | RoleCaption
  | RoleCell
  | RoleCheckbox
  | RoleCode
  | RoleColumnHeader
  | RoleCombobox
  | RoleComplementary
  | RoleContentInfo
  | RoleDefinition
  | RoleDeletion
  | RoleDialog
  | RoleDirectory
  | RoleDocument
  | RoleEmphasis
  | RoleFeed
  | RoleFigure
  | RoleForm
  | RoleGeneric
  | RoleGrid
  | RoleGridCell
  | RoleGroup
  | RoleHeading
  | RoleImage
  | RoleInsertion
  | RoleLink
  | RoleList
  | RoleListItem
  | RoleListbox
  | RoleLog
  | RoleMain
  | RoleMarquee
  | RoleMath
  | RoleMenu
  | RoleMenuItem
  | RoleMenuItemCheckbox
  | RoleMenuItemRadio
  | RoleMenubar
  | RoleNavigation
  | RoleNone
  | RoleNote
  | RoleOption
  | RoleParagraph
  | RolePresentation
  | RoleProgressBar
  | RoleRadio
  | RoleRadioGroup
  | RoleRegion
  | RoleRow
  | RoleRowGroup
  | RoleRowHeader
  | RoleScrollbar
  | RoleSearch
  | RoleSearchbox
  | RoleSeparator
  | RoleSlider
  | RoleSpinButton
  | RoleStatus
  | RoleStrong
  | RoleSubscript
  | RoleSuperscript
  | RoleSwitch
  | RoleTab
  | RoleTabList
  | RoleTabPanel
  | RoleTable
  | RoleTerm
  | RoleTime
  | RoleTimer
  | RoleToolbar
  | RoleTooltip
  deriving (Bounded, Enum, Eq, Show)

roleToBytes :: Role -> LBS.ByteString
roleToBytes role =
  case role of
    RoleAlert            -> "alert"
    RoleAlertDialog      -> "alertdialog"
    RoleArticle          -> "article"
    RoleBanner           -> "banner"
    RoleBlockquote       -> "blockquote"
    RoleButton           -> "button"
    RoleCaption          -> "caption"
    RoleCell             -> "cell"
    RoleCheckbox         -> "checkbox"
    RoleCode             -> "code"
    RoleColumnHeader     -> "columnheader"
    RoleCombobox         -> "combobox"
    RoleComplementary    -> "complementary"
    RoleContentInfo      -> "contentinfo"
    RoleDefinition       -> "definition"
    RoleDeletion         -> "deletion"
    RoleDialog           -> "dialog"
    RoleDirectory        -> "directory"
    RoleDocument         -> "document"
    RoleEmphasis         -> "emphasis"
    RoleFeed             -> "feed"
    RoleFigure           -> "figure"
    RoleForm             -> "form"
    RoleGeneric          -> "generic"
    RoleGrid             -> "grid"
    RoleGridCell         -> "gridcell"
    RoleGroup            -> "group"
    RoleHeading          -> "heading"
    RoleImage            -> "img"
    RoleInsertion        -> "insertion"
    RoleLink             -> "link"
    RoleList             -> "list"
    RoleListItem         -> "listitem"
    RoleListbox          -> "listbox"
    RoleLog              -> "log"
    RoleMain             -> "main"
    RoleMarquee          -> "marquee"
    RoleMath             -> "math"
    RoleMenu             -> "menu"
    RoleMenuItem         -> "menuitem"
    RoleMenuItemCheckbox -> "menuitemcheckbox"
    RoleMenuItemRadio    -> "menuitemradio"
    RoleMenubar          -> "menubar"
    RoleNavigation       -> "navigation"
    RoleNone             -> "none"
    RoleNote             -> "note"
    RoleOption           -> "option"
    RoleParagraph        -> "paragraph"
    RolePresentation     -> "presentation"
    RoleProgressBar      -> "progressbar"
    RoleRadio            -> "radio"
    RoleRadioGroup       -> "radiogroup"
    RoleRegion           -> "region"
    RoleRow              -> "row"
    RoleRowGroup         -> "rowgroup"
    RoleRowHeader        -> "rowheader"
    RoleScrollbar        -> "scrollbar"
    RoleSearch           -> "search"
    RoleSearchbox        -> "searchbox"
    RoleSeparator        -> "separator"
    RoleSlider           -> "slider"
    RoleSpinButton       -> "spinbutton"
    RoleStatus           -> "status"
    RoleStrong           -> "strong"
    RoleSubscript        -> "subscript"
    RoleSuperscript      -> "superscript"
    RoleSwitch           -> "switch"
    RoleTab              -> "tab"
    RoleTabList          -> "tablist"
    RoleTabPanel         -> "tabpanel"
    RoleTable            -> "table"
    RoleTerm             -> "term"
    RoleTime             -> "time"
    RoleTimer            -> "timer"
    RoleToolbar          -> "toolbar"
    RoleTooltip          -> "tooltip"

roleToBytesBuilder :: Role -> Builder
{-# INLINE roleToBytesBuilder #-}
roleToBytesBuilder role =
  case role of
    RoleAlert            -> string8 "alert"
    RoleAlertDialog      -> string8 "alertdialog"
    RoleArticle          -> string8 "article"
    RoleBanner           -> string8 "banner"
    RoleBlockquote       -> string8 "blockquote"
    RoleButton           -> string8 "button"
    RoleCaption          -> string8 "caption"
    RoleCell             -> string8 "cell"
    RoleCheckbox         -> string8 "checkbox"
    RoleCode             -> string8 "code"
    RoleColumnHeader     -> string8 "columnheader"
    RoleCombobox         -> string8 "combobox"
    RoleComplementary    -> string8 "complementary"
    RoleContentInfo      -> string8 "contentinfo"
    RoleDefinition       -> string8 "definition"
    RoleDeletion         -> string8 "deletion"
    RoleDialog           -> string8 "dialog"
    RoleDirectory        -> string8 "directory"
    RoleDocument         -> string8 "document"
    RoleEmphasis         -> string8 "emphasis"
    RoleFeed             -> string8 "feed"
    RoleFigure           -> string8 "figure"
    RoleForm             -> string8 "form"
    RoleGeneric          -> string8 "generic"
    RoleGrid             -> string8 "grid"
    RoleGridCell         -> string8 "gridcell"
    RoleGroup            -> string8 "group"
    RoleHeading          -> string8 "heading"
    RoleImage            -> string8 "img"
    RoleInsertion        -> string8 "insertion"
    RoleLink             -> string8 "link"
    RoleList             -> string8 "list"
    RoleListItem         -> string8 "listitem"
    RoleListbox          -> string8 "listbox"
    RoleLog              -> string8 "log"
    RoleMain             -> string8 "main"
    RoleMarquee          -> string8 "marquee"
    RoleMath             -> string8 "math"
    RoleMenu             -> string8 "menu"
    RoleMenuItem         -> string8 "menuitem"
    RoleMenuItemCheckbox -> string8 "menuitemcheckbox"
    RoleMenuItemRadio    -> string8 "menuitemradio"
    RoleMenubar          -> string8 "menubar"
    RoleNavigation       -> string8 "navigation"
    RoleNone             -> string8 "none"
    RoleNote             -> string8 "note"
    RoleOption           -> string8 "option"
    RoleParagraph        -> string8 "paragraph"
    RolePresentation     -> string8 "presentation"
    RoleProgressBar      -> string8 "progressbar"
    RoleRadio            -> string8 "radio"
    RoleRadioGroup       -> string8 "radiogroup"
    RoleRegion           -> string8 "region"
    RoleRow              -> string8 "row"
    RoleRowGroup         -> string8 "rowgroup"
    RoleRowHeader        -> string8 "rowheader"
    RoleScrollbar        -> string8 "scrollbar"
    RoleSearch           -> string8 "search"
    RoleSearchbox        -> string8 "searchbox"
    RoleSeparator        -> string8 "separator"
    RoleSlider           -> string8 "slider"
    RoleSpinButton       -> string8 "spinbutton"
    RoleStatus           -> string8 "status"
    RoleStrong           -> string8 "strong"
    RoleSubscript        -> string8 "subscript"
    RoleSuperscript      -> string8 "superscript"
    RoleSwitch           -> string8 "switch"
    RoleTab              -> string8 "tab"
    RoleTabList          -> string8 "tablist"
    RoleTabPanel         -> string8 "tabpanel"
    RoleTable            -> string8 "table"
    RoleTerm             -> string8 "term"
    RoleTime             -> string8 "time"
    RoleTimer            -> string8 "timer"
    RoleToolbar          -> string8 "toolbar"
    RoleTooltip          -> string8 "tooltip"

roleToText :: Role -> T.Text
roleToText role =
  case role of
    RoleAlert            -> "alert"
    RoleAlertDialog      -> "alertdialog"
    RoleArticle          -> "article"
    RoleBanner           -> "banner"
    RoleBlockquote       -> "blockquote"
    RoleButton           -> "button"
    RoleCaption          -> "caption"
    RoleCell             -> "cell"
    RoleCheckbox         -> "checkbox"
    RoleCode             -> "code"
    RoleColumnHeader     -> "columnheader"
    RoleCombobox         -> "combobox"
    RoleComplementary    -> "complementary"
    RoleContentInfo      -> "contentinfo"
    RoleDefinition       -> "definition"
    RoleDeletion         -> "deletion"
    RoleDialog           -> "dialog"
    RoleDirectory        -> "directory"
    RoleDocument         -> "document"
    RoleEmphasis         -> "emphasis"
    RoleFeed             -> "feed"
    RoleFigure           -> "figure"
    RoleForm             -> "form"
    RoleGeneric          -> "generic"
    RoleGrid             -> "grid"
    RoleGridCell         -> "gridcell"
    RoleGroup            -> "group"
    RoleHeading          -> "heading"
    RoleImage            -> "img"
    RoleInsertion        -> "insertion"
    RoleLink             -> "link"
    RoleList             -> "list"
    RoleListItem         -> "listitem"
    RoleListbox          -> "listbox"
    RoleLog              -> "log"
    RoleMain             -> "main"
    RoleMarquee          -> "marquee"
    RoleMath             -> "math"
    RoleMenu             -> "menu"
    RoleMenuItem         -> "menuitem"
    RoleMenuItemCheckbox -> "menuitemcheckbox"
    RoleMenuItemRadio    -> "menuitemradio"
    RoleMenubar          -> "menubar"
    RoleNavigation       -> "navigation"
    RoleNone             -> "none"
    RoleNote             -> "note"
    RoleOption           -> "option"
    RoleParagraph        -> "paragraph"
    RolePresentation     -> "presentation"
    RoleProgressBar      -> "progressbar"
    RoleRadio            -> "radio"
    RoleRadioGroup       -> "radiogroup"
    RoleRegion           -> "region"
    RoleRow              -> "row"
    RoleRowGroup         -> "rowgroup"
    RoleRowHeader        -> "rowheader"
    RoleScrollbar        -> "scrollbar"
    RoleSearch           -> "search"
    RoleSearchbox        -> "searchbox"
    RoleSeparator        -> "separator"
    RoleSlider           -> "slider"
    RoleSpinButton       -> "spinbutton"
    RoleStatus           -> "status"
    RoleStrong           -> "strong"
    RoleSubscript        -> "subscript"
    RoleSuperscript      -> "superscript"
    RoleSwitch           -> "switch"
    RoleTab              -> "tab"
    RoleTabList          -> "tablist"
    RoleTabPanel         -> "tabpanel"
    RoleTable            -> "table"
    RoleTerm             -> "term"
    RoleTime             -> "time"
    RoleTimer            -> "timer"
    RoleToolbar          -> "toolbar"
    RoleTooltip          -> "tooltip"

roleToTextBuilder :: Role -> TBL.Builder
roleToTextBuilder = TBL.fromText . roleToText
