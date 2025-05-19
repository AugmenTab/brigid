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
  , roleToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
