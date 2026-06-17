{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Types.Aria.HasPopup
  ( AriaHasPopup (..)
  , AriaHasPopupTypes
  , ariaHasPopupToBytes
  , ariaHasPopupToBytesBuilder
  , ariaHasPopupToText
  , PopupMenu (PopupMenu)
  , PopupListbox (PopupListbox)
  , PopupTree (PopupTree)
  , PopupGrid (PopupGrid)
  , PopupDialog (PopupDialog)
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Shrubbery qualified

import Brigid.Internal.Render qualified as Render

newtype AriaHasPopup = AriaHasPopup (Shrubbery.Union AriaHasPopupTypes)
  deriving (Eq, Show)

type AriaHasPopupTypes =
  [ PopupMenu
  , PopupListbox
  , PopupTree
  , PopupGrid
  , PopupDialog
  , Bool
  ]

ariaHasPopupToBytes :: AriaHasPopup -> LBS.ByteString
ariaHasPopupToBytes (AriaHasPopup hp) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @PopupMenu popupMenuToBytes
      . Shrubbery.branch @PopupListbox popupListboxToBytes
      . Shrubbery.branch @PopupTree popupTreeToBytes
      . Shrubbery.branch @PopupGrid popupGridToBytes
      . Shrubbery.branch @PopupDialog popupDialogToBytes
      . Shrubbery.branch @Bool Render.enumBoolToBytes
      $ Shrubbery.branchEnd
  ) hp

ariaHasPopupToBytesBuilder :: AriaHasPopup -> Builder
ariaHasPopupToBytesBuilder (AriaHasPopup hp) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @PopupMenu    popupMenuToBytesBuilder
      . Shrubbery.branch @PopupListbox popupListboxToBytesBuilder
      . Shrubbery.branch @PopupTree    popupTreeToBytesBuilder
      . Shrubbery.branch @PopupGrid    popupGridToBytesBuilder
      . Shrubbery.branch @PopupDialog  popupDialogToBytesBuilder
      . Shrubbery.branch @Bool         Render.enumBoolToBytesBuilder
      $ Shrubbery.branchEnd
  ) hp

ariaHasPopupToText :: AriaHasPopup -> T.Text
ariaHasPopupToText (AriaHasPopup hp) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @PopupMenu popupMenuToText
      . Shrubbery.branch @PopupListbox popupListboxToText
      . Shrubbery.branch @PopupTree popupTreeToText
      . Shrubbery.branch @PopupGrid popupGridToText
      . Shrubbery.branch @PopupDialog popupDialogToText
      . Shrubbery.branch @Bool Render.enumBoolToText
      $ Shrubbery.branchEnd
  ) hp

data PopupMenu = PopupMenu
  deriving (Eq, Show)

popupMenuToBytes :: PopupMenu -> LBS.ByteString
popupMenuToBytes PopupMenu = LBS8.pack "menu"

popupMenuToBytesBuilder :: PopupMenu -> Builder
popupMenuToBytesBuilder PopupMenu = string8 "menu"

popupMenuToText :: PopupMenu -> T.Text
popupMenuToText PopupMenu = T.pack "menu"

data PopupListbox = PopupListbox
  deriving (Eq, Show)

popupListboxToBytes :: PopupListbox -> LBS.ByteString
popupListboxToBytes PopupListbox = LBS8.pack "listbox"

popupListboxToBytesBuilder :: PopupListbox -> Builder
popupListboxToBytesBuilder PopupListbox = string8 "listbox"

popupListboxToText :: PopupListbox -> T.Text
popupListboxToText PopupListbox = T.pack "listbox"

data PopupTree = PopupTree
  deriving (Eq, Show)

popupTreeToBytes :: PopupTree -> LBS.ByteString
popupTreeToBytes PopupTree = LBS8.pack "tree"

popupTreeToBytesBuilder :: PopupTree -> Builder
popupTreeToBytesBuilder PopupTree = string8 "tree"

popupTreeToText :: PopupTree -> T.Text
popupTreeToText PopupTree = T.pack "tree"

data PopupGrid = PopupGrid
  deriving (Eq, Show)

popupGridToBytes :: PopupGrid -> LBS.ByteString
popupGridToBytes PopupGrid = LBS8.pack "grid"

popupGridToBytesBuilder :: PopupGrid -> Builder
popupGridToBytesBuilder PopupGrid = string8 "grid"

popupGridToText :: PopupGrid -> T.Text
popupGridToText PopupGrid = T.pack "grid"

data PopupDialog = PopupDialog
  deriving (Eq, Show)

popupDialogToBytes :: PopupDialog -> LBS.ByteString
popupDialogToBytes PopupDialog = LBS8.pack "dialog"

popupDialogToBytesBuilder :: PopupDialog -> Builder
popupDialogToBytesBuilder PopupDialog = string8 "dialog"

popupDialogToText :: PopupDialog -> T.Text
popupDialogToText PopupDialog = T.pack "dialog"
