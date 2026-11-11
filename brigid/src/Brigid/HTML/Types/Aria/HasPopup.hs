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

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
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
{-# INLINABLE ariaHasPopupToBytesBuilder #-}
ariaHasPopupToBytesBuilder = lazyByteString . ariaHasPopupToBytes

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
popupMenuToBytes PopupMenu = "menu"

popupMenuToText :: PopupMenu -> T.Text
popupMenuToText PopupMenu = "menu"

data PopupListbox = PopupListbox
  deriving (Eq, Show)

popupListboxToBytes :: PopupListbox -> LBS.ByteString
popupListboxToBytes PopupListbox = "listbox"

popupListboxToText :: PopupListbox -> T.Text
popupListboxToText PopupListbox = "listbox"

data PopupTree = PopupTree
  deriving (Eq, Show)

popupTreeToBytes :: PopupTree -> LBS.ByteString
popupTreeToBytes PopupTree = "tree"

popupTreeToText :: PopupTree -> T.Text
popupTreeToText PopupTree = "tree"

data PopupGrid = PopupGrid
  deriving (Eq, Show)

popupGridToBytes :: PopupGrid -> LBS.ByteString
popupGridToBytes PopupGrid = "grid"

popupGridToText :: PopupGrid -> T.Text
popupGridToText PopupGrid = "grid"

data PopupDialog = PopupDialog
  deriving (Eq, Show)

popupDialogToBytes :: PopupDialog -> LBS.ByteString
popupDialogToBytes PopupDialog = "dialog"

popupDialogToText :: PopupDialog -> T.Text
popupDialogToText PopupDialog = "dialog"
