{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML4.Attributes.Internal
  ( ValidAttribute
  , Attribute
      ( Attr_AccessKey
      , Attr_Autocapitalize
      , Attr_Autofocus
      , Attr_Class
      , Attr_ContentEditable
      , Attr_Data
      , Attr_Dir
      , Attr_Draggable
      , Attr_EnterKeyHint
      , Attr_ExportParts
      , Attr_Hidden
      , Attr_Id
      , Attr_Inert
      , Attr_InputMode
      , Attr_Is
      , Attr_ItemId
      , Attr_ItemProp
      , Attr_ItemRef
      , Attr_ItemScope
      , Attr_ItemType
      , Attr_Lang
      , Attr_Nonce
      , Attr_Part
      , Attr_Popover
      , Attr_Role
      , Attr_Slot
      , Attr_Spellcheck
      , Attr_Style
      , Attr_TabIndex
      , Attr_Title
      , Attr_Translate
      , Attr_Width
      , Attr_Disabled
      )
  ) where

import Data.Text qualified as T

import Data.HTML4.Attributes.AttributeType (AttributeType(..))
import Data.HTML4.Attributes.Elements (ValidAttribute)
import Data.HTML4.Elements.TagType (TagType)
import Data.HTML4.Types qualified as Types

data Attribute (tag :: TagType) where
  -- Global Attributes
  Attr_AccessKey
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Autocapitalize
    :: Types.AutocapitalizeOption
    -> Attribute tag

  Attr_Autofocus
    :: Bool
    -> Attribute tag

  Attr_Class
    :: T.Text
    -> Attribute tag

  Attr_ContentEditable
    :: Types.ContentEditableOption
    -> Attribute tag

  Attr_Data
    :: T.Text
    -> T.Text
    -> Attribute tag

  Attr_Dir
    :: Types.Directionality
    -> Attribute tag

  Attr_Draggable
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  Attr_EnterKeyHint
    :: Types.KeyHintOption
    -> Attribute tag

  Attr_ExportParts
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Hidden
    :: Bool
    -> Attribute tag

  Attr_Id
    :: T.Text
    -> Attribute tag

  Attr_Inert
    :: Bool
    -> Attribute tag

  Attr_InputMode
    :: Types.InputMode
    -> Attribute tag

  Attr_Is
    :: T.Text
    -> Attribute tag

  Attr_ItemId
    :: T.Text -- TODO
    -> Attribute tag

  Attr_ItemProp
    :: T.Text -- TODO
    -> Attribute tag

  Attr_ItemRef
    :: T.Text -- TODO
    -> Attribute tag

  Attr_ItemScope
    :: T.Text -- TODO
    -> Attribute tag

  Attr_ItemType
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Lang
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Nonce
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Part
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Popover
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Role
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Slot
    :: T.Text -- TODO
    -> Attribute tag

  Attr_Spellcheck
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  Attr_Style
    :: T.Text
    -> Attribute tag

  Attr_TabIndex
    :: Int
    -> Attribute tag

  Attr_Title
    :: T.Text
    -> Attribute tag

  Attr_Translate
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  -- Scoped Attributes
  Attr_Width    :: ValidAttribute 'Width    tag => Int  -> Attribute tag
  Attr_Disabled :: ValidAttribute 'Disabled tag => Bool -> Attribute tag
