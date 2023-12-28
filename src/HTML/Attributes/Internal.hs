{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HTML.Attributes.Internal
  ( Attributes
  , Attribute
      ( Attr_Custom
      , Attr_AccessKey
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
  , attributeText
  , buildAttrMap
  ) where

import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T

import HTML.Attributes.AttributeType (AttributeType(..))
import HTML.Attributes.Elements (ValidAttribute)
import HTML.Elements.TagType (TagType)
import HTML.Types qualified as Types

type Attributes tag =
  Map T.Text (Attribute tag)

data Attribute (tag :: TagType) where
  -- Custom Attribute
  Attr_Custom
    :: T.Text
    -> T.Text
    -> Attribute tag

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

attributeText :: Attribute tag -> T.Text
attributeText attr =
  case attr of
    Attr_Custom name _value ->
      name

    -- Attr_AccessKey

    Attr_Autocapitalize _option ->
      "autocapitalize"

    Attr_Autofocus _autofocus ->
      "autofocus"

    Attr_Class _class ->
      "class"

    Attr_ContentEditable _option ->
      "contenteditable"

    Attr_Data data_ _value ->
      "data-" <> data_

    Attr_Dir _directionality ->
      "dir"

    Attr_Draggable _draggable ->
      "draggable"

    Attr_EnterKeyHint _option ->
      "enterkeyhint"

    -- Attr_ExportParts

    Attr_Hidden _hidden ->
      "hidden"

    Attr_Id _id ->
      "id"

    Attr_Inert _inert ->
      "inert"

    Attr_InputMode _mode ->
      "inputmode"

    Attr_Is _is ->
      "is"

    -- Attr_ItemId

    -- Attr_ItemProp

    -- Attr_ItemRef

    -- Attr_ItemScope

    -- Attr_ItemType

    -- Attr_Lang

    -- Attr_Nonce

    -- Attr_Part

    -- Attr_Popover

    -- Attr_Role

    -- Attr_Slot

    Attr_Spellcheck _spellcheck ->
      "spellcheck"

    Attr_Style _style ->
      "style"

    Attr_TabIndex _tabindex ->
      "tabindex"

    Attr_Title _title ->
      "title"

    Attr_Translate _translate ->
      "translate"

    Attr_Width _width ->
      "width"

    Attr_Disabled _disabled ->
      "disabled"

buildAttrMap :: [Attribute tag] -> Attributes tag
buildAttrMap =
  L.foldl'
    (\attrs attr -> Map.insert (attributeText attr) attr attrs)
    Map.empty
