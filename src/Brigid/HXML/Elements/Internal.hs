{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Elements.Internal
  ( ChildHXML
      ( Tag_NoElement
      , Tag_Comment
      , Tag_Content
      , Tag_Entity
      , Tag_RawHXML
      , Tag_CustomHXML
      , Tag_Behavior
      , Tag_Body
      , Tag_DateField
      , Tag_Document
      , Tag_Form
      , Tag_Header
      , Tag_Image
      , Tag_Item
      , Tag_Items
      , Tag_List
      , Tag_Modifier
      , Tag_Navigator
      , Tag_NavRoute
      , Tag_Option
      , Tag_PickerField
      , Tag_PickerItem
      , Tag_Screen
      , Tag_Section
      , Tag_SectionList
      , Tag_SectionTitle
      , Tag_SelectMultiple
      , Tag_SelectSingle
      , Tag_Spinner
      , Tag_Style
      , Tag_Styles
      , Tag_Switch
      , Tag_Text
      , Tag_TextArea
      , Tag_TextField
      , Tag_View
      , Tag_WebView
      )
  ) where

import Data.Text qualified as T

import Brigid.HXML.Attributes.Internal (Attribute)
import Brigid.HXML.Elements.Children (ValidChild)
import Brigid.HXML.Elements.TagType (TagType (..))
import Brigid.HXML.Types (NoContent)

data ChildHXML (parent :: TagType) where
  Tag_NoElement
    :: ChildHXML parent

  Tag_Comment
    :: T.Text
    -> ChildHXML parent

  Tag_Content
    :: ValidChild 'Content parent
    => T.Text
    -> ChildHXML parent

  Tag_Entity
    :: ValidChild 'Content parent
    => String
    -> ChildHXML parent

  Tag_RawHXML
    :: T.Text
    -> ChildHXML parent

  Tag_CustomHXML
    :: T.Text
    -> [Attribute 'CustomHXML]
    -> Either NoContent [ChildHXML 'CustomHXML]
    -> ChildHXML parent

  Tag_Behavior
    :: [Attribute 'Behavior]
    -> ChildHXML parent

  Tag_Body
    :: ValidChild 'Body parent
    => [Attribute 'Body]
    -> [ChildHXML 'Body]
    -> ChildHXML parent

  Tag_DateField
    :: [Attribute 'DateField]
    -> ChildHXML parent

  Tag_Document
    :: ValidChild 'Document parent
    => [Attribute 'Document]
    -> [ChildHXML 'Document]
    -> ChildHXML parent

  Tag_Form
    :: ValidChild 'Form parent
    => [Attribute 'Form]
    -> [ChildHXML 'Form]
    -> ChildHXML parent

  Tag_Header
    :: ValidChild 'Header parent
    => [Attribute 'Header]
    -> [ChildHXML 'Header]
    -> ChildHXML parent

  Tag_Image
    :: [Attribute 'Image]
    -> ChildHXML parent

  Tag_Item
    :: ValidChild 'Item parent
    => [Attribute 'Item]
    -> [ChildHXML 'Item]
    -> ChildHXML parent

  Tag_Items
    :: ValidChild 'Items parent
    => [Attribute 'Items]
    -> [ChildHXML 'Items]
    -> ChildHXML parent

  Tag_List
    :: ValidChild 'List parent
    => [Attribute 'List]
    -> [ChildHXML 'List]
    -> ChildHXML parent

  Tag_Modifier
    :: ValidChild 'Modifier parent
    => [Attribute 'Modifier]
    -> [ChildHXML 'Modifier]
    -> ChildHXML parent

  Tag_Navigator
    :: ValidChild 'Navigator parent
    => [Attribute 'Navigator]
    -> [ChildHXML 'Navigator]
    -> ChildHXML parent

  Tag_NavRoute
    :: [Attribute 'NavRoute]
    -> ChildHXML parent

  Tag_Option
    :: ValidChild 'Option parent
    => [Attribute 'Option]
    -> [ChildHXML 'Option]
    -> ChildHXML parent

  Tag_PickerField
    :: ValidChild 'PickerField parent
    => [Attribute 'PickerField]
    -> [ChildHXML 'PickerField]
    -> ChildHXML parent

  Tag_PickerItem
    :: [Attribute 'PickerItem]
    -> ChildHXML parent

  Tag_Screen
    :: ValidChild 'Screen parent
    => [Attribute 'Screen]
    -> [ChildHXML 'Screen]
    -> ChildHXML parent

  Tag_Section
    :: [Attribute 'Section]
    -> ChildHXML parent

  Tag_SectionList
    :: ValidChild 'SectionList parent
    => [Attribute 'SectionList]
    -> [ChildHXML 'SectionList]
    -> ChildHXML parent

  Tag_SectionTitle
    :: ValidChild 'SectionTitle parent
    => [Attribute 'SectionTitle]
    -> [ChildHXML 'SectionTitle]
    -> ChildHXML parent

  Tag_SelectMultiple
    :: ValidChild 'SelectMultiple parent
    => [Attribute 'SelectMultiple]
    -> [ChildHXML 'SelectMultiple]
    -> ChildHXML parent

  Tag_SelectSingle
    :: ValidChild 'SelectSingle parent
    => [Attribute 'SelectSingle]
    -> [ChildHXML 'SelectSingle]
    -> ChildHXML parent

  Tag_Spinner
    :: [Attribute 'Spinner]
    -> ChildHXML parent

  Tag_Style
    :: ValidChild 'Style parent
    => [Attribute 'Style]
    -> [ChildHXML 'Style]
    -> ChildHXML parent

  Tag_Styles
    :: ValidChild 'Styles parent
    => [Attribute 'Styles]
    -> [ChildHXML 'Styles]
    -> ChildHXML parent

  Tag_Switch
    :: [Attribute 'Switch]
    -> ChildHXML parent

  Tag_Text
    :: ValidChild 'Text parent
    => [Attribute 'Text]
    -> [ChildHXML 'Text]
    -> ChildHXML parent

  Tag_TextArea
    :: [Attribute 'TextArea]
    -> ChildHXML parent

  Tag_TextField
    :: [Attribute 'TextField]
    -> ChildHXML parent

  Tag_View
    :: ValidChild 'View parent
    => [Attribute 'View]
    -> [ChildHXML 'View]
    -> ChildHXML parent

  Tag_WebView
    :: [Attribute 'WebView]
    -> ChildHXML parent
