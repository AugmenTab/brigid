{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Elements.Internal
  ( ChildHXML
      ( Tag_NoElement
      , Tag_Comment
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

import Brigid.HXML.Elements.Children (ValidChild)
import Brigid.HXML.Elements.TagType (TagType (..))
import Brigid.HXML.Types (NoContent)

data ChildHXML (parent :: TagType) where
  Tag_NoElement
    :: ChildHXML parent

  Tag_Comment
    :: T.Text
    -> ChildHXML parent

  Tag_RawHXML
    :: T.Text
    -> ChildHXML parent

  Tag_CustomHXML
    :: T.Text
    -> Either NoContent [ChildHXML 'CustomHXML]
    -> ChildHXML parent

  Tag_Behavior
    :: ChildHXML parent

  Tag_Body
    :: ValidChild 'Body parent
    => [ChildHXML 'Body]
    -> ChildHXML parent

  Tag_DateField
    :: ChildHXML parent

  Tag_Document
    :: ValidChild 'Document parent
    => [ChildHXML 'Document]
    -> ChildHXML parent

  Tag_Form
    :: ValidChild 'Form parent
    => [ChildHXML 'Form]
    -> ChildHXML parent

  Tag_Header
    :: ValidChild 'Header parent
    => [ChildHXML 'Header]
    -> ChildHXML parent

  Tag_Image
    :: ChildHXML parent

  Tag_Item
    :: ValidChild 'Item parent
    => [ChildHXML 'Item]
    -> ChildHXML parent

  Tag_Items
    :: ValidChild 'Items parent
    => [ChildHXML 'Items]
    -> ChildHXML parent

  Tag_List
    :: ValidChild 'List parent
    => [ChildHXML 'List]
    -> ChildHXML parent

  Tag_Modifier
    :: ValidChild 'Modifier parent
    => [ChildHXML 'Modifier]
    -> ChildHXML parent

  Tag_Navigator
    :: ValidChild 'Navigator parent
    => [ChildHXML 'Navigator]
    -> ChildHXML parent

  Tag_NavRoute
    :: ChildHXML parent

  Tag_Option
    :: ValidChild 'Option parent
    => [ChildHXML 'Option]
    -> ChildHXML parent

  Tag_PickerField
    :: ValidChild 'PickerField parent
    => [ChildHXML 'PickerField]
    -> ChildHXML parent

  Tag_PickerItem
    :: ChildHXML parent

  Tag_Screen
    :: ValidChild 'Screen parent
    => [ChildHXML 'Screen]
    -> ChildHXML parent

  Tag_Section
    :: ChildHXML parent

  Tag_SectionList
    :: ValidChild 'SectionList parent
    => [ChildHXML 'SectionList]
    -> ChildHXML parent

  Tag_SectionTitle
    :: ValidChild 'SectionTitle parent
    => [ChildHXML 'SectionTitle]
    -> ChildHXML parent

  Tag_SelectMultiple
    :: ValidChild 'SelectMultiple parent
    => [ChildHXML 'SelectMultiple]
    -> ChildHXML parent

  Tag_SelectSingle
    :: ValidChild 'SelectSingle parent
    => [ChildHXML 'SelectSingle]
    -> ChildHXML parent

  Tag_Spinner
    :: ChildHXML parent

  Tag_Style
    :: ValidChild 'Style parent
    => [ChildHXML 'Style]
    -> ChildHXML parent

  Tag_Styles
    :: ValidChild 'Styles parent
    => [ChildHXML 'Styles]
    -> ChildHXML parent

  Tag_Switch
    :: ChildHXML parent

  Tag_Text
    :: ChildHXML parent

  Tag_TextArea
    :: ChildHXML parent

  Tag_TextField
    :: ChildHXML parent

  Tag_View
    :: ValidChild 'View parent
    => [ChildHXML 'View]
    -> ChildHXML parent

  Tag_WebView
    :: ChildHXML parent
