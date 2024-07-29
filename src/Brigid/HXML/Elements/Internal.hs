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

import Brigid.HXML.Elements.TagType (TagType(..))

data ChildHXML (parent :: TagType) where
  Tag_NoElement
    :: ChildHXML parent

  Tag_Comment
    :: ChildHXML parent

  Tag_RawHXML
    :: ChildHXML parent

  Tag_CustomHXML
    :: ChildHXML parent

  Tag_Behavior
    :: ChildHXML parent

  Tag_Body
    :: ChildHXML parent

  Tag_DateField
    :: ChildHXML parent

  Tag_Document
    :: ChildHXML parent

  Tag_Form
    :: ChildHXML parent

  Tag_Header
    :: ChildHXML parent

  Tag_Image
    :: ChildHXML parent

  Tag_Item
    :: ChildHXML parent

  Tag_Items
    :: ChildHXML parent

  Tag_List
    :: ChildHXML parent

  Tag_Modifier
    :: ChildHXML parent

  Tag_Navigator
    :: ChildHXML parent

  Tag_NavRoute
    :: ChildHXML parent

  Tag_Option
    :: ChildHXML parent

  Tag_PickerField
    :: ChildHXML parent

  Tag_PickerItem
    :: ChildHXML parent

  Tag_Screen
    :: ChildHXML parent

  Tag_Section
    :: ChildHXML parent

  Tag_SectionList
    :: ChildHXML parent

  Tag_SectionTitle
    :: ChildHXML parent

  Tag_SelectMultiple
    :: ChildHXML parent

  Tag_SelectSingle
    :: ChildHXML parent

  Tag_Spinner
    :: ChildHXML parent

  Tag_Style
    :: ChildHXML parent

  Tag_Styles
    :: ChildHXML parent

  Tag_Switch
    :: ChildHXML parent

  Tag_Text
    :: ChildHXML parent

  Tag_TextArea
    :: ChildHXML parent

  Tag_TextField
    :: ChildHXML parent

  Tag_View
    :: ChildHXML parent

  Tag_WebView
    :: ChildHXML parent
