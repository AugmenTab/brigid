{-# LANGUAGE DataKinds #-}

module Brigid.HXML.Elements.TagGroups
  ( AllElements
  , BodyTags
  , FormTags
  , ViewTags
  ) where

import Brigid.HXML.Elements.TagType (TagType (..))
import Brigid.HXML.Internal.TagOperations (Add)

type AllElements =
  [ 'NoElement
  , 'HXML
  , 'Comment
  , 'RawHXML
  , 'CustomHXML
  , 'Behavior
  , 'Body
  , 'DateField
  , 'Document
  , 'Form
  , 'Header
  , 'Image
  , 'Item
  , 'Items
  , 'List
  , 'Modifier
  , 'Navigator
  , 'NavRoute
  , 'Option
  , 'PickerField
  , 'PickerItem
  , 'Screen
  , 'Section
  , 'SectionList
  , 'SectionTitle
  , 'SelectMultiple
  , 'SelectSingle
  , 'Spinner
  , 'Style
  , 'Styles
  , 'Switch
  , 'Text
  , 'TextArea
  , 'TextField
  , 'View
  , 'WebView
  ]

type BodyTags =
  [ 'Form
  , 'Image
  , 'List
  , 'SectionList
  , 'Spinner
  , 'Text
  , 'View
  ]

type FormTags =
  [ 'PickerField
  , 'SelectMultiple
  , 'SelectSingle
  , 'Switch
  , 'TextArea
  , 'TextField
  , 'View
  ]

type ViewTags =
  Add 'Behavior BodyTags
