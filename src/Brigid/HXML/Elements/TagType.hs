{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Elements.TagType
  ( TagErrorMessage
  , TagType
      ( NoElement
      , HXML
      , Comment
      , RawHXML
      , CustomHXML
      , Body
      , DateField
      , Document
      , Form
      , Header
      , Image
      , Item
      , Items
      , List
      , Modifier
      , Navigator
      , NavRoute
      , Option
      , PickerField
      , PickerItem
      , Screen
      , Section
      , SectionList
      , SectionTitle
      , SelectMultiple
      , SelectSingle
      , Spinner
      , Style
      , Styles
      , Switch
      , Text
      , TextArea
      , TextField
      , View
      , WebView
      )
  ) where

import GHC.TypeLits qualified as TypeLits

data TagType
  = NoElement
  | HXML
  | Comment
  | RawHXML
  | CustomHXML
  | Behavior
  | Body
  | DateField
  | Document
  | Form
  | Header
  | Image
  | Item
  | Items
  | List
  | Modifier
  | Navigator
  | NavRoute
  | Option
  | PickerField
  | PickerItem
  | Screen
  | Section
  | SectionList
  | SectionTitle
  | SelectMultiple
  | SelectSingle
  | Spinner
  | Style
  | Styles
  | Switch
  | Text
  | TextArea
  | TextField
  | View
  | WebView

type family TagErrorMessage (tag :: TagType) :: TypeLits.ErrorMessage where
  TagErrorMessage HXML           = 'TypeLits.Text "HXML Document"
  TagErrorMessage Comment        = 'TypeLits.Text "Comment"
  TagErrorMessage RawHXML        = 'TypeLits.Text "RawHXML"
  TagErrorMessage CustomHXML     = 'TypeLits.Text "CustomHXML"
  TagErrorMessage Behavior       = 'TypeLits.Text "Behavior <behavior>"
  TagErrorMessage Body           = 'TypeLits.Text "Body <body>"
  TagErrorMessage DateField      = 'TypeLits.Text "DateField <date-field>"
  TagErrorMessage Document       = 'TypeLits.Text "Document <doc>"
  TagErrorMessage Form           = 'TypeLits.Text "Form <form>"
  TagErrorMessage Header         = 'TypeLits.Text "Header <header>"
  TagErrorMessage Image          = 'TypeLits.Text "Image <image>"
  TagErrorMessage Item           = 'TypeLits.Text "Item <item>"
  TagErrorMessage Items          = 'TypeLits.Text "Items <items>"
  TagErrorMessage List           = 'TypeLits.Text "List <list>"
  TagErrorMessage Modifier       = 'TypeLits.Text "Modifier <modifier>"
  TagErrorMessage Navigator      = 'TypeLits.Text "Navigator <navigator>"
  TagErrorMessage NavRoute       = 'TypeLits.Text "NavRoute <nav-route>"
  TagErrorMessage Option         = 'TypeLits.Text "Option <option>"
  TagErrorMessage PickerField    = 'TypeLits.Text "PickerField <picker-field>"
  TagErrorMessage PickerItem     = 'TypeLits.Text "PickerItem <picker-item>"
  TagErrorMessage Screen         = 'TypeLits.Text "Screen <screen>"
  TagErrorMessage Section        = 'TypeLits.Text "Section <section>"
  TagErrorMessage SectionList    = 'TypeLits.Text "SectionList <section-list>"
  TagErrorMessage SectionTitle   = 'TypeLits.Text "SectionTitle <section-title>"
  TagErrorMessage SelectMultiple = 'TypeLits.Text "SelectMultiple <select-multiple>"
  TagErrorMessage SelectSingle   = 'TypeLits.Text "SelectSingle <select-single>"
  TagErrorMessage Spinner        = 'TypeLits.Text "Spinner <spinner>"
  TagErrorMessage Style          = 'TypeLits.Text "Style <style>"
  TagErrorMessage Styles         = 'TypeLits.Text "Styles <styles>"
  TagErrorMessage Switch         = 'TypeLits.Text "Switch <switch>"
  TagErrorMessage Text           = 'TypeLits.Text "Text <text>"
  TagErrorMessage TextArea       = 'TypeLits.Text "TextArea <text-area>"
  TagErrorMessage TextField      = 'TypeLits.Text "TextField <text-field>"
  TagErrorMessage View           = 'TypeLits.Text "View <view>"
  TagErrorMessage WebView        = 'TypeLits.Text "WebView <web-view>"
