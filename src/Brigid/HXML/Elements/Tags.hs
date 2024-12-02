{-# LANGUAGE DataKinds #-}

module Brigid.HXML.Elements.Tags
  ( NoElement
  , HXML
  , Comment
  , RawHXML
  , CustomHXML
  , Behavior
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
  ) where

import Brigid.HXML.Elements.TagType qualified as TagType

type NoElement = 'TagType.NoElement

-- This type synonym represents the HyperView (HXML) document itself. It exists
-- so that the Document tag can have a parent. It is important that this tag
-- type synonym never be given a corresponding constructor for `ChildHXML`, and
-- that it never be exported from `HXML.Elements`.
type HXML = 'TagType.HXML

-- | This type synonym represents an HXML comment.
type Comment = 'TagType.Comment

-- | This type synonym represents raw HXML. This content is unchecked and
-- should be considered unsafe. Its intended use-case is for writing
-- out-of-spec HXML.
type RawHXML = 'TagType.RawHXML

-- | This type synonym represents a custom HXML element.
type CustomHXML = 'TagType.CustomHXML

-- | This type synonym represents the @\<behavior>@ element.
type Behavior = 'TagType.Behavior

-- | This type synonym represents the @\<body>@ element.
type Body = 'TagType.Body

-- | This type synonym represents the @\<date-field>@ element.
type DateField = 'TagType.DateField

-- | This type synonym represents the @\<doc>@ element.
type Document = 'TagType.Document

-- | This type synonym represents the @\<form>@ element.
type Form = 'TagType.Form

-- | This type synonym represents the @\<header>@ element.
type Header = 'TagType.Header

-- | This type synonym represents the @\<image>@ element.
type Image = 'TagType.Image

-- | This type synonym represents the @\<item>@ element.
type Item = 'TagType.Item

-- | This type synonym represents the @\<items>@ element.
type Items = 'TagType.Items

-- | This type synonym represents the @\<list>@ element.
type List = 'TagType.List

-- | This type synonym represents the @\<modifier>@ element.
type Modifier = 'TagType.Modifier

-- | This type synonym represents the @\<navigator>@ element.
type Navigator = 'TagType.Navigator

-- | This type synonym represents the @\<nav-route>@ element.
type NavRoute = 'TagType.NavRoute

-- | This type synonym represents the @\<option>@ element.
type Option = 'TagType.Option

-- | This type synonym represents the @\<picker-field>@ element.
type PickerField = 'TagType.PickerField

-- | This type synonym represents the @\<picker-item>@ element.
type PickerItem = 'TagType.PickerItem

-- | This type synonym represents the @\<screen>@ element.
type Screen = 'TagType.Screen

-- | This type synonym represents the @\<section>@ element.
type Section = 'TagType.Section

-- | This type synonym represents the @\<section-list>@ element.
type SectionList = 'TagType.SectionList

-- | This type synonym represents the @\<section-title>@ element.
type SectionTitle = 'TagType.SectionTitle

-- | This type synonym represents the @\<select-multiple>@ element.
type SelectMultiple = 'TagType.SelectMultiple

-- | This type synonym represents the @\<select-single>@ element.
type SelectSingle = 'TagType.SelectSingle

-- | This type synonym represents the @\<spinner>@ element.
type Spinner = 'TagType.Spinner

-- | This type synonym represents the @\<style>@ element.
type Style = 'TagType.Style

-- | This type synonym represents the @\<styles>@ element.
type Styles = 'TagType.Styles

-- | This type synonym represents the @\<switch>@ element.
type Switch = 'TagType.Switch

-- | This type synonym represents the @\<text>@ element.
type Text = 'TagType.Text

-- | This type synonym represents the @\<text-area>@ element.
type TextArea = 'TagType.TextArea

-- | This type synonym represents the @\<text-field>@ element.
type TextField = 'TagType.TextField

-- | This type synonym represents the @\<view>@ element.
type View = 'TagType.View

-- | This type synonym represents the @\<web-view>@ element.
type WebView = 'TagType.WebView
