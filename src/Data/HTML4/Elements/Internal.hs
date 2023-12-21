{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML4.Elements.Internal
  ( HTML
  , Document
  , ChildHTML
      ( Tag_NoElement
      , Tag_Comment
      , Tag_Text
      , Tag_Anchor
      , Tag_Abbreviation
      , Tag_ContactAddress
      , Tag_Area
      , Tag_Article
      , Tag_Aside
      , Tag_Audio
      , Tag_BringAttentionTo
      , Tag_Base
      , Tag_BidirectionalIsolation
      , Tag_BidirectionalOverride
      , Tag_Blockquote
      , Tag_Body
      , Tag_LineBreak
      , Tag_Button
      , Tag_Canvas
      , Tag_TableCaption
      , Tag_Citation
      , Tag_Code
      , Tag_TableColumn
      , Tag_TableColumnGroup
      , Tag_Data
      , Tag_DataList
      , Tag_DescriptionDetails
      , Tag_DeletedText
      , Tag_Details
      , Tag_Definition
      , Tag_Dialog
      , Tag_Division
      , Tag_DescriptionList
      , Tag_DescriptionTerm
      , Tag_Emphasis
      , Tag_Embed
      , Tag_Fieldset
      , Tag_FigureCaption
      , Tag_Figure
      , Tag_Footer
      , Tag_Form
      , Tag_H1
      , Tag_H2
      , Tag_H3
      , Tag_H4
      , Tag_H5
      , Tag_H6
      , Tag_Head
      , Tag_Header
      , Tag_HeadingGroup
      , Tag_HorizontalRule
      , Tag_Html
      , Tag_IdiomaticText
      , Tag_IFrame
      , Tag_Image
      , Tag_Input
      , Tag_InsertedText
      , Tag_KeyboardInput
      , Tag_Label
      , Tag_Legend
      , Tag_ListItem
      , Tag_Link
      , Tag_Main
      , Tag_Map
      , Tag_Mark
      , Tag_Menu
      , Tag_Meta
      , Tag_Meter
      , Tag_Nav
      , Tag_NoScript
      , Tag_Object
      , Tag_OrderedList
      , Tag_OptionGroup
      , Tag_Option
      , Tag_Output
      , Tag_Paragraph
      , Tag_Picture
      , Tag_PreformattedText
      , Tag_Progress
      , Tag_Quotation
      , Tag_RubyParenthesis
      , Tag_RubyText
      , Tag_Ruby
      , Tag_Sample
      , Tag_Script
      , Tag_Search
      , Tag_Section
      , Tag_Select
      , Tag_Slot
      , Tag_SideComment
      , Tag_Source
      , Tag_Span
      , Tag_Strikethrough
      , Tag_Strong
      , Tag_Style
      , Tag_Subscript
      , Tag_Summary
      , Tag_Superscript
      , Tag_Table
      , Tag_TableBody
      , Tag_TableDataCell
      , Tag_ContentTemplate
      , Tag_TextArea
      , Tag_TableFoot
      , Tag_TableHeader
      , Tag_TableHead
      , Tag_Time
      , Tag_Title
      , Tag_TableRow
      , Tag_Track
      , Tag_Underline
      , Tag_UnorderedList
      , Tag_Variable
      , Tag_Video
      , Tag_WordBreakOpportunity
      )
  ) where

import Data.Text qualified as T

import Data.HTML4.Attributes.Internal (Attribute)
import Data.HTML4.Elements.Children (ValidChild)
import Data.HTML4.Elements.TagType (TagType(..))

type HTML tag parent =
  ValidChild tag parent => ChildHTML parent

type Document =
  ChildHTML 'Document

data ChildHTML (parent :: TagType) where
  Tag_NoElement
    :: ChildHTML parent

  Tag_Comment
    :: T.Text
    -> ChildHTML parent

  Tag_Text
    :: ValidChild 'Text parent
    => T.Text
    -> ChildHTML parent

  Tag_Anchor
    :: ValidChild 'Anchor parent
    => [Attribute 'Anchor]
    -> [ChildHTML 'Anchor]
    -> ChildHTML parent

  Tag_Abbreviation
    :: ValidChild 'Abbreviation parent
    => [Attribute 'Abbreviation]
    -> [ChildHTML 'Abbreviation]
    -> ChildHTML parent

  Tag_ContactAddress
    :: ValidChild 'ContactAddress parent
    => [Attribute 'ContactAddress]
    -> [ChildHTML 'ContactAddress]
    -> ChildHTML parent

  Tag_Area
    :: ValidChild 'Area parent
    => [Attribute 'Area]
    -> ChildHTML parent

  Tag_Article
    :: ValidChild 'Article parent
    => [Attribute 'Article]
    -> [ChildHTML 'Article]
    -> ChildHTML parent

  Tag_Aside
    :: ValidChild 'Aside parent
    => [Attribute 'Aside]
    -> [ChildHTML 'Aside]
    -> ChildHTML parent

  Tag_Audio
    :: ValidChild 'Audio parent
    => [Attribute 'Audio]
    -> [ChildHTML 'Audio]
    -> ChildHTML parent

  Tag_BringAttentionTo
    :: ValidChild 'BringAttentionTo parent
    => [Attribute 'BringAttentionTo]
    -> [ChildHTML 'BringAttentionTo]
    -> ChildHTML parent

  Tag_Base
    :: ValidChild 'Base parent
    => [Attribute 'Base]
    -> ChildHTML parent

  Tag_BidirectionalIsolation
    :: ValidChild 'BidirectionalIsolation parent
    => [Attribute 'BidirectionalIsolation]
    -> [ChildHTML 'BidirectionalIsolation]
    -> ChildHTML parent

  Tag_BidirectionalOverride
    :: ValidChild 'BidirectionalOverride parent
    => [Attribute 'BidirectionalOverride]
    -> [ChildHTML 'BidirectionalOverride]
    -> ChildHTML parent

  Tag_Blockquote
    :: ValidChild 'Blockquote parent
    => [Attribute 'Blockquote]
    -> [ChildHTML 'Blockquote]
    -> ChildHTML parent

  Tag_Body
    :: ValidChild 'Body parent
    => [Attribute 'Body]
    -> [ChildHTML 'Body]
    -> ChildHTML parent

  Tag_LineBreak
    :: ValidChild 'LineBreak parent
    => [Attribute 'LineBreak]
    -> ChildHTML parent

  Tag_Button
    :: ValidChild 'Button parent
    => [Attribute 'Button]
    -> [ChildHTML 'Button]
    -> ChildHTML parent

  Tag_Canvas
    :: ValidChild 'Canvas parent
    => [Attribute 'Canvas]
    -> [ChildHTML 'Canvas]
    -> ChildHTML parent

  Tag_TableCaption
    :: ValidChild 'TableCaption parent
    => [Attribute 'TableCaption]
    -> [ChildHTML 'TableCaption]
    -> ChildHTML parent

  Tag_Citation
    :: ValidChild 'Citation parent
    => [Attribute 'Citation]
    -> [ChildHTML 'Citation]
    -> ChildHTML parent

  Tag_Code
    :: ValidChild 'Code parent
    => [Attribute 'Code]
    -> [ChildHTML 'Code]
    -> ChildHTML parent

  Tag_TableColumn
    :: ValidChild 'TableColumn parent
    => [Attribute 'TableColumn]
    -> ChildHTML parent

  Tag_TableColumnGroup
    :: ValidChild 'TableColumnGroup parent
    => [Attribute 'TableColumnGroup]
    -> [ChildHTML 'TableColumnGroup]
    -> ChildHTML parent

  Tag_Data
    :: ValidChild 'Data parent
    => [Attribute 'Data]
    -> [ChildHTML 'Data]
    -> ChildHTML parent

  Tag_DataList
    :: ValidChild 'DataList parent
    => [Attribute 'DataList]
    -> [ChildHTML 'DataList]
    -> ChildHTML parent

  Tag_DescriptionDetails
    :: ValidChild 'DescriptionDetails parent
    => [Attribute 'DescriptionDetails]
    -> [ChildHTML 'DescriptionDetails]
    -> ChildHTML parent

  Tag_DeletedText
    :: ValidChild 'DeletedText parent
    => [Attribute 'DeletedText]
    -> [ChildHTML parent]
    -> ChildHTML parent

  Tag_Details
    :: ValidChild 'Details parent
    => [Attribute 'Details]
    -> [ChildHTML 'Details]
    -> ChildHTML parent

  Tag_Definition
    :: ValidChild 'Definition parent
    => [Attribute 'Definition]
    -> [ChildHTML 'Definition]
    -> ChildHTML parent

  Tag_Dialog
    :: ValidChild 'Dialog parent
    => [Attribute 'Dialog]
    -> [ChildHTML 'Dialog]
    -> ChildHTML parent

  Tag_Division
    :: ValidChild 'Division parent
    => [Attribute 'Division]
    -> [ChildHTML 'Division]
    -> ChildHTML parent

  Tag_DescriptionList
    :: ValidChild 'DescriptionList parent
    => [Attribute 'DescriptionList]
    -> [ChildHTML 'DescriptionList]
    -> ChildHTML parent

  Tag_DescriptionTerm
    :: ValidChild 'DescriptionTerm parent
    => [Attribute 'DescriptionTerm]
    -> [ChildHTML 'DescriptionTerm]
    -> ChildHTML parent

  Tag_Emphasis
    :: ValidChild 'Emphasis parent
    => [Attribute 'Emphasis]
    -> [ChildHTML 'Emphasis]
    -> ChildHTML parent

  Tag_Embed
    :: ValidChild 'Embed parent
    => [Attribute 'Embed]
    -> ChildHTML parent

  Tag_Fieldset
    :: ValidChild 'Fieldset parent
    => [Attribute 'Fieldset]
    -> [ChildHTML 'Fieldset]
    -> ChildHTML parent

  Tag_FigureCaption
    :: ValidChild 'FigureCaption parent
    => [Attribute 'FigureCaption]
    -> [ChildHTML 'FigureCaption]
    -> ChildHTML parent

  Tag_Figure
    :: ValidChild 'Figure parent
    => [Attribute 'Figure]
    -> [ChildHTML 'Figure]
    -> ChildHTML parent

  Tag_Footer
    :: ValidChild 'Footer parent
    => [Attribute 'Footer]
    -> [ChildHTML 'Footer]
    -> ChildHTML parent

  Tag_Form
    :: ValidChild 'Form parent
    => [Attribute 'Form]
    -> [ChildHTML 'Form]
    -> ChildHTML parent

  Tag_H1
    :: ValidChild 'H1 parent
    => [Attribute 'H1]
    -> [ChildHTML 'H1]
    -> ChildHTML parent

  Tag_H2
    :: ValidChild 'H2 parent
    => [Attribute 'H2]
    -> [ChildHTML 'H2]
    -> ChildHTML parent

  Tag_H3
    :: ValidChild 'H3 parent
    => [Attribute 'H3]
    -> [ChildHTML 'H3]
    -> ChildHTML parent

  Tag_H4
    :: ValidChild 'H4 parent
    => [Attribute 'H4]
    -> [ChildHTML 'H4]
    -> ChildHTML parent

  Tag_H5
    :: ValidChild 'H5 parent
    => [Attribute 'H5]
    -> [ChildHTML 'H5]
    -> ChildHTML parent

  Tag_H6
    :: ValidChild 'H6 parent
    => [Attribute 'H6]
    -> [ChildHTML 'H6]
    -> ChildHTML parent

  Tag_Head
    :: ValidChild 'Head parent
    => [Attribute 'Head]
    -> [ChildHTML 'Head]
    -> ChildHTML parent

  Tag_Header
    :: ValidChild 'Header parent
    => [Attribute 'Header]
    -> [ChildHTML 'Header]
    -> ChildHTML parent

  Tag_HeadingGroup
    :: ValidChild 'HeadingGroup parent
    => [Attribute 'HeadingGroup]
    -> [ChildHTML 'HeadingGroup]
    -> ChildHTML parent

  Tag_HorizontalRule
    :: ValidChild 'HorizontalRule parent
    => [Attribute 'HorizontalRule]
    -> ChildHTML parent

  Tag_Html
    :: ValidChild 'Html parent
    => [Attribute 'Html]
    -> [ChildHTML 'Html]
    -> ChildHTML parent

  Tag_IdiomaticText
    :: ValidChild 'IdiomaticText parent
    => [Attribute 'IdiomaticText]
    -> [ChildHTML 'IdiomaticText]
    -> ChildHTML parent

  Tag_IFrame
    :: ValidChild 'IFrame parent
    => [Attribute 'IFrame]
    -> ChildHTML parent

  Tag_Image
    :: ValidChild 'Image parent
    => [Attribute 'Image]
    -> ChildHTML parent

  Tag_Input
    :: ValidChild 'Input parent
    => [Attribute 'Input]
    -> ChildHTML parent

  Tag_InsertedText
    :: ValidChild 'InsertedText parent
    => [Attribute 'InsertedText]
    -> [ChildHTML parent]
    -> ChildHTML parent

  Tag_KeyboardInput
    :: ValidChild 'KeyboardInput parent
    => [Attribute 'KeyboardInput]
    -> [ChildHTML 'KeyboardInput]
    -> ChildHTML parent

  Tag_Label
    :: ValidChild 'Label parent
    => [Attribute 'Label]
    -> [ChildHTML 'Label]
    -> ChildHTML parent

  Tag_Legend
    :: ValidChild 'Legend parent
    => [Attribute 'Legend]
    -> [ChildHTML 'Legend]
    -> ChildHTML parent

  Tag_ListItem
    :: ValidChild 'ListItem parent
    => [Attribute 'ListItem]
    -> [ChildHTML 'ListItem]
    -> ChildHTML parent

  Tag_Link
    :: ValidChild 'Link parent
    => [Attribute 'Link]
    -> ChildHTML parent

  Tag_Main
    :: ValidChild 'Main parent
    => [Attribute 'Main]
    -> [ChildHTML 'Main]
    -> ChildHTML parent

  Tag_Map
    :: ValidChild 'Map parent
    => [Attribute 'Map]
    -> [ChildHTML 'Map]
    -> ChildHTML parent

  Tag_Mark
    :: ValidChild 'Mark parent
    => [Attribute 'Mark]
    -> [ChildHTML 'Mark]
    -> ChildHTML parent

  Tag_Menu
    :: ValidChild 'Menu parent
    => [Attribute 'Menu]
    -> [ChildHTML 'Menu]
    -> ChildHTML parent

  Tag_Meta
    :: ValidChild 'Meta parent
    => [Attribute 'Meta]
    -> ChildHTML parent

  Tag_Meter
    :: ValidChild 'Meter parent
    => [Attribute 'Meter]
    -> [ChildHTML 'Meter]
    -> ChildHTML parent

  Tag_Nav
    :: ValidChild 'Nav parent
    => [Attribute 'Nav]
    -> [ChildHTML 'Nav]
    -> ChildHTML parent

  Tag_NoScript
    :: ValidChild 'NoScript parent
    => [Attribute 'NoScript]
    -> [ChildHTML 'NoScript]
    -> ChildHTML parent

  Tag_Object
    :: ValidChild 'Object parent
    => [Attribute 'Object]
    -> [ChildHTML 'Object]
    -> ChildHTML parent

  Tag_OrderedList
    :: ValidChild 'OrderedList parent
    => [Attribute 'OrderedList]
    -> [ChildHTML 'OrderedList]
    -> ChildHTML parent

  Tag_OptionGroup
    :: ValidChild 'OptionGroup parent
    => [Attribute 'OptionGroup]
    -> [ChildHTML 'OptionGroup]
    -> ChildHTML parent

  Tag_Option
    :: ValidChild 'Option parent
    => [Attribute 'Option]
    -> [ChildHTML 'Option]
    -> ChildHTML parent

  Tag_Output
    :: ValidChild 'Output parent
    => [Attribute 'Output]
    -> [ChildHTML 'Output]
    -> ChildHTML parent

  Tag_Paragraph
    :: ValidChild 'Paragraph parent
    => [Attribute 'Paragraph]
    -> [ChildHTML 'Paragraph]
    -> ChildHTML parent

  Tag_Picture
    :: ValidChild 'Picture parent
    => [Attribute 'Picture]
    -> [ChildHTML 'Picture]
    -> ChildHTML parent

  Tag_PreformattedText
    :: ValidChild 'PreformattedText parent
    => [Attribute 'PreformattedText]
    -> [ChildHTML 'PreformattedText]
    -> ChildHTML parent

  Tag_Progress
    :: ValidChild 'Progress parent
    => [Attribute 'Progress]
    -> [ChildHTML 'Progress]
    -> ChildHTML parent

  Tag_Quotation
    :: ValidChild 'Quotation parent
    => [Attribute 'Quotation]
    -> [ChildHTML 'Quotation]
    -> ChildHTML parent

  Tag_RubyParenthesis
    :: ValidChild 'RubyParenthesis parent
    => [Attribute 'RubyParenthesis]
    -> [ChildHTML 'RubyParenthesis]
    -> ChildHTML parent

  Tag_RubyText
    :: ValidChild 'RubyText parent
    => [Attribute 'RubyText]
    -> [ChildHTML 'RubyText]
    -> ChildHTML parent

  Tag_Ruby
    :: ValidChild 'Ruby parent
    => [Attribute 'Ruby]
    -> [ChildHTML 'Ruby]
    -> ChildHTML parent

  Tag_Strikethrough
    :: ValidChild 'Strikethrough parent
    => [Attribute 'Strikethrough]
    -> [ChildHTML 'Strikethrough]
    -> ChildHTML parent

  Tag_Sample
    :: ValidChild 'Sample parent
    => [Attribute 'Sample]
    -> [ChildHTML 'Sample]
    -> ChildHTML parent

  Tag_Script
    :: ValidChild 'Script parent
    => [Attribute 'Script]
    -> [ChildHTML 'Script]
    -> ChildHTML parent

  Tag_Search
    :: ValidChild 'Search parent
    => [Attribute 'Search]
    -> [ChildHTML 'Search]
    -> ChildHTML parent

  Tag_Section
    :: ValidChild 'Section parent
    => [Attribute 'Section]
    -> [ChildHTML 'Section]
    -> ChildHTML parent

  Tag_Select
    :: ValidChild 'Select parent
    => [Attribute 'Select]
    -> [ChildHTML 'Select]
    -> ChildHTML parent

  Tag_Slot
    :: ValidChild 'Slot parent
    => [Attribute 'Slot]
    -> [ChildHTML parent]
    -> ChildHTML parent

  Tag_SideComment
    :: ValidChild 'SideComment parent
    => [Attribute 'SideComment]
    -> [ChildHTML 'SideComment]
    -> ChildHTML parent

  Tag_Source
    :: ValidChild 'Source parent
    => [Attribute 'Source]
    -> ChildHTML parent

  Tag_Span
    :: ValidChild 'Span parent
    => [Attribute 'Span]
    -> [ChildHTML 'Span]
    -> ChildHTML parent

  Tag_Strong
    :: ValidChild 'Strong parent
    => [Attribute 'Strong]
    -> [ChildHTML 'Strong]
    -> ChildHTML parent

  Tag_Style
    :: ValidChild 'Style parent
    => [Attribute 'Style]
    -> [ChildHTML 'Style]
    -> ChildHTML parent

  Tag_Subscript
    :: ValidChild 'Subscript parent
    => [Attribute 'Subscript]
    -> [ChildHTML 'Subscript]
    -> ChildHTML parent

  Tag_Summary
    :: ValidChild 'Summary parent
    => [Attribute 'Summary]
    -> [ChildHTML 'Summary]
    -> ChildHTML parent

  Tag_Superscript
    :: ValidChild 'Superscript parent
    => [Attribute 'Superscript]
    -> [ChildHTML 'Superscript]
    -> ChildHTML parent

  Tag_Table
    :: ValidChild 'Table parent
    => [Attribute 'Table]
    -> [ChildHTML 'Table]
    -> ChildHTML parent

  Tag_TableBody
    :: ValidChild 'TableBody parent
    => [Attribute 'TableBody]
    -> [ChildHTML 'TableBody]
    -> ChildHTML parent

  Tag_TableDataCell
    :: ValidChild 'TableDataCell parent
    => [Attribute 'TableDataCell]
    -> [ChildHTML 'TableDataCell]
    -> ChildHTML parent

  Tag_ContentTemplate
    :: ValidChild 'ContentTemplate parent
    => [Attribute 'ContentTemplate]
    -> [ChildHTML 'ContentTemplate]
    -> ChildHTML parent

  Tag_TextArea
    :: ValidChild 'TextArea parent
    => [Attribute 'TextArea]
    -> [ChildHTML 'TextArea]
    -> ChildHTML parent

  Tag_TableFoot
    :: ValidChild 'TableFoot parent
    => [Attribute 'TableFoot]
    -> [ChildHTML 'TableFoot]
    -> ChildHTML parent

  Tag_TableHeader
    :: ValidChild 'TableHeader parent
    => [Attribute 'TableHeader]
    -> [ChildHTML 'TableHeader]
    -> ChildHTML parent

  Tag_TableHead
    :: ValidChild 'TableHead parent
    => [Attribute 'TableHead]
    -> [ChildHTML 'TableHead]
    -> ChildHTML parent

  Tag_Time
    :: ValidChild 'Time parent
    => [Attribute 'Time]
    -> [ChildHTML 'Time]
    -> ChildHTML parent

  Tag_Title
    :: ValidChild 'Title parent
    => [Attribute 'Title]
    -> [ChildHTML 'Title]
    -> ChildHTML parent

  Tag_TableRow
    :: ValidChild 'TableRow parent
    => [Attribute 'TableRow]
    -> [ChildHTML 'TableRow]
    -> ChildHTML parent

  Tag_Track
    :: ValidChild 'Track parent
    => [Attribute 'Track]
    -> ChildHTML parent

  Tag_Underline
    :: ValidChild 'Underline parent
    => [Attribute 'Underline]
    -> [ChildHTML 'Underline]
    -> ChildHTML parent

  Tag_UnorderedList
    :: ValidChild 'UnorderedList parent
    => [Attribute 'UnorderedList]
    -> [ChildHTML 'UnorderedList]
    -> ChildHTML parent

  Tag_Variable
    :: ValidChild 'Variable parent
    => [Attribute 'Variable]
    -> [ChildHTML 'Variable]
    -> ChildHTML parent

  Tag_Video
    :: ValidChild 'Video parent
    => [Attribute 'Video]
    -> [ChildHTML 'Video]
    -> ChildHTML parent

  Tag_WordBreakOpportunity
    :: ValidChild 'WordBreakOpportunity parent
    => [Attribute 'WordBreakOpportunity]
    -> ChildHTML parent
