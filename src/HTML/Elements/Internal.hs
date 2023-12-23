{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HTML.Elements.Internal
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

import HTML.Attributes.Internal (Attributes)
import HTML.Elements.Children (ValidChild)
import HTML.Elements.TagType (TagType(..))

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
    => Attributes 'Anchor
    -> [ChildHTML 'Anchor]
    -> ChildHTML parent

  Tag_Abbreviation
    :: ValidChild 'Abbreviation parent
    => Attributes 'Abbreviation
    -> [ChildHTML 'Abbreviation]
    -> ChildHTML parent

  Tag_ContactAddress
    :: ValidChild 'ContactAddress parent
    => Attributes 'ContactAddress
    -> [ChildHTML 'ContactAddress]
    -> ChildHTML parent

  Tag_Area
    :: ValidChild 'Area parent
    => Attributes 'Area
    -> ChildHTML parent

  Tag_Article
    :: ValidChild 'Article parent
    => Attributes 'Article
    -> [ChildHTML 'Article]
    -> ChildHTML parent

  Tag_Aside
    :: ValidChild 'Aside parent
    => Attributes 'Aside
    -> [ChildHTML 'Aside]
    -> ChildHTML parent

  Tag_Audio
    :: ValidChild 'Audio parent
    => Attributes 'Audio
    -> [ChildHTML 'Audio]
    -> ChildHTML parent

  Tag_BringAttentionTo
    :: ValidChild 'BringAttentionTo parent
    => Attributes 'BringAttentionTo
    -> [ChildHTML 'BringAttentionTo]
    -> ChildHTML parent

  Tag_Base
    :: ValidChild 'Base parent
    => Attributes 'Base
    -> ChildHTML parent

  Tag_BidirectionalIsolation
    :: ValidChild 'BidirectionalIsolation parent
    => Attributes 'BidirectionalIsolation
    -> [ChildHTML 'BidirectionalIsolation]
    -> ChildHTML parent

  Tag_BidirectionalOverride
    :: ValidChild 'BidirectionalOverride parent
    => Attributes 'BidirectionalOverride
    -> [ChildHTML 'BidirectionalOverride]
    -> ChildHTML parent

  Tag_Blockquote
    :: ValidChild 'Blockquote parent
    => Attributes 'Blockquote
    -> [ChildHTML 'Blockquote]
    -> ChildHTML parent

  Tag_Body
    :: ValidChild 'Body parent
    => Attributes 'Body
    -> [ChildHTML 'Body]
    -> ChildHTML parent

  Tag_LineBreak
    :: ValidChild 'LineBreak parent
    => Attributes 'LineBreak
    -> ChildHTML parent

  Tag_Button
    :: ValidChild 'Button parent
    => Attributes 'Button
    -> [ChildHTML 'Button]
    -> ChildHTML parent

  Tag_Canvas
    :: ValidChild 'Canvas parent
    => Attributes 'Canvas
    -> [ChildHTML 'Canvas]
    -> ChildHTML parent

  Tag_TableCaption
    :: ValidChild 'TableCaption parent
    => Attributes 'TableCaption
    -> [ChildHTML 'TableCaption]
    -> ChildHTML parent

  Tag_Citation
    :: ValidChild 'Citation parent
    => Attributes 'Citation
    -> [ChildHTML 'Citation]
    -> ChildHTML parent

  Tag_Code
    :: ValidChild 'Code parent
    => Attributes 'Code
    -> [ChildHTML 'Code]
    -> ChildHTML parent

  Tag_TableColumn
    :: ValidChild 'TableColumn parent
    => Attributes 'TableColumn
    -> ChildHTML parent

  Tag_TableColumnGroup
    :: ValidChild 'TableColumnGroup parent
    => Attributes 'TableColumnGroup
    -> [ChildHTML 'TableColumnGroup]
    -> ChildHTML parent

  Tag_Data
    :: ValidChild 'Data parent
    => Attributes 'Data
    -> [ChildHTML 'Data]
    -> ChildHTML parent

  Tag_DataList
    :: ValidChild 'DataList parent
    => Attributes 'DataList
    -> [ChildHTML 'DataList]
    -> ChildHTML parent

  Tag_DescriptionDetails
    :: ValidChild 'DescriptionDetails parent
    => Attributes 'DescriptionDetails
    -> [ChildHTML 'DescriptionDetails]
    -> ChildHTML parent

  Tag_DeletedText
    :: ValidChild 'DeletedText parent
    => Attributes 'DeletedText
    -> [ChildHTML parent]
    -> ChildHTML parent

  Tag_Details
    :: ValidChild 'Details parent
    => Attributes 'Details
    -> [ChildHTML 'Details]
    -> ChildHTML parent

  Tag_Definition
    :: ValidChild 'Definition parent
    => Attributes 'Definition
    -> [ChildHTML 'Definition]
    -> ChildHTML parent

  Tag_Dialog
    :: ValidChild 'Dialog parent
    => Attributes 'Dialog
    -> [ChildHTML 'Dialog]
    -> ChildHTML parent

  Tag_Division
    :: ValidChild 'Division parent
    => Attributes 'Division
    -> [ChildHTML 'Division]
    -> ChildHTML parent

  Tag_DescriptionList
    :: ValidChild 'DescriptionList parent
    => Attributes 'DescriptionList
    -> [ChildHTML 'DescriptionList]
    -> ChildHTML parent

  Tag_DescriptionTerm
    :: ValidChild 'DescriptionTerm parent
    => Attributes 'DescriptionTerm
    -> [ChildHTML 'DescriptionTerm]
    -> ChildHTML parent

  Tag_Emphasis
    :: ValidChild 'Emphasis parent
    => Attributes 'Emphasis
    -> [ChildHTML 'Emphasis]
    -> ChildHTML parent

  Tag_Embed
    :: ValidChild 'Embed parent
    => Attributes 'Embed
    -> ChildHTML parent

  Tag_Fieldset
    :: ValidChild 'Fieldset parent
    => Attributes 'Fieldset
    -> [ChildHTML 'Fieldset]
    -> ChildHTML parent

  Tag_FigureCaption
    :: ValidChild 'FigureCaption parent
    => Attributes 'FigureCaption
    -> [ChildHTML 'FigureCaption]
    -> ChildHTML parent

  Tag_Figure
    :: ValidChild 'Figure parent
    => Attributes 'Figure
    -> [ChildHTML 'Figure]
    -> ChildHTML parent

  Tag_Footer
    :: ValidChild 'Footer parent
    => Attributes 'Footer
    -> [ChildHTML 'Footer]
    -> ChildHTML parent

  Tag_Form
    :: ValidChild 'Form parent
    => Attributes 'Form
    -> [ChildHTML 'Form]
    -> ChildHTML parent

  Tag_H1
    :: ValidChild 'H1 parent
    => Attributes 'H1
    -> [ChildHTML 'H1]
    -> ChildHTML parent

  Tag_H2
    :: ValidChild 'H2 parent
    => Attributes 'H2
    -> [ChildHTML 'H2]
    -> ChildHTML parent

  Tag_H3
    :: ValidChild 'H3 parent
    => Attributes 'H3
    -> [ChildHTML 'H3]
    -> ChildHTML parent

  Tag_H4
    :: ValidChild 'H4 parent
    => Attributes 'H4
    -> [ChildHTML 'H4]
    -> ChildHTML parent

  Tag_H5
    :: ValidChild 'H5 parent
    => Attributes 'H5
    -> [ChildHTML 'H5]
    -> ChildHTML parent

  Tag_H6
    :: ValidChild 'H6 parent
    => Attributes 'H6
    -> [ChildHTML 'H6]
    -> ChildHTML parent

  Tag_Head
    :: ValidChild 'Head parent
    => Attributes 'Head
    -> [ChildHTML 'Head]
    -> ChildHTML parent

  Tag_Header
    :: ValidChild 'Header parent
    => Attributes 'Header
    -> [ChildHTML 'Header]
    -> ChildHTML parent

  Tag_HeadingGroup
    :: ValidChild 'HeadingGroup parent
    => Attributes 'HeadingGroup
    -> [ChildHTML 'HeadingGroup]
    -> ChildHTML parent

  Tag_HorizontalRule
    :: ValidChild 'HorizontalRule parent
    => Attributes 'HorizontalRule
    -> ChildHTML parent

  Tag_Html
    :: ValidChild 'Html parent
    => Attributes 'Html
    -> [ChildHTML 'Html]
    -> ChildHTML parent

  Tag_IdiomaticText
    :: ValidChild 'IdiomaticText parent
    => Attributes 'IdiomaticText
    -> [ChildHTML 'IdiomaticText]
    -> ChildHTML parent

  Tag_IFrame
    :: ValidChild 'IFrame parent
    => Attributes 'IFrame
    -> ChildHTML parent

  Tag_Image
    :: ValidChild 'Image parent
    => Attributes 'Image
    -> ChildHTML parent

  Tag_Input
    :: ValidChild 'Input parent
    => Attributes 'Input
    -> ChildHTML parent

  Tag_InsertedText
    :: ValidChild 'InsertedText parent
    => Attributes 'InsertedText
    -> [ChildHTML parent]
    -> ChildHTML parent

  Tag_KeyboardInput
    :: ValidChild 'KeyboardInput parent
    => Attributes 'KeyboardInput
    -> [ChildHTML 'KeyboardInput]
    -> ChildHTML parent

  Tag_Label
    :: ValidChild 'Label parent
    => Attributes 'Label
    -> [ChildHTML 'Label]
    -> ChildHTML parent

  Tag_Legend
    :: ValidChild 'Legend parent
    => Attributes 'Legend
    -> [ChildHTML 'Legend]
    -> ChildHTML parent

  Tag_ListItem
    :: ValidChild 'ListItem parent
    => Attributes 'ListItem
    -> [ChildHTML 'ListItem]
    -> ChildHTML parent

  Tag_Link
    :: ValidChild 'Link parent
    => Attributes 'Link
    -> ChildHTML parent

  Tag_Main
    :: ValidChild 'Main parent
    => Attributes 'Main
    -> [ChildHTML 'Main]
    -> ChildHTML parent

  Tag_Map
    :: ValidChild 'Map parent
    => Attributes 'Map
    -> [ChildHTML 'Map]
    -> ChildHTML parent

  Tag_Mark
    :: ValidChild 'Mark parent
    => Attributes 'Mark
    -> [ChildHTML 'Mark]
    -> ChildHTML parent

  Tag_Menu
    :: ValidChild 'Menu parent
    => Attributes 'Menu
    -> [ChildHTML 'Menu]
    -> ChildHTML parent

  Tag_Meta
    :: ValidChild 'Meta parent
    => Attributes 'Meta
    -> ChildHTML parent

  Tag_Meter
    :: ValidChild 'Meter parent
    => Attributes 'Meter
    -> [ChildHTML 'Meter]
    -> ChildHTML parent

  Tag_Nav
    :: ValidChild 'Nav parent
    => Attributes 'Nav
    -> [ChildHTML 'Nav]
    -> ChildHTML parent

  Tag_NoScript
    :: ValidChild 'NoScript parent
    => Attributes 'NoScript
    -> [ChildHTML 'NoScript]
    -> ChildHTML parent

  Tag_Object
    :: ValidChild 'Object parent
    => Attributes 'Object
    -> [ChildHTML 'Object]
    -> ChildHTML parent

  Tag_OrderedList
    :: ValidChild 'OrderedList parent
    => Attributes 'OrderedList
    -> [ChildHTML 'OrderedList]
    -> ChildHTML parent

  Tag_OptionGroup
    :: ValidChild 'OptionGroup parent
    => Attributes 'OptionGroup
    -> [ChildHTML 'OptionGroup]
    -> ChildHTML parent

  Tag_Option
    :: ValidChild 'Option parent
    => Attributes 'Option
    -> [ChildHTML 'Option]
    -> ChildHTML parent

  Tag_Output
    :: ValidChild 'Output parent
    => Attributes 'Output
    -> [ChildHTML 'Output]
    -> ChildHTML parent

  Tag_Paragraph
    :: ValidChild 'Paragraph parent
    => Attributes 'Paragraph
    -> [ChildHTML 'Paragraph]
    -> ChildHTML parent

  Tag_Picture
    :: ValidChild 'Picture parent
    => Attributes 'Picture
    -> [ChildHTML 'Picture]
    -> ChildHTML parent

  Tag_PreformattedText
    :: ValidChild 'PreformattedText parent
    => Attributes 'PreformattedText
    -> [ChildHTML 'PreformattedText]
    -> ChildHTML parent

  Tag_Progress
    :: ValidChild 'Progress parent
    => Attributes 'Progress
    -> [ChildHTML 'Progress]
    -> ChildHTML parent

  Tag_Quotation
    :: ValidChild 'Quotation parent
    => Attributes 'Quotation
    -> [ChildHTML 'Quotation]
    -> ChildHTML parent

  Tag_RubyParenthesis
    :: ValidChild 'RubyParenthesis parent
    => Attributes 'RubyParenthesis
    -> [ChildHTML 'RubyParenthesis]
    -> ChildHTML parent

  Tag_RubyText
    :: ValidChild 'RubyText parent
    => Attributes 'RubyText
    -> [ChildHTML 'RubyText]
    -> ChildHTML parent

  Tag_Ruby
    :: ValidChild 'Ruby parent
    => Attributes 'Ruby
    -> [ChildHTML 'Ruby]
    -> ChildHTML parent

  Tag_Strikethrough
    :: ValidChild 'Strikethrough parent
    => Attributes 'Strikethrough
    -> [ChildHTML 'Strikethrough]
    -> ChildHTML parent

  Tag_Sample
    :: ValidChild 'Sample parent
    => Attributes 'Sample
    -> [ChildHTML 'Sample]
    -> ChildHTML parent

  Tag_Script
    :: ValidChild 'Script parent
    => Attributes 'Script
    -> [ChildHTML 'Script]
    -> ChildHTML parent

  Tag_Search
    :: ValidChild 'Search parent
    => Attributes 'Search
    -> [ChildHTML 'Search]
    -> ChildHTML parent

  Tag_Section
    :: ValidChild 'Section parent
    => Attributes 'Section
    -> [ChildHTML 'Section]
    -> ChildHTML parent

  Tag_Select
    :: ValidChild 'Select parent
    => Attributes 'Select
    -> [ChildHTML 'Select]
    -> ChildHTML parent

  Tag_Slot
    :: ValidChild 'Slot parent
    => Attributes 'Slot
    -> [ChildHTML parent]
    -> ChildHTML parent

  Tag_SideComment
    :: ValidChild 'SideComment parent
    => Attributes 'SideComment
    -> [ChildHTML 'SideComment]
    -> ChildHTML parent

  Tag_Source
    :: ValidChild 'Source parent
    => Attributes 'Source
    -> ChildHTML parent

  Tag_Span
    :: ValidChild 'Span parent
    => Attributes 'Span
    -> [ChildHTML 'Span]
    -> ChildHTML parent

  Tag_Strong
    :: ValidChild 'Strong parent
    => Attributes 'Strong
    -> [ChildHTML 'Strong]
    -> ChildHTML parent

  Tag_Style
    :: ValidChild 'Style parent
    => Attributes 'Style
    -> [ChildHTML 'Style]
    -> ChildHTML parent

  Tag_Subscript
    :: ValidChild 'Subscript parent
    => Attributes 'Subscript
    -> [ChildHTML 'Subscript]
    -> ChildHTML parent

  Tag_Summary
    :: ValidChild 'Summary parent
    => Attributes 'Summary
    -> [ChildHTML 'Summary]
    -> ChildHTML parent

  Tag_Superscript
    :: ValidChild 'Superscript parent
    => Attributes 'Superscript
    -> [ChildHTML 'Superscript]
    -> ChildHTML parent

  Tag_Table
    :: ValidChild 'Table parent
    => Attributes 'Table
    -> [ChildHTML 'Table]
    -> ChildHTML parent

  Tag_TableBody
    :: ValidChild 'TableBody parent
    => Attributes 'TableBody
    -> [ChildHTML 'TableBody]
    -> ChildHTML parent

  Tag_TableDataCell
    :: ValidChild 'TableDataCell parent
    => Attributes 'TableDataCell
    -> [ChildHTML 'TableDataCell]
    -> ChildHTML parent

  Tag_ContentTemplate
    :: ValidChild 'ContentTemplate parent
    => Attributes 'ContentTemplate
    -> [ChildHTML 'ContentTemplate]
    -> ChildHTML parent

  Tag_TextArea
    :: ValidChild 'TextArea parent
    => Attributes 'TextArea
    -> [ChildHTML 'TextArea]
    -> ChildHTML parent

  Tag_TableFoot
    :: ValidChild 'TableFoot parent
    => Attributes 'TableFoot
    -> [ChildHTML 'TableFoot]
    -> ChildHTML parent

  Tag_TableHeader
    :: ValidChild 'TableHeader parent
    => Attributes 'TableHeader
    -> [ChildHTML 'TableHeader]
    -> ChildHTML parent

  Tag_TableHead
    :: ValidChild 'TableHead parent
    => Attributes 'TableHead
    -> [ChildHTML 'TableHead]
    -> ChildHTML parent

  Tag_Time
    :: ValidChild 'Time parent
    => Attributes 'Time
    -> [ChildHTML 'Time]
    -> ChildHTML parent

  Tag_Title
    :: ValidChild 'Title parent
    => Attributes 'Title
    -> [ChildHTML 'Title]
    -> ChildHTML parent

  Tag_TableRow
    :: ValidChild 'TableRow parent
    => Attributes 'TableRow
    -> [ChildHTML 'TableRow]
    -> ChildHTML parent

  Tag_Track
    :: ValidChild 'Track parent
    => Attributes 'Track
    -> ChildHTML parent

  Tag_Underline
    :: ValidChild 'Underline parent
    => Attributes 'Underline
    -> [ChildHTML 'Underline]
    -> ChildHTML parent

  Tag_UnorderedList
    :: ValidChild 'UnorderedList parent
    => Attributes 'UnorderedList
    -> [ChildHTML 'UnorderedList]
    -> ChildHTML parent

  Tag_Variable
    :: ValidChild 'Variable parent
    => Attributes 'Variable
    -> [ChildHTML 'Variable]
    -> ChildHTML parent

  Tag_Video
    :: ValidChild 'Video parent
    => Attributes 'Video
    -> [ChildHTML 'Video]
    -> ChildHTML parent

  Tag_WordBreakOpportunity
    :: ValidChild 'WordBreakOpportunity parent
    => Attributes 'WordBreakOpportunity
    -> ChildHTML parent
