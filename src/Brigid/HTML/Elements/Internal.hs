{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HTML.Elements.Internal
  ( ChildHTML
      ( Tag_NoElement
      , Tag_Comment
      , Tag_Text
      , Tag_RawHTML
      , Tag_CustomHTML
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

import Data.NonEmptyText qualified as NET
import Data.Text qualified as T

import Brigid.HTML.Attributes.Internal (Attributes)
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.TagType (TagType (..))
import Brigid.HTML.Types (NoContent)

data ChildHTML (parent :: TagType) (grandparent :: TagType) where
  Tag_NoElement
    :: ChildHTML parent grandparent

  Tag_Comment
    :: T.Text
    -> ChildHTML parent grandparent

  Tag_Text
    :: ValidChild 'Text parent grandparent
    => T.Text
    -> ChildHTML parent grandparent

  Tag_RawHTML
    :: T.Text
    -> ChildHTML parent grandparent

  Tag_CustomHTML
    :: T.Text
    -> Attributes 'CustomHTML
    -> Either NoContent [ChildHTML 'CustomHTML parent]
    -> ChildHTML parent grandparent

  Tag_Anchor
    :: ValidChild 'Anchor parent grandparent
    => Attributes 'Anchor
    -> [ChildHTML 'Anchor parent]
    -> ChildHTML parent grandparent

  Tag_Abbreviation
    :: ValidChild 'Abbreviation parent grandparent
    => Attributes 'Abbreviation
    -> [ChildHTML 'Abbreviation parent]
    -> ChildHTML parent grandparent

  Tag_ContactAddress
    :: ValidChild 'ContactAddress parent grandparent
    => Attributes 'ContactAddress
    -> [ChildHTML 'ContactAddress parent]
    -> ChildHTML parent grandparent

  Tag_Area
    :: ValidChild 'Area parent grandparent
    => Attributes 'Area
    -> ChildHTML parent grandparent

  Tag_Article
    :: ValidChild 'Article parent grandparent
    => Attributes 'Article
    -> [ChildHTML 'Article parent]
    -> ChildHTML parent grandparent

  Tag_Aside
    :: ValidChild 'Aside parent grandparent
    => Attributes 'Aside
    -> [ChildHTML 'Aside parent]
    -> ChildHTML parent grandparent

  Tag_Audio
    :: ValidChild 'Audio parent grandparent
    => Attributes 'Audio
    -> [ChildHTML 'Audio parent]
    -> ChildHTML parent grandparent

  Tag_BringAttentionTo
    :: ValidChild 'BringAttentionTo parent grandparent
    => Attributes 'BringAttentionTo
    -> [ChildHTML 'BringAttentionTo parent]
    -> ChildHTML parent grandparent

  Tag_Base
    :: ValidChild 'Base parent grandparent
    => Attributes 'Base
    -> ChildHTML parent grandparent

  Tag_BidirectionalIsolation
    :: ValidChild 'BidirectionalIsolation parent grandparent
    => Attributes 'BidirectionalIsolation
    -> [ChildHTML 'BidirectionalIsolation parent]
    -> ChildHTML parent grandparent

  Tag_BidirectionalOverride
    :: ValidChild 'BidirectionalOverride parent grandparent
    => Attributes 'BidirectionalOverride
    -> [ChildHTML 'BidirectionalOverride parent]
    -> ChildHTML parent grandparent

  Tag_Blockquote
    :: ValidChild 'Blockquote parent grandparent
    => Attributes 'Blockquote
    -> [ChildHTML 'Blockquote parent]
    -> ChildHTML parent grandparent

  Tag_Body
    :: ValidChild 'Body parent grandparent
    => Attributes 'Body
    -> [ChildHTML 'Body parent]
    -> ChildHTML parent grandparent

  Tag_LineBreak
    :: ValidChild 'LineBreak parent grandparent
    => Attributes 'LineBreak
    -> ChildHTML parent grandparent

  Tag_Button
    :: ValidChild 'Button parent grandparent
    => Attributes 'Button
    -> [ChildHTML 'Button parent]
    -> ChildHTML parent grandparent

  Tag_Canvas
    :: ValidChild 'Canvas parent grandparent
    => Attributes 'Canvas
    -> [ChildHTML 'Canvas parent]
    -> ChildHTML parent grandparent

  Tag_TableCaption
    :: ValidChild 'TableCaption parent grandparent
    => Attributes 'TableCaption
    -> [ChildHTML 'TableCaption parent]
    -> ChildHTML parent grandparent

  Tag_Citation
    :: ValidChild 'Citation parent grandparent
    => Attributes 'Citation
    -> [ChildHTML 'Citation parent]
    -> ChildHTML parent grandparent

  Tag_Code
    :: ValidChild 'Code parent grandparent
    => Attributes 'Code
    -> [ChildHTML 'Code parent]
    -> ChildHTML parent grandparent

  Tag_TableColumn
    :: ValidChild 'TableColumn parent grandparent
    => Attributes 'TableColumn
    -> ChildHTML parent grandparent

  Tag_TableColumnGroup
    :: ValidChild 'TableColumnGroup parent grandparent
    => Attributes 'TableColumnGroup
    -> [ChildHTML 'TableColumnGroup parent]
    -> ChildHTML parent grandparent

  Tag_Data
    :: ValidChild 'Data parent grandparent
    => Attributes 'Data
    -> [ChildHTML 'Data parent]
    -> ChildHTML parent grandparent

  Tag_DataList
    :: ValidChild 'DataList parent grandparent
    => Attributes 'DataList
    -> [ChildHTML 'DataList parent]
    -> ChildHTML parent grandparent

  Tag_DescriptionDetails
    :: ValidChild 'DescriptionDetails parent grandparent
    => Attributes 'DescriptionDetails
    -> [ChildHTML 'DescriptionDetails parent]
    -> ChildHTML parent grandparent

  Tag_DeletedText
    :: ValidChild 'DeletedText parent grandparent
    => Attributes 'DeletedText
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent

  Tag_Details
    :: ValidChild 'Details parent grandparent
    => Attributes 'Details
    -> [ChildHTML 'Details parent]
    -> ChildHTML parent grandparent

  Tag_Definition
    :: ValidChild 'Definition parent grandparent
    => Attributes 'Definition
    -> [ChildHTML 'Definition parent]
    -> ChildHTML parent grandparent

  Tag_Dialog
    :: ValidChild 'Dialog parent grandparent
    => Attributes 'Dialog
    -> [ChildHTML 'Dialog parent]
    -> ChildHTML parent grandparent

  Tag_Division
    :: ValidChild 'Division parent grandparent
    => Attributes 'Division
    -> [ChildHTML 'Division parent]
    -> ChildHTML parent grandparent

  Tag_DescriptionList
    :: ValidChild 'DescriptionList parent grandparent
    => Attributes 'DescriptionList
    -> [ChildHTML 'DescriptionList parent]
    -> ChildHTML parent grandparent

  Tag_DescriptionTerm
    :: ValidChild 'DescriptionTerm parent grandparent
    => Attributes 'DescriptionTerm
    -> [ChildHTML 'DescriptionTerm parent]
    -> ChildHTML parent grandparent

  Tag_Emphasis
    :: ValidChild 'Emphasis parent grandparent
    => Attributes 'Emphasis
    -> [ChildHTML 'Emphasis parent]
    -> ChildHTML parent grandparent

  Tag_Embed
    :: ValidChild 'Embed parent grandparent
    => Attributes 'Embed
    -> ChildHTML parent grandparent

  Tag_Fieldset
    :: ValidChild 'Fieldset parent grandparent
    => Attributes 'Fieldset
    -> [ChildHTML 'Fieldset parent]
    -> ChildHTML parent grandparent

  Tag_FigureCaption
    :: ValidChild 'FigureCaption parent grandparent
    => Attributes 'FigureCaption
    -> [ChildHTML 'FigureCaption parent]
    -> ChildHTML parent grandparent

  Tag_Figure
    :: ValidChild 'Figure parent grandparent
    => Attributes 'Figure
    -> [ChildHTML 'Figure parent]
    -> ChildHTML parent grandparent

  Tag_Footer
    :: ValidChild 'Footer parent grandparent
    => Attributes 'Footer
    -> [ChildHTML 'Footer parent]
    -> ChildHTML parent grandparent

  Tag_Form
    :: ValidChild 'Form parent grandparent
    => Attributes 'Form
    -> [ChildHTML 'Form parent]
    -> ChildHTML parent grandparent

  Tag_H1
    :: ValidChild 'H1 parent grandparent
    => Attributes 'H1
    -> [ChildHTML 'H1 parent]
    -> ChildHTML parent grandparent

  Tag_H2
    :: ValidChild 'H2 parent grandparent
    => Attributes 'H2
    -> [ChildHTML 'H2 parent]
    -> ChildHTML parent grandparent

  Tag_H3
    :: ValidChild 'H3 parent grandparent
    => Attributes 'H3
    -> [ChildHTML 'H3 parent]
    -> ChildHTML parent grandparent

  Tag_H4
    :: ValidChild 'H4 parent grandparent
    => Attributes 'H4
    -> [ChildHTML 'H4 parent]
    -> ChildHTML parent grandparent

  Tag_H5
    :: ValidChild 'H5 parent grandparent
    => Attributes 'H5
    -> [ChildHTML 'H5 parent]
    -> ChildHTML parent grandparent

  Tag_H6
    :: ValidChild 'H6 parent grandparent
    => Attributes 'H6
    -> [ChildHTML 'H6 parent]
    -> ChildHTML parent grandparent

  Tag_Head
    :: ValidChild 'Head parent grandparent
    => Attributes 'Head
    -> [ChildHTML 'Head parent]
    -> ChildHTML parent grandparent

  Tag_Header
    :: ValidChild 'Header parent grandparent
    => Attributes 'Header
    -> [ChildHTML 'Header parent]
    -> ChildHTML parent grandparent

  Tag_HeadingGroup
    :: ValidChild 'HeadingGroup parent grandparent
    => Attributes 'HeadingGroup
    -> [ChildHTML 'HeadingGroup parent]
    -> ChildHTML parent grandparent

  Tag_HorizontalRule
    :: ValidChild 'HorizontalRule parent grandparent
    => Attributes 'HorizontalRule
    -> ChildHTML parent grandparent

  Tag_Html
    :: Attributes 'Html
    -> [ChildHTML 'Html 'Document]
    -> ChildHTML 'Document 'NoElement

  Tag_IdiomaticText
    :: ValidChild 'IdiomaticText parent grandparent
    => Attributes 'IdiomaticText
    -> [ChildHTML 'IdiomaticText parent]
    -> ChildHTML parent grandparent

  Tag_IFrame
    :: ValidChild 'IFrame parent grandparent
    => Attributes 'IFrame
    -> ChildHTML parent grandparent

  Tag_Image
    :: ValidChild 'Image parent grandparent
    => Attributes 'Image
    -> ChildHTML parent grandparent

  Tag_Input
    :: ValidChild 'Input parent grandparent
    => Attributes 'Input
    -> ChildHTML parent grandparent

  Tag_InsertedText
    :: ValidChild 'InsertedText parent grandparent
    => Attributes 'InsertedText
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent

  Tag_KeyboardInput
    :: ValidChild 'KeyboardInput parent grandparent
    => Attributes 'KeyboardInput
    -> [ChildHTML 'KeyboardInput parent]
    -> ChildHTML parent grandparent

  Tag_Label
    :: ValidChild 'Label parent grandparent
    => Attributes 'Label
    -> [ChildHTML 'Label parent]
    -> ChildHTML parent grandparent

  Tag_Legend
    :: ValidChild 'Legend parent grandparent
    => Attributes 'Legend
    -> [ChildHTML 'Legend parent]
    -> ChildHTML parent grandparent

  Tag_ListItem
    :: ValidChild 'ListItem parent grandparent
    => Attributes 'ListItem
    -> [ChildHTML 'ListItem parent]
    -> ChildHTML parent grandparent

  Tag_Link
    :: ValidChild 'Link parent grandparent
    => Attributes 'Link
    -> ChildHTML parent grandparent

  Tag_Main
    :: ValidChild 'Main parent grandparent
    => Attributes 'Main
    -> [ChildHTML 'Main parent]
    -> ChildHTML parent grandparent

  Tag_Map
    :: ValidChild 'Map parent grandparent
    => Attributes 'Map
    -> [ChildHTML 'Map parent]
    -> ChildHTML parent grandparent

  Tag_Mark
    :: ValidChild 'Mark parent grandparent
    => Attributes 'Mark
    -> [ChildHTML 'Mark parent]
    -> ChildHTML parent grandparent

  Tag_Menu
    :: ValidChild 'Menu parent grandparent
    => Attributes 'Menu
    -> [ChildHTML 'Menu parent]
    -> ChildHTML parent grandparent

  Tag_Meta
    :: ValidChild 'Meta parent grandparent
    => Attributes 'Meta
    -> ChildHTML parent grandparent

  Tag_Meter
    :: ValidChild 'Meter parent grandparent
    => Attributes 'Meter
    -> [ChildHTML 'Meter parent]
    -> ChildHTML parent grandparent

  Tag_Nav
    :: ValidChild 'Nav parent grandparent
    => Attributes 'Nav
    -> [ChildHTML 'Nav parent]
    -> ChildHTML parent grandparent

  Tag_NoScript
    :: ValidChild 'NoScript parent grandparent
    => Attributes 'NoScript
    -> [ChildHTML 'NoScript parent]
    -> ChildHTML parent grandparent

  Tag_Object
    :: ValidChild 'Object parent grandparent
    => Attributes 'Object
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent

  Tag_OrderedList
    :: ValidChild 'OrderedList parent grandparent
    => Attributes 'OrderedList
    -> [ChildHTML 'OrderedList parent]
    -> ChildHTML parent grandparent

  Tag_OptionGroup
    :: ValidChild 'OptionGroup parent grandparent
    => Attributes 'OptionGroup
    -> [ChildHTML 'OptionGroup parent]
    -> ChildHTML parent grandparent

  Tag_Option
    :: ValidChild 'Option parent grandparent
    => Attributes 'Option
    -> [ChildHTML 'Option parent]
    -> ChildHTML parent grandparent

  Tag_Output
    :: ValidChild 'Output parent grandparent
    => Attributes 'Output
    -> [ChildHTML 'Output parent]
    -> ChildHTML parent grandparent

  Tag_Paragraph
    :: ValidChild 'Paragraph parent grandparent
    => Attributes 'Paragraph
    -> [ChildHTML 'Paragraph parent]
    -> ChildHTML parent grandparent

  Tag_Picture
    :: ValidChild 'Picture parent grandparent
    => Attributes 'Picture
    -> [ChildHTML 'Picture parent]
    -> ChildHTML parent grandparent

  Tag_PreformattedText
    :: ValidChild 'PreformattedText parent grandparent
    => Attributes 'PreformattedText
    -> [ChildHTML 'PreformattedText parent]
    -> ChildHTML parent grandparent

  Tag_Progress
    :: ValidChild 'Progress parent grandparent
    => Attributes 'Progress
    -> [ChildHTML 'Progress parent]
    -> ChildHTML parent grandparent

  Tag_Quotation
    :: ValidChild 'Quotation parent grandparent
    => Attributes 'Quotation
    -> [ChildHTML 'Quotation parent]
    -> ChildHTML parent grandparent

  Tag_RubyParenthesis
    :: ValidChild 'RubyParenthesis parent grandparent
    => Attributes 'RubyParenthesis
    -> [ChildHTML 'RubyParenthesis parent]
    -> ChildHTML parent grandparent

  Tag_RubyText
    :: ValidChild 'RubyText parent grandparent
    => Attributes 'RubyText
    -> [ChildHTML 'RubyText parent]
    -> ChildHTML parent grandparent

  Tag_Ruby
    :: ValidChild 'Ruby parent grandparent
    => Attributes 'Ruby
    -> [ChildHTML 'Ruby parent]
    -> ChildHTML parent grandparent

  Tag_Strikethrough
    :: ValidChild 'Strikethrough parent grandparent
    => Attributes 'Strikethrough
    -> [ChildHTML 'Strikethrough parent]
    -> ChildHTML parent grandparent

  Tag_Sample
    :: ValidChild 'Sample parent grandparent
    => Attributes 'Sample
    -> [ChildHTML 'Sample parent]
    -> ChildHTML parent grandparent

  Tag_Script
    :: ValidChild 'Script parent grandparent
    => Attributes 'Script
    -> Maybe NET.NonEmptyText
    -> ChildHTML parent grandparent

  Tag_Search
    :: ValidChild 'Search parent grandparent
    => Attributes 'Search
    -> [ChildHTML 'Search parent]
    -> ChildHTML parent grandparent

  Tag_Section
    :: ValidChild 'Section parent grandparent
    => Attributes 'Section
    -> [ChildHTML 'Section parent]
    -> ChildHTML parent grandparent

  Tag_Select
    :: ValidChild 'Select parent grandparent
    => Attributes 'Select
    -> [ChildHTML 'Select parent]
    -> ChildHTML parent grandparent

  Tag_Slot
    :: ValidChild 'Slot parent grandparent
    => Attributes 'Slot
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent

  Tag_SideComment
    :: ValidChild 'SideComment parent grandparent
    => Attributes 'SideComment
    -> [ChildHTML 'SideComment parent]
    -> ChildHTML parent grandparent

  Tag_Source
    :: ValidChild 'Source parent grandparent
    => Attributes 'Source
    -> ChildHTML parent grandparent

  Tag_Span
    :: ValidChild 'Span parent grandparent
    => Attributes 'Span
    -> [ChildHTML 'Span parent]
    -> ChildHTML parent grandparent

  Tag_Strong
    :: ValidChild 'Strong parent grandparent
    => Attributes 'Strong
    -> [ChildHTML 'Strong parent]
    -> ChildHTML parent grandparent

  Tag_Style
    :: ValidChild 'Style parent grandparent
    => Attributes 'Style
    -> T.Text
    -> ChildHTML parent grandparent

  Tag_Subscript
    :: ValidChild 'Subscript parent grandparent
    => Attributes 'Subscript
    -> [ChildHTML 'Subscript parent]
    -> ChildHTML parent grandparent

  Tag_Summary
    :: ValidChild 'Summary parent grandparent
    => Attributes 'Summary
    -> [ChildHTML 'Summary parent]
    -> ChildHTML parent grandparent

  Tag_Superscript
    :: ValidChild 'Superscript parent grandparent
    => Attributes 'Superscript
    -> [ChildHTML 'Superscript parent]
    -> ChildHTML parent grandparent

  Tag_Table
    :: ValidChild 'Table parent grandparent
    => Attributes 'Table
    -> [ChildHTML 'Table parent]
    -> ChildHTML parent grandparent

  Tag_TableBody
    :: ValidChild 'TableBody parent grandparent
    => Attributes 'TableBody
    -> [ChildHTML 'TableBody parent]
    -> ChildHTML parent grandparent

  Tag_TableDataCell
    :: ValidChild 'TableDataCell parent grandparent
    => Attributes 'TableDataCell
    -> [ChildHTML 'TableDataCell parent]
    -> ChildHTML parent grandparent

  Tag_ContentTemplate
    :: ValidChild 'ContentTemplate parent grandparent
    => Attributes 'ContentTemplate
    -> [ChildHTML 'ContentTemplate parent]
    -> ChildHTML parent grandparent

  Tag_TextArea
    :: ValidChild 'TextArea parent grandparent
    => Attributes 'TextArea
    -> [ChildHTML 'TextArea parent]
    -> ChildHTML parent grandparent

  Tag_TableFoot
    :: ValidChild 'TableFoot parent grandparent
    => Attributes 'TableFoot
    -> [ChildHTML 'TableFoot parent]
    -> ChildHTML parent grandparent

  Tag_TableHeader
    :: ValidChild 'TableHeader parent grandparent
    => Attributes 'TableHeader
    -> [ChildHTML 'TableHeader parent]
    -> ChildHTML parent grandparent

  Tag_TableHead
    :: ValidChild 'TableHead parent grandparent
    => Attributes 'TableHead
    -> [ChildHTML 'TableHead parent]
    -> ChildHTML parent grandparent

  Tag_Time
    :: ValidChild 'Time parent grandparent
    => Attributes 'Time
    -> [ChildHTML 'Time parent]
    -> ChildHTML parent grandparent

  Tag_Title
    :: ValidChild 'Title parent grandparent
    => Attributes 'Title
    -> [ChildHTML 'Title parent]
    -> ChildHTML parent grandparent

  Tag_TableRow
    :: ValidChild 'TableRow parent grandparent
    => Attributes 'TableRow
    -> [ChildHTML 'TableRow parent]
    -> ChildHTML parent grandparent

  Tag_Track
    :: ValidChild 'Track parent grandparent
    => Attributes 'Track
    -> ChildHTML parent grandparent

  Tag_Underline
    :: ValidChild 'Underline parent grandparent
    => Attributes 'Underline
    -> [ChildHTML 'Underline parent]
    -> ChildHTML parent grandparent

  Tag_UnorderedList
    :: ValidChild 'UnorderedList parent grandparent
    => Attributes 'UnorderedList
    -> [ChildHTML 'UnorderedList parent]
    -> ChildHTML parent grandparent

  Tag_Variable
    :: ValidChild 'Variable parent grandparent
    => Attributes 'Variable
    -> [ChildHTML 'Variable parent]
    -> ChildHTML parent grandparent

  Tag_Video
    :: ValidChild 'Video parent grandparent
    => Attributes 'Video
    -> [ChildHTML 'Video parent]
    -> ChildHTML parent grandparent

  Tag_WordBreakOpportunity
    :: ValidChild 'WordBreakOpportunity parent grandparent
    => Attributes 'WordBreakOpportunity
    -> ChildHTML parent grandparent
