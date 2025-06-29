{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HTML.Elements.Internal
  ( ChildHTML
      ( Tag_NoElement
      , Tag_Comment
      , Tag_Text
      , Tag_Entity
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
      , Tag_InputButton
      , Tag_InputCheckbox
      , Tag_InputColor
      , Tag_InputDate
      , Tag_InputDatetimeLocal
      , Tag_InputEmail
      , Tag_InputFile
      , Tag_InputHidden
      , Tag_InputImage
      , Tag_InputMonth
      , Tag_InputNumber
      , Tag_InputPassword
      , Tag_InputRadio
      , Tag_InputRange
      , Tag_InputReset
      , Tag_InputSearch
      , Tag_InputSubmit
      , Tag_InputTel
      , Tag_InputText
      , Tag_InputTime
      , Tag_InputUrl
      , Tag_InputWeek
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

import Brigid.HTML.Attributes.Internal (Attribute)
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.TagType (TagType (..))
import Brigid.Types.NoContent (NoContent)

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

  Tag_Entity
    :: ValidChild 'Text parent grandparent
    => String
    -> ChildHTML parent grandparent

  Tag_RawHTML
    :: T.Text
    -> ChildHTML parent grandparent

  Tag_CustomHTML
    :: T.Text
    -> [Attribute 'CustomHTML]
    -> Either NoContent [ChildHTML 'CustomHTML parent]
    -> ChildHTML parent grandparent

  Tag_Anchor
    :: ValidChild 'Anchor parent grandparent
    => [Attribute 'Anchor]
    -> [ChildHTML 'Anchor parent]
    -> ChildHTML parent grandparent

  Tag_Abbreviation
    :: ValidChild 'Abbreviation parent grandparent
    => [Attribute 'Abbreviation]
    -> [ChildHTML 'Abbreviation parent]
    -> ChildHTML parent grandparent

  Tag_ContactAddress
    :: ValidChild 'ContactAddress parent grandparent
    => [Attribute 'ContactAddress]
    -> [ChildHTML 'ContactAddress parent]
    -> ChildHTML parent grandparent

  Tag_Area
    :: ValidChild 'Area parent grandparent
    => [Attribute 'Area]
    -> ChildHTML parent grandparent

  Tag_Article
    :: ValidChild 'Article parent grandparent
    => [Attribute 'Article]
    -> [ChildHTML 'Article parent]
    -> ChildHTML parent grandparent

  Tag_Aside
    :: ValidChild 'Aside parent grandparent
    => [Attribute 'Aside]
    -> [ChildHTML 'Aside parent]
    -> ChildHTML parent grandparent

  Tag_Audio
    :: ValidChild 'Audio parent grandparent
    => [Attribute 'Audio]
    -> [ChildHTML 'Audio parent]
    -> ChildHTML parent grandparent

  Tag_BringAttentionTo
    :: ValidChild 'BringAttentionTo parent grandparent
    => [Attribute 'BringAttentionTo]
    -> [ChildHTML 'BringAttentionTo parent]
    -> ChildHTML parent grandparent

  Tag_Base
    :: ValidChild 'Base parent grandparent
    => [Attribute 'Base]
    -> ChildHTML parent grandparent

  Tag_BidirectionalIsolation
    :: ValidChild 'BidirectionalIsolation parent grandparent
    => [Attribute 'BidirectionalIsolation]
    -> [ChildHTML 'BidirectionalIsolation parent]
    -> ChildHTML parent grandparent

  Tag_BidirectionalOverride
    :: ValidChild 'BidirectionalOverride parent grandparent
    => [Attribute 'BidirectionalOverride]
    -> [ChildHTML 'BidirectionalOverride parent]
    -> ChildHTML parent grandparent

  Tag_Blockquote
    :: ValidChild 'Blockquote parent grandparent
    => [Attribute 'Blockquote]
    -> [ChildHTML 'Blockquote parent]
    -> ChildHTML parent grandparent

  Tag_Body
    :: ValidChild 'Body parent grandparent
    => [Attribute 'Body]
    -> [ChildHTML 'Body parent]
    -> ChildHTML parent grandparent

  Tag_LineBreak
    :: ValidChild 'LineBreak parent grandparent
    => [Attribute 'LineBreak]
    -> ChildHTML parent grandparent

  Tag_Button
    :: ValidChild 'Button parent grandparent
    => [Attribute 'Button]
    -> [ChildHTML 'Button parent]
    -> ChildHTML parent grandparent

  Tag_Canvas
    :: ValidChild 'Canvas parent grandparent
    => [Attribute 'Canvas]
    -> [ChildHTML 'Canvas parent]
    -> ChildHTML parent grandparent

  Tag_TableCaption
    :: ValidChild 'TableCaption parent grandparent
    => [Attribute 'TableCaption]
    -> [ChildHTML 'TableCaption parent]
    -> ChildHTML parent grandparent

  Tag_Citation
    :: ValidChild 'Citation parent grandparent
    => [Attribute 'Citation]
    -> [ChildHTML 'Citation parent]
    -> ChildHTML parent grandparent

  Tag_Code
    :: ValidChild 'Code parent grandparent
    => [Attribute 'Code]
    -> [ChildHTML 'Code parent]
    -> ChildHTML parent grandparent

  Tag_TableColumn
    :: ValidChild 'TableColumn parent grandparent
    => [Attribute 'TableColumn]
    -> ChildHTML parent grandparent

  Tag_TableColumnGroup
    :: ValidChild 'TableColumnGroup parent grandparent
    => [Attribute 'TableColumnGroup]
    -> [ChildHTML 'TableColumnGroup parent]
    -> ChildHTML parent grandparent

  Tag_Data
    :: ValidChild 'Data parent grandparent
    => [Attribute 'Data]
    -> [ChildHTML 'Data parent]
    -> ChildHTML parent grandparent

  Tag_DataList
    :: ValidChild 'DataList parent grandparent
    => [Attribute 'DataList]
    -> [ChildHTML 'DataList parent]
    -> ChildHTML parent grandparent

  Tag_DescriptionDetails
    :: ValidChild 'DescriptionDetails parent grandparent
    => [Attribute 'DescriptionDetails]
    -> [ChildHTML 'DescriptionDetails parent]
    -> ChildHTML parent grandparent

  Tag_DeletedText
    :: ValidChild 'DeletedText parent grandparent
    => [Attribute 'DeletedText]
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent

  Tag_Details
    :: ValidChild 'Details parent grandparent
    => [Attribute 'Details]
    -> [ChildHTML 'Details parent]
    -> ChildHTML parent grandparent

  Tag_Definition
    :: ValidChild 'Definition parent grandparent
    => [Attribute 'Definition]
    -> [ChildHTML 'Definition parent]
    -> ChildHTML parent grandparent

  Tag_Dialog
    :: ValidChild 'Dialog parent grandparent
    => [Attribute 'Dialog]
    -> [ChildHTML 'Dialog parent]
    -> ChildHTML parent grandparent

  Tag_Division
    :: ValidChild 'Division parent grandparent
    => [Attribute 'Division]
    -> [ChildHTML 'Division parent]
    -> ChildHTML parent grandparent

  Tag_DescriptionList
    :: ValidChild 'DescriptionList parent grandparent
    => [Attribute 'DescriptionList]
    -> [ChildHTML 'DescriptionList parent]
    -> ChildHTML parent grandparent

  Tag_DescriptionTerm
    :: ValidChild 'DescriptionTerm parent grandparent
    => [Attribute 'DescriptionTerm]
    -> [ChildHTML 'DescriptionTerm parent]
    -> ChildHTML parent grandparent

  Tag_Emphasis
    :: ValidChild 'Emphasis parent grandparent
    => [Attribute 'Emphasis]
    -> [ChildHTML 'Emphasis parent]
    -> ChildHTML parent grandparent

  Tag_Embed
    :: ValidChild 'Embed parent grandparent
    => [Attribute 'Embed]
    -> ChildHTML parent grandparent

  Tag_Fieldset
    :: ValidChild 'Fieldset parent grandparent
    => [Attribute 'Fieldset]
    -> [ChildHTML 'Fieldset parent]
    -> ChildHTML parent grandparent

  Tag_FigureCaption
    :: ValidChild 'FigureCaption parent grandparent
    => [Attribute 'FigureCaption]
    -> [ChildHTML 'FigureCaption parent]
    -> ChildHTML parent grandparent

  Tag_Figure
    :: ValidChild 'Figure parent grandparent
    => [Attribute 'Figure]
    -> [ChildHTML 'Figure parent]
    -> ChildHTML parent grandparent

  Tag_Footer
    :: ValidChild 'Footer parent grandparent
    => [Attribute 'Footer]
    -> [ChildHTML 'Footer parent]
    -> ChildHTML parent grandparent

  Tag_Form
    :: ValidChild 'Form parent grandparent
    => [Attribute 'Form]
    -> [ChildHTML 'Form parent]
    -> ChildHTML parent grandparent

  Tag_H1
    :: ValidChild 'H1 parent grandparent
    => [Attribute 'H1]
    -> [ChildHTML 'H1 parent]
    -> ChildHTML parent grandparent

  Tag_H2
    :: ValidChild 'H2 parent grandparent
    => [Attribute 'H2]
    -> [ChildHTML 'H2 parent]
    -> ChildHTML parent grandparent

  Tag_H3
    :: ValidChild 'H3 parent grandparent
    => [Attribute 'H3]
    -> [ChildHTML 'H3 parent]
    -> ChildHTML parent grandparent

  Tag_H4
    :: ValidChild 'H4 parent grandparent
    => [Attribute 'H4]
    -> [ChildHTML 'H4 parent]
    -> ChildHTML parent grandparent

  Tag_H5
    :: ValidChild 'H5 parent grandparent
    => [Attribute 'H5]
    -> [ChildHTML 'H5 parent]
    -> ChildHTML parent grandparent

  Tag_H6
    :: ValidChild 'H6 parent grandparent
    => [Attribute 'H6]
    -> [ChildHTML 'H6 parent]
    -> ChildHTML parent grandparent

  Tag_Head
    :: ValidChild 'Head parent grandparent
    => [Attribute 'Head]
    -> [ChildHTML 'Head parent]
    -> ChildHTML parent grandparent

  Tag_Header
    :: ValidChild 'Header parent grandparent
    => [Attribute 'Header]
    -> [ChildHTML 'Header parent]
    -> ChildHTML parent grandparent

  Tag_HeadingGroup
    :: ValidChild 'HeadingGroup parent grandparent
    => [Attribute 'HeadingGroup]
    -> [ChildHTML 'HeadingGroup parent]
    -> ChildHTML parent grandparent

  Tag_HorizontalRule
    :: ValidChild 'HorizontalRule parent grandparent
    => [Attribute 'HorizontalRule]
    -> ChildHTML parent grandparent

  Tag_Html
    :: [Attribute 'Html]
    -> [ChildHTML 'Html 'Document]
    -> ChildHTML 'Document 'NoElement

  Tag_IdiomaticText
    :: ValidChild 'IdiomaticText parent grandparent
    => [Attribute 'IdiomaticText]
    -> [ChildHTML 'IdiomaticText parent]
    -> ChildHTML parent grandparent

  Tag_IFrame
    :: ValidChild 'IFrame parent grandparent
    => [Attribute 'IFrame]
    -> ChildHTML parent grandparent

  Tag_Image
    :: ValidChild 'Image parent grandparent
    => [Attribute 'Image]
    -> ChildHTML parent grandparent

  Tag_Input
    :: ValidChild 'Input parent grandparent
    => [Attribute 'Input]
    -> ChildHTML parent grandparent

  Tag_InputButton
    :: ValidChild 'InputButton parent grandparent
    => [Attribute 'InputButton]
    -> ChildHTML parent grandparent

  Tag_InputCheckbox
    :: ValidChild 'InputCheckbox parent grandparent
    => [Attribute 'InputCheckbox]
    -> ChildHTML parent grandparent

  Tag_InputColor
    :: ValidChild 'InputColor parent grandparent
    => [Attribute 'InputColor]
    -> ChildHTML parent grandparent

  Tag_InputDate
    :: ValidChild 'InputDate parent grandparent
    => [Attribute 'InputDate]
    -> ChildHTML parent grandparent

  Tag_InputDatetimeLocal
    :: ValidChild 'InputDatetimeLocal parent grandparent
    => [Attribute 'InputDatetimeLocal]
    -> ChildHTML parent grandparent

  Tag_InputEmail
    :: ValidChild 'InputEmail parent grandparent
    => [Attribute 'InputEmail]
    -> ChildHTML parent grandparent

  Tag_InputFile
    :: ValidChild 'InputFile parent grandparent
    => [Attribute 'InputFile]
    -> ChildHTML parent grandparent

  Tag_InputHidden
    :: ValidChild 'InputHidden parent grandparent
    => [Attribute 'InputHidden]
    -> ChildHTML parent grandparent

  Tag_InputImage
    :: ValidChild 'InputImage parent grandparent
    => [Attribute 'InputImage]
    -> ChildHTML parent grandparent

  Tag_InputMonth
    :: ValidChild 'InputMonth parent grandparent
    => [Attribute 'InputMonth]
    -> ChildHTML parent grandparent

  Tag_InputNumber
    :: ValidChild 'InputNumber parent grandparent
    => [Attribute 'InputNumber]
    -> ChildHTML parent grandparent

  Tag_InputPassword
    :: ValidChild 'InputPassword parent grandparent
    => [Attribute 'InputPassword]
    -> ChildHTML parent grandparent

  Tag_InputRadio
    :: ValidChild 'InputRadio parent grandparent
    => [Attribute 'InputRadio]
    -> ChildHTML parent grandparent

  Tag_InputRange
    :: ValidChild 'InputRange parent grandparent
    => [Attribute 'InputRange]
    -> ChildHTML parent grandparent

  Tag_InputReset
    :: ValidChild 'InputReset parent grandparent
    => [Attribute 'InputReset]
    -> ChildHTML parent grandparent

  Tag_InputSearch
    :: ValidChild 'InputSearch parent grandparent
    => [Attribute 'InputSearch]
    -> ChildHTML parent grandparent

  Tag_InputSubmit
    :: ValidChild 'InputSubmit parent grandparent
    => [Attribute 'InputSubmit]
    -> ChildHTML parent grandparent

  Tag_InputTel
    :: ValidChild 'InputTel parent grandparent
    => [Attribute 'InputTel]
    -> ChildHTML parent grandparent

  Tag_InputText
    :: ValidChild 'InputText parent grandparent
    => [Attribute 'InputText]
    -> ChildHTML parent grandparent

  Tag_InputTime
    :: ValidChild 'InputTime parent grandparent
    => [Attribute 'InputTime]
    -> ChildHTML parent grandparent

  Tag_InputUrl
    :: ValidChild 'InputUrl parent grandparent
    => [Attribute 'InputUrl]
    -> ChildHTML parent grandparent

  Tag_InputWeek
    :: ValidChild 'InputWeek parent grandparent
    => [Attribute 'InputWeek]
    -> ChildHTML parent grandparent

  Tag_InsertedText
    :: ValidChild 'InsertedText parent grandparent
    => [Attribute 'InsertedText]
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent

  Tag_KeyboardInput
    :: ValidChild 'KeyboardInput parent grandparent
    => [Attribute 'KeyboardInput]
    -> [ChildHTML 'KeyboardInput parent]
    -> ChildHTML parent grandparent

  Tag_Label
    :: ValidChild 'Label parent grandparent
    => [Attribute 'Label]
    -> [ChildHTML 'Label parent]
    -> ChildHTML parent grandparent

  Tag_Legend
    :: ValidChild 'Legend parent grandparent
    => [Attribute 'Legend]
    -> [ChildHTML 'Legend parent]
    -> ChildHTML parent grandparent

  Tag_ListItem
    :: ValidChild 'ListItem parent grandparent
    => [Attribute 'ListItem]
    -> [ChildHTML 'ListItem parent]
    -> ChildHTML parent grandparent

  Tag_Link
    :: ValidChild 'Link parent grandparent
    => [Attribute 'Link]
    -> ChildHTML parent grandparent

  Tag_Main
    :: ValidChild 'Main parent grandparent
    => [Attribute 'Main]
    -> [ChildHTML 'Main parent]
    -> ChildHTML parent grandparent

  Tag_Map
    :: ValidChild 'Map parent grandparent
    => [Attribute 'Map]
    -> [ChildHTML 'Map parent]
    -> ChildHTML parent grandparent

  Tag_Mark
    :: ValidChild 'Mark parent grandparent
    => [Attribute 'Mark]
    -> [ChildHTML 'Mark parent]
    -> ChildHTML parent grandparent

  Tag_Menu
    :: ValidChild 'Menu parent grandparent
    => [Attribute 'Menu]
    -> [ChildHTML 'Menu parent]
    -> ChildHTML parent grandparent

  Tag_Meta
    :: ValidChild 'Meta parent grandparent
    => [Attribute 'Meta]
    -> ChildHTML parent grandparent

  Tag_Meter
    :: ValidChild 'Meter parent grandparent
    => [Attribute 'Meter]
    -> [ChildHTML 'Meter parent]
    -> ChildHTML parent grandparent

  Tag_Nav
    :: ValidChild 'Nav parent grandparent
    => [Attribute 'Nav]
    -> [ChildHTML 'Nav parent]
    -> ChildHTML parent grandparent

  Tag_NoScript
    :: ValidChild 'NoScript parent grandparent
    => [Attribute 'NoScript]
    -> [ChildHTML 'NoScript parent]
    -> ChildHTML parent grandparent

  Tag_Object
    :: ValidChild 'Object parent grandparent
    => [Attribute 'Object]
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent

  Tag_OrderedList
    :: ValidChild 'OrderedList parent grandparent
    => [Attribute 'OrderedList]
    -> [ChildHTML 'OrderedList parent]
    -> ChildHTML parent grandparent

  Tag_OptionGroup
    :: ValidChild 'OptionGroup parent grandparent
    => [Attribute 'OptionGroup]
    -> [ChildHTML 'OptionGroup parent]
    -> ChildHTML parent grandparent

  Tag_Option
    :: ValidChild 'Option parent grandparent
    => [Attribute 'Option]
    -> [ChildHTML 'Option parent]
    -> ChildHTML parent grandparent

  Tag_Output
    :: ValidChild 'Output parent grandparent
    => [Attribute 'Output]
    -> [ChildHTML 'Output parent]
    -> ChildHTML parent grandparent

  Tag_Paragraph
    :: ValidChild 'Paragraph parent grandparent
    => [Attribute 'Paragraph]
    -> [ChildHTML 'Paragraph parent]
    -> ChildHTML parent grandparent

  Tag_Picture
    :: ValidChild 'Picture parent grandparent
    => [Attribute 'Picture]
    -> [ChildHTML 'Picture parent]
    -> ChildHTML parent grandparent

  Tag_PreformattedText
    :: ValidChild 'PreformattedText parent grandparent
    => [Attribute 'PreformattedText]
    -> [ChildHTML 'PreformattedText parent]
    -> ChildHTML parent grandparent

  Tag_Progress
    :: ValidChild 'Progress parent grandparent
    => [Attribute 'Progress]
    -> [ChildHTML 'Progress parent]
    -> ChildHTML parent grandparent

  Tag_Quotation
    :: ValidChild 'Quotation parent grandparent
    => [Attribute 'Quotation]
    -> [ChildHTML 'Quotation parent]
    -> ChildHTML parent grandparent

  Tag_RubyParenthesis
    :: ValidChild 'RubyParenthesis parent grandparent
    => [Attribute 'RubyParenthesis]
    -> [ChildHTML 'RubyParenthesis parent]
    -> ChildHTML parent grandparent

  Tag_RubyText
    :: ValidChild 'RubyText parent grandparent
    => [Attribute 'RubyText]
    -> [ChildHTML 'RubyText parent]
    -> ChildHTML parent grandparent

  Tag_Ruby
    :: ValidChild 'Ruby parent grandparent
    => [Attribute 'Ruby]
    -> [ChildHTML 'Ruby parent]
    -> ChildHTML parent grandparent

  Tag_Strikethrough
    :: ValidChild 'Strikethrough parent grandparent
    => [Attribute 'Strikethrough]
    -> [ChildHTML 'Strikethrough parent]
    -> ChildHTML parent grandparent

  Tag_Sample
    :: ValidChild 'Sample parent grandparent
    => [Attribute 'Sample]
    -> [ChildHTML 'Sample parent]
    -> ChildHTML parent grandparent

  Tag_Script
    :: ValidChild 'Script parent grandparent
    => [Attribute 'Script]
    -> Maybe NET.NonEmptyText
    -> ChildHTML parent grandparent

  Tag_Search
    :: ValidChild 'Search parent grandparent
    => [Attribute 'Search]
    -> [ChildHTML 'Search parent]
    -> ChildHTML parent grandparent

  Tag_Section
    :: ValidChild 'Section parent grandparent
    => [Attribute 'Section]
    -> [ChildHTML 'Section parent]
    -> ChildHTML parent grandparent

  Tag_Select
    :: ValidChild 'Select parent grandparent
    => [Attribute 'Select]
    -> [ChildHTML 'Select parent]
    -> ChildHTML parent grandparent

  Tag_Slot
    :: ValidChild 'Slot parent grandparent
    => [Attribute 'Slot]
    -> [ChildHTML parent parent]
    -> ChildHTML parent grandparent

  Tag_SideComment
    :: ValidChild 'SideComment parent grandparent
    => [Attribute 'SideComment]
    -> [ChildHTML 'SideComment parent]
    -> ChildHTML parent grandparent

  Tag_Source
    :: ValidChild 'Source parent grandparent
    => [Attribute 'Source]
    -> ChildHTML parent grandparent

  Tag_Span
    :: ValidChild 'Span parent grandparent
    => [Attribute 'Span]
    -> [ChildHTML 'Span parent]
    -> ChildHTML parent grandparent

  Tag_Strong
    :: ValidChild 'Strong parent grandparent
    => [Attribute 'Strong]
    -> [ChildHTML 'Strong parent]
    -> ChildHTML parent grandparent

  Tag_Style
    :: ValidChild 'Style parent grandparent
    => [Attribute 'Style]
    -> T.Text
    -> ChildHTML parent grandparent

  Tag_Subscript
    :: ValidChild 'Subscript parent grandparent
    => [Attribute 'Subscript]
    -> [ChildHTML 'Subscript parent]
    -> ChildHTML parent grandparent

  Tag_Summary
    :: ValidChild 'Summary parent grandparent
    => [Attribute 'Summary]
    -> [ChildHTML 'Summary parent]
    -> ChildHTML parent grandparent

  Tag_Superscript
    :: ValidChild 'Superscript parent grandparent
    => [Attribute 'Superscript]
    -> [ChildHTML 'Superscript parent]
    -> ChildHTML parent grandparent

  Tag_Table
    :: ValidChild 'Table parent grandparent
    => [Attribute 'Table]
    -> [ChildHTML 'Table parent]
    -> ChildHTML parent grandparent

  Tag_TableBody
    :: ValidChild 'TableBody parent grandparent
    => [Attribute 'TableBody]
    -> [ChildHTML 'TableBody parent]
    -> ChildHTML parent grandparent

  Tag_TableDataCell
    :: ValidChild 'TableDataCell parent grandparent
    => [Attribute 'TableDataCell]
    -> [ChildHTML 'TableDataCell parent]
    -> ChildHTML parent grandparent

  Tag_ContentTemplate
    :: ValidChild 'ContentTemplate parent grandparent
    => [Attribute 'ContentTemplate]
    -> [ChildHTML 'ContentTemplate parent]
    -> ChildHTML parent grandparent

  Tag_TextArea
    :: ValidChild 'TextArea parent grandparent
    => [Attribute 'TextArea]
    -> [ChildHTML 'TextArea parent]
    -> ChildHTML parent grandparent

  Tag_TableFoot
    :: ValidChild 'TableFoot parent grandparent
    => [Attribute 'TableFoot]
    -> [ChildHTML 'TableFoot parent]
    -> ChildHTML parent grandparent

  Tag_TableHeader
    :: ValidChild 'TableHeader parent grandparent
    => [Attribute 'TableHeader]
    -> [ChildHTML 'TableHeader parent]
    -> ChildHTML parent grandparent

  Tag_TableHead
    :: ValidChild 'TableHead parent grandparent
    => [Attribute 'TableHead]
    -> [ChildHTML 'TableHead parent]
    -> ChildHTML parent grandparent

  Tag_Time
    :: ValidChild 'Time parent grandparent
    => [Attribute 'Time]
    -> [ChildHTML 'Time parent]
    -> ChildHTML parent grandparent

  Tag_Title
    :: ValidChild 'Title parent grandparent
    => [Attribute 'Title]
    -> [ChildHTML 'Title parent]
    -> ChildHTML parent grandparent

  Tag_TableRow
    :: ValidChild 'TableRow parent grandparent
    => [Attribute 'TableRow]
    -> [ChildHTML 'TableRow parent]
    -> ChildHTML parent grandparent

  Tag_Track
    :: ValidChild 'Track parent grandparent
    => [Attribute 'Track]
    -> ChildHTML parent grandparent

  Tag_Underline
    :: ValidChild 'Underline parent grandparent
    => [Attribute 'Underline]
    -> [ChildHTML 'Underline parent]
    -> ChildHTML parent grandparent

  Tag_UnorderedList
    :: ValidChild 'UnorderedList parent grandparent
    => [Attribute 'UnorderedList]
    -> [ChildHTML 'UnorderedList parent]
    -> ChildHTML parent grandparent

  Tag_Variable
    :: ValidChild 'Variable parent grandparent
    => [Attribute 'Variable]
    -> [ChildHTML 'Variable parent]
    -> ChildHTML parent grandparent

  Tag_Video
    :: ValidChild 'Video parent grandparent
    => [Attribute 'Video]
    -> [ChildHTML 'Video parent]
    -> ChildHTML parent grandparent

  Tag_WordBreakOpportunity
    :: ValidChild 'WordBreakOpportunity parent grandparent
    => [Attribute 'WordBreakOpportunity]
    -> ChildHTML parent grandparent

deriving instance Eq (ChildHTML parent grandparent)
deriving instance Show (ChildHTML parent grandparent)
