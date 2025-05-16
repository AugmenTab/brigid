module Generation.Element
  ( Element (..)
  , TagType (..)
  ) where

import Data.NonEmptyText qualified as NET
import Data.Text qualified as T

import Generation.Attribute (Attribute)

data Element
  = Comment T.Text
  | Text T.Text
  | Anchor [Attribute] [Element]
  | Abbreviation [Attribute] [Element]
  | ContactAddress [Attribute] [Element]
  | Area [Attribute]
  | Article [Attribute] [Element]
  | Aside [Attribute] [Element]
  | Audio [Attribute] [Element]
  | BringAttentionTo [Attribute] [Element]
  | Base [Attribute]
  | BidirectionalIsolation [Attribute] [Element]
  | BidirectionalOverride [Attribute] [Element]
  | Blockquote [Attribute] [Element]
  | Body [Attribute] [Element]
  | LineBreak [Attribute]
  | Button [Attribute] [Element]
  | Canvas [Attribute] [Element]
  | TableCaption [Attribute] [Element]
  | Citation [Attribute] [Element]
  | Code [Attribute] [Element]
  | TableColumn [Attribute]
  | TableColumnGroup [Attribute] [Element]
  | Data [Attribute] [Element]
  | DataList [Attribute] [Element]
  | DescriptionDetails [Attribute] [Element]
  | DeletedText [Attribute] [Element]
  | Details [Attribute] [Element]
  | Definition [Attribute] [Element]
  | Dialog [Attribute] [Element]
  | Division [Attribute] [Element]
  | DescriptionList [Attribute] [Element]
  | DescriptionTerm [Attribute] [Element]
  | Emphasis [Attribute] [Element]
  | Embed [Attribute]
  | Fieldset [Attribute] [Element]
  | FigureCaption [Attribute] [Element]
  | Figure [Attribute] [Element]
  | Footer [Attribute] [Element]
  | Form [Attribute] [Element]
  | H1 [Attribute] [Element]
  | H2 [Attribute] [Element]
  | H3 [Attribute] [Element]
  | H4 [Attribute] [Element]
  | H5 [Attribute] [Element]
  | H6 [Attribute] [Element]
  | Head [Attribute] [Element]
  | Header [Attribute] [Element]
  | HeadingGroup [Attribute] [Element]
  | HorizontalRule [Attribute]
  | Html [Attribute] [Element]
  | IdiomaticText [Attribute] [Element]
  | IFrame [Attribute]
  | Image [Attribute]
  | Input [Attribute]
  | InsertedText [Attribute] [Element]
  | KeyboardInput [Attribute] [Element]
  | Label [Attribute] [Element]
  | Legend [Attribute] [Element]
  | ListItem [Attribute] [Element]
  | Link [Attribute]
  | Main [Attribute] [Element]
  | Map [Attribute] [Element]
  | Mark [Attribute] [Element]
  | Menu [Attribute] [Element]
  | Meta [Attribute]
  | Meter [Attribute] [Element]
  | Nav [Attribute] [Element]
  | NoScript [Attribute] [Element]
  | Object [Attribute] [Element]
  | OrderedList [Attribute] [Element]
  | OptionGroup [Attribute] [Element]
  | Option [Attribute] T.Text
  | Output [Attribute] [Element]
  | Paragraph [Attribute] [Element]
  | Picture [Attribute] [Element]
  | PreformattedText [Attribute] [Element]
  | Progress [Attribute] [Element]
  | Quotation [Attribute] [Element]
  | RubyParenthesis [Attribute] T.Text
  | RubyText [Attribute] [Element]
  | Ruby [Attribute] [Element]
  | Strikethrough [Attribute] [Element]
  | Sample [Attribute] [Element]
  | Script [Attribute] NET.NonEmptyText
  | Search [Attribute] [Element]
  | Section [Attribute] [Element]
  | Select [Attribute] [Element]
  | Slot [Attribute] [Element]
  | SideComment [Attribute] [Element]
  | Source [Attribute]
  | Span [Attribute] [Element]
  | Strong [Attribute] [Element]
  | Style [Attribute]
  | Subscript [Attribute] [Element]
  | Summary [Attribute] [Element]
  | Superscript [Attribute] [Element]
  | Table [Attribute] [Element]
  | TableBody [Attribute] [Element]
  | TableDataCell [Attribute] [Element]
  | ContentTemplate [Attribute] [Element]
  | TextArea [Attribute] [Element]
  | TableFoot [Attribute] [Element]
  | TableHeader [Attribute] [Element]
  | TableHead [Attribute] [Element]
  | Time [Attribute] [Element]
  | Title [Attribute] T.Text
  | TableRow [Attribute] [Element]
  | Track [Attribute]
  | Underline [Attribute] [Element]
  | UnorderedList [Attribute] [Element]
  | Variable [Attribute] [Element]
  | Video [Attribute] [Element]
  | WordBreakOpportunity [Attribute]
  deriving Show

data TagType
  = Tag_Text
  | Tag_Comment
  | Tag_Anchor
  | Tag_Abbreviation
  | Tag_ContactAddress
  | Tag_Area
  | Tag_Article
  | Tag_Aside
  | Tag_Audio
  | Tag_BringAttentionTo
  | Tag_Base
  | Tag_BidirectionalIsolation
  | Tag_BidirectionalOverride
  | Tag_Blockquote
  | Tag_Body
  | Tag_LineBreak
  | Tag_Button
  | Tag_Canvas
  | Tag_TableCaption
  | Tag_Citation
  | Tag_Code
  | Tag_TableColumn
  | Tag_TableColumnGroup
  | Tag_Data
  | Tag_DataList
  | Tag_DescriptionDetails
  | Tag_DeletedText
  | Tag_Details
  | Tag_Definition
  | Tag_Dialog
  | Tag_Division
  | Tag_DescriptionList
  | Tag_DescriptionTerm
  | Tag_Emphasis
  | Tag_Embed
  | Tag_Fieldset
  | Tag_FigureCaption
  | Tag_Figure
  | Tag_Footer
  | Tag_Form
  | Tag_H1
  | Tag_H2
  | Tag_H3
  | Tag_H4
  | Tag_H5
  | Tag_H6
  | Tag_Head
  | Tag_Header
  | Tag_HeadingGroup
  | Tag_HorizontalRule
  | Tag_Html
  | Tag_IdiomaticText
  | Tag_IFrame
  | Tag_Image
  | Tag_Input
  | Tag_InsertedText
  | Tag_KeyboardInput
  | Tag_Label
  | Tag_Legend
  | Tag_ListItem
  | Tag_Link
  | Tag_Main
  | Tag_Map
  | Tag_Mark
  | Tag_Menu
  | Tag_Meta
  | Tag_Meter
  | Tag_Nav
  | Tag_NoScript
  | Tag_Object
  | Tag_OrderedList
  | Tag_OptionGroup
  | Tag_Option
  | Tag_Output
  | Tag_Paragraph
  | Tag_Picture
  | Tag_PreformattedText
  | Tag_Progress
  | Tag_Quotation
  | Tag_RubyParenthesis
  | Tag_RubyText
  | Tag_Ruby
  | Tag_Strikethrough
  | Tag_Sample
  | Tag_Script
  | Tag_Search
  | Tag_Section
  | Tag_Select
  | Tag_Slot
  | Tag_SideComment
  | Tag_Source
  | Tag_Span
  | Tag_Strong
  | Tag_Style
  | Tag_Subscript
  | Tag_Summary
  | Tag_Superscript
  | Tag_Table
  | Tag_TableBody
  | Tag_TableDataCell
  | Tag_ContentTemplate
  | Tag_TextArea
  | Tag_TableFoot
  | Tag_TableHeader
  | Tag_TableHead
  | Tag_Time
  | Tag_Title
  | Tag_TableRow
  | Tag_Track
  | Tag_Underline
  | Tag_UnorderedList
  | Tag_Variable
  | Tag_Video
  | Tag_WordBreakOpportunity
