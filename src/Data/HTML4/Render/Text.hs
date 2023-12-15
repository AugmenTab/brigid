{-# LANGUAGE GADTs #-}

module Data.HTML4.Render.Text
  ( renderHTML
  ) where

import Data.Bool qualified as B
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text qualified as T

import Data.HTML4.Elements.Internal (ChildHTML(..))
import Data.HTML4.Attributes.Internal (Attribute(..))

renderHTML :: ChildHTML parent -> T.Text
renderHTML html =
  case html of
    Tag_Anchor attrs content ->
      buildTag "a" attrs $ Right content

    Tag_Division attrs content ->
      buildTag "div" attrs $ Right content

    Tag_H1 attrs content ->
      buildTag "h1" attrs $ Right content

    Tag_Html attrs content ->
      ("<!DOCTYPE html>\n" :: T.Text) <> buildTag "html" attrs (Right content)

    Tag_IFrame attrs ->
      buildTag "iframe" attrs $ Left WithTag

    Tag_Image attrs ->
      buildTag "img" attrs $ Left OmitTag

    Tag_ListItem attrs content ->
      buildTag "li" attrs $ Right content

    Tag_Paragraph attrs content ->
      buildTag "p" attrs $ Right content

    Tag_Span attrs content ->
      buildTag "span" attrs $ Right content

    Tag_UnorderedList attrs content ->
      buildTag "ul" attrs $ Right content

    -- Tag_Comment
    -- Tag_Text
    -- Tag_Anchor
    -- Tag_Abbreviation
    -- Tag_Address
    -- Tag_Area
    -- Tag_Article
    -- Tag_Aside
    -- Tag_Audio
    -- Tag_Bold
    -- Tag_Base
    -- Tag_BiDirectionalIsolation
    -- Tag_BiDirectionalOverride
    -- Tag_Blockquote
    -- Tag_Body
    -- Tag_Break
    -- Tag_Button
    -- Tag_Canvas
    -- Tag_Caption
    -- Tag_Cite
    -- Tag_Code
    -- Tag_Column
    -- Tag_ColumnGroup
    -- Tag_Data
    -- Tag_DataList
    -- Tag_Description
    -- Tag_Deletion
    -- Tag_Details
    -- Tag_Definition
    -- Tag_Dialog
    -- Tag_Division
    -- Tag_DescriptionList
    -- Tag_DescriptionTerm
    -- Tag_Emphasis
    -- Tag_Embed
    -- Tag_Fieldset
    -- Tag_FigureCaption
    -- Tag_Figure
    -- Tag_Footer
    -- Tag_Form
    -- Tag_H1
    -- Tag_H2
    -- Tag_H3
    -- Tag_H4
    -- Tag_H5
    -- Tag_H6
    -- Tag_Head
    -- Tag_Header
    -- Tag_HeadingGroup
    -- Tag_HorizontalRule
    -- Tag_Html
    -- Tag_Italic
    -- Tag_IFrame
    -- Tag_Image
    -- Tag_Input
    -- Tag_Insertion
    -- Tag_KeyboardInput
    -- Tag_Label
    -- Tag_Legend
    -- Tag_ListItem
    -- Tag_Link
    -- Tag_Main
    -- Tag_Map
    -- Tag_Mark
    -- Tag_Menu
    -- Tag_Meta
    -- Tag_Meter
    -- Tag_Nav
    -- Tag_NoScript
    -- Tag_Object
    -- Tag_OrderedList
    -- Tag_OptionGroup
    -- Tag_Option
    -- Tag_Output
    -- Tag_Paragraph
    -- Tag_Picture
    -- Tag_PreformattedText
    -- Tag_Progress
    -- Tag_Quotation
    -- Tag_RubyParenthesis
    -- Tag_RubyText
    -- Tag_Ruby
    -- Tag_Sample
    -- Tag_Script
    -- Tag_Search
    -- Tag_Section
    -- Tag_Select
    -- Tag_Slot
    -- Tag_Small
    -- Tag_Source
    -- Tag_Span
    -- Tag_Strikethrough
    -- Tag_Strong
    -- Tag_Style
    -- Tag_Subscript
    -- Tag_Summary
    -- Tag_Superscript
    -- Tag_Table
    -- Tag_TableBody
    -- Tag_TableDataCell
    -- Tag_TableFooter
    -- Tag_TableHeaderCell
    -- Tag_TableHeader
    -- Tag_TableRow
    -- Tag_Template
    -- Tag_TextArea
    -- Tag_Time
    -- Tag_Title
    -- Tag_Track
    -- Tag_Underline
    -- Tag_UnorderedList
    -- Tag_Variable
    -- Tag_Video
    -- Tag_WordBreakOpportunity

-- This represents an element that, for one reason or another, does not contain
-- child elements.
--
data NoContent
  -- OmitTag means the tag is self closing, and thus omits a closing tag.
  = OmitTag
  -- WithTag means the tag requires an explicit closing tag despite not being
  -- able to contain child elements.
  | WithTag

buildTag :: T.Text
         -> [Attribute attr]
         -> Either NoContent [ChildHTML parent]
         -> T.Text
buildTag tag attributes content =
  T.concat
    [ "<"
    , tag
    , B.bool " " T.empty $ L.null attributes
    , T.unwords $ mapMaybe renderAttribute attributes
    , case content of
        Left  OmitTag   -> " />"
        Left  WithTag   -> ">"
        Right _children -> ">"
    , case content of
        Left  _type    -> T.empty
        Right children -> foldMap renderHTML children
    , case content of
        Left  OmitTag   -> T.empty
        Left  WithTag   -> "</" <> tag <> ">"
        Right _children -> "</" <> tag <> ">"
    ]

renderAttribute :: Attribute any -> Maybe T.Text
renderAttribute attr =
  case attr of
    Id       _id        -> Just $ "id=\"" <> _id <> "\""
    Class    _class     -> Just $ "class=\"" <> _class <> "\""
    Width    width      -> Just $ "width=\"" <> T.pack (show width) <> "\""
    Disabled isDisabled -> B.bool Nothing (Just "disabled") isDisabled
