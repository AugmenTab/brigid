{-# LANGUAGE DataKinds #-}

module Data.HTML4.Elements
  ( Document
  , HTML
  , ChildHTML
  , Tags.Comment
  , Tags.Text
  , Tags.Anchor, a
  , Tags.Abbreviation
  , Tags.ContactAddress
  , Tags.Area
  , Tags.Article
  , Tags.Aside
  , Tags.Audio
  , Tags.BringAttentionTo
  , Tags.Base
  , Tags.BidirectionalIsolation
  , Tags.BidirectionalOverride
  , Tags.Blockquote
  , Tags.Body, body
  , Tags.LineBreak
  , Tags.Button
  , Tags.Canvas
  , Tags.TableCaption
  , Tags.Citation
  , Tags.Code
  , Tags.TableColumn
  , Tags.TableColumnGroup
  , Tags.Data
  , Tags.DataList
  , Tags.DescriptionDetails
  , Tags.DeletedText
  , Tags.Details
  , Tags.Definition
  , Tags.Dialog
  , Tags.Division, div
  , Tags.DescriptionList
  , Tags.DescriptionTerm
  , Tags.Emphasis
  , Tags.Embed
  , Tags.Fieldset
  , Tags.FigureCaption
  , Tags.Figure
  , Tags.Footer
  , Tags.Form
  , Tags.H1, h1
  , Tags.H2
  , Tags.H3
  , Tags.H4
  , Tags.H5
  , Tags.H6
  , Tags.Head
  , Tags.Header
  , Tags.HeadingGroup
  , Tags.HorizontalRule
  , Tags.Html, html
  , Tags.IdiomaticText
  , Tags.IFrame, iframe
  , Tags.Image, img
  , Tags.Input
  , Tags.InsertedText
  , Tags.KeyboardInput
  , Tags.Label
  , Tags.Legend
  , Tags.ListItem, li
  , Tags.Link
  , Tags.Main
  , Tags.Map
  , Tags.Mark
  , Tags.Menu
  , Tags.Meta
  , Tags.Meter
  , Tags.Nav
  , Tags.NoScript
  , Tags.Object
  , Tags.OrderedList
  , Tags.OptionGroup
  , Tags.Option
  , Tags.Output
  , Tags.Paragraph, p
  , Tags.Picture
  , Tags.PreformattedText
  , Tags.Progress
  , Tags.Quotation
  , Tags.RubyParenthesis
  , Tags.RubyText
  , Tags.Ruby
  , Tags.Strikethrough
  , Tags.Sample
  , Tags.Script
  , Tags.Search
  , Tags.Section
  , Tags.Select
  , Tags.Slot
  , Tags.SideComment
  , Tags.Source
  , Tags.Span, span
  , Tags.Strong
  , Tags.Style
  , Tags.Subscript
  , Tags.Summary
  , Tags.Superscript
  , Tags.Table
  , Tags.TableBody
  , Tags.TableDataCell
  , Tags.ContentTemplate
  , Tags.TextArea
  , Tags.TableFoot
  , Tags.TableHeader
  , Tags.TableHead
  , Tags.Time
  , Tags.Title
  , Tags.TableRow
  , Tags.Track
  , Tags.Underline
  , Tags.UnorderedList, ul
  , Tags.Variable
  , Tags.Video
  , Tags.WordBreakOpportunity
  ) where

import Prelude hiding (div, span)

import Data.HTML4.Attributes.Internal (Attribute(..))
import Data.HTML4.Elements.Children (ValidChild)
import Data.HTML4.Elements.Tags qualified as Tags
import Data.HTML4.Elements.Internal (Document, HTML, ChildHTML(..))

a :: ValidChild Tags.Anchor parent
  => [Attribute Tags.Anchor]
  -> [ChildHTML Tags.Anchor]
  -> ChildHTML parent
a = Tag_Anchor

body :: ValidChild Tags.Body parent
     => [Attribute Tags.Body]
     -> [ChildHTML Tags.Body]
     -> ChildHTML parent
body = Tag_Body

div :: ValidChild Tags.Division parent
    => [Attribute Tags.Division]
    -> [ChildHTML Tags.Division]
    -> ChildHTML parent
div = Tag_Division

h1 :: ValidChild Tags.H1 parent
   => [Attribute Tags.H1]
   -> [ChildHTML Tags.H1]
   -> ChildHTML parent
h1 = Tag_H1

html :: ValidChild Tags.Html Tags.Document
     => [Attribute Tags.Html]
     -> [ChildHTML Tags.Html]
     -> Document
html = Tag_Html

iframe :: ValidChild Tags.IFrame parent
       => [Attribute Tags.IFrame] -> ChildHTML parent
iframe = Tag_IFrame

img :: ValidChild Tags.Image parent
    => [Attribute Tags.Image] -> ChildHTML parent
img = Tag_Image

li :: ValidChild Tags.ListItem parent
   => [Attribute Tags.ListItem]
   -> [ChildHTML Tags.ListItem]
   -> ChildHTML parent
li = Tag_ListItem

p :: ValidChild Tags.Paragraph parent
  => [Attribute Tags.Paragraph]
  -> [ChildHTML Tags.Paragraph]
  -> ChildHTML parent
p = Tag_Paragraph

span :: ValidChild Tags.Span parent
     => [Attribute Tags.Span]
     -> [ChildHTML Tags.Span]
     -> ChildHTML parent
span = Tag_Span

ul :: ValidChild Tags.UnorderedList parent
   => [Attribute Tags.UnorderedList]
   -> [ChildHTML Tags.UnorderedList]
   -> ChildHTML parent
ul = Tag_UnorderedList
