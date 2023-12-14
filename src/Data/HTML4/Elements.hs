{-# LANGUAGE DataKinds #-}

module Data.HTML4.Elements
  ( Document
  , HTML
  , ChildHTML
  , Tags.Comment
  , Tags.Text
  , Tags.Anchor, a
  , Tags.Abbreviation
  , Tags.Address
  , Tags.Area
  , Tags.Article
  , Tags.Aside
  , Tags.Audio
  , Tags.Bold
  , Tags.Base
  , Tags.BiDirectionalIsolation
  , Tags.BiDirectionalOverride
  , Tags.Blockquote
  , Tags.Body
  , Tags.Break
  , Tags.Button
  , Tags.Canvas
  , Tags.Caption
  , Tags.Cite
  , Tags.Code
  , Tags.Column
  , Tags.ColumnGroup
  , Tags.Data
  , Tags.DataList
  , Tags.Description
  , Tags.Deletion
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
  , Tags.Italic
  , Tags.IFrame, iframe
  , Tags.Image, img
  , Tags.Input
  , Tags.Insertion
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
  , Tags.Sample
  , Tags.Script
  , Tags.Search
  , Tags.Section
  , Tags.Select
  , Tags.Slot
  , Tags.Small
  , Tags.Source
  , Tags.Span, span
  , Tags.Strikethrough
  , Tags.Strong
  , Tags.Style
  , Tags.Subscript
  , Tags.Summary
  , Tags.Superscript
  , Tags.Table
  , Tags.TableBody
  , Tags.TableDataCell
  , Tags.TableFooter
  , Tags.TableHeaderCell
  , Tags.TableHeader
  , Tags.TableRow
  , Tags.Template
  , Tags.TextArea
  , Tags.Time
  , Tags.Title
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
a = A

div :: ValidChild Tags.Division parent
    => [Attribute Tags.Division]
    -> [ChildHTML Tags.Division]
    -> ChildHTML parent
div = Div

span :: ValidChild Tags.Span parent
     => [Attribute Tags.Span]
     -> [ChildHTML Tags.Span]
     -> ChildHTML parent
span = Span

p :: ValidChild Tags.Paragraph parent
  => [Attribute Tags.Paragraph]
  -> [ChildHTML Tags.Paragraph]
  -> ChildHTML parent
p = P

h1 :: ValidChild Tags.H1 parent
   => [Attribute Tags.H1]
   -> [ChildHTML Tags.H1]
   -> ChildHTML parent
h1 = H1

ul :: ValidChild Tags.UnorderedList parent
   => [Attribute Tags.UnorderedList]
   -> [ChildHTML Tags.UnorderedList]
   -> ChildHTML parent
ul = Ul

li :: ValidChild Tags.ListItem parent
   => [Attribute Tags.ListItem]
   -> [ChildHTML Tags.ListItem]
   -> ChildHTML parent
li = Li

img :: ValidChild Tags.Image parent
    => [Attribute Tags.Image] -> ChildHTML parent
img = Img

iframe :: ValidChild Tags.IFrame parent
       => [Attribute Tags.IFrame] -> ChildHTML parent
iframe = Iframe

html :: ValidChild Tags.Html Tags.Document
     => [Attribute Tags.Html]
     -> [ChildHTML Tags.Html]
     -> Document
html = Html
