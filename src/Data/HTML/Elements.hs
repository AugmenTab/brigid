{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML.Elements
  ( HTML
  , ValidChildOf (WrapElement)

  , Tags.Comment, comment
  , Tags.TextContent, text
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
  , Tags.Division
  , Tags.DescriptionList
  , Tags.DescriptionTerm
  , Tags.Emphasis
  , Tags.Embed
  , Tags.Fieldset
  , Tags.FigureCaption
  , Tags.Figure
  , Tags.Footer
  , Tags.Form
  , Tags.H1
  , Tags.H2
  , Tags.H3
  , Tags.H4
  , Tags.H5
  , Tags.H6
  , Tags.Head
  , Tags.Header
  , Tags.HeadingGroup
  , Tags.HorizontalRule
  , Tags.Html
  , Tags.Italic
  , Tags.IFrame
  , Tags.Image, img
  , Tags.Input
  , Tags.Insertion
  , Tags.KeyboardInput
  , Tags.Label
  , Tags.Legend
  , Tags.ListItem
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
  , Tags.Paragraph
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
  , Tags.Span
  , Tags.Strikethrough
  , Tags.Strong
  , Tags.Style
  , Tags.Subscript
  , Tags.Summary
  , Tags.Superscript
  , Tags.Table
  , Tags.TableBody
  , Tags.TableDataCell
  , Tags.TextArea
  , Tags.TableFooter
  , Tags.TableHeaderCell
  , Tags.TableHeader
  , Tags.TableRow
  , Tags.Template
  , Tags.Time
  , Tags.Title
  , Tags.Track
  , Tags.Underline
  , Tags.UnorderedList
  , Tags.Variable
  , Tags.Video
  , Tags.WordBreakOpportunity
  ) where

import           Data.Kind (Type)
import qualified Data.Text as T
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H

import           Data.HTML.Attributes (ValidAttributeOf)
import qualified Data.HTML.Elements.Tags as Tags
import qualified Data.HTML.Elements.TagGroups as TagGroups
import           Data.HTML.Elements.TagType (TagType)
import           Data.HTML.Types (Elem)

type family ValidChildrenFor element :: [Type] where
  ValidChildrenFor Tags.Comment                = TagGroups.NonElement
  ValidChildrenFor Tags.TextContent            = TagGroups.NonElement

--ValidChildrenFor Tags.Anchor                 = _
  ValidChildrenFor Tags.Abbreviation           = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Address                = _
  ValidChildrenFor Tags.Area                   = TagGroups.VoidElement
  ValidChildrenFor Tags.Article                = TagGroups.FlowContent
  ValidChildrenFor Tags.Aside                  = TagGroups.FlowContent
--ValidChildrenFor Tags.Audio                  = _
  ValidChildrenFor Tags.Bold                   = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Base                   = TagGroups.VoidElement
  ValidChildrenFor Tags.BiDirectionalIsolation = TagGroups.PhrasingContent
  ValidChildrenFor Tags.BiDirectionalOverride  = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Blockquote             = TagGroups.FlowContent
  ValidChildrenFor Tags.Body                   = TagGroups.FlowContent
  ValidChildrenFor Tags.Break                  = TagGroups.VoidElement
--ValidChildrenFor Tags.Button                 = _
--ValidChildrenFor Tags.Canvas                 = _
  ValidChildrenFor Tags.Caption                = TagGroups.FlowContent
  ValidChildrenFor Tags.Cite                   = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Code                   = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Column                 = TagGroups.VoidElement
  ValidChildrenFor Tags.ColumnGroup            = '[Tags.Column]
  ValidChildrenFor Tags.Data                   = TagGroups.PhrasingContent
--ValidChildrenFor Tags.DataList               = _
  ValidChildrenFor Tags.Description            = TagGroups.FlowContent
--ValidChildrenFor Tags.Deletion               = _
--ValidChildrenFor Tags.Details                = _
--ValidChildrenFor Tags.Definition             = _
  ValidChildrenFor Tags.Dialog                 = TagGroups.FlowContent
  ValidChildrenFor Tags.Division               = TagGroups.FlowContent
--ValidChildrenFor Tags.DescriptionList        = _
--ValidChildrenFor Tags.DescriptionTerm        = _
  ValidChildrenFor Tags.Emphasis               = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Embed                  = TagGroups.VoidElement
  ValidChildrenFor Tags.Fieldset               = Tags.Legend ': TagGroups.FlowContent
  ValidChildrenFor Tags.FigureCaption          = TagGroups.FlowContent
  ValidChildrenFor Tags.Figure                 = Tags.FigureCaption ': TagGroups.FlowContent
--ValidChildrenFor Tags.Footer                 = _
--ValidChildrenFor Tags.Form                   = _
  ValidChildrenFor Tags.H1                     = TagGroups.PhrasingContent
  ValidChildrenFor Tags.H2                     = TagGroups.PhrasingContent
  ValidChildrenFor Tags.H3                     = TagGroups.PhrasingContent
  ValidChildrenFor Tags.H4                     = TagGroups.PhrasingContent
  ValidChildrenFor Tags.H5                     = TagGroups.PhrasingContent
  ValidChildrenFor Tags.H6                     = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Head                   = _
--ValidChildrenFor Tags.Header                 = _
  ValidChildrenFor Tags.HeadingGroup           = Tags.Paragraph ': TagGroups.Headings
  ValidChildrenFor Tags.HorizontalRule         = TagGroups.VoidElement
  ValidChildrenFor Tags.Html                   = '[Tags.Head, Tags.Body]
  ValidChildrenFor Tags.Italic                 = TagGroups.PhrasingContent
  ValidChildrenFor Tags.IFrame                 = TagGroups.NoContent
  ValidChildrenFor Tags.Image                  = TagGroups.VoidElement
  ValidChildrenFor Tags.Input                  = TagGroups.VoidElement
--ValidChildrenFor Tags.Insertion              = _
  ValidChildrenFor Tags.KeyboardInput          = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Label                  = _
--ValidChildrenFor Tags.Legend                 = TagGroups.PhrasingContent ++ TagGroups.Headings
  ValidChildrenFor Tags.ListItem               = TagGroups.FlowContent
  ValidChildrenFor Tags.Link                   = TagGroups.VoidElement
  ValidChildrenFor Tags.Main                   = TagGroups.FlowContent
--ValidChildrenFor Tags.Map                    = _
  ValidChildrenFor Tags.Mark                   = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Math                   = _
  ValidChildrenFor Tags.Menu                   = TagGroups.ListElements
  ValidChildrenFor Tags.Meta                   = TagGroups.VoidElement
--ValidChildrenFor Tags.Meter                  = _
  ValidChildrenFor Tags.Nav                    = TagGroups.FlowContent
--ValidChildrenFor Tags.NoScript               = _
--ValidChildrenFor Tags.Object                 = _
  ValidChildrenFor Tags.OrderedList            = TagGroups.ListElements
  ValidChildrenFor Tags.OptionGroup            = '[Tags.Option]
  ValidChildrenFor Tags.Option                 = '[Tags.TextContent]
  ValidChildrenFor Tags.Output                 = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Paragraph              = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Picture                = Tags.Source ': Tags.Image ': TagGroups.ScriptSupportingContent
  ValidChildrenFor Tags.PreformattedText       = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Progress               = _
  ValidChildrenFor Tags.Quotation              = TagGroups.PhrasingContent
  ValidChildrenFor Tags.RubyParenthesis        = '[Tags.TextContent]
  ValidChildrenFor Tags.RubyText               = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Ruby                   = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Sample                 = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Script                 = _
  ValidChildrenFor Tags.Search                 = TagGroups.FlowContent
  ValidChildrenFor Tags.Section                = TagGroups.FlowContent
  ValidChildrenFor Tags.Select                 = '[Tags.Option, Tags.OptionGroup]
--ValidChildrenFor Tags.Slot                   = _
  ValidChildrenFor Tags.Small                  = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Source                 = TagGroups.VoidElement
  ValidChildrenFor Tags.Span                   = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Strikethrough          = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Strong                 = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Style                  = _
  ValidChildrenFor Tags.Subscript              = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Summary                = _
  ValidChildrenFor Tags.Superscript            = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Table                  = TagGroups.TableElements
  ValidChildrenFor Tags.TableBody              = '[Tags.TableRow]
  ValidChildrenFor Tags.TableDataCell          = TagGroups.FlowContent
  ValidChildrenFor Tags.TableFooter            = '[Tags.TableRow]
--ValidChildrenFor Tags.TableHeaderCell        = _
  ValidChildrenFor Tags.TableHeader            = '[Tags.TableRow]
  ValidChildrenFor Tags.TableRow               = TagGroups.TableRowElements
--ValidChildrenFor Tags.Template               = _
  ValidChildrenFor Tags.TextArea               = '[Tags.TextContent]
  ValidChildrenFor Tags.Time                   = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Title                  = '[Tags.TextContent]
  ValidChildrenFor Tags.Track                  = TagGroups.VoidElement
  ValidChildrenFor Tags.Underline              = TagGroups.PhrasingContent
  ValidChildrenFor Tags.UnorderedList          = TagGroups.ListElements
  ValidChildrenFor Tags.Variable               = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Video                  = _
  ValidChildrenFor Tags.WordBreakOpportunity   = TagGroups.VoidElement

data ValidChildOf parent where
  WrapElement :: Elem child (ValidChildrenFor parent)
              => HTML child -> ValidChildOf parent

data SelfClosing = SelfClosing

data HTML tag
  = Element (ElementDetails tag)
  | NonElement H.Html

data ElementDetails tag =
  ElementDetails
    { tag        :: TagType tag
    , attributes :: [ValidAttributeOf tag]
    , content    :: Either SelfClosing [ValidChildOf tag]
    }

createElement :: TagType element
              -> [ValidAttributeOf element]
              -> [ValidChildOf element]
              -> HTML element
createElement element attributes children =
  Element $
    ElementDetails
      { tag        = element
      , attributes = attributes
      , content    = Right children
      }

createElementSelfClosing :: TagType element
                         -> [ValidAttributeOf element]
                         -> HTML element
createElementSelfClosing element attributes =
  Element $
    ElementDetails
      { tag        = element
      , attributes = attributes
      , content    = Left SelfClosing
      }

comment :: T.Text -> HTML Tags.TextContent
comment = NonElement . H.toHtml . Blaze.textComment

text :: T.Text -> HTML Tags.TextContent
text = NonElement . H.toHtml . Blaze.text

a :: Elem child (ValidChildrenFor Tags.Anchor)
  => [ValidAttributeOf Tags.Anchor] -> [HTML child] -> HTML Tags.Anchor
a attrs children = createElement H.a attrs $ WrapElement <$> children

img :: [ValidAttributeOf Tags.Image] -> HTML Tags.Image
img = createElementSelfClosing H.img
