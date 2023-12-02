{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML.Elements
  ( HTML
  , ValidChildOf (WrapElement)

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
  , Tags.Keygen
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
  , Tags.Comment, comment
  , Tags.TextContent, text
  ) where

import           Data.Kind (Type)
import qualified Data.Text as T
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H

import           Data.HTML.Attributes (ValidAttributeOf)
import qualified Data.HTML.Elements.Tags as Tags
import           Data.HTML.Elements.TagType (TagType)
import           Data.HTML.Types (Elem)

type family ValidChildrenFor element :: [Type] where
  ValidChildrenFor Tags.Anchor = '[Tags.Anchor]

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

a :: Elem child (ValidChildrenFor Tags.Anchor)
  => [ValidAttributeOf Tags.Anchor] -> [HTML child] -> HTML Tags.Anchor
a attrs children = createElement H.a attrs $ WrapElement <$> children

img :: [ValidAttributeOf Tags.Image] -> HTML Tags.Image
img = createElementSelfClosing H.img

comment :: T.Text -> HTML Tags.TextContent
comment = NonElement . H.toHtml . Blaze.textComment

text :: T.Text -> HTML Tags.TextContent
text = NonElement . H.toHtml . Blaze.text
