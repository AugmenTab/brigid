{-# LANGUAGE DataKinds #-}

module Data.HTML.Elements
  ( HTML
  , ValidChildOf
  , Tags.Comment
  , Tags.Text
  , Tags.Anchor, a
  , Tags.Abbreviation
  , Tags.Address
  , Tags.Area
  , Tags.Article
  , Tags.Aside
  , Tags.Audio
  , Tags.Bold, b
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

import Prelude hiding (div, span)
import Data.Text qualified as T
import Text.Blaze qualified as Blaze
import Text.Blaze.Html qualified as H
import Text.Blaze.Html5 qualified as H

import Data.HTML.Attributes (ValidAttributeOf)
import Data.HTML.Elements.Children (ValidChildrenFor)
import Data.HTML.Elements.Tags qualified as Tags
import Data.HTML.Elements.TagType (TagType, Leaf)
import Data.HTML.Types (Elem)

type HTML element parent =
  IsValidChild element parent => ValidChildOf parent

type IsValidChild child parent =
  Elem child (ValidChildrenFor parent)

data ValidChildOf parent where
  WrapElement :: IsValidChild child parent
              => Node child -> ValidChildOf parent

data Node tag
  = Element (ElementDetails tag)
  | NonElement (Content tag)

data SelfClosing = SelfClosing

data ElementDetails tag =
  ElementDetails
    { tag        :: TagType tag
    , attributes :: [ValidAttributeOf tag]
    , children   :: Either SelfClosing [ValidChildOf tag]
    }

newtype Content tag = Content { unContent :: TagType tag }

createElement :: IsValidChild element parent
              => TagType element
              -> [ValidAttributeOf element]
              -> [ValidChildOf element]
              -> ValidChildOf parent
createElement element attributes children =
  WrapElement
    . Element
    $ ElementDetails
        { tag        = element
        , attributes = attributes
        , children   = Right children
        }

createElementSelfClosing :: IsValidChild element parent
                         => TagType element
                         -> [ValidAttributeOf element]
                         -> ValidChildOf parent
createElementSelfClosing element attributes =
  WrapElement
    . Element
    $ ElementDetails
        { tag        = element
        , attributes = attributes
        , children   = Left SelfClosing
        }

-- createContent :: IsValidChild content parent
--               => TagType content -> ValidChildOf parent
-- createContent = WrapElement . NonElement . Content

-- comment :: IsValidChild Tags.Comment parent
--         => T.Text -> ValidChildOf parent
-- comment = createContent . H.toHtml . Blaze.textComment

-- text :: IsValidChild Tags.Text parent
--      => T.Text -> ValidChildOf parent
-- text = createContent . H.toHtml . Blaze.text

-- texts :: IsValidChild Tags.Text parent
--       => [T.Text] -> ValidChildOf parent
-- texts = text . T.unwords

a :: IsValidChild Tags.Anchor parent
  => [ValidAttributeOf Tags.Anchor]
  -> [ValidChildOf Tags.Anchor]
  -> ValidChildOf parent
a = createElement H.a

b :: IsValidChild Tags.Bold parent
  => [ValidAttributeOf Tags.Bold]
  -> [ValidChildOf Tags.Bold]
  -> ValidChildOf parent
b = createElement H.b

div :: IsValidChild Tags.Division parent
    => [ValidAttributeOf Tags.Division]
    -> [ValidChildOf Tags.Division]
    -> ValidChildOf parent
div = createElement H.div

h1 :: IsValidChild Tags.H1 parent
   => [ValidAttributeOf Tags.H1]
   -> [ValidChildOf Tags.H1]
   -> ValidChildOf parent
h1 = createElement H.h1

img :: IsValidChild Tags.Image parent
    => [ValidAttributeOf Tags.Image]
    -> ValidChildOf parent
img = createElementSelfClosing H.img

span :: IsValidChild Tags.Span parent
     => [ValidAttributeOf Tags.Span]
     -> [ValidChildOf Tags.Span]
     -> ValidChildOf parent
span = createElement H.span
