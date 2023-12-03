{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- This module exports some type lists representing elements grouped together
-- over a common trait - typically, the elements under which they're valid
-- children. This is a convenience for defining valid child elements in
-- Data.HTML.Elements, and occasionally for defining valid attributes in
-- Data.HTML.Attributes (such as with global attributes).
module Data.HTML.Elements.TagGroups
  ( VoidElement
  , NoContent
  , NonElement
  , Headings
  , ListElements
  , TableElements
  , TableRowElements
  , MetadataContent
  , FlowContent
  , SectioningContent
  , HeadingContent
  , PhrasingContent
  , EmbeddedContent
  , InteractiveContent
  , FormAssociatedContent
  , ListedContent
  , LabelableContent
  , SubmittableContent
  , ResettableContent
  , ScriptSupportingContent
  ) where

import           Data.Kind (Type)

import qualified Data.HTML.Elements.Tags as Tags

type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- This list represents void elements - used when defining elements that are
-- self-closing.
--
-- This is provided to give semantic meaning to members of type families that
-- would look identical, but are otherwise representing different states of
-- being.
--
type VoidElement = '[]

-- This list represents elements that cannot contain content - this is
-- different from a void element in that it MUST have a closing tag, so is not
-- self-closing, but still can contain no child elements.
--
-- This is provided to give semantic meaning to members of type families that
-- would look identical, but are otherwise representing different states of
-- being.
--
type NoContent = '[]

-- This list represents non-element entities such as text content and comments.
--
-- This is provided to give semantic meaning to members of type families that
-- would look identical, but are otherwise representing different states of
-- being.
--
type NonElement = '[]

type Headings =
  '[ Tags.H1
   , Tags.H2
   , Tags.H3
   , Tags.H4
   , Tags.H5
   , Tags.H6
   ]

type ListElements =
  '[ Tags.ListItem
   , Tags.Script
   , Tags.Template
   ]

type TableElements =
  '[ Tags.Caption
   , Tags.ColumnGroup
   , Tags.TableHeader
   , Tags.TableBody
   , Tags.TableFooter
   , Tags.TableRow
   ]

type TableRowElements =
  Tags.TableDataCell
    ': Tags.TableHeaderCell
    ': ScriptSupportingContent

-- The following lists represent content categories as defined in the HTML
-- documentation. See here for more information:
--
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Content_categories

-- This list represents all elements that are considered metadata content.
--
type MetadataContent =
  '[ Tags.Base
   , Tags.Link
   , Tags.Meta
   , Tags.NoScript
   , Tags.Script
   , Tags.Style
   , Tags.Title
   ]

-- This list represents all elements that are considered flow content.
--
type FlowContent =
  '[ Tags.Text
   , Tags.Anchor
   , Tags.Abbreviation
   , Tags.Address
   , Tags.Article
   , Tags.Aside
   , Tags.Audio
   , Tags.Bold
   , Tags.BiDirectionalIsolation
   , Tags.BiDirectionalOverride
   , Tags.Blockquote
   , Tags.Break
   , Tags.Button
   , Tags.Canvas
   , Tags.Cite
   , Tags.Code
   , Tags.Data
   , Tags.DataList
   , Tags.Deletion
   , Tags.Details
   , Tags.Definition
   , Tags.Dialog
   , Tags.Division
   , Tags.DescriptionList
   , Tags.Emphasis
   , Tags.Embed
   , Tags.Fieldset
   , Tags.Figure
   , Tags.Footer
   , Tags.Form
   , Tags.H1
   , Tags.H2
   , Tags.H3
   , Tags.H4
   , Tags.H5
   , Tags.H6
   , Tags.Header
   , Tags.HeadingGroup
   , Tags.HorizontalRule
   , Tags.Italic
   , Tags.IFrame
   , Tags.Image
   , Tags.Input
   , Tags.Insertion
   , Tags.KeyboardInput
   , Tags.Label
   , Tags.Main
   , Tags.Map
   , Tags.Mark
-- , Tags.Math
   , Tags.Menu
   , Tags.Meter
   , Tags.Nav
   , Tags.NoScript
   , Tags.Object
   , Tags.OrderedList
   , Tags.Output
   , Tags.Paragraph
   , Tags.Picture
   , Tags.PreformattedText
   , Tags.Progress
   , Tags.Quotation
   , Tags.Ruby
   , Tags.Strikethrough
   , Tags.Sample
   , Tags.Search
   , Tags.Script
   , Tags.Section
   , Tags.Select
   , Tags.Slot
   , Tags.Small
   , Tags.Span
   , Tags.Strong
   , Tags.Subscript
   , Tags.Superscript
-- , Tags.SVG
   , Tags.Table
   , Tags.Template
   , Tags.TextArea
   , Tags.Time
   , Tags.Underline
   , Tags.UnorderedList
   , Tags.Variable
   , Tags.Video
   , Tags.WordBreakOpportunity
   ]

-- This list represents all elements that are considered sectioning content.
--
type SectioningContent =
  '[ Tags.Article
   , Tags.Aside
   , Tags.Nav
   , Tags.Section
   ]

-- This list represents all elements that are considered sectioning content.
--
type HeadingContent =
  Tags.HeadingGroup ': Headings

-- This list represents all elements that are considered phrasing content.
--
type PhrasingContent =
  '[ Tags.Text
   , Tags.Abbreviation
   , Tags.Audio
   , Tags.Bold
   , Tags.BiDirectionalIsolation
   , Tags.BiDirectionalOverride
   , Tags.Break
   , Tags.Button
   , Tags.Canvas
   , Tags.Cite
   , Tags.Code
   , Tags.Data
   , Tags.DataList
   , Tags.Definition
   , Tags.Emphasis
   , Tags.Embed
   , Tags.Italic
   , Tags.IFrame
   , Tags.Image
   , Tags.Input
   , Tags.KeyboardInput
   , Tags.Label
   , Tags.Mark
-- , Tags.Math
   , Tags.Meter
   , Tags.NoScript
   , Tags.Object
   , Tags.Output
   , Tags.Picture
   , Tags.Progress
   , Tags.Quotation
   , Tags.Ruby
   , Tags.Strikethrough
   , Tags.Sample
   , Tags.Script
   , Tags.Select
   , Tags.Slot
   , Tags.Small
   , Tags.Span
   , Tags.Strong
   , Tags.Subscript
   , Tags.Superscript
-- , Tags.SVG
   , Tags.Template
   , Tags.TextArea
   , Tags.Time
   , Tags.Underline
   , Tags.Variable
   , Tags.Video
   , Tags.WordBreakOpportunity
   ]

-- This list represents all elements that are considered embedded content.
--
type EmbeddedContent =
  '[ Tags.Audio
   , Tags.Canvas
   , Tags.Embed
   , Tags.IFrame
   , Tags.Image
-- , Tags.Math
   , Tags.Object
   , Tags.Picture
-- , Tags.SVG
   , Tags.Video
   ]

-- This list represents all elements that are considered interactive content.
--
type InteractiveContent =
  '[ Tags.Button
   , Tags.Details
   , Tags.Embed
   , Tags.IFrame
   , Tags.Label
   , Tags.Select
   , Tags.TextArea
   ]

-- This list represents all elements that are considered form-associated
-- content.
--
type FormAssociatedContent =
  '[ Tags.Button
   , Tags.Fieldset
   , Tags.Input
   , Tags.Label
   , Tags.Meter
   , Tags.Object
   , Tags.Output
   , Tags.Progress
   , Tags.Select
   , Tags.TextArea
   ]

-- This list represents all elements that are of the listed subtype of
-- form-associated content.
--
type ListedContent =
  '[ Tags.Button
   , Tags.Fieldset
   , Tags.Input
   , Tags.Object
   , Tags.Output
   , Tags.Select
   , Tags.TextArea
   ]

-- This list represents all elements that are of the labelable subtype of
-- form-associated content.
--
type LabelableContent =
  '[ Tags.Button
   , Tags.Input
   , Tags.Meter
   , Tags.Output
   , Tags.Progress
   , Tags.Select
   , Tags.TextArea
   ]

-- This list represents all elements that are of the submittable subtype of
-- form-associated content.
--
type SubmittableContent =
  '[ Tags.Button
   , Tags.Input
   , Tags.Object
   , Tags.Select
   , Tags.TextArea
   ]

-- This list represents all elements that are of the resettable subtype of
-- form-associated content.
--
type ResettableContent =
  '[ Tags.Input
   , Tags.Output
   , Tags.Select
   , Tags.TextArea
   ]

-- This list represents all elements that are considered script-supporting
-- elements.
--
type ScriptSupportingContent =
  '[ Tags.Script
   , Tags.Template
   ]
