{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Elements.TagGroups
  ( AllElements
  , TextOnly
  , Headings
  , ListContent
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
  , TransparentContent
  , LegendContent
  , RubyContent
  , SummaryContent
  , TableContent
  , TableRowContent
  , TableRowOnly
  , CrossOriginTags
  , DisableableTags
  ) where

import HTML.Elements.TagType (TagType(..))

type family (xs :: [TagType]) ++ (ys :: [TagType]) :: [TagType] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type AllElements =
  [ 'Document
  , 'Comment
  , 'Text
  , 'RawHTML
  , 'CustomHTML
  , 'Anchor
  , 'Abbreviation
  , 'ContactAddress
  , 'Area
  , 'Article
  , 'Aside
  , 'Audio
  , 'BringAttentionTo
  , 'Base
  , 'BidirectionalIsolation
  , 'BidirectionalOverride
  , 'Blockquote
  , 'Body
  , 'LineBreak
  , 'Button
  , 'Canvas
  , 'TableCaption
  , 'Citation
  , 'Code
  , 'TableColumn
  , 'TableColumnGroup
  , 'Data
  , 'DataList
  , 'DescriptionDetails
  , 'DeletedText
  , 'Details
  , 'Definition
  , 'Dialog
  , 'Division
  , 'DescriptionList
  , 'DescriptionTerm
  , 'Emphasis
  , 'Embed
  , 'Fieldset
  , 'FigureCaption
  , 'Figure
  , 'Footer
  , 'Form
  , 'H1
  , 'H2
  , 'H3
  , 'H4
  , 'H5
  , 'H6
  , 'Head
  , 'Header
  , 'HeadingGroup
  , 'HorizontalRule
  , 'Html
  , 'IdiomaticText
  , 'IFrame
  , 'Image
  , 'Input
  , 'InsertedText
  , 'KeyboardInput
  , 'Label
  , 'Legend
  , 'ListItem
  , 'Link
  , 'Main
  , 'Map
  , 'Mark
  , 'Menu
  , 'Meta
  , 'Meter
  , 'Nav
  , 'NoScript
  , 'Object
  , 'OrderedList
  , 'OptionGroup
  , 'Option
  , 'Output
  , 'Paragraph
  , 'Picture
  , 'PreformattedText
  , 'Progress
  , 'Quotation
  , 'RubyParenthesis
  , 'RubyText
  , 'Ruby
  , 'Strikethrough
  , 'Sample
  , 'Script
  , 'Search
  , 'Section
  , 'Select
  , 'Slot
  , 'SideComment
  , 'Source
  , 'Span
  , 'Strong
  , 'Style
  , 'Subscript
  , 'Summary
  , 'Superscript
  , 'Table
  , 'TableBody
  , 'TableDataCell
  , 'ContentTemplate
  , 'TextArea
  , 'TableFoot
  , 'TableHeader
  , 'TableHead
  , 'Time
  , 'Title
  , 'TableRow
  , 'Track
  , 'Underline
  , 'UnorderedList
  , 'Variable
  , 'Video
  , 'WordBreakOpportunity
  ]

type TextOnly =
  '[ Text ]

type Headings =
  [ 'H1
  , 'H2
  , 'H3
  , 'H4
  , 'H5
  , 'H6
  ]

type ListContent =
  'ListItem ': ScriptSupportingContent

-- The following lists represent content categories as defined in the HTML
-- documentation. See here for more information:
--
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Content_categories

-- This list represents all elements that are considered metadata content.
--
type MetadataContent =
  [ 'Base
  , 'Link
  , 'Meta
  , 'NoScript
  , 'Script
  , 'Style
  , 'Title
  ]

-- This list represents all elements that are considered flow content.
--
type FlowContent =
  [ 'Text
  , 'Anchor
  , 'Abbreviation
  , 'ContactAddress
  , 'Article
  , 'Aside
  , 'Audio
  , 'BringAttentionTo
  , 'BidirectionalIsolation
  , 'BidirectionalOverride
  , 'Blockquote
  , 'LineBreak
  , 'Button
  , 'Canvas
  , 'Citation
  , 'Code
  , 'Data
  , 'DataList
  , 'DeletedText
  , 'Details
  , 'Definition
  , 'Dialog
  , 'Division
  , 'DescriptionList
  , 'Emphasis
  , 'Embed
  , 'Fieldset
  , 'Figure
  , 'Footer
  , 'Form
  , 'H1
  , 'H2
  , 'H3
  , 'H4
  , 'H5
  , 'H6
  , 'Header
  , 'HeadingGroup
  , 'HorizontalRule
  , 'IdiomaticText
  , 'IFrame
  , 'Image
  , 'Input
  , 'InsertedText
  , 'KeyboardInput
  , 'Label
  , 'Main
  , 'Map
  , 'Mark
  --' , Math
  , 'Menu
  , 'Meter
  , 'Nav
  , 'NoScript
  , 'Object
  , 'OrderedList
  , 'Output
  , 'Paragraph
  , 'Picture
  , 'PreformattedText
  , 'Progress
  , 'Quotation
  , 'Ruby
  , 'Strikethrough
  , 'Sample
  , 'Search
  , 'Script
  , 'Section
  , 'Select
  , 'Slot
  , 'SideComment
  , 'Span
  , 'Strong
  , 'Subscript
  , 'Superscript
  --' , SVG
  , 'Table
  , 'ContentTemplate
  , 'TextArea
  , 'Time
  , 'Underline
  , 'UnorderedList
  , 'Variable
  , 'Video
  , 'WordBreakOpportunity
  ]

-- This list represents all elements that are considered sectioning content.
--
type SectioningContent =
  [ 'Article
  , 'Aside
  , 'Nav
  , 'Section
  ]

-- This list represents all elements that are considered sectioning content.
--
type HeadingContent =
  'HeadingGroup ': Headings

-- This list represents all elements that are considered phrasing content.
--
type PhrasingContent =
  [ 'Text
  , 'Abbreviation
  , 'Audio
  , 'BringAttentionTo
  , 'BidirectionalIsolation
  , 'BidirectionalOverride
  , 'LineBreak
  , 'Button
  , 'Canvas
  , 'Citation
  , 'Code
  , 'Data
  , 'DataList
  , 'Definition
  , 'Emphasis
  , 'Embed
  , 'IdiomaticText
  , 'IFrame
  , 'Image
  , 'Input
  , 'KeyboardInput
  , 'Label
  , 'Mark
  --' , Math
  , 'Meter
  , 'NoScript
  , 'Object
  , 'Output
  , 'Picture
  , 'Progress
  , 'Quotation
  , 'Ruby
  , 'Strikethrough
  , 'Sample
  , 'Script
  , 'Select
  , 'Slot
  , 'SideComment
  , 'Span
  , 'Strong
  , 'Subscript
  , 'Superscript
  --' , SVG
  , 'ContentTemplate
  , 'TextArea
  , 'Time
  , 'Underline
  , 'Variable
  , 'Video
  , 'WordBreakOpportunity
  ]

-- This list represents all elements that are considered embedded content.
--
type EmbeddedContent =
  [ 'Audio
  , 'Canvas
  , 'Embed
  , 'IFrame
  , 'Image
  --' , Math
  , 'Object
  , 'Picture
  --' , SVG
  , 'Video
  ]

-- This list represents all elements that are considered interactive content.
--
type InteractiveContent =
  [ 'Button
  , 'Details
  , 'Embed
  , 'IFrame
  , 'Label
  , 'Select
  , 'TextArea
  ]

-- This list represents all elements that are considered form-associated
-- content.
--
type FormAssociatedContent =
  [ 'Button
  , 'Fieldset
  , 'Input
  , 'Label
  , 'Meter
  , 'Object
  , 'Output
  , 'Progress
  , 'Select
  , 'TextArea
  ]

-- This list represents all elements that are of the listed subtype of
-- form-associated content.
--
type ListedContent =
  [ 'Button
  , 'Fieldset
  , 'Input
  , 'Object
  , 'Output
  , 'Select
  , 'TextArea
  ]

-- This list represents all elements that are of the labelable subtype of
-- form-associated content.
--
type LabelableContent =
  [ 'Button
  , 'Input
  , 'Meter
  , 'Output
  , 'Progress
  , 'Select
  , 'TextArea
  ]

-- This list represents all elements that are of the submittable subtype of
-- form-associated content.
--
type SubmittableContent =
  [ 'Button
  , 'Input
  , 'Object
  , 'Select
  , 'TextArea
  ]

-- This list represents all elements that are of the resettable subtype of
-- form-associated content.
--
type ResettableContent =
  [ 'Input
  , 'Output
  , 'Select
  , 'TextArea
  ]

-- This list represents all elements that are considered script-supporting
-- elements.
--
type ScriptSupportingContent =
  [ 'Script
  , 'ContentTemplate
  ]

type TransparentContent =
  [ 'Anchor
  , 'Audio
  , 'Canvas
  , 'DeletedText
  , 'InsertedText
  , 'Map
  , 'NoScript
  , 'Object
  , 'Slot
  , 'Video
  ]

-- This list represents all elements that are valid under a `<legend>` tag.
type LegendContent =
  'H1
    ': 'H2
    ': 'H3
    ': 'H4
    ': 'H5
    ': 'H6
    ': PhrasingContent

-- This list represents all elements that are valid under a `<ruby>` tag.
type RubyContent =
  'RubyParenthesis
    ': 'RubyText
    ': PhrasingContent

-- This list represents all elements that are valid under a `<summary>` tag.
type SummaryContent =
  'HeadingGroup
    ': 'H1
    ': 'H2
    ': 'H3
    ': 'H4
    ': 'H5
    ': 'H6
    ': PhrasingContent

-- This list represents all elements that are valid under a `<table>` tag.
type TableContent =
  [ 'TableCaption
  , 'TableColumnGroup
  , 'TableHead
  , 'TableBody
  , 'TableRow
  , 'TableFoot
  ]

-- This list represents all elements that are valid under a `<tr>` tag.
type TableRowContent =
  'TableDataCell
    ': 'TableHeader
    ': ScriptSupportingContent

type TableRowOnly =
  '[ TableRow ]

-- Attribute-Focused Tag Groups
--

type CrossOriginTags =
  [ 'Audio
  , 'CustomHTML
  , 'Image
  , 'Link
  , 'Script
  , 'Video
  ]

type DisableableTags =
  [ 'Button
  , 'CustomHTML
  , 'Fieldset
  , 'Input
  , 'OptionGroup
  , 'Option
  , 'Select
  , 'TextArea
  ]
