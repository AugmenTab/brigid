{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Elements.TagGroups
  ( AllElements
  , Headings
  , ListContent

  , EmbeddedContent
  , FlowContent
  , FormAssociatedContent
  , HeadingContent
  , InteractiveContent
  , LabelableContent
  , ListedContent
  , MarginalContent
  , MediaContent
  , MetadataContent
  , PhrasingContent
  , ResettableContent
  , ScriptSupportingContent
  , SectioningContent
  , SubmittableContent
  , TransparentContent

  , AnchorExcluded
  , CanvasExcluded
  , ContactAddressExcluded
  , DescriptionTermExcluded
  , TableHeaderExcluded

  , AudioVideoContent
  , CanvasContent
  , DescriptionListContent
  , LegendContent
  , NoScriptBodyContent
  , NoScriptHeadContent
  , PictureContent
  , RubyContent
  , SummaryContent
  , TableContent
  , TableCells
  , TableRowContent

  , AltTags
  , CheckableTags
  , CitableTags
  , CrossOriginTags
  , DirnameableTags
  , DisableableTags
  , FormMethodTags
  , FormNoValidateTags
  , HrefTags
  , InputTags
  , LabelableTags
  , LengthTags
  , NameTags
  , PlaceholderableTags
  , ReadOnlyTags
  , RelTags
  , RequireableTags
  , SizableTags
  , SrcTags
  , TargetableTags
  , TimestampableTags
  , TypeableTags
  , URLTags
  , ValuableTags
  , TextValueTags
  ) where

import Brigid.HTML.Elements.TagType (TagType(..))
import Brigid.HTML.Internal.TagOperations (Remove, Union)

type AllElements =
  [ 'Comment
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
  , 'InputButton
  , 'InputCheckbox
  , 'InputColor
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputEmail
  , 'InputFile
  , 'InputHidden
  , 'InputImage
  , 'InputMonth
  , 'InputNumber
  , 'InputPassword
  , 'InputRadio
  , 'InputRange
  , 'InputReset
  , 'InputSearch
  , 'InputSubmit
  , 'InputTel
  , 'InputText
  , 'InputTime
  , 'InputUrl
  , 'InputWeek
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
  , 'InputButton
  , 'InputCheckbox
  , 'InputColor
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputEmail
  , 'InputFile
  , 'InputHidden
  , 'InputImage
  , 'InputMonth
  , 'InputNumber
  , 'InputPassword
  , 'InputRadio
  , 'InputRange
  , 'InputReset
  , 'InputSearch
  , 'InputSubmit
  , 'InputTel
  , 'InputText
  , 'InputTime
  , 'InputUrl
  , 'InputWeek
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

-- This list represents all elements that are considered form-associated
-- content.
--
type FormAssociatedContent =
  Union InputTags
    [ 'Button
    , 'Fieldset
    , 'Label
    , 'Meter
    , 'Object
    , 'Output
    , 'Progress
    , 'Select
    , 'TextArea
    ]

-- This list represents all elements that are considered sectioning content.
--
type HeadingContent =
  'HeadingGroup ': Headings

-- This list represents all elements that are considered interactive content.
--
type InteractiveContent =
  Union (Remove 'InputHidden InputTags)
    [ 'Button
    , 'Details
    , 'Embed
    , 'IFrame
    , 'Label
    , 'Select
    , 'TextArea
    ]

-- This list represents all elements that are of the labelable subtype of
-- form-associated content.
--
type LabelableContent =
  Union InputTags
    [ 'Button
    , 'Meter
    , 'Output
    , 'Progress
    , 'Select
    , 'TextArea
    ]

-- This list represents all elements that are of the listed subtype of
-- form-associated content.
--
type ListedContent =
  Union InputTags
    [ 'Button
    , 'Fieldset
    , 'Object
    , 'Output
    , 'Select
    , 'TextArea
    ]

-- This list represents all marginal content.
--
type MarginalContent =
  [ 'Header
  , 'Footer
  ]

-- This list represents all elements that play media.
--
type MediaContent =
  [ 'Audio
  , 'Video
  ]

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

-- This list represents all elements that are considered phrasing content.
--
type PhrasingContent =
  [ 'Text
  , 'Abbreviation
  , 'Area
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
  , 'InputButton
  , 'InputCheckbox
  , 'InputColor
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputEmail
  , 'InputFile
  , 'InputHidden
  , 'InputImage
  , 'InputMonth
  , 'InputNumber
  , 'InputPassword
  , 'InputRadio
  , 'InputRange
  , 'InputReset
  , 'InputSearch
  , 'InputSubmit
  , 'InputTel
  , 'InputText
  , 'InputTime
  , 'InputUrl
  , 'InputWeek
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

-- This list represents all elements that are of the resettable subtype of
-- form-associated content.
--
type ResettableContent =
  Union InputTags
    [ 'Output
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

-- This list represents all elements that are considered sectioning content.
--
type SectioningContent =
  [ 'Article
  , 'Aside
  , 'Nav
  , 'Section
  ]

-- This list represents all elements that are of the submittable subtype of
-- form-associated content.
--
type SubmittableContent =
  Union InputTags
    [ 'Button
    , 'Object
    , 'Select
    , 'TextArea
    ]

-- This list represents all elements that are considered transparent to some
-- degree.
--
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

-- Element Exclusion Tag Groups
--

type AnchorExcluded =
  'Anchor ': InteractiveContent

type CanvasExcluded =
  [ 'Details
  , 'Embed
  , 'IFrame
  , 'Label
  , 'Select
  , 'TextArea
  ]

type ContactAddressExcluded =
  'ContactAddress ': HeadingSectioningAndMarginalsExcluded

type DescriptionTermExcluded =
  HeadingSectioningAndMarginalsExcluded

type HeadingSectioningAndMarginalsExcluded =
  Union
    SectioningContent
    (Union HeadingContent MarginalContent)

type TableHeaderExcluded =
  HeadingSectioningAndMarginalsExcluded

-- Element-Focused Tag Groups
--

-- This list represents all elements that are valid under an `<audio>` or
-- `<video>` tag.
--
type AudioVideoContent =
  [ 'Source
  , 'Track
  ]

-- This list represents all elements that are valid under a `<canvas>` tag.
--
type CanvasContent =
  Union InputTags
    [ 'Anchor
    , 'Button
    ]

-- This list represents all elements that are valid under a `<dlist>` tag.
--
type DescriptionListContent =
  Union
    ScriptSupportingContent
    [ 'DescriptionTerm
    , 'DescriptionDetails
    , 'Division
    ]

-- This list represents all elements that are valid under a `<legend>` tag.
--
type LegendContent =
  'H1
    ': 'H2
    ': 'H3
    ': 'H4
    ': 'H5
    ': 'H6
    ': PhrasingContent

-- This list represents all elements that are valid under a `<noscript>` tag
-- when it is the descendent of a `<body>` tag.
--
type NoScriptBodyContent =
  Union FlowContent PhrasingContent

-- This list represents all elements that are valid under a `<noscript>` tag
-- when it is the descendent of a `<head>` tag.
--
type NoScriptHeadContent =
  [ 'Link
  , 'Meta
  , 'Style
  ]

-- This list represents all elements that are valid under a `<picture>` tag.
--
type PictureContent =
  'Source
    ': 'Image
    ': ScriptSupportingContent

-- This list represents all elements that are valid under a `<ruby>` tag.
--
type RubyContent =
  'RubyParenthesis
    ': 'RubyText
    ': PhrasingContent

-- This list represents all elements that are valid under a `<summary>` tag.
--
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
--
type TableContent =
  [ 'TableCaption
  , 'TableColumnGroup
  , 'TableHead
  , 'TableBody
  , 'TableRow
  , 'TableFoot
  ]

type TableCells =
  [ 'TableDataCell
  , 'TableHeader
  ]

-- This list represents all elements that are valid under a `<tr>` tag.
--
type TableRowContent =
  Union
    TableCells
    ScriptSupportingContent

-- Attribute-Focused Tag Groups
--

type AltTags =
  [ 'Area
  , 'Image
  , 'Input
  , 'InputImage
  ]

type CheckableTags =
  [ 'Input
  , 'InputCheckbox
  , 'InputRadio
  ]

type CitableTags =
  [ 'Blockquote
  , 'DeletedText
  , 'InsertedText
  , 'Quotation
  ]

type CrossOriginTags =
  [ 'Audio
  , 'CustomHTML
  , 'Image
  , 'Link
  , 'Script
  , 'Video
  ]

type DirnameableTags =
  [ 'Input
  , 'InputEmail
  , 'InputSearch
  , 'InputTel
  , 'InputText
  , 'InputUrl
  , 'TextArea
  ]

type DisableableTags =
  Union InputTags
    [ 'Button
    , 'CustomHTML
    , 'Fieldset
    , 'OptionGroup
    , 'Option
    , 'Select
    , 'TextArea
    ]

type FormMethodTags =
  [ 'Input
  , 'InputImage
  , 'InputSubmit
  ]

type FormNoValidateTags =
  [ 'Button
  , 'Input
  , 'InputSubmit
  ]

type HrefTags =
  'Base
    ': 'Link
    ': URLTags

type InputTags =
  [ 'Input
  , 'InputButton
  , 'InputCheckbox
  , 'InputColor
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputEmail
  , 'InputFile
  , 'InputHidden
  , 'InputImage
  , 'InputMonth
  , 'InputNumber
  , 'InputPassword
  , 'InputRadio
  , 'InputRange
  , 'InputReset
  , 'InputSearch
  , 'InputSubmit
  , 'InputTel
  , 'InputText
  , 'InputTime
  , 'InputUrl
  , 'InputWeek
  ]

type LabelableTags =
  [ 'OptionGroup
  , 'Track
  ]

type LengthTags =
  [ 'Input
  , 'InputEmail
  , 'InputPassword
  , 'InputSearch
  , 'InputTel
  , 'InputText
  , 'InputUrl
  , 'TextArea
  ]

type NameTags =
  Union InputTags
    [ 'Button
    , 'Fieldset
    , 'Form
    , 'IFrame
    , 'Map
    , 'Meta
    , 'Object
    , 'Output
    , 'Select
    , 'TextArea
    ]

type PlaceholderableTags =
  [ 'Input
  , 'InputEmail
  , 'InputPassword
  , 'InputSearch
  , 'InputTel
  , 'InputText
  , 'InputUrl
  , 'TextArea
  ]

type ReadOnlyTags =
  [ 'Input
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputEmail
  , 'InputMonth
  , 'InputNumber
  , 'InputPassword
  , 'InputSearch
  , 'InputTel
  , 'InputText
  , 'InputTime
  , 'InputUrl
  , 'InputWeek
  ]

type RelTags =
  [ 'Anchor
  , 'Area
  , 'Form
  , 'Link
  ]

type RequireableTags =
  [ 'Input
  , 'InputCheckbox
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputEmail
  , 'InputFile
  , 'InputMonth
  , 'InputNumber
  , 'InputPassword
  , 'InputRadio
  , 'InputSearch
  , 'InputTel
  , 'InputText
  , 'InputTime
  , 'InputUrl
  , 'InputWeek
  , 'Select
  , 'TextArea
  ]

type SizableTags =
  [ 'Canvas
  , 'Embed
  , 'IFrame
  , 'Image
  , 'Input
  , 'InputImage
  , 'Object
  , 'Video
  ]

type SrcTags =
  [ 'Audio
  , 'Embed
  , 'IFrame
  , 'Image
  , 'Input
  , 'InputImage
  , 'Script
  , 'Source
  , 'Track
  , 'Video
  ]

type TargetableTags =
  [ 'Anchor
  , 'Area
  , 'Base
  , 'Form
  ]

type TimestampableTags =
  [ 'DeletedText
  , 'InsertedText
  , 'Time
  ]

type TypeableTags =
  [ 'Anchor
  , 'Button
  , 'Embed
  , 'Input
  , 'InputButton
  , 'InputCheckbox
  , 'InputColor
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputEmail
  , 'InputFile
  , 'InputHidden
  , 'InputImage
  , 'InputMonth
  , 'InputNumber
  , 'InputPassword
  , 'InputRadio
  , 'InputRange
  , 'InputReset
  , 'InputSearch
  , 'InputSubmit
  , 'InputTel
  , 'InputText
  , 'InputTime
  , 'InputUrl
  , 'InputWeek
  , 'Link
  , 'Object
  , 'OrderedList
  , 'Script
  , 'Source
  , 'Style
  ]

type URLTags =
  [ 'Anchor
  , 'Area
  ]

type ValuableTags =
  Union (Remove 'InputFile InputTags)
    [ 'Button
    , 'Meter
    , 'ListItem
    , 'Option
    , 'Progress
    ]

type TextValueTags =
  [ 'Button
  , 'Input
  , 'InputButton
  , 'InputCheckbox
  , 'InputHidden
  , 'InputImage
  , 'InputPassword
  , 'InputRadio
  , 'InputReset
  , 'InputSearch
  , 'InputSubmit
  , 'InputText
  ]
