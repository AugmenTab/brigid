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
  , ParagraphContent
  , PictureContent
  , RubyContent
  , SummaryContent
  , TableContent
  , TableCells
  , TableRowContent

  , SVG_AnimationElements
  , SVG_BasicShapes
  , SVG_ContainerElements
  , SVG_DescriptiveElements
  , SVG_FilterPrimitiveElements
  , SVG_GradientElements
  , SVG_GraphicsElements
  , SVG_LightSourceElements
  , SVG_NeverRenderedElements
  , SVG_PaintServerElements
  , SVG_RenderableElements
  , SVG_ShapeElements
  , SVG_StructuralElements
  , SVG_TextContentElements
  , SVG_TextContentChildElements
  , SVG_UncategorizedElements

  , SVG_CommonContent
  , SVG_PathElements

  , AltTags
  , AutocompletableTags
  , BlockingTags
  , CheckableTags
  , CitableTags
  , CrossOriginTags
  , DirnameableTags
  , DisableableTags
  , ElementTimingTags
  , FormTags
  , FormMethodTags
  , FormNoValidateTags
  , FormSubmitTags
  , FreeTextInputTags
  , HrefTags
  , HrefLangTags
  , InputTags
  , IntegrityTags
  , LabelableTags
  , MediaTags
  , NameTags
  , RangedNumberTags
  , RangedTags
  , ReadOnlyTags
  , ReferrerPolicyTags
  , RelTags
  , RequireableTags
  , SizableTags
  , SrcTags
  , SteppableTags
  , TargetableTags
  , TimestampableTags
  , TypeableTags
  , URLTags
  , ValidatableTags
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
  , 'SVG
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
  , 'SVG
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
  , 'CustomHTML
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
  , 'SVG
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

-- This list represents all elements that are valid under a `<p>` tag.
--
-- Everything being unioned with 'PhrasingContent' here has a condition that
-- must be met in order to be a valid child, but for most of these, we can't
-- enforce it, so we trust the user to make the right call.
--
-- @a@ and @map@ are valid only if they contain only phrasing content. This
-- should be taken care of with the existing mechanics, since they are
-- transparent and can only contain what is valid for their parents (with some
-- exceptions).
--
-- @area@ is valid only if it is the descendent of a @map@ element. Presumably,
-- this means that the parent @p@ tag can itself be a child of a @map@ tag. We
-- have no way to check for this, unless it's a direct parent of @p@, which it
-- may not have to be.
--
-- @del@ and @ins@ are transparent, and is valid if it contains only phrasing
-- content, so that is safe.
--
-- @link@ and @meta@ are valid only if the @itemprop@ attribute is present. We
-- have no way to check for this, so we trust the user to make the right call.
--
type ParagraphContent =
  Union PhrasingContent
    [ 'Anchor
    , 'Area
    , 'DeletedText
    , 'InsertedText
    , 'Link
    , 'Map
    , 'Meta
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
  [ 'CustomHTML
  , 'TableDataCell
  , 'TableHeader
  ]

-- This list represents all elements that are valid under a `<tr>` tag.
--
type TableRowContent =
  Union
    TableCells
    ScriptSupportingContent

-- SVG Tag Groups
--

type SVG_AnimationElements =
  [ 'Animate
  , 'AnimateMotion
  , 'AnimateTransform
  , 'MotionPath
  , 'Set
  ]

type SVG_BasicShapes =
  [ 'Circle
  , 'Ellipse
  , 'Line
  , 'Polygon
  , 'Polyline
  , 'Rectangle
  ]

type SVG_ContainerElements =
  -- [ 'Anchor
  [ 'Definitions
  , 'Group
  , 'Marker
  , 'Mask
  , 'Pattern
  , 'SVG
  , 'Switch
  , 'Symbol
  ]

type SVG_DescriptiveElements =
  [ 'Description
  , 'Metadata
  -- , 'Title
  ]

type SVG_FilterPrimitiveElements =
  [ 'FilterEffectBlend
  , 'FilterEffectColorMatrix
  , 'FilterEffectComponentTransfer
  , 'FilterEffectComposite
  , 'FilterEffectConvolveMatrix
  , 'FilterEffectDiffuseLighting
  , 'FilterEffectDisplacementMap
  , 'FilterEffectDropShadow
  , 'FilterEffectFlood
  , 'FilterEffectFuncA
  , 'FilterEffectFuncB
  , 'FilterEffectFuncG
  , 'FilterEffectFuncR
  , 'FilterEffectGaussianBlur
  , 'FilterEffectImage
  , 'FilterEffectMerge
  , 'FilterEffectMergeNode
  , 'FilterEffectMorphology
  , 'FilterEffectOffset
  , 'FilterEffectSpecularLighting
  , 'FilterEffectTile
  , 'FilterEffectTurbulence
  ]

type SVG_GradientElements =
  [ 'LinearGradient
  , 'RadialGradient
  , 'Stop
  ]

type SVG_GraphicsElements =
  [ 'Circle
  , 'Ellipse
  -- 'Image
  , 'Line
  , 'Path
  , 'Polygon
  , 'Polyline
  , 'Rectangle
  -- , 'Text
  , 'Use
  ]

type SVG_LightSourceElements =
  [ 'FilterEffectDistantLight
  , 'FilterEffectPointLight
  , 'FilterEffectSpotLight
  ]

type SVG_NeverRenderedElements =
  [ 'ClipPath
  , 'Definitions
  , 'LinearGradient
  , 'Marker
  , 'Mask
  , 'Metadata
  , 'Pattern
  , 'RadialGradient
  -- 'Script
  -- 'Style
  , 'Symbol
  -- 'Title
  ]

type SVG_PaintServerElements =
  [ 'LinearGradient
  , 'Pattern
  , 'RadialGradient
  ]

type SVG_RenderableElements =
  -- 'Anchor
  [ 'Circle
  , 'Ellipse
  , 'ForeignObject
  , 'Group
  -- 'Image
  , 'Line
  , 'Path
  , 'Polygon
  , 'Polyline
  , 'Rectangle
  , 'SVG
  , 'Switch
  -- 'Symbol
  , 'TextPath
  , 'TextSpan
  , 'Use
  ]

type SVG_ShapeElements =
  [ 'Circle
  , 'Ellipse
  , 'Line
  , 'Path
  , 'Polygon
  , 'Polyline
  , 'Rectangle
  ]

type SVG_StructuralElements =
  [ 'Definitions
  , 'Group
  , 'SVG
  , 'Symbol
  , 'Use
  ]

type SVG_TextContentElements =
  -- Add 'Text SVG_TextContentChildElements
  SVG_TextContentChildElements

type SVG_TextContentChildElements =
  [ 'TextPath
  , 'TextSpan
  ]

type SVG_UncategorizedElements =
  [ 'ClipPath
  , 'Filter
  , 'ForeignObject
  -- Script
  -- Style
  , 'View
  ]

type SVG_CommonContent =
  Union SVG_AnimationElements
    ( Union SVG_DescriptiveElements
        ( Union SVG_ShapeElements
            ( Union SVG_StructuralElements
                ( Union SVG_GradientElements
                    -- 'Anchor
                    [ 'ClipPath
                    , 'Filter
                    , 'ForeignObject
                    -- 'Image
                    , 'Marker
                    , 'Mask
                    , 'Pattern
                    -- 'Script
                    -- 'Style
                    , 'Switch
                    -- 'Text
                    , 'View
                    ]
                )
            )
        )
    )

type SVG_PathElements =
  Union SVG_AnimationElements SVG_DescriptiveElements

-- Attribute-Focused Tag Groups
--

type AltTags =
  [ 'Area
  , 'CustomHTML
  , 'Image
  , 'Input
  , 'InputImage
  ]

type AutocompletableTags =
  [ 'CustomHTML
  , 'Form
  , 'Input
  , 'InputDate
  , 'InputEmail
  , 'InputMonth
  , 'InputNumber
  , 'InputPassword
  , 'InputTel
  , 'InputText
  , 'InputUrl
  , 'Select
  , 'TextArea
  ]

type BlockingTags =
  [ 'CustomHTML
  , 'Link
  , 'Script
  , 'Style
  ]

type CheckableTags =
  [ 'CustomHTML
  , 'Input
  , 'InputCheckbox
  , 'InputRadio
  ]

type CitableTags =
  [ 'Blockquote
  , 'CustomHTML
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
  [ 'CustomHTML
  , 'Input
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
    , 'Fieldset
    , 'Link
    , 'OptionGroup
    , 'Option
    , 'Select
    , 'TextArea
    ]

type ElementTimingTags =
  [ 'Abbreviation
  , 'Anchor
  , 'Article
  , 'Aside
  , 'Blockquote
  , 'BringAttentionTo
  , 'Button
  , 'Canvas
  , 'Citation
  , 'Code
  , 'CustomHTML
  , 'Definition
  , 'DescriptionDetails
  , 'DescriptionTerm
  , 'Details
  , 'Division
  , 'Emphasis
  , 'Fieldset
  , 'FigureCaption
  , 'Footer
  , 'H1
  , 'H2
  , 'H3
  , 'H4
  , 'H5
  , 'H6
  , 'Header
  , 'IFrame
  , 'IdiomaticText
  , 'Image
  , 'KeyboardInput
  , 'Label
  , 'Legend
  , 'ListItem
  , 'Main
  , 'Mark
  , 'Nav
  , 'Output
  , 'Paragraph
  , 'PreformattedText
  , 'Quotation
  , 'Sample
  , 'Section
  , 'SideComment
  , 'Span
  , 'Strong
  , 'Summary
  , 'TableCaption
  , 'TableDataCell
  , 'TableHeader
  , 'Time
  , 'Underline
  , 'Variable
  , 'Video
  ]

type FormTags =
  Union InputTags
    [ 'Button
    , 'Fieldset
    , 'Meter
    , 'Object
    , 'Output
    , 'Select
    , 'TextArea
    ]

type FormMethodTags =
  [ 'Button
  , 'CustomHTML
  , 'Input
  , 'InputImage
  , 'InputSubmit
  ]

type FormNoValidateTags =
  [ 'Button
  , 'CustomHTML
  , 'Input
  , 'InputSubmit
  ]

type FormSubmitTags =
  [ 'Button
  , 'CustomHTML
  , 'Input
  , 'InputImage
  , 'InputSubmit
  ]

type FreeTextInputTags =
  [ 'CustomHTML
  , 'Input
  , 'InputEmail
  , 'InputPassword
  , 'InputSearch
  , 'InputTel
  , 'InputText
  , 'InputUrl
  ]

type HrefTags =
  'Base
    ': 'Link
    ': URLTags

type HrefLangTags =
  [ 'Anchor
  , 'Area
  , 'CustomHTML
  , 'Link
  ]

type InputTags =
  [ 'CustomHTML
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
  ]

type IntegrityTags =
  [ 'Audio
  , 'CustomHTML
  , 'Image
  , 'Link
  , 'Script
  , 'Video
  ]

type LabelableTags =
  [ 'CustomHTML
  , 'OptionGroup
  , 'Option
  , 'Track
  ]

type MediaTags =
  [ 'Link
  , 'Meta
  , 'Source
  , 'Style
  ]

type NameTags =
  Union InputTags
    [ 'Button
    , 'Details
    , 'Fieldset
    , 'Form
    , 'IFrame
    , 'Map
    , 'Meta
    , 'Object
    , 'Output
    , 'Select
    , 'Slot
    , 'TextArea
    ]

type RangedNumberTags =
  [ 'CustomHTML
  , 'Input
  , 'InputNumber
  , 'InputRange
  , 'Meter
  , 'Progress
  ]

type RangedTags =
  [ 'CustomHTML
  , 'Input
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputMonth
  , 'InputNumber
  , 'InputRange
  , 'InputTime
  , 'InputWeek
  , 'Meter
  , 'Progress
  ]

type ReadOnlyTags =
  [ 'CustomHTML
  , 'Input
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
  , 'TextArea
  ]

type ReferrerPolicyTags =
  [ 'Anchor
  , 'Area
  , 'CustomHTML
  , 'IFrame
  , 'Image
  , 'Link
  , 'Script
  ]

type RelTags =
  [ 'Anchor
  , 'Area
  , 'CustomHTML
  , 'Form
  , 'Link
  ]

type RequireableTags =
  [ 'CustomHTML
  , 'Input
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
  , 'CustomHTML
  , 'Embed
  , 'IFrame
  , 'Image
  , 'Input
  , 'InputImage
  , 'Object
  , 'Source
  , 'Video
  ]

type SrcTags =
  [ 'Audio
  , 'CustomHTML
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

type SteppableTags =
  [ 'CustomHTML
  , 'Input
  , 'InputDate
  , 'InputDatetimeLocal
  , 'InputMonth
  , 'InputNumber
  , 'InputRange
  , 'InputTime
  , 'InputWeek
  ]

type TargetableTags =
  [ 'Anchor
  , 'Area
  , 'Base
  , 'CustomHTML
  , 'Form
  ]

type TimestampableTags =
  [ 'CustomHTML
  , 'DeletedText
  , 'InsertedText
  , 'Time
  ]

type TypeableTags =
  [ 'Anchor
  , 'Button
  , 'CustomHTML
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
  , 'CustomHTML
  , 'Area
  ]

type ValidatableTags =
  Union InputTags
    [ 'Form
    , 'Select
    , 'TextArea
    ]

type ValuableTags =
  Union (Remove 'InputFile InputTags)
    [ 'Button
    , 'Data
    , 'ListItem
    , 'Meter
    , 'Option
    , 'Progress
    ]

type TextValueTags =
  Union InputTags
    [ 'Button
    , 'Data
    , 'Option
    ]
