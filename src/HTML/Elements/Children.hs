{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This is required in order for `Filter` to work. Using this language
-- extension is always risky, but given that we can be sure that every argument
-- that will be passed to `Filter` will be a finite list, and that both
-- `Remove` and `Filter` have base cases that will resolve, it's a relatively
-- safe use case for it. If a better solution ever comes along that allows for
-- the elimination of this extension, we shouldn't hesitate to jump on the
-- opportunity.
{-# LANGUAGE UndecidableInstances #-}

module HTML.Elements.Children
  ( ValidChild
  ) where

import HTML.Elements.TagGroups qualified as TagGroups
import HTML.Elements.TagType (TagType(..))
import HTML.Internal.TagOperations (AlertElement, Elem, Filter, Remove)

type ValidChild tag parent =
  AlertElement (Elem tag (ValidChildrenFor parent)) tag parent 'False ~ 'True

-- Anchor
  -- Transparent, except that no descendant may be interactive content or an a
  -- element, and no descendant may have a specified tabindex attribute.

-- Audio
  -- If the element has a src attribute: zero or more <track> elements followed
  -- by transparent content that contains no <audio> or <video> media elements.
  -- Else: zero or more <source> elements followed by zero or more <track>
  -- elements followed by transparent content that contains no <audio> or
  -- <video> media elements.

-- Canvas
  -- Transparent but with no interactive content descendants except for <a>
  -- elements, <button> elements, <input> elements whose type attribute is
  -- checkbox, radio, or button.

-- NoScript
  -- When scripting is disabled and when it is a descendant of the <head>
  -- element: in any order, zero or more <link> elements, zero or more <style>
  -- elements, and zero or more <meta> elements. When scripting is disabled and
  -- when it isn't a descendant of the <head> element: any transparent content,
  -- but no <noscript> element must be among its descendants. Otherwise: flow
  -- content or phrasing content.

-- Video
  -- If the element has a src attribute: zero or more <track> elements,
  -- followed by transparent content that contains no media elements–that is no
  -- <audio> or <video>. Else: zero or more <source> elements, followed by zero
  -- or more <track> elements, followed by transparent content that contains no
  -- media elements–that is no <audio> or <video>.

type family ValidChildrenFor (parent :: TagType) :: [TagType] where
  ValidChildrenFor Document               = '[ Html ]
  ValidChildrenFor CustomHTML             = TagGroups.AllElements
  ValidChildrenFor Anchor                 = '[]
  ValidChildrenFor Abbreviation           = TagGroups.PhrasingContent
  ValidChildrenFor ContactAddress         = Filter TagGroups.ContactAddressExcluded TagGroups.FlowContent
  ValidChildrenFor Article                = TagGroups.FlowContent
  ValidChildrenFor Aside                  = TagGroups.FlowContent
  ValidChildrenFor Audio                  = '[]
  ValidChildrenFor BringAttentionTo       = TagGroups.PhrasingContent
  ValidChildrenFor BidirectionalIsolation = TagGroups.PhrasingContent
  ValidChildrenFor BidirectionalOverride  = TagGroups.PhrasingContent
  ValidChildrenFor Blockquote             = TagGroups.FlowContent
  ValidChildrenFor Body                   = TagGroups.FlowContent
  ValidChildrenFor Button                 = Filter TagGroups.InteractiveContent TagGroups.PhrasingContent
  ValidChildrenFor Canvas                 = '[]
  ValidChildrenFor TableCaption           = TagGroups.FlowContent
  ValidChildrenFor Citation               = TagGroups.PhrasingContent
  ValidChildrenFor Code                   = TagGroups.PhrasingContent
  ValidChildrenFor TableColumnGroup       = '[ TableColumn ]
  ValidChildrenFor Data                   = TagGroups.PhrasingContent
  ValidChildrenFor DataList               = 'Option ': TagGroups.PhrasingContent
  ValidChildrenFor DescriptionDetails     = TagGroups.FlowContent
  ValidChildrenFor Details                = 'Summary ': TagGroups.FlowContent
  ValidChildrenFor Definition             = Remove Definition TagGroups.PhrasingContent
  ValidChildrenFor Dialog                 = TagGroups.FlowContent
  ValidChildrenFor Division               = TagGroups.FlowContent
  ValidChildrenFor DescriptionList        = TagGroups.DescriptionListContent
  ValidChildrenFor DescriptionTerm        = Filter TagGroups.DescriptionTermExcluded TagGroups.FlowContent
  ValidChildrenFor Emphasis               = TagGroups.PhrasingContent
  ValidChildrenFor Fieldset               = 'Legend ': TagGroups.FlowContent
  ValidChildrenFor FigureCaption          = TagGroups.FlowContent
  ValidChildrenFor Figure                 = FigureCaption ': TagGroups.FlowContent
  ValidChildrenFor Footer                 = Filter TagGroups.MarginalContent TagGroups.FlowContent
  ValidChildrenFor Form                   = Remove Form TagGroups.FlowContent
  ValidChildrenFor H1                     = TagGroups.PhrasingContent
  ValidChildrenFor H2                     = TagGroups.PhrasingContent
  ValidChildrenFor H3                     = TagGroups.PhrasingContent
  ValidChildrenFor H4                     = TagGroups.PhrasingContent
  ValidChildrenFor H5                     = TagGroups.PhrasingContent
  ValidChildrenFor H6                     = TagGroups.PhrasingContent
  ValidChildrenFor Head                   = TagGroups.MetadataContent
  ValidChildrenFor Header                 = Filter TagGroups.MarginalContent TagGroups.FlowContent
  ValidChildrenFor HeadingGroup           = 'Paragraph ': TagGroups.Headings
  ValidChildrenFor Html                   = [ 'Head, 'Body ]
  ValidChildrenFor IdiomaticText          = TagGroups.PhrasingContent
  ValidChildrenFor KeyboardInput          = TagGroups.PhrasingContent
  ValidChildrenFor Label                  = Remove Label TagGroups.PhrasingContent
  ValidChildrenFor Legend                 = TagGroups.LegendContent
  ValidChildrenFor ListItem               = TagGroups.FlowContent
  ValidChildrenFor Main                   = TagGroups.FlowContent
  ValidChildrenFor Map                    = TagGroups.TransparentContent
  ValidChildrenFor Mark                   = TagGroups.PhrasingContent
  ValidChildrenFor Menu                   = TagGroups.ListContent
  ValidChildrenFor Meter                  = Remove Meter TagGroups.PhrasingContent
  ValidChildrenFor Nav                    = TagGroups.FlowContent
  ValidChildrenFor NoScript               = '[]
  ValidChildrenFor OrderedList            = TagGroups.ListContent
  ValidChildrenFor OptionGroup            = '[ Option ]
  ValidChildrenFor Option                 = TagGroups.TextOnly
  ValidChildrenFor Output                 = TagGroups.PhrasingContent
  ValidChildrenFor Paragraph              = TagGroups.PhrasingContent
  ValidChildrenFor Picture                = TagGroups.PictureContent
  ValidChildrenFor PreformattedText       = TagGroups.PhrasingContent
  ValidChildrenFor Progress               = Remove Progress TagGroups.PhrasingContent
  ValidChildrenFor Quotation              = TagGroups.PhrasingContent
  ValidChildrenFor RubyParenthesis        = TagGroups.TextOnly
  ValidChildrenFor RubyText               = TagGroups.PhrasingContent
  ValidChildrenFor Ruby                   = TagGroups.RubyContent
  ValidChildrenFor Strikethrough          = TagGroups.PhrasingContent
  ValidChildrenFor Sample                 = TagGroups.PhrasingContent
  ValidChildrenFor Search                 = TagGroups.FlowContent
  ValidChildrenFor Section                = TagGroups.FlowContent
  ValidChildrenFor Select                 = [ 'Option, OptionGroup ]
  ValidChildrenFor SideComment            = TagGroups.PhrasingContent
  ValidChildrenFor Span                   = TagGroups.PhrasingContent
  ValidChildrenFor Strong                 = TagGroups.PhrasingContent
  ValidChildrenFor Subscript              = TagGroups.PhrasingContent
  ValidChildrenFor Summary                = TagGroups.SummaryContent
  ValidChildrenFor Superscript            = TagGroups.PhrasingContent
  ValidChildrenFor Table                  = TagGroups.TableContent
  ValidChildrenFor TableBody              = TagGroups.TableRowOnly
  ValidChildrenFor TableDataCell          = TagGroups.FlowContent
  ValidChildrenFor ContentTemplate        = TagGroups.AllElements
  ValidChildrenFor TextArea               = TagGroups.TextOnly
  ValidChildrenFor TableFoot              = TagGroups.TableRowOnly
  ValidChildrenFor TableHeader            = Filter TagGroups.TableHeaderExcluded TagGroups.FlowContent
  ValidChildrenFor TableHead              = TagGroups.TableRowOnly
  ValidChildrenFor Time                   = TagGroups.PhrasingContent
  ValidChildrenFor Title                  = TagGroups.TextOnly
  ValidChildrenFor TableRow               = TagGroups.TableRowContent
  ValidChildrenFor Underline              = TagGroups.PhrasingContent
  ValidChildrenFor UnorderedList          = TagGroups.ListContent
  ValidChildrenFor Variable               = TagGroups.PhrasingContent
  ValidChildrenFor Video                  = '[]
