{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HTML.Elements.Children
  ( ValidChild
  ) where

import HTML.Elements.TagGroups qualified as TagGroups
import HTML.Elements.TagType (TagType(..))
import HTML.Contains (Contains)

type ValidChild tag parent =
  Contains (ValidChildrenFor parent) tag

type family ValidChildrenFor (parent :: TagType) :: [TagType] where
  ValidChildrenFor Document               = '[ Html ]
  ValidChildrenFor Comment                = TagGroups.NonElement
  ValidChildrenFor Text                   = TagGroups.NonElement
  ValidChildrenFor Anchor                 = '[] -- Transparent, except that no descendant may be interactive content or an a element, and no descendant may have a specified tabindex attribute.
  ValidChildrenFor Abbreviation           = TagGroups.PhrasingContent
  ValidChildrenFor ContactAddress         = '[] -- Flow content, but with no nested <address> element, no heading content (<hgroup>, h1, h2, h3, h4, h5, h6), no sectioning content (<article>, <aside>, <section>, <nav>), and no <header> or <footer> element.
  ValidChildrenFor Area                   = TagGroups.VoidElement
  ValidChildrenFor Article                = TagGroups.FlowContent
  ValidChildrenFor Aside                  = TagGroups.FlowContent
  ValidChildrenFor Audio                  = '[] -- If the element has a src attribute: zero or more <track> elements followed by transparent content that contains no <audio> or <video> media elements. Else: zero or more <source> elements followed by zero or more <track> elements followed by transparent content that contains no <audio> or <video> media elements.
  ValidChildrenFor BringAttentionTo       = TagGroups.PhrasingContent
  ValidChildrenFor Base                   = TagGroups.VoidElement
  ValidChildrenFor BidirectionalIsolation = TagGroups.PhrasingContent
  ValidChildrenFor BidirectionalOverride  = TagGroups.PhrasingContent
  ValidChildrenFor Blockquote             = TagGroups.FlowContent
  ValidChildrenFor Body                   = TagGroups.FlowContent
  ValidChildrenFor LineBreak              = TagGroups.VoidElement
  ValidChildrenFor Button                 = '[] -- Phrasing content but there must be no Interactive content
  ValidChildrenFor Canvas                 = '[] -- Transparent but with no interactive content descendants except for <a> elements, <button> elements, <input> elements whose type attribute is checkbox, radio, or button.
  ValidChildrenFor TableCaption           = TagGroups.FlowContent
  ValidChildrenFor Citation               = TagGroups.PhrasingContent
  ValidChildrenFor Code                   = TagGroups.PhrasingContent
  ValidChildrenFor TableColumn            = TagGroups.VoidElement
  ValidChildrenFor TableColumnGroup       = '[ TableColumn ]
  ValidChildrenFor Data                   = TagGroups.PhrasingContent
  ValidChildrenFor DataList               = 'Option ': TagGroups.PhrasingContent
  ValidChildrenFor DescriptionDetails     = TagGroups.FlowContent
  ValidChildrenFor Details                = 'Summary ': TagGroups.FlowContent
  ValidChildrenFor Definition             = '[] -- Phrasing content, but no <dfn> element must be a descendant.
  ValidChildrenFor Dialog                 = TagGroups.FlowContent
  ValidChildrenFor Division               = TagGroups.FlowContent
  ValidChildrenFor DescriptionList        = '[] -- Either: Zero or more groups each consisting of one or more <dt> elements followed by one or more <dd> elements, optionally intermixed with <script> and <template> elements. Or: (in WHATWG HTML, W3C HTML 5.2 and later) One or more <div> elements, optionally intermixed with <script> and <template> elements.
  ValidChildrenFor DescriptionTerm        = '[] -- Flow content, but with no <header>, <footer>, sectioning content or heading content descendants.
  ValidChildrenFor Emphasis               = TagGroups.PhrasingContent
  ValidChildrenFor Embed                  = TagGroups.VoidElement
  ValidChildrenFor Fieldset               = 'Legend ': TagGroups.FlowContent
  ValidChildrenFor FigureCaption          = TagGroups.FlowContent
  ValidChildrenFor Figure                 = FigureCaption ': TagGroups.FlowContent
  ValidChildrenFor Footer                 = '[] -- Flow content, but with no <footer> or <header> descendants.
  ValidChildrenFor Form                   = '[] -- Flow content, but not containing <form> elements
  ValidChildrenFor H1                     = TagGroups.PhrasingContent
  ValidChildrenFor H2                     = TagGroups.PhrasingContent
  ValidChildrenFor H3                     = TagGroups.PhrasingContent
  ValidChildrenFor H4                     = TagGroups.PhrasingContent
  ValidChildrenFor H5                     = TagGroups.PhrasingContent
  ValidChildrenFor H6                     = TagGroups.PhrasingContent
  ValidChildrenFor Head                   = TagGroups.MetadataContent
  ValidChildrenFor Header                 = '[] -- Flow content, but with no <header> or <footer> descendant.
  ValidChildrenFor HeadingGroup           = 'Paragraph ': TagGroups.Headings
  ValidChildrenFor HorizontalRule         = TagGroups.VoidElement
  ValidChildrenFor Html                   = [ 'Head, 'Body ]
  ValidChildrenFor IdiomaticText          = TagGroups.PhrasingContent
  ValidChildrenFor IFrame                 = TagGroups.NoContent
  ValidChildrenFor Image                  = TagGroups.VoidElement
  ValidChildrenFor Input                  = TagGroups.VoidElement
  ValidChildrenFor KeyboardInput          = TagGroups.PhrasingContent
  ValidChildrenFor Label                  = '[] -- Phrasing content, but no descendant label elements. No labelable elements other than the labeled control are allowed.
  ValidChildrenFor Legend                 = TagGroups.LegendContent
  ValidChildrenFor ListItem               = TagGroups.FlowContent
  ValidChildrenFor Link                   = TagGroups.VoidElement
  ValidChildrenFor Main                   = TagGroups.FlowContent
  ValidChildrenFor Map                    = TagGroups.TransparentContent
  ValidChildrenFor Mark                   = TagGroups.PhrasingContent
  ValidChildrenFor Menu                   = TagGroups.ListContent
  ValidChildrenFor Meta                   = TagGroups.VoidElement
  ValidChildrenFor Meter                  = '[] -- Phrasing content, but there must be no <meter> element among its descendants.
  ValidChildrenFor Nav                    = TagGroups.FlowContent
  ValidChildrenFor NoScript               = '[] -- When scripting is disabled and when it is a descendant of the <head> element: in any order, zero or more <link> elements, zero or more <style> elements, and zero or more <meta> elements. When scripting is disabled and when it isn't a descendant of the <head> element: any transparent content, but no <noscript> element must be among its descendants. Otherwise: flow content or phrasing content.
  ValidChildrenFor Object                 = '[] -- zero or more <param> elements, then transparent.
  ValidChildrenFor OrderedList            = TagGroups.ListContent
  ValidChildrenFor OptionGroup            = '[ Option ]
  ValidChildrenFor Option                 = TagGroups.TextOnly
  ValidChildrenFor Output                 = TagGroups.PhrasingContent
  ValidChildrenFor Paragraph              = TagGroups.PhrasingContent
  ValidChildrenFor Picture                = '[] -- Zero or more <source> elements, followed by one <img> element, optionally intermixed with script-supporting elements.
  ValidChildrenFor PreformattedText       = TagGroups.PhrasingContent
  ValidChildrenFor Progress               = '[] -- Phrasing content, but there must be no <progress> element among its descendants.
  ValidChildrenFor Quotation              = TagGroups.PhrasingContent
  ValidChildrenFor RubyParenthesis        = TagGroups.TextOnly
  ValidChildrenFor RubyText               = TagGroups.PhrasingContent
  ValidChildrenFor Ruby                   = TagGroups.RubyContent
  ValidChildrenFor Strikethrough          = TagGroups.PhrasingContent
  ValidChildrenFor Sample                 = TagGroups.PhrasingContent
  ValidChildrenFor Script                 = '[] -- Dynamic script such as text/javascript.
  ValidChildrenFor Search                 = TagGroups.FlowContent
  ValidChildrenFor Section                = TagGroups.FlowContent
  ValidChildrenFor Select                 = [ 'Option, OptionGroup ]
  ValidChildrenFor SideComment            = TagGroups.PhrasingContent
  ValidChildrenFor Source                 = TagGroups.VoidElement
  ValidChildrenFor Span                   = TagGroups.PhrasingContent
  ValidChildrenFor Strong                 = TagGroups.PhrasingContent
  ValidChildrenFor Style                  = TagGroups.TextOnly
  ValidChildrenFor Subscript              = TagGroups.PhrasingContent
  ValidChildrenFor Summary                = TagGroups.SummaryContent
  ValidChildrenFor Superscript            = TagGroups.PhrasingContent
  ValidChildrenFor Table                  = TagGroups.TableContent
  ValidChildrenFor TableBody              = TagGroups.TableRowOnly
  ValidChildrenFor TableDataCell          = TagGroups.FlowContent
  ValidChildrenFor ContentTemplate        = '[] -- No restrictions.
  ValidChildrenFor TextArea               = TagGroups.TextOnly
  ValidChildrenFor TableFoot              = TagGroups.TableRowOnly
  ValidChildrenFor TableHeader            = '[ Text ] -- Currently only `Text` to support testing of `Table` module. Flow content, but with no header, footer, sectioning content, or heading content descendants.
  ValidChildrenFor TableHead              = TagGroups.TableRowOnly
  ValidChildrenFor Time                   = TagGroups.PhrasingContent
  ValidChildrenFor Title                  = TagGroups.TextOnly
  ValidChildrenFor TableRow               = TagGroups.TableRowContent
  ValidChildrenFor Track                  = TagGroups.VoidElement
  ValidChildrenFor Underline              = TagGroups.PhrasingContent
  ValidChildrenFor UnorderedList          = TagGroups.ListContent
  ValidChildrenFor Variable               = TagGroups.PhrasingContent
  ValidChildrenFor Video                  = '[] -- If the element has a src attribute: zero or more <track> elements, followed by transparent content that contains no media elements–that is no <audio> or <video>. Else: zero or more <source> elements, followed by zero or more <track> elements, followed by transparent content that contains no media elements–that is no <audio> or <video>.
  ValidChildrenFor WordBreakOpportunity   = TagGroups.VoidElement
