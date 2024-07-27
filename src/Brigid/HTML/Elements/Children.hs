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

module Brigid.HTML.Elements.Children
  ( ValidChild
  ) where

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.TagType (TagType(..))
import Brigid.HTML.Internal.TagOperations (AlertElement, Elem, Filter, Remove, Union)

type ValidChild tag parent grandparent =
  AlertElement (Elem tag (ValidChildrenFor parent grandparent)) tag parent ~ 'True

type family ValidChildrenFor (parent :: TagType) (grandparent :: TagType) :: [TagType] where
  ValidChildrenFor NoElement              grandparent = '[]
  ValidChildrenFor Document               grandparent = '[ Html ]
  ValidChildrenFor CustomHTML             grandparent = TagGroups.AllElements
  ValidChildrenFor Anchor                 grandparent = Filter TagGroups.AnchorExcluded (ValidChildrenFor grandparent NoElement)
  ValidChildrenFor Abbreviation           grandparent = TagGroups.PhrasingContent
  ValidChildrenFor ContactAddress         grandparent = Filter TagGroups.ContactAddressExcluded TagGroups.FlowContent
  ValidChildrenFor Article                grandparent = TagGroups.FlowContent
  ValidChildrenFor Aside                  grandparent = TagGroups.FlowContent
  ValidChildrenFor Audio                  grandparent = TagGroups.AudioVideoContent
  ValidChildrenFor BringAttentionTo       grandparent = TagGroups.PhrasingContent
  ValidChildrenFor BidirectionalIsolation grandparent = TagGroups.PhrasingContent
  ValidChildrenFor BidirectionalOverride  grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Blockquote             grandparent = TagGroups.FlowContent
  ValidChildrenFor Body                   grandparent = TagGroups.FlowContent
  ValidChildrenFor Button                 grandparent = Filter TagGroups.InteractiveContent TagGroups.PhrasingContent
  ValidChildrenFor Canvas                 grandparent = Filter TagGroups.CanvasExcluded (Union TagGroups.CanvasContent (ValidChildrenFor grandparent NoElement))
  ValidChildrenFor TableCaption           grandparent = TagGroups.FlowContent
  ValidChildrenFor Citation               grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Code                   grandparent = TagGroups.PhrasingContent
  ValidChildrenFor TableColumnGroup       grandparent = '[ TableColumn ]
  ValidChildrenFor Data                   grandparent = TagGroups.PhrasingContent
  ValidChildrenFor DataList               grandparent = 'Option ': TagGroups.PhrasingContent
  ValidChildrenFor DescriptionDetails     grandparent = TagGroups.FlowContent
  ValidChildrenFor Details                grandparent = 'Summary ': TagGroups.FlowContent
  ValidChildrenFor Definition             grandparent = Remove Definition TagGroups.PhrasingContent
  ValidChildrenFor Dialog                 grandparent = TagGroups.FlowContent
  ValidChildrenFor Division               grandparent = TagGroups.FlowContent
  ValidChildrenFor DescriptionList        grandparent = TagGroups.DescriptionListContent
  ValidChildrenFor DescriptionTerm        grandparent = Filter TagGroups.DescriptionTermExcluded TagGroups.FlowContent
  ValidChildrenFor Emphasis               grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Fieldset               grandparent = 'Legend ': TagGroups.FlowContent
  ValidChildrenFor FigureCaption          grandparent = TagGroups.FlowContent
  ValidChildrenFor Figure                 grandparent = FigureCaption ': TagGroups.FlowContent
  ValidChildrenFor Footer                 grandparent = Filter TagGroups.MarginalContent TagGroups.FlowContent
  ValidChildrenFor Form                   grandparent = Remove Form TagGroups.FlowContent
  ValidChildrenFor H1                     grandparent = TagGroups.PhrasingContent
  ValidChildrenFor H2                     grandparent = TagGroups.PhrasingContent
  ValidChildrenFor H3                     grandparent = TagGroups.PhrasingContent
  ValidChildrenFor H4                     grandparent = TagGroups.PhrasingContent
  ValidChildrenFor H5                     grandparent = TagGroups.PhrasingContent
  ValidChildrenFor H6                     grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Head                   grandparent = TagGroups.MetadataContent
  ValidChildrenFor Header                 grandparent = Filter TagGroups.MarginalContent TagGroups.FlowContent
  ValidChildrenFor HeadingGroup           grandparent = 'Paragraph ': TagGroups.Headings
  ValidChildrenFor Html                   grandparent = [ 'Head, 'Body ]
  ValidChildrenFor IdiomaticText          grandparent = TagGroups.PhrasingContent
  ValidChildrenFor KeyboardInput          grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Label                  grandparent = Remove Label TagGroups.PhrasingContent
  ValidChildrenFor Legend                 grandparent = TagGroups.LegendContent
  ValidChildrenFor ListItem               grandparent = TagGroups.FlowContent
  ValidChildrenFor Main                   grandparent = TagGroups.FlowContent
  ValidChildrenFor Map                    grandparent = TagGroups.TransparentContent
  ValidChildrenFor Mark                   grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Menu                   grandparent = TagGroups.ListContent
  ValidChildrenFor Meter                  grandparent = Remove Meter TagGroups.PhrasingContent
  ValidChildrenFor Nav                    grandparent = TagGroups.FlowContent
  ValidChildrenFor NoScript               Head        = TagGroups.NoScriptHeadContent
  ValidChildrenFor NoScript               grandparent = Remove NoScript (Union TagGroups.NoScriptBodyContent (ValidChildrenFor grandparent NoElement))
  ValidChildrenFor OrderedList            grandparent = TagGroups.ListContent
  ValidChildrenFor OptionGroup            grandparent = '[ Option ]
  ValidChildrenFor Option                 grandparent = TagGroups.TextOnly
  ValidChildrenFor Output                 grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Paragraph              grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Picture                grandparent = TagGroups.PictureContent
  ValidChildrenFor PreformattedText       grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Progress               grandparent = Remove Progress TagGroups.PhrasingContent
  ValidChildrenFor Quotation              grandparent = TagGroups.PhrasingContent
  ValidChildrenFor RubyParenthesis        grandparent = TagGroups.TextOnly
  ValidChildrenFor RubyText               grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Ruby                   grandparent = TagGroups.RubyContent
  ValidChildrenFor Strikethrough          grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Sample                 grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Search                 grandparent = TagGroups.FlowContent
  ValidChildrenFor Section                grandparent = TagGroups.FlowContent
  ValidChildrenFor Select                 grandparent = [ 'Option, OptionGroup ]
  ValidChildrenFor SideComment            grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Span                   grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Strong                 grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Subscript              grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Summary                grandparent = TagGroups.SummaryContent
  ValidChildrenFor Superscript            grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Table                  grandparent = TagGroups.TableContent
  ValidChildrenFor TableBody              grandparent = TagGroups.TableRowOnly
  ValidChildrenFor TableDataCell          grandparent = TagGroups.FlowContent
  ValidChildrenFor ContentTemplate        grandparent = TagGroups.AllElements
  ValidChildrenFor TextArea               grandparent = TagGroups.TextOnly
  ValidChildrenFor TableFoot              grandparent = TagGroups.TableRowOnly
  ValidChildrenFor TableHeader            grandparent = Filter TagGroups.TableHeaderExcluded TagGroups.FlowContent
  ValidChildrenFor TableHead              grandparent = TagGroups.TableRowOnly
  ValidChildrenFor Time                   grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Title                  grandparent = TagGroups.TextOnly
  ValidChildrenFor TableRow               grandparent = TagGroups.TableRowContent
  ValidChildrenFor Underline              grandparent = TagGroups.PhrasingContent
  ValidChildrenFor UnorderedList          grandparent = TagGroups.ListContent
  ValidChildrenFor Variable               grandparent = TagGroups.PhrasingContent
  ValidChildrenFor Video                  grandparent = TagGroups.AudioVideoContent
