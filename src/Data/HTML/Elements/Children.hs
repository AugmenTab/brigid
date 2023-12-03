{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML.Elements.Children
  ( ValidChildrenFor
  ) where

import           Data.Kind (Type)

import qualified Data.HTML.Elements.Tags as Tags
import qualified Data.HTML.Elements.TagGroups as TagGroups

type family ValidChildrenFor element :: [Type] where
  ValidChildrenFor Tags.Comment                = TagGroups.NonElement
  ValidChildrenFor Tags.Text                   = TagGroups.NonElement

  ValidChildrenFor Tags.Anchor                 = '[]
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
  ValidChildrenFor Tags.Option                 = '[Tags.Text]
  ValidChildrenFor Tags.Output                 = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Paragraph              = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Picture                = Tags.Source ': Tags.Image ': TagGroups.ScriptSupportingContent
  ValidChildrenFor Tags.PreformattedText       = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Progress               = _
  ValidChildrenFor Tags.Quotation              = TagGroups.PhrasingContent
  ValidChildrenFor Tags.RubyParenthesis        = '[Tags.Text]
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
  ValidChildrenFor Tags.TextArea               = '[Tags.Text]
  ValidChildrenFor Tags.Time                   = TagGroups.PhrasingContent
  ValidChildrenFor Tags.Title                  = '[Tags.Text]
  ValidChildrenFor Tags.Track                  = TagGroups.VoidElement
  ValidChildrenFor Tags.Underline              = TagGroups.PhrasingContent
  ValidChildrenFor Tags.UnorderedList          = TagGroups.ListElements
  ValidChildrenFor Tags.Variable               = TagGroups.PhrasingContent
--ValidChildrenFor Tags.Video                  = _
  ValidChildrenFor Tags.WordBreakOpportunity   = TagGroups.VoidElement
