{-# LANGUAGE TypeFamilies #-}

module Data.HTML.Elements.TagType
  ( TagType
  , Branch
  , Leaf
  ) where

import Text.Blaze.Html5 qualified as H

import Data.HTML.Elements.Tags qualified as Tags

type Branch = H.Html -> H.Html
type Leaf   = H.Html

type family TagType element

type instance TagType Tags.Anchor                 = Branch
type instance TagType Tags.Abbreviation           = Branch
type instance TagType Tags.Address                = Branch
type instance TagType Tags.Area                   = Leaf
type instance TagType Tags.Article                = Branch
type instance TagType Tags.Aside                  = Branch
type instance TagType Tags.Audio                  = Branch
type instance TagType Tags.Bold                   = Branch
type instance TagType Tags.Base                   = Leaf
type instance TagType Tags.BiDirectionalIsolation = Branch
type instance TagType Tags.BiDirectionalOverride  = Branch
type instance TagType Tags.Blockquote             = Branch
type instance TagType Tags.Body                   = Branch
type instance TagType Tags.Break                  = Leaf
type instance TagType Tags.Button                 = Branch
type instance TagType Tags.Canvas                 = Branch
type instance TagType Tags.Caption                = Branch
type instance TagType Tags.Cite                   = Branch
type instance TagType Tags.Code                   = Branch
type instance TagType Tags.Column                 = Leaf
type instance TagType Tags.ColumnGroup            = Branch
type instance TagType Tags.Data                   = Branch
type instance TagType Tags.DataList               = Branch
type instance TagType Tags.Description            = Branch
type instance TagType Tags.Deletion               = Branch
type instance TagType Tags.Details                = Branch
type instance TagType Tags.Definition             = Branch
type instance TagType Tags.Dialog                 = Branch
type instance TagType Tags.Division               = Branch
type instance TagType Tags.DescriptionList        = Branch
type instance TagType Tags.DescriptionTerm        = Branch
type instance TagType Tags.Emphasis               = Branch
type instance TagType Tags.Embed                  = Leaf
type instance TagType Tags.Fieldset               = Branch
type instance TagType Tags.FigureCaption          = Branch
type instance TagType Tags.Figure                 = Branch
type instance TagType Tags.Footer                 = Branch
type instance TagType Tags.Form                   = Branch
type instance TagType Tags.H1                     = Branch
type instance TagType Tags.H2                     = Branch
type instance TagType Tags.H3                     = Branch
type instance TagType Tags.H4                     = Branch
type instance TagType Tags.H5                     = Branch
type instance TagType Tags.H6                     = Branch
type instance TagType Tags.Head                   = Branch
type instance TagType Tags.Header                 = Branch
type instance TagType Tags.HeadingGroup           = Branch
type instance TagType Tags.HorizontalRule         = Leaf
type instance TagType Tags.Html                   = Branch
type instance TagType Tags.Italic                 = Branch
type instance TagType Tags.IFrame                 = Branch
type instance TagType Tags.Image                  = Leaf
type instance TagType Tags.Input                  = Leaf
type instance TagType Tags.Insertion              = Branch
type instance TagType Tags.KeyboardInput          = Branch
type instance TagType Tags.Label                  = Branch
type instance TagType Tags.Legend                 = Branch
type instance TagType Tags.ListItem               = Branch
type instance TagType Tags.Link                   = Leaf
type instance TagType Tags.Main                   = Branch
type instance TagType Tags.Map                    = Branch
type instance TagType Tags.Mark                   = Branch
type instance TagType Tags.Menu                   = Branch
type instance TagType Tags.Meta                   = Leaf
type instance TagType Tags.Meter                  = Branch
type instance TagType Tags.Nav                    = Branch
type instance TagType Tags.NoScript               = Branch
type instance TagType Tags.Object                 = Branch
type instance TagType Tags.OrderedList            = Branch
type instance TagType Tags.OptionGroup            = Branch
type instance TagType Tags.Option                 = Branch
type instance TagType Tags.Output                 = Branch
type instance TagType Tags.Paragraph              = Branch
type instance TagType Tags.Picture                = Branch
type instance TagType Tags.PreformattedText       = Branch
type instance TagType Tags.Progress               = Branch
type instance TagType Tags.Quotation              = Branch
type instance TagType Tags.RubyParenthesis        = Branch
type instance TagType Tags.RubyText               = Branch
type instance TagType Tags.Ruby                   = Branch
type instance TagType Tags.Sample                 = Branch
type instance TagType Tags.Script                 = Branch
type instance TagType Tags.Search                 = Branch
type instance TagType Tags.Section                = Branch
type instance TagType Tags.Select                 = Branch
type instance TagType Tags.Slot                   = Branch
type instance TagType Tags.Small                  = Branch
type instance TagType Tags.Source                 = Leaf
type instance TagType Tags.Span                   = Branch
type instance TagType Tags.Strikethrough          = Branch
type instance TagType Tags.Strong                 = Branch
type instance TagType Tags.Style                  = Branch
type instance TagType Tags.Subscript              = Branch
type instance TagType Tags.Summary                = Branch
type instance TagType Tags.Superscript            = Branch
type instance TagType Tags.Table                  = Branch
type instance TagType Tags.TableBody              = Branch
type instance TagType Tags.TableDataCell          = Branch
type instance TagType Tags.TableFooter            = Branch
type instance TagType Tags.TableHeaderCell        = Branch
type instance TagType Tags.TableHeader            = Branch
type instance TagType Tags.TableRow               = Branch
type instance TagType Tags.Template               = Branch
type instance TagType Tags.TextArea               = Branch
type instance TagType Tags.Time                   = Branch
type instance TagType Tags.Title                  = Branch
type instance TagType Tags.Track                  = Leaf
type instance TagType Tags.Underline              = Branch
type instance TagType Tags.UnorderedList          = Branch
type instance TagType Tags.Variable               = Branch
type instance TagType Tags.Video                  = Branch
type instance TagType Tags.WordBreakOpportunity   = Leaf

type instance TagType Tags.Comment = Leaf
type instance TagType Tags.Text    = Leaf
