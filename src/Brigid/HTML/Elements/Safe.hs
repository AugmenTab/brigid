-- | This module contains a number of the safe element modules designed to
-- enforce compliance with the HTML spec, either by restricting arguments or
-- offering smart constructors to handle the building of complex or restrictive
-- element/attribute combinations.
--
-- It is intended that the user import it qualified, preferably as `Safe`. Care
-- has been taken to prevent name collisions.
--
module Brigid.HTML.Elements.Safe
  ( module Export
  ) where

-- import Brigid.HTML.Elements.Safe.Anchor as Export
-- import Brigid.HTML.Elements.Safe.Abbreviation as Export
-- import Brigid.HTML.Elements.Safe.ContactAddress as Export
import Brigid.HTML.Elements.Safe.Area as Export
-- import Brigid.HTML.Elements.Safe.Article as Export
-- import Brigid.HTML.Elements.Safe.Aside as Export
-- import Brigid.HTML.Elements.Safe.Audio as Export
-- import Brigid.HTML.Elements.Safe.BringAttentionTo as Export
import Brigid.HTML.Elements.Safe.Base as Export
-- import Brigid.HTML.Elements.Safe.BidirectionalIsolation as Export
-- import Brigid.HTML.Elements.Safe.BidirectionalOverride as Export
-- import Brigid.HTML.Elements.Safe.Blockquote as Export
-- import Brigid.HTML.Elements.Safe.Body as Export
-- import Brigid.HTML.Elements.Safe.LineBreak as Export
-- import Brigid.HTML.Elements.Safe.Button as Export
-- import Brigid.HTML.Elements.Safe.Canvas as Export
-- import Brigid.HTML.Elements.Safe.Citation as Export
-- import Brigid.HTML.Elements.Safe.Code as Export
-- import Brigid.HTML.Elements.Safe.Data as Export
-- import Brigid.HTML.Elements.Safe.DataList as Export
-- import Brigid.HTML.Elements.Safe.DescriptionDetails as Export
-- import Brigid.HTML.Elements.Safe.DeletedText as Export
-- import Brigid.HTML.Elements.Safe.Details as Export
-- import Brigid.HTML.Elements.Safe.Definition as Export
-- import Brigid.HTML.Elements.Safe.Dialog as Export
-- import Brigid.HTML.Elements.Safe.Division as Export
-- import Brigid.HTML.Elements.Safe.DescriptionList as Export
-- import Brigid.HTML.Elements.Safe.DescriptionTerm as Export
-- import Brigid.HTML.Elements.Safe.Emphasis as Export
-- import Brigid.HTML.Elements.Safe.Embed as Export
-- import Brigid.HTML.Elements.Safe.Fieldset as Export
-- import Brigid.HTML.Elements.Safe.FigureCaption as Export
-- import Brigid.HTML.Elements.Safe.Figure as Export
-- import Brigid.HTML.Elements.Safe.Footer as Export
-- import Brigid.HTML.Elements.Safe.Form as Export
-- import Brigid.HTML.Elements.Safe.H1 as Export
-- import Brigid.HTML.Elements.Safe.H2 as Export
-- import Brigid.HTML.Elements.Safe.H3 as Export
-- import Brigid.HTML.Elements.Safe.H4 as Export
-- import Brigid.HTML.Elements.Safe.H5 as Export
-- import Brigid.HTML.Elements.Safe.H6 as Export
-- import Brigid.HTML.Elements.Safe.Head as Export
-- import Brigid.HTML.Elements.Safe.Header as Export
-- import Brigid.HTML.Elements.Safe.HeadingGroup as Export
-- import Brigid.HTML.Elements.Safe.HorizontalRule as Export
-- import Brigid.HTML.Elements.Safe.Html as Export
-- import Brigid.HTML.Elements.Safe.IdiomaticText as Export
-- import Brigid.HTML.Elements.Safe.IFrame as Export
import Brigid.HTML.Elements.Safe.Image as Export
import Brigid.HTML.Elements.Safe.Input as Export
-- import Brigid.HTML.Elements.Safe.InsertedText as Export
-- import Brigid.HTML.Elements.Safe.KeyboardInput as Export
-- import Brigid.HTML.Elements.Safe.Label as Export
-- import Brigid.HTML.Elements.Safe.Legend as Export
-- import Brigid.HTML.Elements.Safe.ListItem as Export
-- import Brigid.HTML.Elements.Safe.Link as Export
-- import Brigid.HTML.Elements.Safe.Main as Export
-- import Brigid.HTML.Elements.Safe.Map as Export
-- import Brigid.HTML.Elements.Safe.Mark as Export
-- import Brigid.HTML.Elements.Safe.Menu as Export
import Brigid.HTML.Elements.Safe.Meta as Export
-- import Brigid.HTML.Elements.Safe.Meter as Export
-- import Brigid.HTML.Elements.Safe.Nav as Export
-- import Brigid.HTML.Elements.Safe.NoScript as Export
-- import Brigid.HTML.Elements.Safe.Object as Export
-- import Brigid.HTML.Elements.Safe.OrderedList as Export
-- import Brigid.HTML.Elements.Safe.OptionGroup as Export
-- import Brigid.HTML.Elements.Safe.Option as Export
-- import Brigid.HTML.Elements.Safe.Output as Export
-- import Brigid.HTML.Elements.Safe.Paragraph as Export
-- import Brigid.HTML.Elements.Safe.Picture as Export
-- import Brigid.HTML.Elements.Safe.PreformattedText as Export
-- import Brigid.HTML.Elements.Safe.Progress as Export
-- import Brigid.HTML.Elements.Safe.Quotation as Export
import Brigid.HTML.Elements.Safe.Ruby as Export
-- import Brigid.HTML.Elements.Safe.Strikethrough as Export
-- import Brigid.HTML.Elements.Safe.Sample as Export
import Brigid.HTML.Elements.Safe.Script as Export
-- import Brigid.HTML.Elements.Safe.Search as Export
-- import Brigid.HTML.Elements.Safe.Section as Export
-- import Brigid.HTML.Elements.Safe.Select as Export
-- import Brigid.HTML.Elements.Safe.Slot as Export
-- import Brigid.HTML.Elements.Safe.SideComment as Export
-- import Brigid.HTML.Elements.Safe.Source as Export
-- import Brigid.HTML.Elements.Safe.Span as Export
-- import Brigid.HTML.Elements.Safe.Strong as Export
-- import Brigid.HTML.Elements.Safe.Style as Export
-- import Brigid.HTML.Elements.Safe.Subscript as Export
-- import Brigid.HTML.Elements.Safe.Summary as Export
-- import Brigid.HTML.Elements.Safe.Superscript as Export
import Brigid.HTML.Elements.Safe.Table as Export
-- import Brigid.HTML.Elements.Safe.ContentTemplate as Export
-- import Brigid.HTML.Elements.Safe.TextArea as Export
-- import Brigid.HTML.Elements.Safe.Time as Export
-- import Brigid.HTML.Elements.Safe.Title as Export
-- import Brigid.HTML.Elements.Safe.Track as Export
-- import Brigid.HTML.Elements.Safe.Underline as Export
-- import Brigid.HTML.Elements.Safe.UnorderedList as Export
-- import Brigid.HTML.Elements.Safe.Variable as Export
-- import Brigid.HTML.Elements.Safe.Video as Export
-- import Brigid.HTML.Elements.Safe.WordBreakOpportunity as Export
