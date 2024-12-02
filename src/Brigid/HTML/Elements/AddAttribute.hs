{-# LANGUAGE GADTs #-}

module Brigid.HTML.Elements.AddAttribute
  ( addAnchorAttribute
  , addAbbreviationAttribute
  , addContactAddressAttribute
  , addAreaAttribute
  , addArticleAttribute
  , addAsideAttribute
  , addAudioAttribute
  , addBringAttentionToAttribute
  , addBaseAttribute
  , addBidirectionalIsolationAttribute
  , addBidirectionalOverrideAttribute
  , addBlockquoteAttribute
  , addBodyAttribute
  , addLineBreakAttribute
  , addButtonAttribute
  , addCanvasAttribute
  , addTableCaptionAttribute
  , addCitationAttribute
  , addCodeAttribute
  , addTableColumnAttribute
  , addTableColumnGroupAttribute
  , addDataAttribute
  , addDataListAttribute
  , addDescriptionDetailsAttribute
  , addDeletedTextAttribute
  , addDetailsAttribute
  , addDefinitionAttribute
  , addDialogAttribute
  , addDivisionAttribute
  , addDescriptionListAttribute
  , addDescriptionTermAttribute
  , addEmphasisAttribute
  , addEmbedAttribute
  , addFieldsetAttribute
  , addFigureCaptionAttribute
  , addFigureAttribute
  , addFooterAttribute
  , addFormAttribute
  , addH1Attribute
  , addH2Attribute
  , addH3Attribute
  , addH4Attribute
  , addH5Attribute
  , addH6Attribute
  , addHeadAttribute
  , addHeaderAttribute
  , addHeadingGroupAttribute
  , addHorizontalRuleAttribute
  , addIdiomaticTextAttribute
  , addIFrameAttribute
  , addImageAttribute
  , addInputAttribute
  , addInsertedTextAttribute
  , addKeyboardInputAttribute
  , addLabelAttribute
  , addLegendAttribute
  , addListItemAttribute
  , addLinkAttribute
  , addMainAttribute
  , addMapAttribute
  , addMarkAttribute
  , addMenuAttribute
  , addMetaAttribute
  , addMeterAttribute
  , addNavAttribute
  , addNoScriptAttribute
  , addObjectAttribute
  , addOrderedListAttribute
  , addOptionGroupAttribute
  , addOptionAttribute
  , addOutputAttribute
  , addParagraphAttribute
  , addPictureAttribute
  , addPreformattedTextAttribute
  , addProgressAttribute
  , addQuotationAttribute
  , addRubyParenthesisAttribute
  , addRubyTextAttribute
  , addRubyAttribute
  , addStrikethroughAttribute
  , addSampleAttribute
  , addScriptAttribute
  , addSearchAttribute
  , addSectionAttribute
  , addSelectAttribute
  , addSlotAttribute
  , addSideCommentAttribute
  , addSourceAttribute
  , addSpanAttribute
  , addStrongAttribute
  , addStyleAttribute
  , addSubscriptAttribute
  , addSummaryAttribute
  , addSuperscriptAttribute
  , addTableAttribute
  , addTableBodyAttribute
  , addTableDataCellAttribute
  , addContentTemplateAttribute
  , addTextAreaAttribute
  , addTableFootAttribute
  , addTableHeaderAttribute
  , addTableHeadAttribute
  , addTimeAttribute
  , addTitleAttribute
  , addTableRowAttribute
  , addTrackAttribute
  , addUnderlineAttribute
  , addUnorderedListAttribute
  , addVariableAttribute
  , addVideoAttribute
  , addWordBreakOpportunityAttribute
  ) where

import Data.Map qualified as Map

import Brigid.HTML.Attributes.Internal (Attribute, attributeText)
import Brigid.HTML.Elements.Internal (ChildHTML(..))
import Brigid.HTML.Elements.Tags qualified as Tags

addAnchorAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Anchor
                   -> ChildHTML parent grandparent
addAnchorAttribute tag attr =
  case tag of
    Tag_Anchor attrs content ->
      Tag_Anchor (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addAbbreviationAttribute :: ChildHTML parent grandparent
                         -> Attribute Tags.Abbreviation
                         -> ChildHTML parent grandparent
addAbbreviationAttribute tag attr =
  case tag of
    Tag_Abbreviation attrs content ->
      Tag_Abbreviation (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addContactAddressAttribute :: ChildHTML parent grandparent
                           -> Attribute Tags.ContactAddress
                           -> ChildHTML parent grandparent
addContactAddressAttribute tag attr =
  case tag of
    Tag_ContactAddress attrs content ->
      Tag_ContactAddress (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addAreaAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Area
                 -> ChildHTML parent grandparent
addAreaAttribute tag attr =
  case tag of
    Tag_Area attrs ->
      Tag_Area (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addArticleAttribute :: ChildHTML parent grandparent
                    -> Attribute Tags.Article
                    -> ChildHTML parent grandparent
addArticleAttribute tag attr =
  case tag of
    Tag_Article attrs content ->
      Tag_Article (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addAsideAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Aside
                  -> ChildHTML parent grandparent
addAsideAttribute tag attr =
  case tag of
    Tag_Aside attrs content ->
      Tag_Aside (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addAudioAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Audio
                  -> ChildHTML parent grandparent
addAudioAttribute tag attr =
  case tag of
    Tag_Audio attrs content ->
      Tag_Audio (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addBringAttentionToAttribute :: ChildHTML parent grandparent
                             -> Attribute Tags.BringAttentionTo
                             -> ChildHTML parent grandparent
addBringAttentionToAttribute tag attr =
  case tag of
    Tag_BringAttentionTo attrs content ->
      Tag_BringAttentionTo (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addBaseAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Base
                 -> ChildHTML parent grandparent
addBaseAttribute tag attr =
  case tag of
    Tag_Base attrs ->
      Tag_Base (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addBidirectionalIsolationAttribute :: ChildHTML parent grandparent
                                   -> Attribute Tags.BidirectionalIsolation
                                   -> ChildHTML parent grandparent
addBidirectionalIsolationAttribute tag attr =
  case tag of
    Tag_BidirectionalIsolation attrs content ->
      Tag_BidirectionalIsolation
        (Map.insert (attributeText attr) attr attrs)
        content

    _ ->
      tag

addBidirectionalOverrideAttribute :: ChildHTML parent grandparent
                                  -> Attribute Tags.BidirectionalOverride
                                  -> ChildHTML parent grandparent
addBidirectionalOverrideAttribute tag attr =
  case tag of
    Tag_BidirectionalOverride attrs content ->
      Tag_BidirectionalOverride
        (Map.insert (attributeText attr) attr attrs)
        content

    _ ->
      tag

addBlockquoteAttribute :: ChildHTML parent grandparent
                       -> Attribute Tags.Blockquote
                       -> ChildHTML parent grandparent
addBlockquoteAttribute tag attr =
  case tag of
    Tag_Blockquote attrs content ->
      Tag_Blockquote (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addBodyAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Body
                 -> ChildHTML parent grandparent
addBodyAttribute tag attr =
  case tag of
    Tag_Body attrs content ->
      Tag_Body (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addLineBreakAttribute :: ChildHTML parent grandparent
                      -> Attribute Tags.LineBreak
                      -> ChildHTML parent grandparent
addLineBreakAttribute tag attr =
  case tag of
    Tag_LineBreak attrs ->
      Tag_LineBreak (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addButtonAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Button
                   -> ChildHTML parent grandparent
addButtonAttribute tag attr =
  case tag of
    Tag_Button attrs content ->
      Tag_Button (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addCanvasAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Canvas
                   -> ChildHTML parent grandparent
addCanvasAttribute tag attr =
  case tag of
    Tag_Canvas attrs content ->
      Tag_Canvas (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableCaptionAttribute :: ChildHTML parent grandparent
                         -> Attribute Tags.TableCaption
                         -> ChildHTML parent grandparent
addTableCaptionAttribute tag attr =
  case tag of
    Tag_TableCaption attrs content ->
      Tag_TableCaption (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addCitationAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.Citation
                     -> ChildHTML parent grandparent
addCitationAttribute tag attr =
  case tag of
    Tag_Citation attrs content ->
      Tag_Citation (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addCodeAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Code
                 -> ChildHTML parent grandparent
addCodeAttribute tag attr =
  case tag of
    Tag_Code attrs content ->
      Tag_Code (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableColumnAttribute :: ChildHTML parent grandparent
                        -> Attribute Tags.TableColumn
                        -> ChildHTML parent grandparent
addTableColumnAttribute tag attr =
  case tag of
    Tag_TableColumn attrs ->
      Tag_TableColumn (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addTableColumnGroupAttribute :: ChildHTML parent grandparent
                             -> Attribute Tags.TableColumnGroup
                             -> ChildHTML parent grandparent
addTableColumnGroupAttribute tag attr =
  case tag of
    Tag_TableColumnGroup attrs content ->
      Tag_TableColumnGroup (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDataAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Data
                 -> ChildHTML parent grandparent
addDataAttribute tag attr =
  case tag of
    Tag_Data attrs content ->
      Tag_Data (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDataListAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.DataList
                     -> ChildHTML parent grandparent
addDataListAttribute tag attr =
  case tag of
    Tag_DataList attrs content ->
      Tag_DataList (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDescriptionDetailsAttribute :: ChildHTML parent grandparent
                               -> Attribute Tags.DescriptionDetails
                               -> ChildHTML parent grandparent
addDescriptionDetailsAttribute tag attr =
  case tag of
    Tag_DescriptionDetails attrs content ->
      Tag_DescriptionDetails
        (Map.insert (attributeText attr) attr attrs)
        content

    _ ->
      tag

addDeletedTextAttribute :: ChildHTML parent grandparent
                        -> Attribute Tags.DeletedText
                        -> ChildHTML parent grandparent
addDeletedTextAttribute tag attr =
  case tag of
    Tag_DeletedText attrs content ->
      Tag_DeletedText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDetailsAttribute :: ChildHTML parent grandparent
                    -> Attribute Tags.Details
                    -> ChildHTML parent grandparent
addDetailsAttribute tag attr =
  case tag of
    Tag_Details attrs content ->
      Tag_Details (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDefinitionAttribute :: ChildHTML parent grandparent
                       -> Attribute Tags.Definition
                       -> ChildHTML parent grandparent
addDefinitionAttribute tag attr =
  case tag of
    Tag_Definition attrs content ->
      Tag_Definition (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDialogAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Dialog
                   -> ChildHTML parent grandparent
addDialogAttribute tag attr =
  case tag of
    Tag_Dialog attrs content ->
      Tag_Dialog (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDivisionAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.Division
                     -> ChildHTML parent grandparent
addDivisionAttribute tag attr =
  case tag of
    Tag_Division attrs content ->
      Tag_Division (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDescriptionListAttribute :: ChildHTML parent grandparent
                            -> Attribute Tags.DescriptionList
                            -> ChildHTML parent grandparent
addDescriptionListAttribute tag attr =
  case tag of
    Tag_DescriptionList attrs content ->
      Tag_DescriptionList (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDescriptionTermAttribute :: ChildHTML parent grandparent
                            -> Attribute Tags.DescriptionTerm
                            -> ChildHTML parent grandparent
addDescriptionTermAttribute tag attr =
  case tag of
    Tag_DescriptionTerm attrs content ->
      Tag_DescriptionTerm (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addEmphasisAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.Emphasis
                     -> ChildHTML parent grandparent
addEmphasisAttribute tag attr =
  case tag of
    Tag_Emphasis attrs content ->
      Tag_Emphasis (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addEmbedAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Embed
                  -> ChildHTML parent grandparent
addEmbedAttribute tag attr =
  case tag of
    Tag_Embed attrs ->
      Tag_Embed (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addFieldsetAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.Fieldset
                     -> ChildHTML parent grandparent
addFieldsetAttribute tag attr =
  case tag of
    Tag_Fieldset attrs content ->
      Tag_Fieldset (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addFigureCaptionAttribute :: ChildHTML parent grandparent
                          -> Attribute Tags.FigureCaption
                          -> ChildHTML parent grandparent
addFigureCaptionAttribute tag attr =
  case tag of
    Tag_FigureCaption attrs content ->
      Tag_FigureCaption (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addFigureAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Figure
                   -> ChildHTML parent grandparent
addFigureAttribute tag attr =
  case tag of
    Tag_Figure attrs content ->
      Tag_Figure (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addFooterAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Footer
                   -> ChildHTML parent grandparent
addFooterAttribute tag attr =
  case tag of
    Tag_Footer attrs content ->
      Tag_Footer (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addFormAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Form
                 -> ChildHTML parent grandparent
addFormAttribute tag attr =
  case tag of
    Tag_Form attrs content ->
      Tag_Form (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH1Attribute :: ChildHTML parent grandparent
               -> Attribute Tags.H1
               -> ChildHTML parent grandparent
addH1Attribute tag attr =
  case tag of
    Tag_H1 attrs content ->
      Tag_H1 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH2Attribute :: ChildHTML parent grandparent
               -> Attribute Tags.H2
               -> ChildHTML parent grandparent
addH2Attribute tag attr =
  case tag of
    Tag_H2 attrs content ->
      Tag_H2 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH3Attribute :: ChildHTML parent grandparent
               -> Attribute Tags.H3
               -> ChildHTML parent grandparent
addH3Attribute tag attr =
  case tag of
    Tag_H3 attrs content ->
      Tag_H3 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH4Attribute :: ChildHTML parent grandparent
               -> Attribute Tags.H4
               -> ChildHTML parent grandparent
addH4Attribute tag attr =
  case tag of
    Tag_H4 attrs content ->
      Tag_H4 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH5Attribute :: ChildHTML parent grandparent
               -> Attribute Tags.H5
               -> ChildHTML parent grandparent
addH5Attribute tag attr =
  case tag of
    Tag_H5 attrs content ->
      Tag_H5 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH6Attribute :: ChildHTML parent grandparent
               -> Attribute Tags.H6
               -> ChildHTML parent grandparent
addH6Attribute tag attr =
  case tag of
    Tag_H6 attrs content ->
      Tag_H6 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addHeadAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Head
                 -> ChildHTML parent grandparent
addHeadAttribute tag attr =
  case tag of
    Tag_Head attrs content ->
      Tag_Head (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addHeaderAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Header
                   -> ChildHTML parent grandparent
addHeaderAttribute tag attr =
  case tag of
    Tag_Header attrs content ->
      Tag_Header (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addHeadingGroupAttribute :: ChildHTML parent grandparent
                         -> Attribute Tags.HeadingGroup
                         -> ChildHTML parent grandparent
addHeadingGroupAttribute tag attr =
  case tag of
    Tag_HeadingGroup attrs content ->
      Tag_HeadingGroup (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addHorizontalRuleAttribute :: ChildHTML parent grandparent
                           -> Attribute Tags.HorizontalRule
                           -> ChildHTML parent grandparent
addHorizontalRuleAttribute tag attr =
  case tag of
    Tag_HorizontalRule attrs ->
      Tag_HorizontalRule (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addIdiomaticTextAttribute :: ChildHTML parent grandparent
                          -> Attribute Tags.IdiomaticText
                          -> ChildHTML parent grandparent
addIdiomaticTextAttribute tag attr =
  case tag of
    Tag_IdiomaticText attrs content ->
      Tag_IdiomaticText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addIFrameAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.IFrame
                   -> ChildHTML parent grandparent
addIFrameAttribute tag attr =
  case tag of
    Tag_IFrame attrs ->
      Tag_IFrame (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addImageAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Image
                  -> ChildHTML parent grandparent
addImageAttribute tag attr =
  case tag of
    Tag_Image attrs ->
      Tag_Image (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addInputAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Input
                  -> ChildHTML parent grandparent
addInputAttribute tag attr =
  case tag of
    Tag_Input attrs ->
      Tag_Input (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addInsertedTextAttribute :: ChildHTML parent grandparent
                         -> Attribute Tags.InsertedText
                         -> ChildHTML parent grandparent
addInsertedTextAttribute tag attr =
  case tag of
    Tag_InsertedText attrs content ->
      Tag_InsertedText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addKeyboardInputAttribute :: ChildHTML parent grandparent
                          -> Attribute Tags.KeyboardInput
                          -> ChildHTML parent grandparent
addKeyboardInputAttribute tag attr =
  case tag of
    Tag_KeyboardInput attrs content ->
      Tag_KeyboardInput (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addLabelAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Label
                  -> ChildHTML parent grandparent
addLabelAttribute tag attr =
  case tag of
    Tag_Label attrs content ->
      Tag_Label (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addLegendAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Legend
                   -> ChildHTML parent grandparent
addLegendAttribute tag attr =
  case tag of
    Tag_Legend attrs content ->
      Tag_Legend (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addListItemAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.ListItem
                     -> ChildHTML parent grandparent
addListItemAttribute tag attr =
  case tag of
    Tag_ListItem attrs content ->
      Tag_ListItem (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addLinkAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Link
                 -> ChildHTML parent grandparent
addLinkAttribute tag attr =
  case tag of
    Tag_Link attrs ->
      Tag_Link (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addMainAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Main
                 -> ChildHTML parent grandparent
addMainAttribute tag attr =
  case tag of
    Tag_Main attrs content ->
      Tag_Main (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addMapAttribute :: ChildHTML parent grandparent
                -> Attribute Tags.Map
                -> ChildHTML parent grandparent
addMapAttribute tag attr =
  case tag of
    Tag_Map attrs content ->
      Tag_Map (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addMarkAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Mark
                 -> ChildHTML parent grandparent
addMarkAttribute tag attr =
  case tag of
    Tag_Mark attrs content ->
      Tag_Mark (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addMenuAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Menu
                 -> ChildHTML parent grandparent
addMenuAttribute tag attr =
  case tag of
    Tag_Menu attrs content ->
      Tag_Menu (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addMetaAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Meta
                 -> ChildHTML parent grandparent
addMetaAttribute tag attr =
  case tag of
    Tag_Meta attrs ->
      Tag_Meta (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addMeterAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Meter
                  -> ChildHTML parent grandparent
addMeterAttribute tag attr =
  case tag of
    Tag_Meter attrs content ->
      Tag_Meter (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addNavAttribute :: ChildHTML parent grandparent
                -> Attribute Tags.Nav
                -> ChildHTML parent grandparent
addNavAttribute tag attr =
  case tag of
    Tag_Nav attrs content ->
      Tag_Nav (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addNoScriptAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.NoScript
                     -> ChildHTML parent grandparent
addNoScriptAttribute tag attr =
  case tag of
    Tag_NoScript attrs content ->
      Tag_NoScript (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addObjectAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Object
                   -> ChildHTML parent grandparent
addObjectAttribute tag attr =
  case tag of
    Tag_Object attrs content ->
      Tag_Object (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addOrderedListAttribute :: ChildHTML parent grandparent
                        -> Attribute Tags.OrderedList
                        -> ChildHTML parent grandparent
addOrderedListAttribute tag attr =
  case tag of
    Tag_OrderedList attrs content ->
      Tag_OrderedList (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addOptionGroupAttribute :: ChildHTML parent grandparent
                        -> Attribute Tags.OptionGroup
                        -> ChildHTML parent grandparent
addOptionGroupAttribute tag attr =
  case tag of
    Tag_OptionGroup attrs content ->
      Tag_OptionGroup (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addOptionAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Option
                   -> ChildHTML parent grandparent
addOptionAttribute tag attr =
  case tag of
    Tag_Option attrs content ->
      Tag_Option (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addOutputAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Output
                   -> ChildHTML parent grandparent
addOutputAttribute tag attr =
  case tag of
    Tag_Output attrs content ->
      Tag_Output (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addParagraphAttribute :: ChildHTML parent grandparent
                      -> Attribute Tags.Paragraph
                      -> ChildHTML parent grandparent
addParagraphAttribute tag attr =
  case tag of
    Tag_Paragraph attrs content ->
      Tag_Paragraph (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addPictureAttribute :: ChildHTML parent grandparent
                    -> Attribute Tags.Picture
                    -> ChildHTML parent grandparent
addPictureAttribute tag attr =
  case tag of
    Tag_Picture attrs content ->
      Tag_Picture (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addPreformattedTextAttribute :: ChildHTML parent grandparent
                             -> Attribute Tags.PreformattedText
                             -> ChildHTML parent grandparent
addPreformattedTextAttribute tag attr =
  case tag of
    Tag_PreformattedText attrs content ->
      Tag_PreformattedText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addProgressAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.Progress
                     -> ChildHTML parent grandparent
addProgressAttribute tag attr =
  case tag of
    Tag_Progress attrs content ->
      Tag_Progress (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addQuotationAttribute :: ChildHTML parent grandparent
                      -> Attribute Tags.Quotation
                      -> ChildHTML parent grandparent
addQuotationAttribute tag attr =
  case tag of
    Tag_Quotation attrs content ->
      Tag_Quotation (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addRubyParenthesisAttribute :: ChildHTML parent grandparent
                            -> Attribute Tags.RubyParenthesis
                            -> ChildHTML parent grandparent
addRubyParenthesisAttribute tag attr =
  case tag of
    Tag_RubyParenthesis attrs content ->
      Tag_RubyParenthesis (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addRubyTextAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.RubyText
                     -> ChildHTML parent grandparent
addRubyTextAttribute tag attr =
  case tag of
    Tag_RubyText attrs content ->
      Tag_RubyText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addRubyAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Ruby
                 -> ChildHTML parent grandparent
addRubyAttribute tag attr =
  case tag of
    Tag_Ruby attrs content ->
      Tag_Ruby (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addStrikethroughAttribute :: ChildHTML parent grandparent
                          -> Attribute Tags.Strikethrough
                          -> ChildHTML parent grandparent
addStrikethroughAttribute tag attr =
  case tag of
    Tag_Strikethrough attrs content ->
      Tag_Strikethrough (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSampleAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Sample
                   -> ChildHTML parent grandparent
addSampleAttribute tag attr =
  case tag of
    Tag_Sample attrs content ->
      Tag_Sample (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addScriptAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Script
                   -> ChildHTML parent grandparent
addScriptAttribute tag attr =
  case tag of
    Tag_Script attrs content ->
      Tag_Script (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSearchAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Search
                   -> ChildHTML parent grandparent
addSearchAttribute tag attr =
  case tag of
    Tag_Search attrs content ->
      Tag_Search (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSectionAttribute :: ChildHTML parent grandparent
                    -> Attribute Tags.Section
                    -> ChildHTML parent grandparent
addSectionAttribute tag attr =
  case tag of
    Tag_Section attrs content ->
      Tag_Section (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSelectAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Select
                   -> ChildHTML parent grandparent
addSelectAttribute tag attr =
  case tag of
    Tag_Select attrs content ->
      Tag_Select (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSlotAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Slot
                 -> ChildHTML parent grandparent
addSlotAttribute tag attr =
  case tag of
    Tag_Slot attrs content ->
      Tag_Slot (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSideCommentAttribute :: ChildHTML parent grandparent
                        -> Attribute Tags.SideComment
                        -> ChildHTML parent grandparent
addSideCommentAttribute tag attr =
  case tag of
    Tag_SideComment attrs content ->
      Tag_SideComment (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSourceAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Source
                   -> ChildHTML parent grandparent
addSourceAttribute tag attr =
  case tag of
    Tag_Source attrs ->
      Tag_Source (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addSpanAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Span
                 -> ChildHTML parent grandparent
addSpanAttribute tag attr =
  case tag of
    Tag_Span attrs content ->
      Tag_Span (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addStrongAttribute :: ChildHTML parent grandparent
                   -> Attribute Tags.Strong
                   -> ChildHTML parent grandparent
addStrongAttribute tag attr =
  case tag of
    Tag_Strong attrs content ->
      Tag_Strong (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addStyleAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Style
                  -> ChildHTML parent grandparent
addStyleAttribute tag attr =
  case tag of
    Tag_Style attrs content ->
      Tag_Style (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSubscriptAttribute :: ChildHTML parent grandparent
                      -> Attribute Tags.Subscript
                      -> ChildHTML parent grandparent
addSubscriptAttribute tag attr =
  case tag of
    Tag_Subscript attrs content ->
      Tag_Subscript (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSummaryAttribute :: ChildHTML parent grandparent
                    -> Attribute Tags.Summary
                    -> ChildHTML parent grandparent
addSummaryAttribute tag attr =
  case tag of
    Tag_Summary attrs content ->
      Tag_Summary (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSuperscriptAttribute :: ChildHTML parent grandparent
                        -> Attribute Tags.Superscript
                        -> ChildHTML parent grandparent
addSuperscriptAttribute tag attr =
  case tag of
    Tag_Superscript attrs content ->
      Tag_Superscript (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Table
                  -> ChildHTML parent grandparent
addTableAttribute tag attr =
  case tag of
    Tag_Table attrs content ->
      Tag_Table (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableBodyAttribute :: ChildHTML parent grandparent
                      -> Attribute Tags.TableBody
                      -> ChildHTML parent grandparent
addTableBodyAttribute tag attr =
  case tag of
    Tag_TableBody attrs content ->
      Tag_TableBody (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableDataCellAttribute :: ChildHTML parent grandparent
                          -> Attribute Tags.TableDataCell
                          -> ChildHTML parent grandparent
addTableDataCellAttribute tag attr =
  case tag of
    Tag_TableDataCell attrs content ->
      Tag_TableDataCell (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addContentTemplateAttribute :: ChildHTML parent grandparent
                            -> Attribute Tags.ContentTemplate
                            -> ChildHTML parent grandparent
addContentTemplateAttribute tag attr =
  case tag of
    Tag_ContentTemplate attrs content ->
      Tag_ContentTemplate (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTextAreaAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.TextArea
                     -> ChildHTML parent grandparent
addTextAreaAttribute tag attr =
  case tag of
    Tag_TextArea attrs content ->
      Tag_TextArea (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableFootAttribute :: ChildHTML parent grandparent
                      -> Attribute Tags.TableFoot
                      -> ChildHTML parent grandparent
addTableFootAttribute tag attr =
  case tag of
    Tag_TableFoot attrs content ->
      Tag_TableFoot (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableHeaderAttribute :: ChildHTML parent grandparent
                        -> Attribute Tags.TableHeader
                        -> ChildHTML parent grandparent
addTableHeaderAttribute tag attr =
  case tag of
    Tag_TableHeader attrs content ->
      Tag_TableHeader (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableHeadAttribute :: ChildHTML parent grandparent
                      -> Attribute Tags.TableHead
                      -> ChildHTML parent grandparent
addTableHeadAttribute tag attr =
  case tag of
    Tag_TableHead attrs content ->
      Tag_TableHead (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTimeAttribute :: ChildHTML parent grandparent
                 -> Attribute Tags.Time
                 -> ChildHTML parent grandparent
addTimeAttribute tag attr =
  case tag of
    Tag_Time attrs content ->
      Tag_Time (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTitleAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Title
                  -> ChildHTML parent grandparent
addTitleAttribute tag attr =
  case tag of
    Tag_Title attrs content ->
      Tag_Title (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableRowAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.TableRow
                     -> ChildHTML parent grandparent
addTableRowAttribute tag attr =
  case tag of
    Tag_TableRow attrs content ->
      Tag_TableRow (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTrackAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Track
                  -> ChildHTML parent grandparent
addTrackAttribute tag attr =
  case tag of
    Tag_Track attrs ->
      Tag_Track (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addUnderlineAttribute :: ChildHTML parent grandparent
                      -> Attribute Tags.Underline
                      -> ChildHTML parent grandparent
addUnderlineAttribute tag attr =
  case tag of
    Tag_Underline attrs content ->
      Tag_Underline (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addUnorderedListAttribute :: ChildHTML parent grandparent
                          -> Attribute Tags.UnorderedList
                          -> ChildHTML parent grandparent
addUnorderedListAttribute tag attr =
  case tag of
    Tag_UnorderedList attrs content ->
      Tag_UnorderedList (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addVariableAttribute :: ChildHTML parent grandparent
                     -> Attribute Tags.Variable
                     -> ChildHTML parent grandparent
addVariableAttribute tag attr =
  case tag of
    Tag_Variable attrs content ->
      Tag_Variable (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addVideoAttribute :: ChildHTML parent grandparent
                  -> Attribute Tags.Video
                  -> ChildHTML parent grandparent
addVideoAttribute tag attr =
  case tag of
    Tag_Video attrs content ->
      Tag_Video (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addWordBreakOpportunityAttribute :: ChildHTML parent grandparent
                                 -> Attribute Tags.WordBreakOpportunity
                                 -> ChildHTML parent grandparent
addWordBreakOpportunityAttribute tag attr =
  case tag of
    Tag_WordBreakOpportunity attrs ->
      Tag_WordBreakOpportunity (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag
