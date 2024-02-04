{-# LANGUAGE GADTs #-}

module HTML.Elements.AddAttribute
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
  , addHtmlAttribute
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

import HTML.Attributes.Internal (Attribute, attributeText)
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Elements.Tags qualified as Tags

addAnchorAttribute :: ChildHTML parent
                   -> Attribute Tags.Anchor
                   -> ChildHTML parent
addAnchorAttribute tag attr =
  case tag of
    Tag_Anchor attrs content ->
      Tag_Anchor (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addAbbreviationAttribute :: ChildHTML parent
                         -> Attribute Tags.Abbreviation
                         -> ChildHTML parent
addAbbreviationAttribute tag attr =
  case tag of
    Tag_Abbreviation attrs content ->
      Tag_Abbreviation (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addContactAddressAttribute :: ChildHTML parent
                           -> Attribute Tags.ContactAddress
                           -> ChildHTML parent
addContactAddressAttribute tag attr =
  case tag of
    Tag_ContactAddress attrs content ->
      Tag_ContactAddress (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addAreaAttribute :: ChildHTML parent
                 -> Attribute Tags.Area
                 -> ChildHTML parent
addAreaAttribute tag attr =
  case tag of
    Tag_Area attrs ->
      Tag_Area (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addArticleAttribute :: ChildHTML parent
                    -> Attribute Tags.Article
                    -> ChildHTML parent
addArticleAttribute tag attr =
  case tag of
    Tag_Article attrs content ->
      Tag_Article (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addAsideAttribute :: ChildHTML parent
                  -> Attribute Tags.Aside
                  -> ChildHTML parent
addAsideAttribute tag attr =
  case tag of
    Tag_Aside attrs content ->
      Tag_Aside (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addAudioAttribute :: ChildHTML parent
                  -> Attribute Tags.Audio
                  -> ChildHTML parent
addAudioAttribute tag attr =
  case tag of
    Tag_Audio attrs content ->
      Tag_Audio (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addBringAttentionToAttribute :: ChildHTML parent
                             -> Attribute Tags.BringAttentionTo
                             -> ChildHTML parent
addBringAttentionToAttribute tag attr =
  case tag of
    Tag_BringAttentionTo attrs content ->
      Tag_BringAttentionTo (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addBaseAttribute :: ChildHTML parent
                 -> Attribute Tags.Base
                 -> ChildHTML parent
addBaseAttribute tag attr =
  case tag of
    Tag_Base attrs ->
      Tag_Base (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addBidirectionalIsolationAttribute :: ChildHTML parent
                                   -> Attribute Tags.BidirectionalIsolation
                                   -> ChildHTML parent
addBidirectionalIsolationAttribute tag attr =
  case tag of
    Tag_BidirectionalIsolation attrs content ->
      Tag_BidirectionalIsolation
        (Map.insert (attributeText attr) attr attrs)
        content

    _ ->
      tag

addBidirectionalOverrideAttribute :: ChildHTML parent
                                  -> Attribute Tags.BidirectionalOverride
                                  -> ChildHTML parent
addBidirectionalOverrideAttribute tag attr =
  case tag of
    Tag_BidirectionalOverride attrs content ->
      Tag_BidirectionalOverride
        (Map.insert (attributeText attr) attr attrs)
        content

    _ ->
      tag

addBlockquoteAttribute :: ChildHTML parent
                       -> Attribute Tags.Blockquote
                       -> ChildHTML parent
addBlockquoteAttribute tag attr =
  case tag of
    Tag_Blockquote attrs content ->
      Tag_Blockquote (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addBodyAttribute :: ChildHTML parent
                 -> Attribute Tags.Body
                 -> ChildHTML parent
addBodyAttribute tag attr =
  case tag of
    Tag_Body attrs content ->
      Tag_Body (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addLineBreakAttribute :: ChildHTML parent
                      -> Attribute Tags.LineBreak
                      -> ChildHTML parent
addLineBreakAttribute tag attr =
  case tag of
    Tag_LineBreak attrs ->
      Tag_LineBreak (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addButtonAttribute :: ChildHTML parent
                   -> Attribute Tags.Button
                   -> ChildHTML parent
addButtonAttribute tag attr =
  case tag of
    Tag_Button attrs content ->
      Tag_Button (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addCanvasAttribute :: ChildHTML parent
                   -> Attribute Tags.Canvas
                   -> ChildHTML parent
addCanvasAttribute tag attr =
  case tag of
    Tag_Canvas attrs content ->
      Tag_Canvas (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableCaptionAttribute :: ChildHTML parent
                         -> Attribute Tags.TableCaption
                         -> ChildHTML parent
addTableCaptionAttribute tag attr =
  case tag of
    Tag_TableCaption attrs content ->
      Tag_TableCaption (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addCitationAttribute :: ChildHTML parent
                     -> Attribute Tags.Citation
                     -> ChildHTML parent
addCitationAttribute tag attr =
  case tag of
    Tag_Citation attrs content ->
      Tag_Citation (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addCodeAttribute :: ChildHTML parent
                 -> Attribute Tags.Code
                 -> ChildHTML parent
addCodeAttribute tag attr =
  case tag of
    Tag_Code attrs content ->
      Tag_Code (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableColumnAttribute :: ChildHTML parent
                        -> Attribute Tags.TableColumn
                        -> ChildHTML parent
addTableColumnAttribute tag attr =
  case tag of
    Tag_TableColumn attrs ->
      Tag_TableColumn (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addTableColumnGroupAttribute :: ChildHTML parent
                             -> Attribute Tags.TableColumnGroup
                             -> ChildHTML parent
addTableColumnGroupAttribute tag attr =
  case tag of
    Tag_TableColumnGroup attrs content ->
      Tag_TableColumnGroup (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDataAttribute :: ChildHTML parent
                 -> Attribute Tags.Data
                 -> ChildHTML parent
addDataAttribute tag attr =
  case tag of
    Tag_Data attrs content ->
      Tag_Data (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDataListAttribute :: ChildHTML parent
                     -> Attribute Tags.DataList
                     -> ChildHTML parent
addDataListAttribute tag attr =
  case tag of
    Tag_DataList attrs content ->
      Tag_DataList (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDescriptionDetailsAttribute :: ChildHTML parent
                               -> Attribute Tags.DescriptionDetails
                               -> ChildHTML parent
addDescriptionDetailsAttribute tag attr =
  case tag of
    Tag_DescriptionDetails attrs content ->
      Tag_DescriptionDetails
        (Map.insert (attributeText attr) attr attrs)
        content

    _ ->
      tag

addDeletedTextAttribute :: ChildHTML parent
                        -> Attribute Tags.DeletedText
                        -> ChildHTML parent
addDeletedTextAttribute tag attr =
  case tag of
    Tag_DeletedText attrs content ->
      Tag_DeletedText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDetailsAttribute :: ChildHTML parent
                    -> Attribute Tags.Details
                    -> ChildHTML parent
addDetailsAttribute tag attr =
  case tag of
    Tag_Details attrs content ->
      Tag_Details (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDefinitionAttribute :: ChildHTML parent
                       -> Attribute Tags.Definition
                       -> ChildHTML parent
addDefinitionAttribute tag attr =
  case tag of
    Tag_Definition attrs content ->
      Tag_Definition (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDialogAttribute :: ChildHTML parent
                   -> Attribute Tags.Dialog
                   -> ChildHTML parent
addDialogAttribute tag attr =
  case tag of
    Tag_Dialog attrs content ->
      Tag_Dialog (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDivisionAttribute :: ChildHTML parent
                     -> Attribute Tags.Division
                     -> ChildHTML parent
addDivisionAttribute tag attr =
  case tag of
    Tag_Division attrs content ->
      Tag_Division (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDescriptionListAttribute :: ChildHTML parent
                            -> Attribute Tags.DescriptionList
                            -> ChildHTML parent
addDescriptionListAttribute tag attr =
  case tag of
    Tag_DescriptionList attrs content ->
      Tag_DescriptionList (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addDescriptionTermAttribute :: ChildHTML parent
                            -> Attribute Tags.DescriptionTerm
                            -> ChildHTML parent
addDescriptionTermAttribute tag attr =
  case tag of
    Tag_DescriptionTerm attrs content ->
      Tag_DescriptionTerm (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addEmphasisAttribute :: ChildHTML parent
                     -> Attribute Tags.Emphasis
                     -> ChildHTML parent
addEmphasisAttribute tag attr =
  case tag of
    Tag_Emphasis attrs content ->
      Tag_Emphasis (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addEmbedAttribute :: ChildHTML parent
                  -> Attribute Tags.Embed
                  -> ChildHTML parent
addEmbedAttribute tag attr =
  case tag of
    Tag_Embed attrs ->
      Tag_Embed (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addFieldsetAttribute :: ChildHTML parent
                     -> Attribute Tags.Fieldset
                     -> ChildHTML parent
addFieldsetAttribute tag attr =
  case tag of
    Tag_Fieldset attrs content ->
      Tag_Fieldset (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addFigureCaptionAttribute :: ChildHTML parent
                          -> Attribute Tags.FigureCaption
                          -> ChildHTML parent
addFigureCaptionAttribute tag attr =
  case tag of
    Tag_FigureCaption attrs content ->
      Tag_FigureCaption (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addFigureAttribute :: ChildHTML parent
                   -> Attribute Tags.Figure
                   -> ChildHTML parent
addFigureAttribute tag attr =
  case tag of
    Tag_Figure attrs content ->
      Tag_Figure (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addFooterAttribute :: ChildHTML parent
                   -> Attribute Tags.Footer
                   -> ChildHTML parent
addFooterAttribute tag attr =
  case tag of
    Tag_Footer attrs content ->
      Tag_Footer (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addFormAttribute :: ChildHTML parent
                 -> Attribute Tags.Form
                 -> ChildHTML parent
addFormAttribute tag attr =
  case tag of
    Tag_Form attrs content ->
      Tag_Form (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH1Attribute :: ChildHTML parent
               -> Attribute Tags.H1
               -> ChildHTML parent
addH1Attribute tag attr =
  case tag of
    Tag_H1 attrs content ->
      Tag_H1 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH2Attribute :: ChildHTML parent
               -> Attribute Tags.H2
               -> ChildHTML parent
addH2Attribute tag attr =
  case tag of
    Tag_H2 attrs content ->
      Tag_H2 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH3Attribute :: ChildHTML parent
               -> Attribute Tags.H3
               -> ChildHTML parent
addH3Attribute tag attr =
  case tag of
    Tag_H3 attrs content ->
      Tag_H3 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH4Attribute :: ChildHTML parent
               -> Attribute Tags.H4
               -> ChildHTML parent
addH4Attribute tag attr =
  case tag of
    Tag_H4 attrs content ->
      Tag_H4 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH5Attribute :: ChildHTML parent
               -> Attribute Tags.H5
               -> ChildHTML parent
addH5Attribute tag attr =
  case tag of
    Tag_H5 attrs content ->
      Tag_H5 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addH6Attribute :: ChildHTML parent
               -> Attribute Tags.H6
               -> ChildHTML parent
addH6Attribute tag attr =
  case tag of
    Tag_H6 attrs content ->
      Tag_H6 (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addHeadAttribute :: ChildHTML parent
                 -> Attribute Tags.Head
                 -> ChildHTML parent
addHeadAttribute tag attr =
  case tag of
    Tag_Head attrs content ->
      Tag_Head (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addHeaderAttribute :: ChildHTML parent
                   -> Attribute Tags.Header
                   -> ChildHTML parent
addHeaderAttribute tag attr =
  case tag of
    Tag_Header attrs content ->
      Tag_Header (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addHeadingGroupAttribute :: ChildHTML parent
                         -> Attribute Tags.HeadingGroup
                         -> ChildHTML parent
addHeadingGroupAttribute tag attr =
  case tag of
    Tag_HeadingGroup attrs content ->
      Tag_HeadingGroup (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addHorizontalRuleAttribute :: ChildHTML parent
                           -> Attribute Tags.HorizontalRule
                           -> ChildHTML parent
addHorizontalRuleAttribute tag attr =
  case tag of
    Tag_HorizontalRule attrs ->
      Tag_HorizontalRule (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addHtmlAttribute :: ChildHTML parent
                 -> Attribute Tags.Html
                 -> ChildHTML parent
addHtmlAttribute tag attr =
  case tag of
    Tag_Html attrs content ->
      Tag_Html (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addIdiomaticTextAttribute :: ChildHTML parent
                          -> Attribute Tags.IdiomaticText
                          -> ChildHTML parent
addIdiomaticTextAttribute tag attr =
  case tag of
    Tag_IdiomaticText attrs content ->
      Tag_IdiomaticText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addIFrameAttribute :: ChildHTML parent
                   -> Attribute Tags.IFrame
                   -> ChildHTML parent
addIFrameAttribute tag attr =
  case tag of
    Tag_IFrame attrs ->
      Tag_IFrame (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addImageAttribute :: ChildHTML parent
                  -> Attribute Tags.Image
                  -> ChildHTML parent
addImageAttribute tag attr =
  case tag of
    Tag_Image attrs ->
      Tag_Image (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addInputAttribute :: ChildHTML parent
                  -> Attribute Tags.Input
                  -> ChildHTML parent
addInputAttribute tag attr =
  case tag of
    Tag_Input attrs ->
      Tag_Input (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addInsertedTextAttribute :: ChildHTML parent
                         -> Attribute Tags.InsertedText
                         -> ChildHTML parent
addInsertedTextAttribute tag attr =
  case tag of
    Tag_InsertedText attrs content ->
      Tag_InsertedText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addKeyboardInputAttribute :: ChildHTML parent
                          -> Attribute Tags.KeyboardInput
                          -> ChildHTML parent
addKeyboardInputAttribute tag attr =
  case tag of
    Tag_KeyboardInput attrs content ->
      Tag_KeyboardInput (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addLabelAttribute :: ChildHTML parent
                  -> Attribute Tags.Label
                  -> ChildHTML parent
addLabelAttribute tag attr =
  case tag of
    Tag_Label attrs content ->
      Tag_Label (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addLegendAttribute :: ChildHTML parent
                   -> Attribute Tags.Legend
                   -> ChildHTML parent
addLegendAttribute tag attr =
  case tag of
    Tag_Legend attrs content ->
      Tag_Legend (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addListItemAttribute :: ChildHTML parent
                     -> Attribute Tags.ListItem
                     -> ChildHTML parent
addListItemAttribute tag attr =
  case tag of
    Tag_ListItem attrs content ->
      Tag_ListItem (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addLinkAttribute :: ChildHTML parent
                 -> Attribute Tags.Link
                 -> ChildHTML parent
addLinkAttribute tag attr =
  case tag of
    Tag_Link attrs ->
      Tag_Link (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addMainAttribute :: ChildHTML parent
                 -> Attribute Tags.Main
                 -> ChildHTML parent
addMainAttribute tag attr =
  case tag of
    Tag_Main attrs content ->
      Tag_Main (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addMapAttribute :: ChildHTML parent
                -> Attribute Tags.Map
                -> ChildHTML parent
addMapAttribute tag attr =
  case tag of
    Tag_Map attrs content ->
      Tag_Map (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addMarkAttribute :: ChildHTML parent
                 -> Attribute Tags.Mark
                 -> ChildHTML parent
addMarkAttribute tag attr =
  case tag of
    Tag_Mark attrs content ->
      Tag_Mark (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addMenuAttribute :: ChildHTML parent
                 -> Attribute Tags.Menu
                 -> ChildHTML parent
addMenuAttribute tag attr =
  case tag of
    Tag_Menu attrs content ->
      Tag_Menu (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addMetaAttribute :: ChildHTML parent
                 -> Attribute Tags.Meta
                 -> ChildHTML parent
addMetaAttribute tag attr =
  case tag of
    Tag_Meta attrs ->
      Tag_Meta (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addMeterAttribute :: ChildHTML parent
                  -> Attribute Tags.Meter
                  -> ChildHTML parent
addMeterAttribute tag attr =
  case tag of
    Tag_Meter attrs content ->
      Tag_Meter (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addNavAttribute :: ChildHTML parent
                -> Attribute Tags.Nav
                -> ChildHTML parent
addNavAttribute tag attr =
  case tag of
    Tag_Nav attrs content ->
      Tag_Nav (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addNoScriptAttribute :: ChildHTML parent
                     -> Attribute Tags.NoScript
                     -> ChildHTML parent
addNoScriptAttribute tag attr =
  case tag of
    Tag_NoScript attrs content ->
      Tag_NoScript (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addObjectAttribute :: ChildHTML parent
                   -> Attribute Tags.Object
                   -> ChildHTML parent
addObjectAttribute tag attr =
  case tag of
    Tag_Object attrs content ->
      Tag_Object (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addOrderedListAttribute :: ChildHTML parent
                        -> Attribute Tags.OrderedList
                        -> ChildHTML parent
addOrderedListAttribute tag attr =
  case tag of
    Tag_OrderedList attrs content ->
      Tag_OrderedList (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addOptionGroupAttribute :: ChildHTML parent
                        -> Attribute Tags.OptionGroup
                        -> ChildHTML parent
addOptionGroupAttribute tag attr =
  case tag of
    Tag_OptionGroup attrs content ->
      Tag_OptionGroup (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addOptionAttribute :: ChildHTML parent
                   -> Attribute Tags.Option
                   -> ChildHTML parent
addOptionAttribute tag attr =
  case tag of
    Tag_Option attrs content ->
      Tag_Option (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addOutputAttribute :: ChildHTML parent
                   -> Attribute Tags.Output
                   -> ChildHTML parent
addOutputAttribute tag attr =
  case tag of
    Tag_Output attrs content ->
      Tag_Output (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addParagraphAttribute :: ChildHTML parent
                      -> Attribute Tags.Paragraph
                      -> ChildHTML parent
addParagraphAttribute tag attr =
  case tag of
    Tag_Paragraph attrs content ->
      Tag_Paragraph (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addPictureAttribute :: ChildHTML parent
                    -> Attribute Tags.Picture
                    -> ChildHTML parent
addPictureAttribute tag attr =
  case tag of
    Tag_Picture attrs content ->
      Tag_Picture (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addPreformattedTextAttribute :: ChildHTML parent
                             -> Attribute Tags.PreformattedText
                             -> ChildHTML parent
addPreformattedTextAttribute tag attr =
  case tag of
    Tag_PreformattedText attrs content ->
      Tag_PreformattedText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addProgressAttribute :: ChildHTML parent
                     -> Attribute Tags.Progress
                     -> ChildHTML parent
addProgressAttribute tag attr =
  case tag of
    Tag_Progress attrs content ->
      Tag_Progress (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addQuotationAttribute :: ChildHTML parent
                      -> Attribute Tags.Quotation
                      -> ChildHTML parent
addQuotationAttribute tag attr =
  case tag of
    Tag_Quotation attrs content ->
      Tag_Quotation (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addRubyParenthesisAttribute :: ChildHTML parent
                            -> Attribute Tags.RubyParenthesis
                            -> ChildHTML parent
addRubyParenthesisAttribute tag attr =
  case tag of
    Tag_RubyParenthesis attrs content ->
      Tag_RubyParenthesis (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addRubyTextAttribute :: ChildHTML parent
                     -> Attribute Tags.RubyText
                     -> ChildHTML parent
addRubyTextAttribute tag attr =
  case tag of
    Tag_RubyText attrs content ->
      Tag_RubyText (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addRubyAttribute :: ChildHTML parent
                 -> Attribute Tags.Ruby
                 -> ChildHTML parent
addRubyAttribute tag attr =
  case tag of
    Tag_Ruby attrs content ->
      Tag_Ruby (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addStrikethroughAttribute :: ChildHTML parent
                          -> Attribute Tags.Strikethrough
                          -> ChildHTML parent
addStrikethroughAttribute tag attr =
  case tag of
    Tag_Strikethrough attrs content ->
      Tag_Strikethrough (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSampleAttribute :: ChildHTML parent
                   -> Attribute Tags.Sample
                   -> ChildHTML parent
addSampleAttribute tag attr =
  case tag of
    Tag_Sample attrs content ->
      Tag_Sample (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addScriptAttribute :: ChildHTML parent
                   -> Attribute Tags.Script
                   -> ChildHTML parent
addScriptAttribute tag attr =
  case tag of
    Tag_Script attrs content ->
      Tag_Script (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSearchAttribute :: ChildHTML parent
                   -> Attribute Tags.Search
                   -> ChildHTML parent
addSearchAttribute tag attr =
  case tag of
    Tag_Search attrs content ->
      Tag_Search (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSectionAttribute :: ChildHTML parent
                    -> Attribute Tags.Section
                    -> ChildHTML parent
addSectionAttribute tag attr =
  case tag of
    Tag_Section attrs content ->
      Tag_Section (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSelectAttribute :: ChildHTML parent
                   -> Attribute Tags.Select
                   -> ChildHTML parent
addSelectAttribute tag attr =
  case tag of
    Tag_Select attrs content ->
      Tag_Select (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSlotAttribute :: ChildHTML parent
                 -> Attribute Tags.Slot
                 -> ChildHTML parent
addSlotAttribute tag attr =
  case tag of
    Tag_Slot attrs content ->
      Tag_Slot (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSideCommentAttribute :: ChildHTML parent
                        -> Attribute Tags.SideComment
                        -> ChildHTML parent
addSideCommentAttribute tag attr =
  case tag of
    Tag_SideComment attrs content ->
      Tag_SideComment (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSourceAttribute :: ChildHTML parent
                   -> Attribute Tags.Source
                   -> ChildHTML parent
addSourceAttribute tag attr =
  case tag of
    Tag_Source attrs ->
      Tag_Source (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addSpanAttribute :: ChildHTML parent
                 -> Attribute Tags.Span
                 -> ChildHTML parent
addSpanAttribute tag attr =
  case tag of
    Tag_Span attrs content ->
      Tag_Span (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addStrongAttribute :: ChildHTML parent
                   -> Attribute Tags.Strong
                   -> ChildHTML parent
addStrongAttribute tag attr =
  case tag of
    Tag_Strong attrs content ->
      Tag_Strong (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addStyleAttribute :: ChildHTML parent
                  -> Attribute Tags.Style
                  -> ChildHTML parent
addStyleAttribute tag attr =
  case tag of
    Tag_Style attrs content ->
      Tag_Style (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSubscriptAttribute :: ChildHTML parent
                      -> Attribute Tags.Subscript
                      -> ChildHTML parent
addSubscriptAttribute tag attr =
  case tag of
    Tag_Subscript attrs content ->
      Tag_Subscript (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSummaryAttribute :: ChildHTML parent
                    -> Attribute Tags.Summary
                    -> ChildHTML parent
addSummaryAttribute tag attr =
  case tag of
    Tag_Summary attrs content ->
      Tag_Summary (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addSuperscriptAttribute :: ChildHTML parent
                        -> Attribute Tags.Superscript
                        -> ChildHTML parent
addSuperscriptAttribute tag attr =
  case tag of
    Tag_Superscript attrs content ->
      Tag_Superscript (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableAttribute :: ChildHTML parent
                  -> Attribute Tags.Table
                  -> ChildHTML parent
addTableAttribute tag attr =
  case tag of
    Tag_Table attrs content ->
      Tag_Table (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableBodyAttribute :: ChildHTML parent
                      -> Attribute Tags.TableBody
                      -> ChildHTML parent
addTableBodyAttribute tag attr =
  case tag of
    Tag_TableBody attrs content ->
      Tag_TableBody (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableDataCellAttribute :: ChildHTML parent
                          -> Attribute Tags.TableDataCell
                          -> ChildHTML parent
addTableDataCellAttribute tag attr =
  case tag of
    Tag_TableDataCell attrs content ->
      Tag_TableDataCell (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addContentTemplateAttribute :: ChildHTML parent
                            -> Attribute Tags.ContentTemplate
                            -> ChildHTML parent
addContentTemplateAttribute tag attr =
  case tag of
    Tag_ContentTemplate attrs content ->
      Tag_ContentTemplate (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTextAreaAttribute :: ChildHTML parent
                     -> Attribute Tags.TextArea
                     -> ChildHTML parent
addTextAreaAttribute tag attr =
  case tag of
    Tag_TextArea attrs content ->
      Tag_TextArea (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableFootAttribute :: ChildHTML parent
                      -> Attribute Tags.TableFoot
                      -> ChildHTML parent
addTableFootAttribute tag attr =
  case tag of
    Tag_TableFoot attrs content ->
      Tag_TableFoot (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableHeaderAttribute :: ChildHTML parent
                        -> Attribute Tags.TableHeader
                        -> ChildHTML parent
addTableHeaderAttribute tag attr =
  case tag of
    Tag_TableHeader attrs content ->
      Tag_TableHeader (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableHeadAttribute :: ChildHTML parent
                      -> Attribute Tags.TableHead
                      -> ChildHTML parent
addTableHeadAttribute tag attr =
  case tag of
    Tag_TableHead attrs content ->
      Tag_TableHead (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTimeAttribute :: ChildHTML parent
                 -> Attribute Tags.Time
                 -> ChildHTML parent
addTimeAttribute tag attr =
  case tag of
    Tag_Time attrs content ->
      Tag_Time (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTitleAttribute :: ChildHTML parent
                  -> Attribute Tags.Title
                  -> ChildHTML parent
addTitleAttribute tag attr =
  case tag of
    Tag_Title attrs content ->
      Tag_Title (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTableRowAttribute :: ChildHTML parent
                     -> Attribute Tags.TableRow
                     -> ChildHTML parent
addTableRowAttribute tag attr =
  case tag of
    Tag_TableRow attrs content ->
      Tag_TableRow (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addTrackAttribute :: ChildHTML parent
                  -> Attribute Tags.Track
                  -> ChildHTML parent
addTrackAttribute tag attr =
  case tag of
    Tag_Track attrs ->
      Tag_Track (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag

addUnderlineAttribute :: ChildHTML parent
                      -> Attribute Tags.Underline
                      -> ChildHTML parent
addUnderlineAttribute tag attr =
  case tag of
    Tag_Underline attrs content ->
      Tag_Underline (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addUnorderedListAttribute :: ChildHTML parent
                          -> Attribute Tags.UnorderedList
                          -> ChildHTML parent
addUnorderedListAttribute tag attr =
  case tag of
    Tag_UnorderedList attrs content ->
      Tag_UnorderedList (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addVariableAttribute :: ChildHTML parent
                     -> Attribute Tags.Variable
                     -> ChildHTML parent
addVariableAttribute tag attr =
  case tag of
    Tag_Variable attrs content ->
      Tag_Variable (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addVideoAttribute :: ChildHTML parent
                  -> Attribute Tags.Video
                  -> ChildHTML parent
addVideoAttribute tag attr =
  case tag of
    Tag_Video attrs content ->
      Tag_Video (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag

addWordBreakOpportunityAttribute :: ChildHTML parent
                                 -> Attribute Tags.WordBreakOpportunity
                                 -> ChildHTML parent
addWordBreakOpportunityAttribute tag attr =
  case tag of
    Tag_WordBreakOpportunity attrs ->
      Tag_WordBreakOpportunity (Map.insert (attributeText attr) attr attrs)

    _ ->
      tag
