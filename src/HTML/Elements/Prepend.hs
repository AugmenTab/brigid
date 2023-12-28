module HTML.Elements.Prepend
  ( prependAnchor
  , prependAbbreviation
  , prependContactAddress
  , prependArticle
  , prependAside
  , prependAudio
  , prependBringAttentionTo
  , prependBidirectionalIsolation
  , prependBidirectionalOverride
  , prependBlockquote
  , prependBody
  , prependButton
  , prependCanvas
  , prependTableCaption
  , prependCitation
  , prependCode
  , prependTableColumnGroup
  , prependData
  , prependDataList
  , prependDescriptionDetails
  , prependDeletedText
  , prependDetails
  , prependDefinition
  , prependDialog
  , prependDivision
  , prependDescriptionList
  , prependDescriptionTerm
  , prependEmphasis
  , prependFieldset
  , prependFigureCaption
  , prependFigure
  , prependFooter
  , prependForm
  , prependH1
  , prependH2
  , prependH3
  , prependH4
  , prependH5
  , prependH6
  , prependHead
  , prependHeader
  , prependHeadingGroup
  , prependHtml
  , prependIdiomaticText
  , prependInsertedText
  , prependKeyboardInput
  , prependLabel
  , prependLegend
  , prependListItem
  , prependMain
  , prependMap
  , prependMark
  , prependMenu
  , prependMeter
  , prependNav
  , prependNoScript
  , prependObject
  , prependOrderedList
  , prependOptionGroup
  , prependOption
  , prependOutput
  , prependParagraph
  , prependPicture
  , prependPreformattedText
  , prependProgress
  , prependQuotation
  , prependRubyParenthesis
  , prependRubyText
  , prependRuby
  , prependStrikethrough
  , prependSample
  , prependScript
  , prependSearch
  , prependSection
  , prependSelect
  , prependSlot
  , prependSideComment
  , prependSpan
  , prependStrong
  , prependStyle
  , prependSubscript
  , prependSummary
  , prependSuperscript
  , prependTable
  , prependTableBody
  , prependTableDataCell
  , prependContentTemplate
  , prependTextArea
  , prependTableFoot
  , prependTableHeader
  , prependTableHead
  , prependTime
  , prependTitle
  , prependTableRow
  , prependUnderline
  , prependUnorderedList
  , prependVariable
  , prependVideo
  ) where

import HTML.Elements.Children (ValidChild)
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Elements.Tags qualified as Tags

prependAnchor :: ValidChild Tags.Anchor parent
              => ChildHTML Tags.Anchor
              -> ChildHTML parent
              -> ChildHTML parent
prependAnchor child tag =
  case tag of
    Tag_Anchor attrs content ->
      Tag_Anchor attrs $ child : content

    _ ->
      tag

prependAbbreviation :: ValidChild Tags.Abbreviation parent
                    => ChildHTML Tags.Abbreviation
                    -> ChildHTML parent
                    -> ChildHTML parent
prependAbbreviation child tag =
  case tag of
    Tag_Abbreviation attrs content ->
      Tag_Abbreviation attrs $ child : content

    _ ->
      tag

prependContactAddress :: ValidChild Tags.ContactAddress parent
                      => ChildHTML Tags.ContactAddress
                      -> ChildHTML parent
                      -> ChildHTML parent
prependContactAddress child tag =
  case tag of
    Tag_ContactAddress attrs content ->
      Tag_ContactAddress attrs $ child : content

    _ ->
      tag

prependArticle :: ValidChild Tags.Article parent
               => ChildHTML Tags.Article
               -> ChildHTML parent
               -> ChildHTML parent
prependArticle child tag =
  case tag of
    Tag_Article attrs content ->
      Tag_Article attrs $ child : content

    _ ->
      tag

prependAside :: ValidChild Tags.Aside parent
             => ChildHTML Tags.Aside
             -> ChildHTML parent
             -> ChildHTML parent
prependAside child tag =
  case tag of
    Tag_Aside attrs content ->
      Tag_Aside attrs $ child : content

    _ ->
      tag

prependAudio :: ValidChild Tags.Audio parent
             => ChildHTML Tags.Audio
             -> ChildHTML parent
             -> ChildHTML parent
prependAudio child tag =
  case tag of
    Tag_Audio attrs content ->
      Tag_Audio attrs $ child : content

    _ ->
      tag

prependBringAttentionTo :: ValidChild Tags.BringAttentionTo parent
                        => ChildHTML Tags.BringAttentionTo
                        -> ChildHTML parent
                        -> ChildHTML parent
prependBringAttentionTo child tag =
  case tag of
    Tag_BringAttentionTo attrs content ->
      Tag_BringAttentionTo attrs $ child : content

    _ ->
      tag

prependBidirectionalIsolation :: ValidChild Tags.BidirectionalIsolation parent
                              => ChildHTML Tags.BidirectionalIsolation
                              -> ChildHTML parent
                              -> ChildHTML parent
prependBidirectionalIsolation child tag =
  case tag of
    Tag_BidirectionalIsolation attrs content ->
      Tag_BidirectionalIsolation attrs $ child : content

    _ ->
      tag

prependBidirectionalOverride :: ValidChild Tags.BidirectionalOverride parent
                             => ChildHTML Tags.BidirectionalOverride
                             -> ChildHTML parent
                             -> ChildHTML parent
prependBidirectionalOverride child tag =
  case tag of
    Tag_BidirectionalOverride attrs content ->
      Tag_BidirectionalOverride attrs $ child : content

    _ ->
      tag

prependBlockquote :: ValidChild Tags.Blockquote parent
                  => ChildHTML Tags.Blockquote
                  -> ChildHTML parent
                  -> ChildHTML parent
prependBlockquote child tag =
  case tag of
    Tag_Blockquote attrs content ->
      Tag_Blockquote attrs $ child : content

    _ ->
      tag

prependBody :: ValidChild Tags.Body parent
            => ChildHTML Tags.Body
            -> ChildHTML parent
            -> ChildHTML parent
prependBody child tag =
  case tag of
    Tag_Body attrs content ->
      Tag_Body attrs $ child : content

    _ ->
      tag

prependButton :: ValidChild Tags.Button parent
              => ChildHTML Tags.Button
              -> ChildHTML parent
              -> ChildHTML parent
prependButton child tag =
  case tag of
    Tag_Button attrs content ->
      Tag_Button attrs $ child : content

    _ ->
      tag

prependCanvas :: ValidChild Tags.Canvas parent
              => ChildHTML Tags.Canvas
              -> ChildHTML parent
              -> ChildHTML parent
prependCanvas child tag =
  case tag of
    Tag_Canvas attrs content ->
      Tag_Canvas attrs $ child : content

    _ ->
      tag

prependTableCaption :: ValidChild Tags.TableCaption parent
                    => ChildHTML Tags.TableCaption
                    -> ChildHTML parent
                    -> ChildHTML parent
prependTableCaption child tag =
  case tag of
    Tag_TableCaption attrs content ->
      Tag_TableCaption attrs $ child : content

    _ ->
      tag

prependCitation :: ValidChild Tags.Citation parent
                => ChildHTML Tags.Citation
                -> ChildHTML parent
                -> ChildHTML parent
prependCitation child tag =
  case tag of
    Tag_Citation attrs content ->
      Tag_Citation attrs $ child : content

    _ ->
      tag

prependCode :: ValidChild Tags.Code parent
            => ChildHTML Tags.Code
            -> ChildHTML parent
            -> ChildHTML parent
prependCode child tag =
  case tag of
    Tag_Code attrs content ->
      Tag_Code attrs $ child : content

    _ ->
      tag

prependTableColumnGroup :: ValidChild Tags.TableColumnGroup parent
                        => ChildHTML Tags.TableColumnGroup
                        -> ChildHTML parent
                        -> ChildHTML parent
prependTableColumnGroup child tag =
  case tag of
    Tag_TableColumnGroup attrs content ->
      Tag_TableColumnGroup attrs $ child : content

    _ ->
      tag

prependData :: ValidChild Tags.Data parent
            => ChildHTML Tags.Data
            -> ChildHTML parent
            -> ChildHTML parent
prependData child tag =
  case tag of
    Tag_Data attrs content ->
      Tag_Data attrs $ child : content

    _ ->
      tag

prependDataList :: ValidChild Tags.DataList parent
                => ChildHTML Tags.DataList
                -> ChildHTML parent
                -> ChildHTML parent
prependDataList child tag =
  case tag of
    Tag_DataList attrs content ->
      Tag_DataList attrs $ child : content

    _ ->
      tag

prependDescriptionDetails :: ValidChild Tags.DescriptionDetails parent
                          => ChildHTML Tags.DescriptionDetails
                          -> ChildHTML parent
                          -> ChildHTML parent
prependDescriptionDetails child tag =
  case tag of
    Tag_DescriptionDetails attrs content ->
      Tag_DescriptionDetails attrs $ child : content

    _ ->
      tag

prependDeletedText :: ChildHTML parent
                   -> ChildHTML parent
                   -> ChildHTML parent
prependDeletedText child tag =
  case tag of
    Tag_DeletedText attrs content ->
      Tag_DeletedText attrs $ child : content

    _ ->
      tag

prependDetails :: ValidChild Tags.Details parent
               => ChildHTML Tags.Details
               -> ChildHTML parent
               -> ChildHTML parent
prependDetails child tag =
  case tag of
    Tag_Details attrs content ->
      Tag_Details attrs $ child : content

    _ ->
      tag

prependDefinition :: ValidChild Tags.Definition parent
                  => ChildHTML Tags.Definition
                  -> ChildHTML parent
                  -> ChildHTML parent
prependDefinition child tag =
  case tag of
    Tag_Definition attrs content ->
      Tag_Definition attrs $ child : content

    _ ->
      tag

prependDialog :: ValidChild Tags.Dialog parent
              => ChildHTML Tags.Dialog
              -> ChildHTML parent
              -> ChildHTML parent
prependDialog child tag =
  case tag of
    Tag_Dialog attrs content ->
      Tag_Dialog attrs $ child : content

    _ ->
      tag

prependDivision :: ValidChild Tags.Division parent
                => ChildHTML Tags.Division
                -> ChildHTML parent
                -> ChildHTML parent
prependDivision child tag =
  case tag of
    Tag_Division attrs content ->
      Tag_Division attrs $ child : content

    _ ->
      tag

prependDescriptionList :: ValidChild Tags.DescriptionList parent
                       => ChildHTML Tags.DescriptionList
                       -> ChildHTML parent
                       -> ChildHTML parent
prependDescriptionList child tag =
  case tag of
    Tag_DescriptionList attrs content ->
      Tag_DescriptionList attrs $ child : content

    _ ->
      tag

prependDescriptionTerm :: ValidChild Tags.DescriptionTerm parent
                       => ChildHTML Tags.DescriptionTerm
                       -> ChildHTML parent
                       -> ChildHTML parent
prependDescriptionTerm child tag =
  case tag of
    Tag_DescriptionTerm attrs content ->
      Tag_DescriptionTerm attrs $ child : content

    _ ->
      tag

prependEmphasis :: ValidChild Tags.Emphasis parent
                => ChildHTML Tags.Emphasis
                -> ChildHTML parent
                -> ChildHTML parent
prependEmphasis child tag =
  case tag of
    Tag_Emphasis attrs content ->
      Tag_Emphasis attrs $ child : content

    _ ->
      tag

prependFieldset :: ValidChild Tags.Fieldset parent
                => ChildHTML Tags.Fieldset
                -> ChildHTML parent
                -> ChildHTML parent
prependFieldset child tag =
  case tag of
    Tag_Fieldset attrs content ->
      Tag_Fieldset attrs $ child : content

    _ ->
      tag

prependFigureCaption :: ValidChild Tags.FigureCaption parent
                     => ChildHTML Tags.FigureCaption
                     -> ChildHTML parent
                     -> ChildHTML parent
prependFigureCaption child tag =
  case tag of
    Tag_FigureCaption attrs content ->
      Tag_FigureCaption attrs $ child : content

    _ ->
      tag

prependFigure :: ValidChild Tags.Figure parent
              => ChildHTML Tags.Figure
              -> ChildHTML parent
              -> ChildHTML parent
prependFigure child tag =
  case tag of
    Tag_Figure attrs content ->
      Tag_Figure attrs $ child : content

    _ ->
      tag

prependFooter :: ValidChild Tags.Footer parent
              => ChildHTML Tags.Footer
              -> ChildHTML parent
              -> ChildHTML parent
prependFooter child tag =
  case tag of
    Tag_Footer attrs content ->
      Tag_Footer attrs $ child : content

    _ ->
      tag

prependForm :: ValidChild Tags.Form parent
            => ChildHTML Tags.Form
            -> ChildHTML parent
            -> ChildHTML parent
prependForm child tag =
  case tag of
    Tag_Form attrs content ->
      Tag_Form attrs $ child : content

    _ ->
      tag

prependH1 :: ValidChild Tags.H1 parent
          => ChildHTML Tags.H1
          -> ChildHTML parent
          -> ChildHTML parent
prependH1 child tag =
  case tag of
    Tag_H1 attrs content ->
      Tag_H1 attrs $ child : content

    _ ->
      tag

prependH2 :: ValidChild Tags.H2 parent
          => ChildHTML Tags.H2
          -> ChildHTML parent
          -> ChildHTML parent
prependH2 child tag =
  case tag of
    Tag_H2 attrs content ->
      Tag_H2 attrs $ child : content

    _ ->
      tag

prependH3 :: ValidChild Tags.H3 parent
          => ChildHTML Tags.H3
          -> ChildHTML parent
          -> ChildHTML parent
prependH3 child tag =
  case tag of
    Tag_H3 attrs content ->
      Tag_H3 attrs $ child : content

    _ ->
      tag

prependH4 :: ValidChild Tags.H4 parent
          => ChildHTML Tags.H4
          -> ChildHTML parent
          -> ChildHTML parent
prependH4 child tag =
  case tag of
    Tag_H4 attrs content ->
      Tag_H4 attrs $ child : content

    _ ->
      tag

prependH5 :: ValidChild Tags.H5 parent
          => ChildHTML Tags.H5
          -> ChildHTML parent
          -> ChildHTML parent
prependH5 child tag =
  case tag of
    Tag_H5 attrs content ->
      Tag_H5 attrs $ child : content

    _ ->
      tag

prependH6 :: ValidChild Tags.H6 parent
          => ChildHTML Tags.H6
          -> ChildHTML parent
          -> ChildHTML parent
prependH6 child tag =
  case tag of
    Tag_H6 attrs content ->
      Tag_H6 attrs $ child : content

    _ ->
      tag

prependHead :: ValidChild Tags.Head parent
            => ChildHTML Tags.Head
            -> ChildHTML parent
            -> ChildHTML parent
prependHead child tag =
  case tag of
    Tag_Head attrs content ->
      Tag_Head attrs $ child : content

    _ ->
      tag

prependHeader :: ValidChild Tags.Header parent
              => ChildHTML Tags.Header
              -> ChildHTML parent
              -> ChildHTML parent
prependHeader child tag =
  case tag of
    Tag_Header attrs content ->
      Tag_Header attrs $ child : content

    _ ->
      tag

prependHeadingGroup :: ValidChild Tags.HeadingGroup parent
                    => ChildHTML Tags.HeadingGroup
                    -> ChildHTML parent
                    -> ChildHTML parent
prependHeadingGroup child tag =
  case tag of
    Tag_HeadingGroup attrs content ->
      Tag_HeadingGroup attrs $ child : content

    _ ->
      tag

prependHtml :: ValidChild Tags.Html parent
            => ChildHTML Tags.Html
            -> ChildHTML parent
            -> ChildHTML parent
prependHtml child tag =
  case tag of
    Tag_Html attrs content ->
      Tag_Html attrs $ child : content

    _ ->
      tag

prependIdiomaticText :: ValidChild Tags.IdiomaticText parent
                     => ChildHTML Tags.IdiomaticText
                     -> ChildHTML parent
                     -> ChildHTML parent
prependIdiomaticText child tag =
  case tag of
    Tag_IdiomaticText attrs content ->
      Tag_IdiomaticText attrs $ child : content

    _ ->
      tag

prependInsertedText :: ChildHTML parent
                    -> ChildHTML parent
                    -> ChildHTML parent
prependInsertedText child tag =
  case tag of
    Tag_InsertedText attrs content ->
      Tag_InsertedText attrs $ child : content

    _ ->
      tag

prependKeyboardInput :: ValidChild Tags.KeyboardInput parent
                     => ChildHTML Tags.KeyboardInput
                     -> ChildHTML parent
                     -> ChildHTML parent
prependKeyboardInput child tag =
  case tag of
    Tag_KeyboardInput attrs content ->
      Tag_KeyboardInput attrs $ child : content

    _ ->
      tag

prependLabel :: ValidChild Tags.Label parent
             => ChildHTML Tags.Label
             -> ChildHTML parent
             -> ChildHTML parent
prependLabel child tag =
  case tag of
    Tag_Label attrs content ->
      Tag_Label attrs $ child : content

    _ ->
      tag

prependLegend :: ValidChild Tags.Legend parent
              => ChildHTML Tags.Legend
              -> ChildHTML parent
              -> ChildHTML parent
prependLegend child tag =
  case tag of
    Tag_Legend attrs content ->
      Tag_Legend attrs $ child : content

    _ ->
      tag

prependListItem :: ValidChild Tags.ListItem parent
                => ChildHTML Tags.ListItem
                -> ChildHTML parent
                -> ChildHTML parent
prependListItem child tag =
  case tag of
    Tag_ListItem attrs content ->
      Tag_ListItem attrs $ child : content

    _ ->
      tag

prependMain :: ValidChild Tags.Main parent
            => ChildHTML Tags.Main
            -> ChildHTML parent
            -> ChildHTML parent
prependMain child tag =
  case tag of
    Tag_Main attrs content ->
      Tag_Main attrs $ child : content

    _ ->
      tag

prependMap :: ValidChild Tags.Map parent
           => ChildHTML Tags.Map
           -> ChildHTML parent
           -> ChildHTML parent
prependMap child tag =
  case tag of
    Tag_Map attrs content ->
      Tag_Map attrs $ child : content

    _ ->
      tag

prependMark :: ValidChild Tags.Mark parent
            => ChildHTML Tags.Mark
            -> ChildHTML parent
            -> ChildHTML parent
prependMark child tag =
  case tag of
    Tag_Mark attrs content ->
      Tag_Mark attrs $ child : content

    _ ->
      tag

prependMenu :: ValidChild Tags.Menu parent
            => ChildHTML Tags.Menu
            -> ChildHTML parent
            -> ChildHTML parent
prependMenu child tag =
  case tag of
    Tag_Menu attrs content ->
      Tag_Menu attrs $ child : content

    _ ->
      tag

prependMeter :: ValidChild Tags.Meter parent
             => ChildHTML Tags.Meter
             -> ChildHTML parent
             -> ChildHTML parent
prependMeter child tag =
  case tag of
    Tag_Meter attrs content ->
      Tag_Meter attrs $ child : content

    _ ->
      tag

prependNav :: ValidChild Tags.Nav parent
           => ChildHTML Tags.Nav
           -> ChildHTML parent
           -> ChildHTML parent
prependNav child tag =
  case tag of
    Tag_Nav attrs content ->
      Tag_Nav attrs $ child : content

    _ ->
      tag

prependNoScript :: ValidChild Tags.NoScript parent
                => ChildHTML Tags.NoScript
                -> ChildHTML parent
                -> ChildHTML parent
prependNoScript child tag =
  case tag of
    Tag_NoScript attrs content ->
      Tag_NoScript attrs $ child : content

    _ ->
      tag

prependObject :: ValidChild Tags.Object parent
              => ChildHTML Tags.Object
              -> ChildHTML parent
              -> ChildHTML parent
prependObject child tag =
  case tag of
    Tag_Object attrs content ->
      Tag_Object attrs $ child : content

    _ ->
      tag

prependOrderedList :: ValidChild Tags.OrderedList parent
                   => ChildHTML Tags.OrderedList
                   -> ChildHTML parent
                   -> ChildHTML parent
prependOrderedList child tag =
  case tag of
    Tag_OrderedList attrs content ->
      Tag_OrderedList attrs $ child : content

    _ ->
      tag

prependOptionGroup :: ValidChild Tags.OptionGroup parent
                   => ChildHTML Tags.OptionGroup
                   -> ChildHTML parent
                   -> ChildHTML parent
prependOptionGroup child tag =
  case tag of
    Tag_OptionGroup attrs content ->
      Tag_OptionGroup attrs $ child : content

    _ ->
      tag

prependOption :: ValidChild Tags.Option parent
              => ChildHTML Tags.Option
              -> ChildHTML parent
              -> ChildHTML parent
prependOption child tag =
  case tag of
    Tag_Option attrs content ->
      Tag_Option attrs $ child : content

    _ ->
      tag

prependOutput :: ValidChild Tags.Output parent
              => ChildHTML Tags.Output
              -> ChildHTML parent
              -> ChildHTML parent
prependOutput child tag =
  case tag of
    Tag_Output attrs content ->
      Tag_Output attrs $ child : content

    _ ->
      tag

prependParagraph :: ValidChild Tags.Paragraph parent
                 => ChildHTML Tags.Paragraph
                 -> ChildHTML parent
                 -> ChildHTML parent
prependParagraph child tag =
  case tag of
    Tag_Paragraph attrs content ->
      Tag_Paragraph attrs $ child : content

    _ ->
      tag

prependPicture :: ValidChild Tags.Picture parent
               => ChildHTML Tags.Picture
               -> ChildHTML parent
               -> ChildHTML parent
prependPicture child tag =
  case tag of
    Tag_Picture attrs content ->
      Tag_Picture attrs $ child : content

    _ ->
      tag

prependPreformattedText :: ValidChild Tags.PreformattedText parent
                        => ChildHTML Tags.PreformattedText
                        -> ChildHTML parent
                        -> ChildHTML parent
prependPreformattedText child tag =
  case tag of
    Tag_PreformattedText attrs content ->
      Tag_PreformattedText attrs $ child : content

    _ ->
      tag

prependProgress :: ValidChild Tags.Progress parent
                => ChildHTML Tags.Progress
                -> ChildHTML parent
                -> ChildHTML parent
prependProgress child tag =
  case tag of
    Tag_Progress attrs content ->
      Tag_Progress attrs $ child : content

    _ ->
      tag

prependQuotation :: ValidChild Tags.Quotation parent
                 => ChildHTML Tags.Quotation
                 -> ChildHTML parent
                 -> ChildHTML parent
prependQuotation child tag =
  case tag of
    Tag_Quotation attrs content ->
      Tag_Quotation attrs $ child : content

    _ ->
      tag

prependRubyParenthesis :: ValidChild Tags.RubyParenthesis parent
                       => ChildHTML Tags.RubyParenthesis
                       -> ChildHTML parent
                       -> ChildHTML parent
prependRubyParenthesis child tag =
  case tag of
    Tag_RubyParenthesis attrs content ->
      Tag_RubyParenthesis attrs $ child : content

    _ ->
      tag

prependRubyText :: ValidChild Tags.RubyText parent
                => ChildHTML Tags.RubyText
                -> ChildHTML parent
                -> ChildHTML parent
prependRubyText child tag =
  case tag of
    Tag_RubyText attrs content ->
      Tag_RubyText attrs $ child : content

    _ ->
      tag

prependRuby :: ValidChild Tags.Ruby parent
            => ChildHTML Tags.Ruby
            -> ChildHTML parent
            -> ChildHTML parent
prependRuby child tag =
  case tag of
    Tag_Ruby attrs content ->
      Tag_Ruby attrs $ child : content

    _ ->
      tag

prependStrikethrough :: ValidChild Tags.Strikethrough parent
                     => ChildHTML Tags.Strikethrough
                     -> ChildHTML parent
                     -> ChildHTML parent
prependStrikethrough child tag =
  case tag of
    Tag_Strikethrough attrs content ->
      Tag_Strikethrough attrs $ child : content

    _ ->
      tag

prependSample :: ValidChild Tags.Sample parent
              => ChildHTML Tags.Sample
              -> ChildHTML parent
              -> ChildHTML parent
prependSample child tag =
  case tag of
    Tag_Sample attrs content ->
      Tag_Sample attrs $ child : content

    _ ->
      tag

prependScript :: ValidChild Tags.Script parent
              => ChildHTML Tags.Script
              -> ChildHTML parent
              -> ChildHTML parent
prependScript child tag =
  case tag of
    Tag_Script attrs content ->
      Tag_Script attrs $ child : content

    _ ->
      tag

prependSearch :: ValidChild Tags.Search parent
              => ChildHTML Tags.Search
              -> ChildHTML parent
              -> ChildHTML parent
prependSearch child tag =
  case tag of
    Tag_Search attrs content ->
      Tag_Search attrs $ child : content

    _ ->
      tag

prependSection :: ValidChild Tags.Section parent
               => ChildHTML Tags.Section
               -> ChildHTML parent
               -> ChildHTML parent
prependSection child tag =
  case tag of
    Tag_Section attrs content ->
      Tag_Section attrs $ child : content

    _ ->
      tag

prependSelect :: ValidChild Tags.Select parent
              => ChildHTML Tags.Select
              -> ChildHTML parent
              -> ChildHTML parent
prependSelect child tag =
  case tag of
    Tag_Select attrs content ->
      Tag_Select attrs $ child : content

    _ ->
      tag

prependSlot :: ChildHTML parent
            -> ChildHTML parent
            -> ChildHTML parent
prependSlot child tag =
  case tag of
    Tag_Slot attrs content ->
      Tag_Slot attrs $ child : content

    _ ->
      tag

prependSideComment :: ValidChild Tags.SideComment parent
                   => ChildHTML Tags.SideComment
                   -> ChildHTML parent
                   -> ChildHTML parent
prependSideComment child tag =
  case tag of
    Tag_SideComment attrs content ->
      Tag_SideComment attrs $ child : content

    _ ->
      tag

prependSpan :: ValidChild Tags.Span parent
            => ChildHTML Tags.Span
            -> ChildHTML parent
            -> ChildHTML parent
prependSpan child tag =
  case tag of
    Tag_Span attrs content ->
      Tag_Span attrs $ child : content

    _ ->
      tag

prependStrong :: ValidChild Tags.Strong parent
              => ChildHTML Tags.Strong
              -> ChildHTML parent
              -> ChildHTML parent
prependStrong child tag =
  case tag of
    Tag_Strong attrs content ->
      Tag_Strong attrs $ child : content

    _ ->
      tag

prependStyle :: ValidChild Tags.Style parent
             => ChildHTML Tags.Style
             -> ChildHTML parent
             -> ChildHTML parent
prependStyle child tag =
  case tag of
    Tag_Style attrs content ->
      Tag_Style attrs $ child : content

    _ ->
      tag

prependSubscript :: ValidChild Tags.Subscript parent
                 => ChildHTML Tags.Subscript
                 -> ChildHTML parent
                 -> ChildHTML parent
prependSubscript child tag =
  case tag of
    Tag_Subscript attrs content ->
      Tag_Subscript attrs $ child : content

    _ ->
      tag

prependSummary :: ValidChild Tags.Summary parent
               => ChildHTML Tags.Summary
               -> ChildHTML parent
               -> ChildHTML parent
prependSummary child tag =
  case tag of
    Tag_Summary attrs content ->
      Tag_Summary attrs $ child : content

    _ ->
      tag

prependSuperscript :: ValidChild Tags.Superscript parent
                   => ChildHTML Tags.Superscript
                   -> ChildHTML parent
                   -> ChildHTML parent
prependSuperscript child tag =
  case tag of
    Tag_Superscript attrs content ->
      Tag_Superscript attrs $ child : content

    _ ->
      tag

prependTable :: ValidChild Tags.Table parent
             => ChildHTML Tags.Table
             -> ChildHTML parent
             -> ChildHTML parent
prependTable child tag =
  case tag of
    Tag_Table attrs content ->
      Tag_Table attrs $ child : content

    _ ->
      tag

prependTableBody :: ValidChild Tags.TableBody parent
                 => ChildHTML Tags.TableBody
                 -> ChildHTML parent
                 -> ChildHTML parent
prependTableBody child tag =
  case tag of
    Tag_TableBody attrs content ->
      Tag_TableBody attrs $ child : content

    _ ->
      tag

prependTableDataCell :: ValidChild Tags.TableDataCell parent
                     => ChildHTML Tags.TableDataCell
                     -> ChildHTML parent
                     -> ChildHTML parent
prependTableDataCell child tag =
  case tag of
    Tag_TableDataCell attrs content ->
      Tag_TableDataCell attrs $ child : content

    _ ->
      tag

prependContentTemplate :: ValidChild Tags.ContentTemplate parent
                       => ChildHTML Tags.ContentTemplate
                       -> ChildHTML parent
                       -> ChildHTML parent
prependContentTemplate child tag =
  case tag of
    Tag_ContentTemplate attrs content ->
      Tag_ContentTemplate attrs $ child : content

    _ ->
      tag

prependTextArea :: ValidChild Tags.TextArea parent
                => ChildHTML Tags.TextArea
                -> ChildHTML parent
                -> ChildHTML parent
prependTextArea child tag =
  case tag of
    Tag_TextArea attrs content ->
      Tag_TextArea attrs $ child : content

    _ ->
      tag

prependTableFoot :: ValidChild Tags.TableFoot parent
                 => ChildHTML Tags.TableFoot
                 -> ChildHTML parent
                 -> ChildHTML parent
prependTableFoot child tag =
  case tag of
    Tag_TableFoot attrs content ->
      Tag_TableFoot attrs $ child : content

    _ ->
      tag

prependTableHeader :: ValidChild Tags.TableHeader parent
                   => ChildHTML Tags.TableHeader
                   -> ChildHTML parent
                   -> ChildHTML parent
prependTableHeader child tag =
  case tag of
    Tag_TableHeader attrs content ->
      Tag_TableHeader attrs $ child : content

    _ ->
      tag

prependTableHead :: ValidChild Tags.TableHead parent
                 => ChildHTML Tags.TableHead
                 -> ChildHTML parent
                 -> ChildHTML parent
prependTableHead child tag =
  case tag of
    Tag_TableHead attrs content ->
      Tag_TableHead attrs $ child : content

    _ ->
      tag

prependTime :: ValidChild Tags.Time parent
            => ChildHTML Tags.Time
            -> ChildHTML parent
            -> ChildHTML parent
prependTime child tag =
  case tag of
    Tag_Time attrs content ->
      Tag_Time attrs $ child : content

    _ ->
      tag

prependTitle :: ValidChild Tags.Title parent
             => ChildHTML Tags.Title
             -> ChildHTML parent
             -> ChildHTML parent
prependTitle child tag =
  case tag of
    Tag_Title attrs content ->
      Tag_Title attrs $ child : content

    _ ->
      tag

prependTableRow :: ValidChild Tags.TableRow parent
                => ChildHTML Tags.TableRow
                -> ChildHTML parent
                -> ChildHTML parent
prependTableRow child tag =
  case tag of
    Tag_TableRow attrs content ->
      Tag_TableRow attrs $ child : content

    _ ->
      tag

prependUnderline :: ValidChild Tags.Underline parent
                 => ChildHTML Tags.Underline
                 -> ChildHTML parent
                 -> ChildHTML parent
prependUnderline child tag =
  case tag of
    Tag_Underline attrs content ->
      Tag_Underline attrs $ child : content

    _ ->
      tag

prependUnorderedList :: ValidChild Tags.UnorderedList parent
                     => ChildHTML Tags.UnorderedList
                     -> ChildHTML parent
                     -> ChildHTML parent
prependUnorderedList child tag =
  case tag of
    Tag_UnorderedList attrs content ->
      Tag_UnorderedList attrs $ child : content

    _ ->
      tag

prependVariable :: ValidChild Tags.Variable parent
                => ChildHTML Tags.Variable
                -> ChildHTML parent
                -> ChildHTML parent
prependVariable child tag =
  case tag of
    Tag_Variable attrs content ->
      Tag_Variable attrs $ child : content

    _ ->
      tag

prependVideo :: ValidChild Tags.Video parent
             => ChildHTML Tags.Video
             -> ChildHTML parent
             -> ChildHTML parent
prependVideo child tag =
  case tag of
    Tag_Video attrs content ->
      Tag_Video attrs $ child : content

    _ ->
      tag
