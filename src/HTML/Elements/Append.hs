module HTML.Elements.Append
  ( appendAnchor
  , appendAbbreviation
  , appendContactAddress
  , appendArticle
  , appendAside
  , appendAudio
  , appendBringAttentionTo
  , appendBidirectionalIsolation
  , appendBidirectionalOverride
  , appendBlockquote
  , appendBody
  , appendButton
  , appendCanvas
  , appendTableCaption
  , appendCitation
  , appendCode
  , appendTableColumnGroup
  , appendData
  , appendDataList
  , appendDescriptionDetails
  , appendDeletedText
  , appendDetails
  , appendDefinition
  , appendDialog
  , appendDivision
  , appendDescriptionList
  , appendDescriptionTerm
  , appendEmphasis
  , appendFieldset
  , appendFigureCaption
  , appendFigure
  , appendFooter
  , appendForm
  , appendH1
  , appendH2
  , appendH3
  , appendH4
  , appendH5
  , appendH6
  , appendHead
  , appendHeader
  , appendHeadingGroup
  , appendHtml
  , appendIdiomaticText
  , appendInsertedText
  , appendKeyboardInput
  , appendLabel
  , appendLegend
  , appendListItem
  , appendMain
  , appendMap
  , appendMark
  , appendMenu
  , appendMeter
  , appendNav
  , appendNoScript
  , appendObject
  , appendOrderedList
  , appendOptionGroup
  , appendOption
  , appendOutput
  , appendParagraph
  , appendPicture
  , appendPreformattedText
  , appendProgress
  , appendQuotation
  , appendRubyParenthesis
  , appendRubyText
  , appendRuby
  , appendStrikethrough
  , appendSample
  , appendScript
  , appendSearch
  , appendSection
  , appendSelect
  , appendSlot
  , appendSideComment
  , appendSpan
  , appendStrong
  , appendStyle
  , appendSubscript
  , appendSummary
  , appendSuperscript
  , appendTable
  , appendTableBody
  , appendTableDataCell
  , appendContentTemplate
  , appendTextArea
  , appendTableFoot
  , appendTableHeader
  , appendTableHead
  , appendTime
  , appendTitle
  , appendTableRow
  , appendUnderline
  , appendUnorderedList
  , appendVariable
  , appendVideo
  ) where

import HTML.Elements.Children (ValidChild)
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Elements.Tags qualified as Tags

appendAnchor :: ValidChild Tags.Anchor parent
             => ChildHTML Tags.Anchor
             -> ChildHTML parent
             -> ChildHTML parent
appendAnchor child tag =
  case tag of
    Tag_Anchor attrs content ->
      Tag_Anchor attrs $ content <> [ child ]

    _ ->
      tag

appendAbbreviation :: ValidChild Tags.Abbreviation parent
                   => ChildHTML Tags.Abbreviation
                   -> ChildHTML parent
                   -> ChildHTML parent
appendAbbreviation child tag =
  case tag of
    Tag_Abbreviation attrs content ->
      Tag_Abbreviation attrs $ content <> [ child ]

    _ ->
      tag

appendContactAddress :: ValidChild Tags.ContactAddress parent
                     => ChildHTML Tags.ContactAddress
                     -> ChildHTML parent
                     -> ChildHTML parent
appendContactAddress child tag =
  case tag of
    Tag_ContactAddress attrs content ->
      Tag_ContactAddress attrs $ content <> [ child ]

    _ ->
      tag

appendArticle :: ValidChild Tags.Article parent
              => ChildHTML Tags.Article
              -> ChildHTML parent
              -> ChildHTML parent
appendArticle child tag =
  case tag of
    Tag_Article attrs content ->
      Tag_Article attrs $ content <> [ child ]

    _ ->
      tag

appendAside :: ValidChild Tags.Aside parent
            => ChildHTML Tags.Aside
            -> ChildHTML parent
            -> ChildHTML parent
appendAside child tag =
  case tag of
    Tag_Aside attrs content ->
      Tag_Aside attrs $ content <> [ child ]

    _ ->
      tag

appendAudio :: ValidChild Tags.Audio parent
            => ChildHTML Tags.Audio
            -> ChildHTML parent
            -> ChildHTML parent
appendAudio child tag =
  case tag of
    Tag_Audio attrs content ->
      Tag_Audio attrs $ content <> [ child ]

    _ ->
      tag

appendBringAttentionTo :: ValidChild Tags.BringAttentionTo parent
                       => ChildHTML Tags.BringAttentionTo
                       -> ChildHTML parent
                       -> ChildHTML parent
appendBringAttentionTo child tag =
  case tag of
    Tag_BringAttentionTo attrs content ->
      Tag_BringAttentionTo attrs $ content <> [ child ]

    _ ->
      tag

appendBidirectionalIsolation :: ValidChild Tags.BidirectionalIsolation parent
                             => ChildHTML Tags.BidirectionalIsolation
                             -> ChildHTML parent
                             -> ChildHTML parent
appendBidirectionalIsolation child tag =
  case tag of
    Tag_BidirectionalIsolation attrs content ->
      Tag_BidirectionalIsolation attrs $ content <> [ child ]

    _ ->
      tag

appendBidirectionalOverride :: ValidChild Tags.BidirectionalOverride parent
                            => ChildHTML Tags.BidirectionalOverride
                            -> ChildHTML parent
                            -> ChildHTML parent
appendBidirectionalOverride child tag =
  case tag of
    Tag_BidirectionalOverride attrs content ->
      Tag_BidirectionalOverride attrs $ content <> [ child ]

    _ ->
      tag

appendBlockquote :: ValidChild Tags.Blockquote parent
                 => ChildHTML Tags.Blockquote
                 -> ChildHTML parent
                 -> ChildHTML parent
appendBlockquote child tag =
  case tag of
    Tag_Blockquote attrs content ->
      Tag_Blockquote attrs $ content <> [ child ]

    _ ->
      tag

appendBody :: ValidChild Tags.Body parent
           => ChildHTML Tags.Body
           -> ChildHTML parent
           -> ChildHTML parent
appendBody child tag =
  case tag of
    Tag_Body attrs content ->
      Tag_Body attrs $ content <> [ child ]

    _ ->
      tag

appendButton :: ValidChild Tags.Button parent
             => ChildHTML Tags.Button
             -> ChildHTML parent
             -> ChildHTML parent
appendButton child tag =
  case tag of
    Tag_Button attrs content ->
      Tag_Button attrs $ content <> [ child ]

    _ ->
      tag

appendCanvas :: ValidChild Tags.Canvas parent
             => ChildHTML Tags.Canvas
             -> ChildHTML parent
             -> ChildHTML parent
appendCanvas child tag =
  case tag of
    Tag_Canvas attrs content ->
      Tag_Canvas attrs $ content <> [ child ]

    _ ->
      tag

appendTableCaption :: ValidChild Tags.TableCaption parent
                   => ChildHTML Tags.TableCaption
                   -> ChildHTML parent
                   -> ChildHTML parent
appendTableCaption child tag =
  case tag of
    Tag_TableCaption attrs content ->
      Tag_TableCaption attrs $ content <> [ child ]

    _ ->
      tag

appendCitation :: ValidChild Tags.Citation parent
               => ChildHTML Tags.Citation
               -> ChildHTML parent
               -> ChildHTML parent
appendCitation child tag =
  case tag of
    Tag_Citation attrs content ->
      Tag_Citation attrs $ content <> [ child ]

    _ ->
      tag

appendCode :: ValidChild Tags.Code parent
           => ChildHTML Tags.Code
           -> ChildHTML parent
           -> ChildHTML parent
appendCode child tag =
  case tag of
    Tag_Code attrs content ->
      Tag_Code attrs $ content <> [ child ]

    _ ->
      tag

appendTableColumnGroup :: ValidChild Tags.TableColumnGroup parent
                       => ChildHTML Tags.TableColumnGroup
                       -> ChildHTML parent
                       -> ChildHTML parent
appendTableColumnGroup child tag =
  case tag of
    Tag_TableColumnGroup attrs content ->
      Tag_TableColumnGroup attrs $ content <> [ child ]

    _ ->
      tag

appendData :: ValidChild Tags.Data parent
           => ChildHTML Tags.Data
           -> ChildHTML parent
           -> ChildHTML parent
appendData child tag =
  case tag of
    Tag_Data attrs content ->
      Tag_Data attrs $ content <> [ child ]

    _ ->
      tag

appendDataList :: ValidChild Tags.DataList parent
               => ChildHTML Tags.DataList
               -> ChildHTML parent
               -> ChildHTML parent
appendDataList child tag =
  case tag of
    Tag_DataList attrs content ->
      Tag_DataList attrs $ content <> [ child ]

    _ ->
      tag

appendDescriptionDetails :: ValidChild Tags.DescriptionDetails parent
                         => ChildHTML Tags.DescriptionDetails
                         -> ChildHTML parent
                         -> ChildHTML parent
appendDescriptionDetails child tag =
  case tag of
    Tag_DescriptionDetails attrs content ->
      Tag_DescriptionDetails attrs $ content <> [ child ]

    _ ->
      tag

appendDeletedText :: ChildHTML parent
                  -> ChildHTML parent
                  -> ChildHTML parent
appendDeletedText child tag =
  case tag of
    Tag_DeletedText attrs content ->
      Tag_DeletedText attrs $ content <> [ child ]

    _ ->
      tag

appendDetails :: ValidChild Tags.Details parent
              => ChildHTML Tags.Details
              -> ChildHTML parent
              -> ChildHTML parent
appendDetails child tag =
  case tag of
    Tag_Details attrs content ->
      Tag_Details attrs $ content <> [ child ]

    _ ->
      tag

appendDefinition :: ValidChild Tags.Definition parent
                 => ChildHTML Tags.Definition
                 -> ChildHTML parent
                 -> ChildHTML parent
appendDefinition child tag =
  case tag of
    Tag_Definition attrs content ->
      Tag_Definition attrs $ content <> [ child ]

    _ ->
      tag

appendDialog :: ValidChild Tags.Dialog parent
             => ChildHTML Tags.Dialog
             -> ChildHTML parent
             -> ChildHTML parent
appendDialog child tag =
  case tag of
    Tag_Dialog attrs content ->
      Tag_Dialog attrs $ content <> [ child ]

    _ ->
      tag

appendDivision :: ValidChild Tags.Division parent
               => ChildHTML Tags.Division
               -> ChildHTML parent
               -> ChildHTML parent
appendDivision child tag =
  case tag of
    Tag_Division attrs content ->
      Tag_Division attrs $ content <> [ child ]

    _ ->
      tag

appendDescriptionList :: ValidChild Tags.DescriptionList parent
                      => ChildHTML Tags.DescriptionList
                      -> ChildHTML parent
                      -> ChildHTML parent
appendDescriptionList child tag =
  case tag of
    Tag_DescriptionList attrs content ->
      Tag_DescriptionList attrs $ content <> [ child ]

    _ ->
      tag

appendDescriptionTerm :: ValidChild Tags.DescriptionTerm parent
                      => ChildHTML Tags.DescriptionTerm
                      -> ChildHTML parent
                      -> ChildHTML parent
appendDescriptionTerm child tag =
  case tag of
    Tag_DescriptionTerm attrs content ->
      Tag_DescriptionTerm attrs $ content <> [ child ]

    _ ->
      tag

appendEmphasis :: ValidChild Tags.Emphasis parent
               => ChildHTML Tags.Emphasis
               -> ChildHTML parent
               -> ChildHTML parent
appendEmphasis child tag =
  case tag of
    Tag_Emphasis attrs content ->
      Tag_Emphasis attrs $ content <> [ child ]

    _ ->
      tag

appendFieldset :: ValidChild Tags.Fieldset parent
               => ChildHTML Tags.Fieldset
               -> ChildHTML parent
               -> ChildHTML parent
appendFieldset child tag =
  case tag of
    Tag_Fieldset attrs content ->
      Tag_Fieldset attrs $ content <> [ child ]

    _ ->
      tag

appendFigureCaption :: ValidChild Tags.FigureCaption parent
                    => ChildHTML Tags.FigureCaption
                    -> ChildHTML parent
                    -> ChildHTML parent
appendFigureCaption child tag =
  case tag of
    Tag_FigureCaption attrs content ->
      Tag_FigureCaption attrs $ content <> [ child ]

    _ ->
      tag

appendFigure :: ValidChild Tags.Figure parent
             => ChildHTML Tags.Figure
             -> ChildHTML parent
             -> ChildHTML parent
appendFigure child tag =
  case tag of
    Tag_Figure attrs content ->
      Tag_Figure attrs $ content <> [ child ]

    _ ->
      tag

appendFooter :: ValidChild Tags.Footer parent
             => ChildHTML Tags.Footer
             -> ChildHTML parent
             -> ChildHTML parent
appendFooter child tag =
  case tag of
    Tag_Footer attrs content ->
      Tag_Footer attrs $ content <> [ child ]

    _ ->
      tag

appendForm :: ValidChild Tags.Form parent
           => ChildHTML Tags.Form
           -> ChildHTML parent
           -> ChildHTML parent
appendForm child tag =
  case tag of
    Tag_Form attrs content ->
      Tag_Form attrs $ content <> [ child ]

    _ ->
      tag

appendH1 :: ValidChild Tags.H1 parent
         => ChildHTML Tags.H1
         -> ChildHTML parent
         -> ChildHTML parent
appendH1 child tag =
  case tag of
    Tag_H1 attrs content ->
      Tag_H1 attrs $ content <> [ child ]

    _ ->
      tag

appendH2 :: ValidChild Tags.H2 parent
         => ChildHTML Tags.H2
         -> ChildHTML parent
         -> ChildHTML parent
appendH2 child tag =
  case tag of
    Tag_H2 attrs content ->
      Tag_H2 attrs $ content <> [ child ]

    _ ->
      tag

appendH3 :: ValidChild Tags.H3 parent
         => ChildHTML Tags.H3
         -> ChildHTML parent
         -> ChildHTML parent
appendH3 child tag =
  case tag of
    Tag_H3 attrs content ->
      Tag_H3 attrs $ content <> [ child ]

    _ ->
      tag

appendH4 :: ValidChild Tags.H4 parent
         => ChildHTML Tags.H4
         -> ChildHTML parent
         -> ChildHTML parent
appendH4 child tag =
  case tag of
    Tag_H4 attrs content ->
      Tag_H4 attrs $ content <> [ child ]

    _ ->
      tag

appendH5 :: ValidChild Tags.H5 parent
         => ChildHTML Tags.H5
         -> ChildHTML parent
         -> ChildHTML parent
appendH5 child tag =
  case tag of
    Tag_H5 attrs content ->
      Tag_H5 attrs $ content <> [ child ]

    _ ->
      tag

appendH6 :: ValidChild Tags.H6 parent
         => ChildHTML Tags.H6
         -> ChildHTML parent
         -> ChildHTML parent
appendH6 child tag =
  case tag of
    Tag_H6 attrs content ->
      Tag_H6 attrs $ content <> [ child ]

    _ ->
      tag

appendHead :: ValidChild Tags.Head parent
           => ChildHTML Tags.Head
           -> ChildHTML parent
           -> ChildHTML parent
appendHead child tag =
  case tag of
    Tag_Head attrs content ->
      Tag_Head attrs $ content <> [ child ]

    _ ->
      tag

appendHeader :: ValidChild Tags.Header parent
             => ChildHTML Tags.Header
             -> ChildHTML parent
             -> ChildHTML parent
appendHeader child tag =
  case tag of
    Tag_Header attrs content ->
      Tag_Header attrs $ content <> [ child ]

    _ ->
      tag

appendHeadingGroup :: ValidChild Tags.HeadingGroup parent
                   => ChildHTML Tags.HeadingGroup
                   -> ChildHTML parent
                   -> ChildHTML parent
appendHeadingGroup child tag =
  case tag of
    Tag_HeadingGroup attrs content ->
      Tag_HeadingGroup attrs $ content <> [ child ]

    _ ->
      tag

appendHtml :: ValidChild Tags.Html parent
           => ChildHTML Tags.Html
           -> ChildHTML parent
           -> ChildHTML parent
appendHtml child tag =
  case tag of
    Tag_Html attrs content ->
      Tag_Html attrs $ content <> [ child ]

    _ ->
      tag

appendIdiomaticText :: ValidChild Tags.IdiomaticText parent
                    => ChildHTML Tags.IdiomaticText
                    -> ChildHTML parent
                    -> ChildHTML parent
appendIdiomaticText child tag =
  case tag of
    Tag_IdiomaticText attrs content ->
      Tag_IdiomaticText attrs $ content <> [ child ]

    _ ->
      tag

appendInsertedText :: ChildHTML parent
                   -> ChildHTML parent
                   -> ChildHTML parent
appendInsertedText child tag =
  case tag of
    Tag_InsertedText attrs content ->
      Tag_InsertedText attrs $ content <> [ child ]

    _ ->
      tag

appendKeyboardInput :: ValidChild Tags.KeyboardInput parent
                    => ChildHTML Tags.KeyboardInput
                    -> ChildHTML parent
                    -> ChildHTML parent
appendKeyboardInput child tag =
  case tag of
    Tag_KeyboardInput attrs content ->
      Tag_KeyboardInput attrs $ content <> [ child ]

    _ ->
      tag

appendLabel :: ValidChild Tags.Label parent
            => ChildHTML Tags.Label
            -> ChildHTML parent
            -> ChildHTML parent
appendLabel child tag =
  case tag of
    Tag_Label attrs content ->
      Tag_Label attrs $ content <> [ child ]

    _ ->
      tag

appendLegend :: ValidChild Tags.Legend parent
             => ChildHTML Tags.Legend
             -> ChildHTML parent
             -> ChildHTML parent
appendLegend child tag =
  case tag of
    Tag_Legend attrs content ->
      Tag_Legend attrs $ content <> [ child ]

    _ ->
      tag

appendListItem :: ValidChild Tags.ListItem parent
               => ChildHTML Tags.ListItem
               -> ChildHTML parent
               -> ChildHTML parent
appendListItem child tag =
  case tag of
    Tag_ListItem attrs content ->
      Tag_ListItem attrs $ content <> [ child ]

    _ ->
      tag

appendMain :: ValidChild Tags.Main parent
           => ChildHTML Tags.Main
           -> ChildHTML parent
           -> ChildHTML parent
appendMain child tag =
  case tag of
    Tag_Main attrs content ->
      Tag_Main attrs $ content <> [ child ]

    _ ->
      tag

appendMap :: ValidChild Tags.Map parent
          => ChildHTML Tags.Map
          -> ChildHTML parent
          -> ChildHTML parent
appendMap child tag =
  case tag of
    Tag_Map attrs content ->
      Tag_Map attrs $ content <> [ child ]

    _ ->
      tag

appendMark :: ValidChild Tags.Mark parent
           => ChildHTML Tags.Mark
           -> ChildHTML parent
           -> ChildHTML parent
appendMark child tag =
  case tag of
    Tag_Mark attrs content ->
      Tag_Mark attrs $ content <> [ child ]

    _ ->
      tag

appendMenu :: ValidChild Tags.Menu parent
           => ChildHTML Tags.Menu
           -> ChildHTML parent
           -> ChildHTML parent
appendMenu child tag =
  case tag of
    Tag_Menu attrs content ->
      Tag_Menu attrs $ content <> [ child ]

    _ ->
      tag

appendMeter :: ValidChild Tags.Meter parent
            => ChildHTML Tags.Meter
            -> ChildHTML parent
            -> ChildHTML parent
appendMeter child tag =
  case tag of
    Tag_Meter attrs content ->
      Tag_Meter attrs $ content <> [ child ]

    _ ->
      tag

appendNav :: ValidChild Tags.Nav parent
          => ChildHTML Tags.Nav
          -> ChildHTML parent
          -> ChildHTML parent
appendNav child tag =
  case tag of
    Tag_Nav attrs content ->
      Tag_Nav attrs $ content <> [ child ]

    _ ->
      tag

appendNoScript :: ValidChild Tags.NoScript parent
               => ChildHTML Tags.NoScript
               -> ChildHTML parent
               -> ChildHTML parent
appendNoScript child tag =
  case tag of
    Tag_NoScript attrs content ->
      Tag_NoScript attrs $ content <> [ child ]

    _ ->
      tag

appendObject :: ValidChild Tags.Object parent
             => ChildHTML Tags.Object
             -> ChildHTML parent
             -> ChildHTML parent
appendObject child tag =
  case tag of
    Tag_Object attrs content ->
      Tag_Object attrs $ content <> [ child ]

    _ ->
      tag

appendOrderedList :: ValidChild Tags.OrderedList parent
                  => ChildHTML Tags.OrderedList
                  -> ChildHTML parent
                  -> ChildHTML parent
appendOrderedList child tag =
  case tag of
    Tag_OrderedList attrs content ->
      Tag_OrderedList attrs $ content <> [ child ]

    _ ->
      tag

appendOptionGroup :: ValidChild Tags.OptionGroup parent
                  => ChildHTML Tags.OptionGroup
                  -> ChildHTML parent
                  -> ChildHTML parent
appendOptionGroup child tag =
  case tag of
    Tag_OptionGroup attrs content ->
      Tag_OptionGroup attrs $ content <> [ child ]

    _ ->
      tag

appendOption :: ValidChild Tags.Option parent
             => ChildHTML Tags.Option
             -> ChildHTML parent
             -> ChildHTML parent
appendOption child tag =
  case tag of
    Tag_Option attrs content ->
      Tag_Option attrs $ content <> [ child ]

    _ ->
      tag

appendOutput :: ValidChild Tags.Output parent
             => ChildHTML Tags.Output
             -> ChildHTML parent
             -> ChildHTML parent
appendOutput child tag =
  case tag of
    Tag_Output attrs content ->
      Tag_Output attrs $ content <> [ child ]

    _ ->
      tag

appendParagraph :: ValidChild Tags.Paragraph parent
                => ChildHTML Tags.Paragraph
                -> ChildHTML parent
                -> ChildHTML parent
appendParagraph child tag =
  case tag of
    Tag_Paragraph attrs content ->
      Tag_Paragraph attrs $ content <> [ child ]

    _ ->
      tag

appendPicture :: ValidChild Tags.Picture parent
              => ChildHTML Tags.Picture
              -> ChildHTML parent
              -> ChildHTML parent
appendPicture child tag =
  case tag of
    Tag_Picture attrs content ->
      Tag_Picture attrs $ content <> [ child ]

    _ ->
      tag

appendPreformattedText :: ValidChild Tags.PreformattedText parent
                       => ChildHTML Tags.PreformattedText
                       -> ChildHTML parent
                       -> ChildHTML parent
appendPreformattedText child tag =
  case tag of
    Tag_PreformattedText attrs content ->
      Tag_PreformattedText attrs $ content <> [ child ]

    _ ->
      tag

appendProgress :: ValidChild Tags.Progress parent
               => ChildHTML Tags.Progress
               -> ChildHTML parent
               -> ChildHTML parent
appendProgress child tag =
  case tag of
    Tag_Progress attrs content ->
      Tag_Progress attrs $ content <> [ child ]

    _ ->
      tag

appendQuotation :: ValidChild Tags.Quotation parent
                => ChildHTML Tags.Quotation
                -> ChildHTML parent
                -> ChildHTML parent
appendQuotation child tag =
  case tag of
    Tag_Quotation attrs content ->
      Tag_Quotation attrs $ content <> [ child ]

    _ ->
      tag

appendRubyParenthesis :: ValidChild Tags.RubyParenthesis parent
                      => ChildHTML Tags.RubyParenthesis
                      -> ChildHTML parent
                      -> ChildHTML parent
appendRubyParenthesis child tag =
  case tag of
    Tag_RubyParenthesis attrs content ->
      Tag_RubyParenthesis attrs $ content <> [ child ]

    _ ->
      tag

appendRubyText :: ValidChild Tags.RubyText parent
               => ChildHTML Tags.RubyText
               -> ChildHTML parent
               -> ChildHTML parent
appendRubyText child tag =
  case tag of
    Tag_RubyText attrs content ->
      Tag_RubyText attrs $ content <> [ child ]

    _ ->
      tag

appendRuby :: ValidChild Tags.Ruby parent
           => ChildHTML Tags.Ruby
           -> ChildHTML parent
           -> ChildHTML parent
appendRuby child tag =
  case tag of
    Tag_Ruby attrs content ->
      Tag_Ruby attrs $ content <> [ child ]

    _ ->
      tag

appendStrikethrough :: ValidChild Tags.Strikethrough parent
                    => ChildHTML Tags.Strikethrough
                    -> ChildHTML parent
                    -> ChildHTML parent
appendStrikethrough child tag =
  case tag of
    Tag_Strikethrough attrs content ->
      Tag_Strikethrough attrs $ content <> [ child ]

    _ ->
      tag

appendSample :: ValidChild Tags.Sample parent
             => ChildHTML Tags.Sample
             -> ChildHTML parent
             -> ChildHTML parent
appendSample child tag =
  case tag of
    Tag_Sample attrs content ->
      Tag_Sample attrs $ content <> [ child ]

    _ ->
      tag

appendScript :: ValidChild Tags.Script parent
             => ChildHTML Tags.Script
             -> ChildHTML parent
             -> ChildHTML parent
appendScript child tag =
  case tag of
    Tag_Script attrs content ->
      Tag_Script attrs $ content <> [ child ]

    _ ->
      tag

appendSearch :: ValidChild Tags.Search parent
             => ChildHTML Tags.Search
             -> ChildHTML parent
             -> ChildHTML parent
appendSearch child tag =
  case tag of
    Tag_Search attrs content ->
      Tag_Search attrs $ content <> [ child ]

    _ ->
      tag

appendSection :: ValidChild Tags.Section parent
              => ChildHTML Tags.Section
              -> ChildHTML parent
              -> ChildHTML parent
appendSection child tag =
  case tag of
    Tag_Section attrs content ->
      Tag_Section attrs $ content <> [ child ]

    _ ->
      tag

appendSelect :: ValidChild Tags.Select parent
             => ChildHTML Tags.Select
             -> ChildHTML parent
             -> ChildHTML parent
appendSelect child tag =
  case tag of
    Tag_Select attrs content ->
      Tag_Select attrs $ content <> [ child ]

    _ ->
      tag

appendSlot :: ChildHTML parent
           -> ChildHTML parent
           -> ChildHTML parent
appendSlot child tag =
  case tag of
    Tag_Slot attrs content ->
      Tag_Slot attrs $ content <> [ child ]

    _ ->
      tag

appendSideComment :: ValidChild Tags.SideComment parent
                  => ChildHTML Tags.SideComment
                  -> ChildHTML parent
                  -> ChildHTML parent
appendSideComment child tag =
  case tag of
    Tag_SideComment attrs content ->
      Tag_SideComment attrs $ content <> [ child ]

    _ ->
      tag

appendSpan :: ValidChild Tags.Span parent
           => ChildHTML Tags.Span
           -> ChildHTML parent
           -> ChildHTML parent
appendSpan child tag =
  case tag of
    Tag_Span attrs content ->
      Tag_Span attrs $ content <> [ child ]

    _ ->
      tag

appendStrong :: ValidChild Tags.Strong parent
             => ChildHTML Tags.Strong
             -> ChildHTML parent
             -> ChildHTML parent
appendStrong child tag =
  case tag of
    Tag_Strong attrs content ->
      Tag_Strong attrs $ content <> [ child ]

    _ ->
      tag

appendStyle :: ValidChild Tags.Style parent
            => ChildHTML Tags.Style
            -> ChildHTML parent
            -> ChildHTML parent
appendStyle child tag =
  case tag of
    Tag_Style attrs content ->
      Tag_Style attrs $ content <> [ child ]

    _ ->
      tag

appendSubscript :: ValidChild Tags.Subscript parent
                => ChildHTML Tags.Subscript
                -> ChildHTML parent
                -> ChildHTML parent
appendSubscript child tag =
  case tag of
    Tag_Subscript attrs content ->
      Tag_Subscript attrs $ content <> [ child ]

    _ ->
      tag

appendSummary :: ValidChild Tags.Summary parent
              => ChildHTML Tags.Summary
              -> ChildHTML parent
              -> ChildHTML parent
appendSummary child tag =
  case tag of
    Tag_Summary attrs content ->
      Tag_Summary attrs $ content <> [ child ]

    _ ->
      tag

appendSuperscript :: ValidChild Tags.Superscript parent
                  => ChildHTML Tags.Superscript
                  -> ChildHTML parent
                  -> ChildHTML parent
appendSuperscript child tag =
  case tag of
    Tag_Superscript attrs content ->
      Tag_Superscript attrs $ content <> [ child ]

    _ ->
      tag

appendTable :: ValidChild Tags.Table parent
            => ChildHTML Tags.Table
            -> ChildHTML parent
            -> ChildHTML parent
appendTable child tag =
  case tag of
    Tag_Table attrs content ->
      Tag_Table attrs $ content <> [ child ]

    _ ->
      tag

appendTableBody :: ValidChild Tags.TableBody parent
                => ChildHTML Tags.TableBody
                -> ChildHTML parent
                -> ChildHTML parent
appendTableBody child tag =
  case tag of
    Tag_TableBody attrs content ->
      Tag_TableBody attrs $ content <> [ child ]

    _ ->
      tag

appendTableDataCell :: ValidChild Tags.TableDataCell parent
                    => ChildHTML Tags.TableDataCell
                    -> ChildHTML parent
                    -> ChildHTML parent
appendTableDataCell child tag =
  case tag of
    Tag_TableDataCell attrs content ->
      Tag_TableDataCell attrs $ content <> [ child ]

    _ ->
      tag

appendContentTemplate :: ValidChild Tags.ContentTemplate parent
                      => ChildHTML Tags.ContentTemplate
                      -> ChildHTML parent
                      -> ChildHTML parent
appendContentTemplate child tag =
  case tag of
    Tag_ContentTemplate attrs content ->
      Tag_ContentTemplate attrs $ content <> [ child ]

    _ ->
      tag

appendTextArea :: ValidChild Tags.TextArea parent
               => ChildHTML Tags.TextArea
               -> ChildHTML parent
               -> ChildHTML parent
appendTextArea child tag =
  case tag of
    Tag_TextArea attrs content ->
      Tag_TextArea attrs $ content <> [ child ]

    _ ->
      tag

appendTableFoot :: ValidChild Tags.TableFoot parent
                => ChildHTML Tags.TableFoot
                -> ChildHTML parent
                -> ChildHTML parent
appendTableFoot child tag =
  case tag of
    Tag_TableFoot attrs content ->
      Tag_TableFoot attrs $ content <> [ child ]

    _ ->
      tag

appendTableHeader :: ValidChild Tags.TableHeader parent
                  => ChildHTML Tags.TableHeader
                  -> ChildHTML parent
                  -> ChildHTML parent
appendTableHeader child tag =
  case tag of
    Tag_TableHeader attrs content ->
      Tag_TableHeader attrs $ content <> [ child ]

    _ ->
      tag

appendTableHead :: ValidChild Tags.TableHead parent
                => ChildHTML Tags.TableHead
                -> ChildHTML parent
                -> ChildHTML parent
appendTableHead child tag =
  case tag of
    Tag_TableHead attrs content ->
      Tag_TableHead attrs $ content <> [ child ]

    _ ->
      tag

appendTime :: ValidChild Tags.Time parent
           => ChildHTML Tags.Time
           -> ChildHTML parent
           -> ChildHTML parent
appendTime child tag =
  case tag of
    Tag_Time attrs content ->
      Tag_Time attrs $ content <> [ child ]

    _ ->
      tag

appendTitle :: ValidChild Tags.Title parent
            => ChildHTML Tags.Title
            -> ChildHTML parent
            -> ChildHTML parent
appendTitle child tag =
  case tag of
    Tag_Title attrs content ->
      Tag_Title attrs $ content <> [ child ]

    _ ->
      tag

appendTableRow :: ValidChild Tags.TableRow parent
               => ChildHTML Tags.TableRow
               -> ChildHTML parent
               -> ChildHTML parent
appendTableRow child tag =
  case tag of
    Tag_TableRow attrs content ->
      Tag_TableRow attrs $ content <> [ child ]

    _ ->
      tag

appendUnderline :: ValidChild Tags.Underline parent
                => ChildHTML Tags.Underline
                -> ChildHTML parent
                -> ChildHTML parent
appendUnderline child tag =
  case tag of
    Tag_Underline attrs content ->
      Tag_Underline attrs $ content <> [ child ]

    _ ->
      tag

appendUnorderedList :: ValidChild Tags.UnorderedList parent
                    => ChildHTML Tags.UnorderedList
                    -> ChildHTML parent
                    -> ChildHTML parent
appendUnorderedList child tag =
  case tag of
    Tag_UnorderedList attrs content ->
      Tag_UnorderedList attrs $ content <> [ child ]

    _ ->
      tag

appendVariable :: ValidChild Tags.Variable parent
               => ChildHTML Tags.Variable
               -> ChildHTML parent
               -> ChildHTML parent
appendVariable child tag =
  case tag of
    Tag_Variable attrs content ->
      Tag_Variable attrs $ content <> [ child ]

    _ ->
      tag

appendVideo :: ValidChild Tags.Video parent
            => ChildHTML Tags.Video
            -> ChildHTML parent
            -> ChildHTML parent
appendVideo child tag =
  case tag of
    Tag_Video attrs content ->
      Tag_Video attrs $ content <> [ child ]

    _ ->
      tag
