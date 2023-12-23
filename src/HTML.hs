module HTML
  ( addAttribute
  , consChild
  , snocChild
  ) where

import Data.Map qualified as Map

import HTML.Attributes.Elements (ValidAttribute)
import HTML.Attributes.Internal (Attribute, attributeText)
import HTML.Elements.Children (ValidChild)
import HTML.Elements.Internal (Document, HTML, ChildHTML(..))

addAttribute :: ChildHTML parent
             -> Attribute tag
             -> ChildHTML parent
addAttribute _tag _attr = undefined
  -- case tag of
    -- Tag_NoElement ->
    --   tag

    -- Tag_Comment _comment ->
    --   tag

    -- Tag_Text _text ->
    --   tag

    -- Tag_Anchor attrs content ->
    --   Tag_Anchor (Map.insert (attributeText attr) attr attrs) content

    -- Tag_Abbreviation attrs content ->
    --   Tag_Abbreviation (Map.insert (attributeText attr) attr attrs) content

    -- Tag_ContactAddress attrs content ->
    --   buildTag "address" attrs $ Right content

    -- Tag_Area attrs ->
    --   buildTag "area" attrs $ Left OmitTag

    -- Tag_Article attrs content ->
    --   buildTag "article" attrs $ Right content

    -- Tag_Aside attrs content ->
    --   buildTag "aside" attrs $ Right content

    -- Tag_Audio attrs content ->
    --   buildTag "audio" attrs $ Right content

    -- Tag_BringAttentionTo attrs content ->
    --   buildTag "b" attrs $ Right content

    -- Tag_Base attrs ->
    --   buildTag "base" attrs $ Left OmitTag

    -- Tag_BidirectionalIsolation attrs content ->
    --   buildTag "bdi" attrs $ Right content

    -- Tag_BidirectionalOverride attrs content ->
    --   buildTag "bdo" attrs $ Right content

    -- Tag_Blockquote attrs content ->
    --   buildTag "blockquote" attrs $ Right content

    -- Tag_Body attrs content ->
    --   buildTag "body" attrs $ Right content

    -- Tag_LineBreak attrs ->
    --   buildTag "br" attrs $ Left OmitTag

    -- Tag_Button attrs content ->
    --   buildTag "button" attrs $ Right content

    -- Tag_Canvas attrs content ->
    --   buildTag "canbvas" attrs $ Right content

    -- Tag_TableCaption attrs content ->
    --   buildTag "caption" attrs $ Right content

    -- Tag_Citation attrs content ->
    --   buildTag "cite" attrs $ Right content

    -- Tag_Code attrs content ->
    --   buildTag "code" attrs $ Right content

    -- Tag_TableColumn attrs ->
    --   buildTag "col" attrs $ Left OmitTag

    -- Tag_TableColumnGroup attrs content ->
    --   buildTag "colgroup" attrs $ Right content

    -- Tag_Data attrs content ->
    --   buildTag "data" attrs $ Right content

    -- Tag_DataList attrs content ->
    --   buildTag "datalist" attrs $ Right content

    -- Tag_DescriptionDetails attrs content ->
    --   buildTag "dd" attrs $ Right content

    -- Tag_DeletedText attrs content ->
    --   buildTag "del" attrs $ Right content

    -- Tag_Details attrs content ->
    --   buildTag "details" attrs $ Right content

    -- Tag_Definition attrs content ->
    --   buildTag "dfn" attrs $ Right content

    -- Tag_Dialog attrs content ->
    --   buildTag "dialog" attrs $ Right content

    -- Tag_Division attrs content ->
    --   buildTag "div" attrs $ Right content

    -- Tag_DescriptionList attrs content ->
    --   buildTag "dl" attrs $ Right content

    -- Tag_DescriptionTerm attrs content ->
    --   buildTag "dt" attrs $ Right content

    -- Tag_Emphasis attrs content ->
    --   buildTag "em" attrs $ Right content

    -- Tag_Embed attrs ->
    --   buildTag "embed" attrs $ Left OmitTag

    -- Tag_Fieldset attrs content ->
    --   buildTag "fieldset" attrs $ Right content

    -- Tag_FigureCaption attrs content ->
    --   buildTag "figcaption" attrs $ Right content

    -- Tag_Figure attrs content ->
    --   buildTag "figure" attrs $ Right content

    -- Tag_Footer attrs content ->
    --   buildTag "footer" attrs $ Right content

    -- Tag_Form attrs content ->
    --   buildTag "form" attrs $ Right content

    -- Tag_H1 attrs content ->
    --   buildTag "h1" attrs $ Right content

    -- Tag_H2 attrs content ->
    --   buildTag "h2" attrs $ Right content

    -- Tag_H3 attrs content ->
    --   buildTag "h3" attrs $ Right content

    -- Tag_H4 attrs content ->
    --   buildTag "h4" attrs $ Right content

    -- Tag_H5 attrs content ->
    --   buildTag "h5" attrs $ Right content

    -- Tag_H6 attrs content ->
    --   buildTag "h6" attrs $ Right content

    -- Tag_Head attrs content ->
    --   buildTag "head" attrs $ Right content

    -- Tag_Header attrs content ->
    --   buildTag "header" attrs $ Right content

    -- Tag_HeadingGroup attrs content ->
    --   buildTag "hgroup" attrs $ Right content

    -- Tag_HorizontalRule attrs ->
    --   buildTag "hr" attrs $ Left OmitTag

    -- Tag_Html attrs content ->
    --   ("<!DOCTYPE html>" :: T.Text) <> buildTag "html" attrs (Right content)

    -- Tag_IdiomaticText attrs content ->
    --   buildTag "i" attrs $ Right content

    -- Tag_IFrame attrs ->
    --   buildTag "iframe" attrs $ Left WithTag

    -- Tag_Image attrs ->
    --   buildTag "img" attrs $ Left OmitTag

    -- Tag_Input attrs ->
    --   buildTag "input" attrs $ Left OmitTag

    -- Tag_InsertedText attrs content ->
    --   buildTag "ins" attrs $ Right content

    -- Tag_KeyboardInput attrs content ->
    --   buildTag "kbd" attrs $ Right content

    -- Tag_Label attrs content ->
    --   buildTag "label" attrs $ Right content

    -- Tag_Legend attrs content ->
    --   buildTag "legend" attrs $ Right content

    -- Tag_ListItem attrs content ->
    --   buildTag "li" attrs $ Right content

    -- Tag_Link attrs ->
    --   buildTag "link" attrs $ Left OmitTag

    -- Tag_Main attrs content ->
    --   buildTag "main" attrs $ Right content

    -- Tag_Map attrs content ->
    --   buildTag "map" attrs $ Right content

    -- Tag_Mark attrs content ->
    --   buildTag "mark" attrs $ Right content

    -- Tag_Menu attrs content ->
    --   buildTag "menu" attrs $ Right content

    -- Tag_Meta attrs ->
    --   buildTag "meta" attrs $ Left OmitTag

    -- Tag_Meter attrs content ->
    --   buildTag "meter" attrs $ Right content

    -- Tag_Nav attrs content ->
    --   buildTag "nav" attrs $ Right content

    -- Tag_NoScript attrs content ->
    --   buildTag "noscript" attrs $ Right content

    -- Tag_Object attrs content ->
    --   buildTag "object" attrs $ Right content

    -- Tag_OrderedList attrs content ->
    --   buildTag "ol" attrs $ Right content

    -- Tag_OptionGroup attrs content ->
    --   buildTag "optgroup" attrs $ Right content

    -- Tag_Option attrs content ->
    --   buildTag "option" attrs $ Right content

    -- Tag_Output attrs content ->
    --   buildTag "output" attrs $ Right content

    -- Tag_Paragraph attrs content ->
    --   buildTag "p" attrs $ Right content

    -- Tag_Picture attrs content ->
    --   buildTag "picture" attrs $ Right content

    -- Tag_PreformattedText attrs content ->
    --   buildTag "pre" attrs $ Right content

    -- Tag_Progress attrs content ->
    --   buildTag "progress" attrs $ Right content

    -- Tag_Quotation attrs content ->
    --   buildTag "q" attrs $ Right content

    -- Tag_RubyParenthesis attrs content ->
    --   buildTag "rp" attrs $ Right content

    -- Tag_RubyText attrs content ->
    --   buildTag "rt" attrs $ Right content

    -- Tag_Ruby attrs content ->
    --   buildTag "ruby" attrs $ Right content

    -- Tag_Strikethrough attrs content ->
    --   buildTag "s" attrs $ Right content

    -- Tag_Sample attrs content ->
    --   buildTag "sample" attrs $ Right content

    -- Tag_Script attrs content ->
    --   buildTag "script" attrs $ Right content

    -- Tag_Search attrs content ->
    --   buildTag "search" attrs $ Right content

    -- Tag_Section attrs content ->
    --   buildTag "section" attrs $ Right content

    -- Tag_Select attrs content ->
    --   buildTag "select" attrs $ Right content

    -- Tag_Slot attrs content ->
    --   buildTag "slot" attrs $ Right content

    -- Tag_SideComment attrs content ->
    --   buildTag "small" attrs $ Right content

    -- Tag_Source attrs ->
    --   buildTag "source" attrs $ Left OmitTag

    -- Tag_Span attrs content ->
    --   buildTag "span" attrs $ Right content

    -- Tag_Strong attrs content ->
    --   buildTag "strong" attrs $ Right content

    -- Tag_Style attrs content ->
    --   buildTag "style" attrs $ Right content

    -- Tag_Subscript attrs content ->
    --   buildTag "sub" attrs $ Right content

    -- Tag_Summary attrs content ->
    --   buildTag "summary" attrs $ Right content

    -- Tag_Superscript attrs content ->
    --   buildTag "sup" attrs $ Right content

    -- Tag_Table attrs content ->
    --   buildTag "table" attrs $ Right content

    -- Tag_TableBody attrs content ->
    --   buildTag "tbody" attrs $ Right content

    -- Tag_TableDataCell attrs content ->
    --   buildTag "td" attrs $ Right content

    -- Tag_ContentTemplate attrs content ->
    --   buildTag "template" attrs $ Right content

    -- Tag_TextArea attrs content ->
    --   buildTag "textarea" attrs $ Right content

    -- Tag_TableFoot attrs content ->
    --   buildTag "tfoot" attrs $ Right content

    -- Tag_TableHeader attrs content ->
    --   buildTag "th" attrs $ Right content

    -- Tag_TableHead attrs content ->
    --   buildTag "thead" attrs $ Right content

    -- Tag_Time attrs content ->
    --   buildTag "time" attrs $ Right content

    -- Tag_Title attrs content ->
    --   buildTag "title" attrs $ Right content

    -- Tag_TableRow attrs content ->
    --   buildTag "tr" attrs $ Right content

    -- Tag_Track attrs ->
    --   buildTag "track" attrs $ Left OmitTag

    -- Tag_Underline attrs content ->
    --   buildTag "u" attrs $ Right content

    -- Tag_UnorderedList attrs content ->
    --   buildTag "ul" attrs $ Right content

    -- Tag_Variable attrs content ->
    --   buildTag "var" attrs $ Right content

    -- Tag_Video attrs content ->
    --   buildTag "video" attrs $ Right content

    -- Tag_WordBreakOpportunity attrs ->
    --   buildTag "wbr" attrs $ Left OmitTag

consChild :: ValidChild tag parent
          => ChildHTML parent
          -> ChildHTML tag
          -> ChildHTML parent
consChild _child _tag = undefined
  -- case tag of
    -- Tag_NoElement ->
    --   tag

    -- Tag_Comment _comment ->
    --   tag

    -- Tag_Text _text ->
    --   tag

    -- Tag_Anchor attrs content ->
    --   Tag_Anchor attrs $ child : content

    -- Tag_Abbreviation attrs content ->
    --   Tag_Abbreviation attrs $ content <> [ child ]

    -- Tag_ContactAddress attrs content ->
    --   Tag_Abbreviation attrs $ content <> [ child ]

    -- Tag_Area attrs ->
    --   Tag_Area attrs

    -- Tag_Article attrs content ->
    --   Tag_Article attrs $ content <> [ child ]

    -- Tag_Aside attrs content ->
    --   Tag_Aside attrs $ content <> [ child ]

    -- Tag_Audio attrs content ->
    --   Tag_Audio attrs $ content <> [ child ]

    -- Tag_BringAttentionTo attrs content ->
    --   Tag_BringAttentionTo attrs $ content <> [ child ]

    -- Tag_Base attrs ->
    --   tag

    -- Tag_BidirectionalIsolation attrs content ->
    --   Tag_BidirectionalIsolation attrs $ content <> [ child ]

    -- Tag_BidirectionalOverride attrs content ->
    --   Tag_BidirectionalOverride attrs $ content <> [ child ]

    -- Tag_Blockquote attrs content ->
    --   buildTag "blockquote" attrs $ Right content

    -- Tag_Body attrs content ->
    --   buildTag "body" attrs $ Right content

    -- Tag_LineBreak attrs ->
    --   buildTag "br" attrs $ Left OmitTag

    -- Tag_Button attrs content ->
    --   buildTag "button" attrs $ Right content

    -- Tag_Canvas attrs content ->
    --   buildTag "canbvas" attrs $ Right content

    -- Tag_TableCaption attrs content ->
    --   buildTag "caption" attrs $ Right content

    -- Tag_Citation attrs content ->
    --   buildTag "cite" attrs $ Right content

    -- Tag_Code attrs content ->
    --   buildTag "code" attrs $ Right content

    -- Tag_TableColumn attrs ->
    --   buildTag "col" attrs $ Left OmitTag

    -- Tag_TableColumnGroup attrs content ->
    --   buildTag "colgroup" attrs $ Right content

    -- Tag_Data attrs content ->
    --   buildTag "data" attrs $ Right content

    -- Tag_DataList attrs content ->
    --   buildTag "datalist" attrs $ Right content

    -- Tag_DescriptionDetails attrs content ->
    --   buildTag "dd" attrs $ Right content

    -- Tag_DeletedText attrs content ->
    --   buildTag "del" attrs $ Right content

    -- Tag_Details attrs content ->
    --   buildTag "details" attrs $ Right content

    -- Tag_Definition attrs content ->
    --   buildTag "dfn" attrs $ Right content

    -- Tag_Dialog attrs content ->
    --   buildTag "dialog" attrs $ Right content

    -- Tag_Division attrs content ->
    --   buildTag "div" attrs $ Right content

    -- Tag_DescriptionList attrs content ->
    --   buildTag "dl" attrs $ Right content

    -- Tag_DescriptionTerm attrs content ->
    --   buildTag "dt" attrs $ Right content

    -- Tag_Emphasis attrs content ->
    --   buildTag "em" attrs $ Right content

    -- Tag_Embed attrs ->
    --   buildTag "embed" attrs $ Left OmitTag

    -- Tag_Fieldset attrs content ->
    --   buildTag "fieldset" attrs $ Right content

    -- Tag_FigureCaption attrs content ->
    --   buildTag "figcaption" attrs $ Right content

    -- Tag_Figure attrs content ->
    --   buildTag "figure" attrs $ Right content

    -- Tag_Footer attrs content ->
    --   buildTag "footer" attrs $ Right content

    -- Tag_Form attrs content ->
    --   buildTag "form" attrs $ Right content

    -- Tag_H1 attrs content ->
    --   buildTag "h1" attrs $ Right content

    -- Tag_H2 attrs content ->
    --   buildTag "h2" attrs $ Right content

    -- Tag_H3 attrs content ->
    --   buildTag "h3" attrs $ Right content

    -- Tag_H4 attrs content ->
    --   buildTag "h4" attrs $ Right content

    -- Tag_H5 attrs content ->
    --   buildTag "h5" attrs $ Right content

    -- Tag_H6 attrs content ->
    --   buildTag "h6" attrs $ Right content

    -- Tag_Head attrs content ->
    --   buildTag "head" attrs $ Right content

    -- Tag_Header attrs content ->
    --   buildTag "header" attrs $ Right content

    -- Tag_HeadingGroup attrs content ->
    --   buildTag "hgroup" attrs $ Right content

    -- Tag_HorizontalRule attrs ->
    --   buildTag "hr" attrs $ Left OmitTag

    -- Tag_Html attrs content ->
    --   ("<!DOCTYPE html>" :: T.Text) <> buildTag "html" attrs (Right content)

    -- Tag_IdiomaticText attrs content ->
    --   buildTag "i" attrs $ Right content

    -- Tag_IFrame attrs ->
    --   buildTag "iframe" attrs $ Left WithTag

    -- Tag_Image attrs ->
    --   buildTag "img" attrs $ Left OmitTag

    -- Tag_Input attrs ->
    --   buildTag "input" attrs $ Left OmitTag

    -- Tag_InsertedText attrs content ->
    --   buildTag "ins" attrs $ Right content

    -- Tag_KeyboardInput attrs content ->
    --   buildTag "kbd" attrs $ Right content

    -- Tag_Label attrs content ->
    --   buildTag "label" attrs $ Right content

    -- Tag_Legend attrs content ->
    --   buildTag "legend" attrs $ Right content

    -- Tag_ListItem attrs content ->
    --   buildTag "li" attrs $ Right content

    -- Tag_Link attrs ->
    --   buildTag "link" attrs $ Left OmitTag

    -- Tag_Main attrs content ->
    --   buildTag "main" attrs $ Right content

    -- Tag_Map attrs content ->
    --   buildTag "map" attrs $ Right content

    -- Tag_Mark attrs content ->
    --   buildTag "mark" attrs $ Right content

    -- Tag_Menu attrs content ->
    --   buildTag "menu" attrs $ Right content

    -- Tag_Meta attrs ->
    --   buildTag "meta" attrs $ Left OmitTag

    -- Tag_Meter attrs content ->
    --   buildTag "meter" attrs $ Right content

    -- Tag_Nav attrs content ->
    --   buildTag "nav" attrs $ Right content

    -- Tag_NoScript attrs content ->
    --   buildTag "noscript" attrs $ Right content

    -- Tag_Object attrs content ->
    --   buildTag "object" attrs $ Right content

    -- Tag_OrderedList attrs content ->
    --   buildTag "ol" attrs $ Right content

    -- Tag_OptionGroup attrs content ->
    --   buildTag "optgroup" attrs $ Right content

    -- Tag_Option attrs content ->
    --   buildTag "option" attrs $ Right content

    -- Tag_Output attrs content ->
    --   buildTag "output" attrs $ Right content

    -- Tag_Paragraph attrs content ->
    --   buildTag "p" attrs $ Right content

    -- Tag_Picture attrs content ->
    --   buildTag "picture" attrs $ Right content

    -- Tag_PreformattedText attrs content ->
    --   buildTag "pre" attrs $ Right content

    -- Tag_Progress attrs content ->
    --   buildTag "progress" attrs $ Right content

    -- Tag_Quotation attrs content ->
    --   buildTag "q" attrs $ Right content

    -- Tag_RubyParenthesis attrs content ->
    --   buildTag "rp" attrs $ Right content

    -- Tag_RubyText attrs content ->
    --   buildTag "rt" attrs $ Right content

    -- Tag_Ruby attrs content ->
    --   buildTag "ruby" attrs $ Right content

    -- Tag_Strikethrough attrs content ->
    --   buildTag "s" attrs $ Right content

    -- Tag_Sample attrs content ->
    --   buildTag "sample" attrs $ Right content

    -- Tag_Script attrs content ->
    --   buildTag "script" attrs $ Right content

    -- Tag_Search attrs content ->
    --   buildTag "search" attrs $ Right content

    -- Tag_Section attrs content ->
    --   buildTag "section" attrs $ Right content

    -- Tag_Select attrs content ->
    --   buildTag "select" attrs $ Right content

    -- Tag_Slot attrs content ->
    --   buildTag "slot" attrs $ Right content

    -- Tag_SideComment attrs content ->
    --   buildTag "small" attrs $ Right content

    -- Tag_Source attrs ->
    --   buildTag "source" attrs $ Left OmitTag

    -- Tag_Span attrs content ->
    --   buildTag "span" attrs $ Right content

    -- Tag_Strong attrs content ->
    --   buildTag "strong" attrs $ Right content

    -- Tag_Style attrs content ->
    --   buildTag "style" attrs $ Right content

    -- Tag_Subscript attrs content ->
    --   buildTag "sub" attrs $ Right content

    -- Tag_Summary attrs content ->
    --   buildTag "summary" attrs $ Right content

    -- Tag_Superscript attrs content ->
    --   buildTag "sup" attrs $ Right content

    -- Tag_Table attrs content ->
    --   buildTag "table" attrs $ Right content

    -- Tag_TableBody attrs content ->
    --   buildTag "tbody" attrs $ Right content

    -- Tag_TableDataCell attrs content ->
    --   buildTag "td" attrs $ Right content

    -- Tag_ContentTemplate attrs content ->
    --   buildTag "template" attrs $ Right content

    -- Tag_TextArea attrs content ->
    --   buildTag "textarea" attrs $ Right content

    -- Tag_TableFoot attrs content ->
    --   buildTag "tfoot" attrs $ Right content

    -- Tag_TableHeader attrs content ->
    --   buildTag "th" attrs $ Right content

    -- Tag_TableHead attrs content ->
    --   buildTag "thead" attrs $ Right content

    -- Tag_Time attrs content ->
    --   buildTag "time" attrs $ Right content

    -- Tag_Title attrs content ->
    --   buildTag "title" attrs $ Right content

    -- Tag_TableRow attrs content ->
    --   buildTag "tr" attrs $ Right content

    -- Tag_Track attrs ->
    --   buildTag "track" attrs $ Left OmitTag

    -- Tag_Underline attrs content ->
    --   buildTag "u" attrs $ Right content

    -- Tag_UnorderedList attrs content ->
    --   buildTag "ul" attrs $ Right content

    -- Tag_Variable attrs content ->
    --   buildTag "var" attrs $ Right content

    -- Tag_Video attrs content ->
    --   buildTag "video" attrs $ Right content

    -- Tag_WordBreakOpportunity attrs ->
    --   buildTag "wbr" attrs $ Left OmitTag

snocChild :: ValidChild tag parent
          => ChildHTML parent
          -> ChildHTML tag
          -> ChildHTML parent
snocChild _tag _child = undefined
  -- case tag of
    -- Tag_NoElement ->
    --   tag

    -- Tag_Comment _comment ->
    --   tag

    -- Tag_Text _text ->
    --   tag

    -- Tag_Anchor attrs content ->
    --   Tag_Anchor attrs $ content <> [ child ]

    -- Tag_Abbreviation attrs content ->
    --   Tag_Abbreviation attrs $ content <> [ child ]

    -- Tag_ContactAddress attrs content ->
    --   Tag_Abbreviation attrs $ content <> [ child ]

    -- Tag_Area attrs ->
    --   Tag_Area attrs

    -- Tag_Article attrs content ->
    --   Tag_Article attrs $ content <> [ child ]

    -- Tag_Aside attrs content ->
    --   Tag_Aside attrs $ content <> [ child ]

    -- Tag_Audio attrs content ->
    --   Tag_Audio attrs $ content <> [ child ]

    -- Tag_BringAttentionTo attrs content ->
    --   Tag_BringAttentionTo attrs $ content <> [ child ]

    -- Tag_Base attrs ->
    --   tag

    -- Tag_BidirectionalIsolation attrs content ->
    --   Tag_BidirectionalIsolation attrs $ content <> [ child ]

    -- Tag_BidirectionalOverride attrs content ->
    --   Tag_BidirectionalOverride attrs $ content <> [ child ]

    -- Tag_Blockquote attrs content ->
    --   buildTag "blockquote" attrs $ Right content

    -- Tag_Body attrs content ->
    --   buildTag "body" attrs $ Right content

    -- Tag_LineBreak attrs ->
    --   buildTag "br" attrs $ Left OmitTag

    -- Tag_Button attrs content ->
    --   buildTag "button" attrs $ Right content

    -- Tag_Canvas attrs content ->
    --   buildTag "canbvas" attrs $ Right content

    -- Tag_TableCaption attrs content ->
    --   buildTag "caption" attrs $ Right content

    -- Tag_Citation attrs content ->
    --   buildTag "cite" attrs $ Right content

    -- Tag_Code attrs content ->
    --   buildTag "code" attrs $ Right content

    -- Tag_TableColumn attrs ->
    --   buildTag "col" attrs $ Left OmitTag

    -- Tag_TableColumnGroup attrs content ->
    --   buildTag "colgroup" attrs $ Right content

    -- Tag_Data attrs content ->
    --   buildTag "data" attrs $ Right content

    -- Tag_DataList attrs content ->
    --   buildTag "datalist" attrs $ Right content

    -- Tag_DescriptionDetails attrs content ->
    --   buildTag "dd" attrs $ Right content

    -- Tag_DeletedText attrs content ->
    --   buildTag "del" attrs $ Right content

    -- Tag_Details attrs content ->
    --   buildTag "details" attrs $ Right content

    -- Tag_Definition attrs content ->
    --   buildTag "dfn" attrs $ Right content

    -- Tag_Dialog attrs content ->
    --   buildTag "dialog" attrs $ Right content

    -- Tag_Division attrs content ->
    --   buildTag "div" attrs $ Right content

    -- Tag_DescriptionList attrs content ->
    --   buildTag "dl" attrs $ Right content

    -- Tag_DescriptionTerm attrs content ->
    --   buildTag "dt" attrs $ Right content

    -- Tag_Emphasis attrs content ->
    --   buildTag "em" attrs $ Right content

    -- Tag_Embed attrs ->
    --   buildTag "embed" attrs $ Left OmitTag

    -- Tag_Fieldset attrs content ->
    --   buildTag "fieldset" attrs $ Right content

    -- Tag_FigureCaption attrs content ->
    --   buildTag "figcaption" attrs $ Right content

    -- Tag_Figure attrs content ->
    --   buildTag "figure" attrs $ Right content

    -- Tag_Footer attrs content ->
    --   buildTag "footer" attrs $ Right content

    -- Tag_Form attrs content ->
    --   buildTag "form" attrs $ Right content

    -- Tag_H1 attrs content ->
    --   buildTag "h1" attrs $ Right content

    -- Tag_H2 attrs content ->
    --   buildTag "h2" attrs $ Right content

    -- Tag_H3 attrs content ->
    --   buildTag "h3" attrs $ Right content

    -- Tag_H4 attrs content ->
    --   buildTag "h4" attrs $ Right content

    -- Tag_H5 attrs content ->
    --   buildTag "h5" attrs $ Right content

    -- Tag_H6 attrs content ->
    --   buildTag "h6" attrs $ Right content

    -- Tag_Head attrs content ->
    --   buildTag "head" attrs $ Right content

    -- Tag_Header attrs content ->
    --   buildTag "header" attrs $ Right content

    -- Tag_HeadingGroup attrs content ->
    --   buildTag "hgroup" attrs $ Right content

    -- Tag_HorizontalRule attrs ->
    --   buildTag "hr" attrs $ Left OmitTag

    -- Tag_Html attrs content ->
    --   ("<!DOCTYPE html>" :: T.Text) <> buildTag "html" attrs (Right content)

    -- Tag_IdiomaticText attrs content ->
    --   buildTag "i" attrs $ Right content

    -- Tag_IFrame attrs ->
    --   buildTag "iframe" attrs $ Left WithTag

    -- Tag_Image attrs ->
    --   buildTag "img" attrs $ Left OmitTag

    -- Tag_Input attrs ->
    --   buildTag "input" attrs $ Left OmitTag

    -- Tag_InsertedText attrs content ->
    --   buildTag "ins" attrs $ Right content

    -- Tag_KeyboardInput attrs content ->
    --   buildTag "kbd" attrs $ Right content

    -- Tag_Label attrs content ->
    --   buildTag "label" attrs $ Right content

    -- Tag_Legend attrs content ->
    --   buildTag "legend" attrs $ Right content

    -- Tag_ListItem attrs content ->
    --   buildTag "li" attrs $ Right content

    -- Tag_Link attrs ->
    --   buildTag "link" attrs $ Left OmitTag

    -- Tag_Main attrs content ->
    --   buildTag "main" attrs $ Right content

    -- Tag_Map attrs content ->
    --   buildTag "map" attrs $ Right content

    -- Tag_Mark attrs content ->
    --   buildTag "mark" attrs $ Right content

    -- Tag_Menu attrs content ->
    --   buildTag "menu" attrs $ Right content

    -- Tag_Meta attrs ->
    --   buildTag "meta" attrs $ Left OmitTag

    -- Tag_Meter attrs content ->
    --   buildTag "meter" attrs $ Right content

    -- Tag_Nav attrs content ->
    --   buildTag "nav" attrs $ Right content

    -- Tag_NoScript attrs content ->
    --   buildTag "noscript" attrs $ Right content

    -- Tag_Object attrs content ->
    --   buildTag "object" attrs $ Right content

    -- Tag_OrderedList attrs content ->
    --   buildTag "ol" attrs $ Right content

    -- Tag_OptionGroup attrs content ->
    --   buildTag "optgroup" attrs $ Right content

    -- Tag_Option attrs content ->
    --   buildTag "option" attrs $ Right content

    -- Tag_Output attrs content ->
    --   buildTag "output" attrs $ Right content

    -- Tag_Paragraph attrs content ->
    --   buildTag "p" attrs $ Right content

    -- Tag_Picture attrs content ->
    --   buildTag "picture" attrs $ Right content

    -- Tag_PreformattedText attrs content ->
    --   buildTag "pre" attrs $ Right content

    -- Tag_Progress attrs content ->
    --   buildTag "progress" attrs $ Right content

    -- Tag_Quotation attrs content ->
    --   buildTag "q" attrs $ Right content

    -- Tag_RubyParenthesis attrs content ->
    --   buildTag "rp" attrs $ Right content

    -- Tag_RubyText attrs content ->
    --   buildTag "rt" attrs $ Right content

    -- Tag_Ruby attrs content ->
    --   buildTag "ruby" attrs $ Right content

    -- Tag_Strikethrough attrs content ->
    --   buildTag "s" attrs $ Right content

    -- Tag_Sample attrs content ->
    --   buildTag "sample" attrs $ Right content

    -- Tag_Script attrs content ->
    --   buildTag "script" attrs $ Right content

    -- Tag_Search attrs content ->
    --   buildTag "search" attrs $ Right content

    -- Tag_Section attrs content ->
    --   buildTag "section" attrs $ Right content

    -- Tag_Select attrs content ->
    --   buildTag "select" attrs $ Right content

    -- Tag_Slot attrs content ->
    --   buildTag "slot" attrs $ Right content

    -- Tag_SideComment attrs content ->
    --   buildTag "small" attrs $ Right content

    -- Tag_Source attrs ->
    --   buildTag "source" attrs $ Left OmitTag

    -- Tag_Span attrs content ->
    --   buildTag "span" attrs $ Right content

    -- Tag_Strong attrs content ->
    --   buildTag "strong" attrs $ Right content

    -- Tag_Style attrs content ->
    --   buildTag "style" attrs $ Right content

    -- Tag_Subscript attrs content ->
    --   buildTag "sub" attrs $ Right content

    -- Tag_Summary attrs content ->
    --   buildTag "summary" attrs $ Right content

    -- Tag_Superscript attrs content ->
    --   buildTag "sup" attrs $ Right content

    -- Tag_Table attrs content ->
    --   buildTag "table" attrs $ Right content

    -- Tag_TableBody attrs content ->
    --   buildTag "tbody" attrs $ Right content

    -- Tag_TableDataCell attrs content ->
    --   buildTag "td" attrs $ Right content

    -- Tag_ContentTemplate attrs content ->
    --   buildTag "template" attrs $ Right content

    -- Tag_TextArea attrs content ->
    --   buildTag "textarea" attrs $ Right content

    -- Tag_TableFoot attrs content ->
    --   buildTag "tfoot" attrs $ Right content

    -- Tag_TableHeader attrs content ->
    --   buildTag "th" attrs $ Right content

    -- Tag_TableHead attrs content ->
    --   buildTag "thead" attrs $ Right content

    -- Tag_Time attrs content ->
    --   buildTag "time" attrs $ Right content

    -- Tag_Title attrs content ->
    --   buildTag "title" attrs $ Right content

    -- Tag_TableRow attrs content ->
    --   buildTag "tr" attrs $ Right content

    -- Tag_Track attrs ->
    --   buildTag "track" attrs $ Left OmitTag

    -- Tag_Underline attrs content ->
    --   buildTag "u" attrs $ Right content

    -- Tag_UnorderedList attrs content ->
    --   buildTag "ul" attrs $ Right content

    -- Tag_Variable attrs content ->
    --   buildTag "var" attrs $ Right content

    -- Tag_Video attrs content ->
    --   buildTag "video" attrs $ Right content

    -- Tag_WordBreakOpportunity attrs ->
    --   buildTag "wbr" attrs $ Left OmitTag

