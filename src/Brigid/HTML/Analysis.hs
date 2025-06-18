{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brigid.HTML.Analysis
  ( totalNodes
  , maxDepth
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty.Extra (maximum1)

import Brigid.HTML.Elements.Internal (ChildHTML (..))

newtype Count =
  Count
    { unCount :: Int
    } deriving (Eq, Num, Ord)

instance Semigroup Count where
  (<>) = (+)

instance Monoid Count where
  mempty = 0

totalNodes :: ChildHTML parent grandparent -> Int
totalNodes =
  unCount . countNodes

countNodes :: ChildHTML parent grandparent -> Count
countNodes element =
  case element of
    Tag_NoElement ->
      Count 0

    Tag_Comment _comment ->
      Count 1

    Tag_Text _content ->
      Count 0

    Tag_Entity _entity ->
      Count 0

    Tag_RawHTML _content ->
      Count 1

    Tag_CustomHTML _elemName _attrs eiCloserOrContent ->
      case eiCloserOrContent of
        Left _tag -> Count 1
        Right content -> Count 1 + foldMap countNodes content

    Tag_Anchor _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Abbreviation _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_ContactAddress _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Area _attrs ->
      Count 1

    Tag_Article _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Aside _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Audio _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_BringAttentionTo _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Base _attrs ->
      Count 1

    Tag_BidirectionalIsolation _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_BidirectionalOverride _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Blockquote _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Body _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_LineBreak _attrs ->
      Count 1

    Tag_Button _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Canvas _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TableCaption _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Citation _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Code _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TableColumn _attrs ->
      Count 1

    Tag_TableColumnGroup _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Data _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_DataList _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_DescriptionDetails _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_DeletedText _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Details _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Definition _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Dialog _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Division _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_DescriptionList _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_DescriptionTerm _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Emphasis _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Embed _attrs ->
      Count 1

    Tag_Fieldset _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_FigureCaption _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Figure _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Footer _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Form _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_H1 _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_H2 _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_H3 _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_H4 _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_H5 _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_H6 _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Head _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Header _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_HeadingGroup _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_HorizontalRule _attrs ->
      Count 1

    Tag_Html _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_IdiomaticText _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_IFrame _attrs ->
      Count 1

    Tag_Image _attrs ->
      Count 1

    Tag_Input _attrs ->
      Count 1

    Tag_InputButton _attrs ->
      Count 1

    Tag_InputCheckbox _attrs ->
      Count 1

    Tag_InputColor _attrs ->
      Count 1

    Tag_InputDate _attrs ->
      Count 1

    Tag_InputDatetimeLocal _attrs ->
      Count 1

    Tag_InputEmail _attrs ->
      Count 1

    Tag_InputFile _attrs ->
      Count 1

    Tag_InputHidden _attrs ->
      Count 1

    Tag_InputImage _attrs ->
      Count 1

    Tag_InputMonth _attrs ->
      Count 1

    Tag_InputNumber _attrs ->
      Count 1

    Tag_InputPassword _attrs ->
      Count 1

    Tag_InputRadio _attrs ->
      Count 1

    Tag_InputRange _attrs ->
      Count 1

    Tag_InputReset _attrs ->
      Count 1

    Tag_InputSearch _attrs ->
      Count 1

    Tag_InputSubmit _attrs ->
      Count 1

    Tag_InputTel _attrs ->
      Count 1

    Tag_InputText _attrs ->
      Count 1

    Tag_InputTime _attrs ->
      Count 1

    Tag_InputUrl _attrs ->
      Count 1

    Tag_InputWeek _attrs ->
      Count 1

    Tag_InsertedText _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_KeyboardInput _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Label _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Legend _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_ListItem _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Link _attrs ->
      Count 1

    Tag_Main _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Map _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Mark _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Menu _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Meta _attrs ->
      Count 1

    Tag_Meter _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Nav _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_NoScript _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Object _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_OrderedList _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_OptionGroup _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Option _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Output _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Paragraph _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Picture _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_PreformattedText _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Progress _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Quotation _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_RubyParenthesis _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_RubyText _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Ruby _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Strikethrough _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Sample _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Script _attrs _mbScript ->
      Count 1

    Tag_Search _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Section _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Select _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Slot _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_SideComment _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Source _attrs ->
      Count 1

    Tag_Span _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Strong _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Style _attrs _content ->
      Count 1

    Tag_Subscript _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Summary _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Superscript _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Table _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TableBody _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TableDataCell _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_ContentTemplate _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TextArea _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TableFoot _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TableHeader _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TableHead _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Time _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Title _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_TableRow _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Track _attrs ->
      Count 1

    Tag_Underline _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_UnorderedList _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Variable _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_Video _attrs content ->
      Count 1 + foldMap countNodes content

    Tag_WordBreakOpportunity _attrs ->
      Count 1


maxDepth :: ChildHTML parent grandparent -> Int
maxDepth element =
  case element of
    Tag_NoElement ->
      0

    Tag_Comment _comment ->
      1

    Tag_Text _content ->
      0

    Tag_Entity _entity ->
      0

    Tag_RawHTML _content ->
      1

    Tag_CustomHTML _elemName _attrs eiCloserOrContent ->
      case eiCloserOrContent of
        Left _closer ->
          1

        Right content ->
          1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Anchor _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Abbreviation _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_ContactAddress _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Area _attrs ->
      1

    Tag_Article _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Aside _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Audio _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_BringAttentionTo _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Base _attrs ->
      1

    Tag_BidirectionalIsolation _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_BidirectionalOverride _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Blockquote _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Body _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_LineBreak _attrs ->
      1

    Tag_Button _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Canvas _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TableCaption _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Citation _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Code _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TableColumn _attrs ->
      1

    Tag_TableColumnGroup _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Data _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_DataList _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_DescriptionDetails _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_DeletedText _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Details _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Definition _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Dialog _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Division _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_DescriptionList _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_DescriptionTerm _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Emphasis _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Embed _attrs ->
      1

    Tag_Fieldset _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_FigureCaption _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Figure _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Footer _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Form _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_H1 _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_H2 _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_H3 _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_H4 _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_H5 _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_H6 _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Head _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Header _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_HeadingGroup _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_HorizontalRule _attrs ->
      1

    Tag_Html _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_IdiomaticText _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_IFrame _attrs ->
      1

    Tag_Image _attrs ->
      1

    Tag_Input _attrs ->
      1

    Tag_InputButton _attrs ->
      1

    Tag_InputCheckbox _attrs ->
      1

    Tag_InputColor _attrs ->
      1

    Tag_InputDate _attrs ->
      1

    Tag_InputDatetimeLocal _attrs ->
      1

    Tag_InputEmail _attrs ->
      1

    Tag_InputFile _attrs ->
      1

    Tag_InputHidden _attrs ->
      1

    Tag_InputImage _attrs ->
      1

    Tag_InputMonth _attrs ->
      1

    Tag_InputNumber _attrs ->
      1

    Tag_InputPassword _attrs ->
      1

    Tag_InputRadio _attrs ->
      1

    Tag_InputRange _attrs ->
      1

    Tag_InputReset _attrs ->
      1

    Tag_InputSearch _attrs ->
      1

    Tag_InputSubmit _attrs ->
      1

    Tag_InputTel _attrs ->
      1

    Tag_InputText _attrs ->
      1

    Tag_InputTime _attrs ->
      1

    Tag_InputUrl _attrs ->
      1

    Tag_InputWeek _attrs ->
      1

    Tag_InsertedText _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_KeyboardInput _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Label _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Legend _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_ListItem _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Link _attrs ->
      1

    Tag_Main _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Map _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Mark _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Menu _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Meta _attrs ->
      1

    Tag_Meter _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Nav _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_NoScript _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Object _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_OrderedList _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_OptionGroup _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Option _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Output _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Paragraph _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Picture _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_PreformattedText _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Progress _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Quotation _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_RubyParenthesis _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_RubyText _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Ruby _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Strikethrough _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Sample _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Script _attrs _mbScript ->
      1

    Tag_Search _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Section _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Select _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Slot _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_SideComment _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Source _attrs ->
      1

    Tag_Span _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Strong _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Style _attrs _content ->
      1

    Tag_Subscript _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Summary _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Superscript _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Table _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TableBody _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TableDataCell _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_ContentTemplate _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TextArea _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TableFoot _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TableHeader _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TableHead _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Time _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Title _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_TableRow _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Track _attrs ->
      1

    Tag_Underline _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_UnorderedList _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Variable _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_Video _attrs content ->
      1 + maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty content)

    Tag_WordBreakOpportunity _attrs ->
      1
