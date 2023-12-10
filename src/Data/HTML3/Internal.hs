{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- This is to prevent warnings for the non-matching case in the third `Elem`
-- instance. GHC claims that this is a redundant constraint, but attributes
-- will fail to compile without it. I believe this is related to a known GHC
-- bug.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Data.HTML3.Internal
--( HTML
--    ( Tag_Comment
--    , Tag_Text
   -- , Tag_Anchor
   -- , Tag_Abbreviation
   -- , Tag_Address
   -- , Tag_Area
   -- , Tag_Article
   -- , Tag_Aside
   -- , Tag_Audio
   -- , Tag_Bold
   -- , Tag_Base
   -- , Tag_BiDirectionalIsolation
   -- , Tag_BiDirectionalOverride
   -- , Tag_Blockquote
   -- , Tag_Body
   -- , Tag_Break
   -- , Tag_Button
   -- , Tag_Canvas
   -- , Tag_Caption
   -- , Tag_Cite
   -- , Tag_Code
   -- , Tag_Column
   -- , Tag_ColumnGroup
   -- , Tag_Data
   -- , Tag_DataList
   -- , Tag_Description
   -- , Tag_Deletion
   -- , Tag_Details
   -- , Tag_Definition
   -- , Tag_Dialog
   -- , Tag_Division
   -- , Tag_DescriptionList
   -- , Tag_DescriptionTerm
   -- , Tag_Emphasis
   -- , Tag_Embed
   -- , Tag_Fieldset
   -- , Tag_FigureCaption
   -- , Tag_Figure
   -- , Tag_Footer
   -- , Tag_Form
   -- , Tag_H1
   -- , Tag_H2
   -- , Tag_H3
   -- , Tag_H4
   -- , Tag_H5
   -- , Tag_H6
   -- , Tag_Head
   -- , Tag_Header
   -- , Tag_HeadingGroup
   -- , Tag_HorizontalRule
   -- , Tag_Html
   -- , Tag_Italic
   -- , Tag_IFrame
   -- , Tag_Image
   -- , Tag_Input
   -- , Tag_Insertion
   -- , Tag_KeyboardInput
   -- , Tag_Label
   -- , Tag_Legend
   -- , Tag_ListItem
   -- , Tag_Link
   -- , Tag_Main
   -- , Tag_Map
   -- , Tag_Mark
   -- , Tag_Menu
   -- , Tag_Meta
   -- , Tag_Meter
   -- , Tag_Nav
   -- , Tag_NoScript
   -- , Tag_Object
   -- , Tag_OrderedList
   -- , Tag_OptionGroup
   -- , Tag_Option
   -- , Tag_Output
   -- , Tag_Paragraph
   -- , Tag_Picture
   -- , Tag_PreformattedText
   -- , Tag_Progress
   -- , Tag_Quotation
   -- , Tag_RubyParenthesis
   -- , Tag_RubyText
   -- , Tag_Ruby
   -- , Tag_Sample
   -- , Tag_Script
   -- , Tag_Search
   -- , Tag_Section
   -- , Tag_Select
   -- , Tag_Slot
   -- , Tag_Small
   -- , Tag_Source
   -- , Tag_Span
   -- , Tag_Strikethrough
   -- , Tag_Strong
   -- , Tag_Style
   -- , Tag_Subscript
   -- , Tag_Summary
   -- , Tag_Superscript
   -- , Tag_Table
   -- , Tag_TableBody
   -- , Tag_TableDataCell
   -- , Tag_TableFooter
   -- , Tag_TableHeaderCell
   -- , Tag_TableHeader
   -- , Tag_TableRow
   -- , Tag_Template
   -- , Tag_TextArea
   -- , Tag_Time
   -- , Tag_Title
   -- , Tag_Track
   -- , Tag_Underline
   -- , Tag_UnorderedList
   -- , Tag_Variable
   -- , Tag_Video
   -- , Tag_WordBreakOpportunity
   -- )
  (
  ) where

-- import Data.Kind (Constraint, Type)
-- import Data.Text qualified as T

-- import Data.HTML3.Attributes.AttributeType qualified as A
-- import Data.HTML3.Elements.TagType qualified as E

-- type family ValidElementsFor (attribute :: A.AttributeType) :: [E.TagType] where
--   ValidElementsFor A.Id    = ['E.Division, 'E.Span]
--  ValidElementsFor A.Class = '[E.Division]

  {-
-- Helper type class to check if a type is in a type-level list
class Contains (list :: [E.TagType]) (eType :: E.TagType)
instance Contains (eType ': es) eType
instance {-# OVERLAPPABLE #-} Contains es eType => Contains (e ': es) eType

-- Type class for checking attribute validity
class IsValidAttribute (eType :: E.TagType) (aType :: A.AttributeType)

data Attribute (tag :: E.TagType) where
  Attr_Id    :: Contains (ValidElementsFor 'A.Id)    tag => T.Text -> Attribute tag
  Attr_Class :: Contains (ValidElementsFor 'A.Class) tag => T.Text -> Attribute tag
-}





-- type family ValidChildrenOf (eType :: E.TagType) :: [E.TagType] where
--   ValidChildrenOf E.Division = ['E.Division, 'E.Span]
--   ValidChildrenOf E.Span     = '[E.Text]

-- class AllValidChildren (parent :: E.TagType) (children :: [*])
-- instance AllValidChildren parent '[]
-- instance (IsValidChild parent child, AllValidChildren parent children) => AllValidChildren parent (child ': children)

-- class IsValidChild (parent :: E.TagType) (child :: *)
-- instance Contains (ValidChildrenOf parent) (TypeOf child) => IsValidChild parent child

-- type family TypeOf (html :: *) :: E.TagType
-- type instance TypeOf (HTML eType) = eType

-- type family ValidChildren (eType :: E.TagType) :: [E.TagType] where
--   ValidChildren E.Division = '[E.Span, E.Text]
--   ValidChildren E.Span     = '[E.Text]





-- type family IsInList (t :: E.TagType) (ts :: [E.TagType]) :: Bool where
--     IsInList t '[]       = 'False
--     IsInList t (t ': ts) = 'True
--     IsInList t (x ': ts) = IsInList t ts

-- type family ValidChildList (valids :: [E.TagType]) (children :: [*]) :: Bool where
--     ValidChildList valids '[] =
--       'True

--     ValidChildList valids (child ': children) =
--         And (IsInList (TypeOf child) valids) (ValidChildList valids children)

-- type family And (x :: Bool) (y :: Bool) :: Bool where
--     And 'True 'True = 'True
--     And x     y     = 'False

-- type family TypeOf (html :: *) :: E.TagType
-- type instance TypeOf (HTML eType) = eType

-- exampleDiv :: HTML E.Division
-- exampleDiv =
--   Tag_Division [ Attr_Id "div1", Attr_Class "fancy"]
--     [ Tag_Span [ Attr_Class "text" ]
--         []
--     , Tag_Text "Hello"
--     ]





-- type family ValidChildren (eType :: E.TagType) :: [E.TagType]
-- type instance ValidChildren E.Division = '[E.Span, E.Text]
-- type instance ValidChildren E.Span = '[E.Text]

-- type family TypeOf (html :: Type) :: E.TagType
-- type instance TypeOf (HTML eType) = eType

-- class In (eType :: E.TagType) (es :: [E.TagType])
-- instance In eType (eType ': es)
-- instance In eType es => In eType (x ': es)

-- class All (c :: E.TagType -> Constraint) (es :: [Type])
-- instance All c '[]
-- instance (c (TypeOf e), All c es) => All c (e ': es)

-- exampleDiv :: HTML E.Division
-- exampleDiv =
--   Tag_Division [ Attr_Id "div1", Attr_Class "fancy"]
--     [ Tag_Span [ Attr_Class "text" ]
--         []
--     , Tag_Text "Hello"
--     ]







  {-
data HTML (tag :: E.TagType) where
  Tag_Comment                :: T.Text                                                                               -> HTML E.Comment
  Tag_Text                   :: T.Text                                                                               -> HTML E.Text

  -- Test version of div
  Tag_Division :: All (In ValidChildren E.Division) children
               => [Attribute E.Division]
               -> children -- [forall child. Contains (ValidChildrenOf E.Division) child => HTML child]
               -> HTML E.Division

  Tag_Span :: All (In ValidChildren E.Span) children
           => [Attribute E.Span]
           -> children -- [forall child. Contains (ValidChildrenOf E.Span) child => HTML child]
           -> HTML E.Span

-- type Branch tag children =
--   ValidChildList (ValidChildren tag) children ~ 'True
--     => [Attribute tag]
--     -> children
--     -> HTML tag

-- type Leaf tag =
--   [Attribute tag] -> HTML tag

-- Tag_Anchor :: Branch E.Anchor children
-- Tag_Image  :: Leaf E.Image

--Tag_Anchor                 :: [ValidAttributeOf 'Anchor]                 -> [ValidChildOf 'Anchor]                 -> HTML Anchor
--Tag_Abbreviation           :: [ValidAttributeOf 'Abbreviation]           -> [ValidChildOf 'Abbreviation]           -> HTML Abbreviation
--Tag_Address                :: [ValidAttributeOf 'Address]                -> [ValidChildOf 'Address]                -> HTML Address
--Tag_Area                   :: [ValidAttributeOf 'Area]                                                             -> HTML Area
--Tag_Article                :: [ValidAttributeOf 'Article]                -> [ValidChildOf 'Article]                -> HTML Article
--Tag_Aside                  :: [ValidAttributeOf 'Aside]                  -> [ValidChildOf 'Aside]                  -> HTML Aside
--Tag_Audio                  :: [ValidAttributeOf 'Audio]                  -> [ValidChildOf 'Audio]                  -> HTML Audio
--Tag_Bold                   :: [ValidAttributeOf 'Bold]                   -> [ValidChildOf 'Bold]                   -> HTML Bold
--Tag_Base                   :: [ValidAttributeOf 'Base]                                                             -> HTML Base
--Tag_BiDirectionalIsolation :: [ValidAttributeOf 'BiDirectionalIsolation] -> [ValidChildOf 'BiDirectionalIsolation] -> HTML BiDirectionalIsolation
--Tag_BiDirectionalOverride  :: [ValidAttributeOf 'BiDirectionalOverride]  -> [ValidChildOf 'BiDirectionalOverride]  -> HTML BiDirectionalOverride
--Tag_Blockquote             :: [ValidAttributeOf 'Blockquote]             -> [ValidChildOf 'Blockquote]             -> HTML Blockquote
--Tag_Body                   :: [ValidAttributeOf 'Body]                   -> [ValidChildOf 'Body]                   -> HTML Body
--Tag_Break                  :: [ValidAttributeOf 'Break]                                                            -> HTML Break
--Tag_Button                 :: [ValidAttributeOf 'Button]                 -> [ValidChildOf 'Button]                 -> HTML Button
--Tag_Canvas                 :: [ValidAttributeOf 'Canvas]                 -> [ValidChildOf 'Canvas]                 -> HTML Canvas
--Tag_Caption                :: [ValidAttributeOf 'Caption]                -> [ValidChildOf 'Caption]                -> HTML Caption
--Tag_Cite                   :: [ValidAttributeOf 'Cite]                   -> [ValidChildOf 'Cite]                   -> HTML Cite
--Tag_Code                   :: [ValidAttributeOf 'Code]                   -> [ValidChildOf 'Code]                   -> HTML Code
--Tag_Column                 :: [ValidAttributeOf 'Column]                                                           -> HTML Column
--Tag_ColumnGroup            :: [ValidAttributeOf 'ColumnGroup]            -> [ValidChildOf 'ColumnGroup]            -> HTML ColumnGroup
--Tag_Data                   :: [ValidAttributeOf 'Data]                   -> [ValidChildOf 'Data]                   -> HTML Data
--Tag_DataList               :: [ValidAttributeOf 'DataList]               -> [ValidChildOf 'DataList]               -> HTML DataList
--Tag_Description            :: [ValidAttributeOf 'Description]            -> [ValidChildOf 'Description]            -> HTML Description
--Tag_Deletion               :: [ValidAttributeOf 'Deletion]               -> [ValidChildOf 'Deletion]               -> HTML Deletion
--Tag_Details                :: [ValidAttributeOf 'Details]                -> [ValidChildOf 'Details]                -> HTML Details
--Tag_Definition             :: [ValidAttributeOf 'Definition]             -> [ValidChildOf 'Definition]             -> HTML Definition
--Tag_Dialog                 :: [ValidAttributeOf 'Dialog]                 -> [ValidChildOf 'Dialog]                 -> HTML Dialog
--Tag_Division               :: [ValidAttributeOf 'Division]               -> [ValidChildOf 'Division]               -> HTML Division
--Tag_DescriptionList        :: [ValidAttributeOf 'DescriptionList]        -> [ValidChildOf 'DescriptionList]        -> HTML DescriptionList
--Tag_DescriptionTerm        :: [ValidAttributeOf 'DescriptionTerm]        -> [ValidChildOf 'DescriptionTerm]        -> HTML DescriptionTerm
--Tag_Emphasis               :: [ValidAttributeOf 'Emphasis]               -> [ValidChildOf 'Emphasis]               -> HTML Emphasis
--Tag_Embed                  :: [ValidAttributeOf 'Embed]                                                            -> HTML Embed
--Tag_Fieldset               :: [ValidAttributeOf 'Fieldset]               -> [ValidChildOf 'Fieldset]               -> HTML Fieldset
--Tag_FigureCaption          :: [ValidAttributeOf 'FigureCaption]          -> [ValidChildOf 'FigureCaption]          -> HTML FigureCaption
--Tag_Figure                 :: [ValidAttributeOf 'Figure]                 -> [ValidChildOf 'Figure]                 -> HTML Figure
--Tag_Footer                 :: [ValidAttributeOf 'Footer]                 -> [ValidChildOf 'Foote]                  -> HTML Footer
--Tag_Form                   :: [ValidAttributeOf 'Form]                   -> [ValidChildOf 'Form]                   -> HTML Form
--Tag_H1                     :: [ValidAttributeOf 'H1]                     -> [ValidChildOf 'H1]                     -> HTML H1
--Tag_H2                     :: [ValidAttributeOf 'H2]                     -> [ValidChildOf 'H2]                     -> HTML H2
--Tag_H3                     :: [ValidAttributeOf 'H3]                     -> [ValidChildOf 'H3]                     -> HTML H3
--Tag_H4                     :: [ValidAttributeOf 'H4]                     -> [ValidChildOf 'H4]                     -> HTML H4
--Tag_H5                     :: [ValidAttributeOf 'H5]                     -> [ValidChildOf 'H5]                     -> HTML H5
--Tag_H6                     :: [ValidAttributeOf 'H6]                     -> [ValidChildOf 'H6]                     -> HTML H6
--Tag_Head                   :: [ValidAttributeOf 'Head]                   -> [ValidChildOf 'Head]                   -> HTML Head
--Tag_Header                 :: [ValidAttributeOf 'Header]                 -> [ValidChildOf 'Header]                 -> HTML Header
--Tag_HeadingGroup           :: [ValidAttributeOf 'HeadingGroup]           -> [ValidChildOf 'HeadingGroup]           -> HTML HeadingGroup
--Tag_HorizontalRule         :: [ValidAttributeOf 'HorizontalRule]                                                   -> HTML HorizontalRule
--Tag_Html                   :: [ValidAttributeOf 'Html]                   -> [ValidChildOf 'Html]                   -> HTML Html
--Tag_Italic                 :: [ValidAttributeOf 'Italic]                 -> [ValidChildOf 'Italic]                 -> HTML Italic
--Tag_IFrame                 :: [ValidAttributeOf 'IFrame]                 -> [ValidChildOf 'IFrame]                 -> HTML IFrame
--Tag_Image                  :: [ValidAttributeOf 'Image]                                                            -> HTML Image
--Tag_Input                  :: [ValidAttributeOf 'Input]                                                            -> HTML Input
--Tag_Insertion              :: [ValidAttributeOf 'Insertion]              -> [ValidChildOf 'Insertion]              -> HTML Insertion
--Tag_KeyboardInput          :: [ValidAttributeOf 'KeyboardInput]          -> [ValidChildOf 'KeyboardInput]          -> HTML KeyboardInput
--Tag_Label                  :: [ValidAttributeOf 'Label]                  -> [ValidChildOf 'Label]                  -> HTML Label
--Tag_Legend                 :: [ValidAttributeOf 'Legend]                 -> [ValidChildOf 'Legend]                 -> HTML Legend
--Tag_ListItem               :: [ValidAttributeOf 'ListItem]               -> [ValidChildOf 'ListItem]               -> HTML ListItem
--Tag_Link                   :: [ValidAttributeOf 'Link]                                                             -> HTML Link
--Tag_Main                   :: [ValidAttributeOf 'Main]                   -> [ValidChildOf 'Main]                   -> HTML Main
--Tag_Map                    :: [ValidAttributeOf 'Map]                    -> [ValidChildOf 'Map]                    -> HTML Map
--Tag_Mark                   :: [ValidAttributeOf 'Mark]                   -> [ValidChildOf 'Mark]                   -> HTML Mark
--Tag_Menu                   :: [ValidAttributeOf 'Menu]                   -> [ValidChildOf 'Menu]                   -> HTML Menu
--Tag_Meta                   :: [ValidAttributeOf 'Meta]                                                             -> HTML Meta
--Tag_Meter                  :: [ValidAttributeOf 'Meter]                  -> [ValidChildOf 'Meter]                  -> HTML Meter
--Tag_Nav                    :: [ValidAttributeOf 'Nav]                    -> [ValidChildOf 'Nav]                    -> HTML Nav
--Tag_NoScript               :: [ValidAttributeOf 'NoScript]               -> [ValidChildOf 'NoScript]               -> HTML NoScript
--Tag_Object                 :: [ValidAttributeOf 'Object]                 -> [ValidChildOf 'Object]                 -> HTML Object
--Tag_OrderedList            :: [ValidAttributeOf 'OrderedList]            -> [ValidChildOf 'OrderedList]            -> HTML OrderedList
--Tag_OptionGroup            :: [ValidAttributeOf 'OptionGroup]            -> [ValidChildOf 'OptionGroup]            -> HTML OptionGroup
--Tag_Option                 :: [ValidAttributeOf 'Option]                 -> [ValidChildOf 'Option]                 -> HTML Option
--Tag_Output                 :: [ValidAttributeOf 'Output]                 -> [ValidChildOf 'Output]                 -> HTML Output
--Tag_Paragraph              :: [ValidAttributeOf 'Paragraph]              -> [ValidChildOf 'Paragraph]              -> HTML Paragraph
--Tag_Picture                :: [ValidAttributeOf 'Picture]                -> [ValidChildOf 'Picture]                -> HTML Picture
--Tag_PreformattedText       :: [ValidAttributeOf 'PreformattedText]       -> [ValidChildOf 'PreformattedText]       -> HTML PreformattedText
--Tag_Progress               :: [ValidAttributeOf 'Progress]               -> [ValidChildOf 'Progress]               -> HTML Progress
--Tag_Quotation              :: [ValidAttributeOf 'Quotation]              -> [ValidChildOf 'Quotation]              -> HTML Quotation
--Tag_RubyParenthesis        :: [ValidAttributeOf 'RubyParenthesis]        -> [ValidChildOf 'RubyParenthesis]        -> HTML RubyParenthesis
--Tag_RubyText               :: [ValidAttributeOf 'RubyText]               -> [ValidChildOf 'RubyText]               -> HTML RubyText
--Tag_Ruby                   :: [ValidAttributeOf 'Ruby]                   -> [ValidChildOf 'Ruby]                   -> HTML Ruby
--Tag_Sample                 :: [ValidAttributeOf 'Sample]                 -> [ValidChildOf 'Sample]                 -> HTML Sample
--Tag_Script                 :: [ValidAttributeOf 'Script]                 -> [ValidChildOf 'Script]                 -> HTML Script
--Tag_Search                 :: [ValidAttributeOf 'Search]                 -> [ValidChildOf 'Search]                 -> HTML Search
--Tag_Section                :: [ValidAttributeOf 'Section]                -> [ValidChildOf 'Section]                -> HTML Section
--Tag_Select                 :: [ValidAttributeOf 'Select]                 -> [ValidChildOf 'Select]                 -> HTML Select
--Tag_Slot                   :: [ValidAttributeOf 'Slot]                   -> [ValidChildOf 'Slot]                   -> HTML Slot
--Tag_Small                  :: [ValidAttributeOf 'Small]                  -> [ValidChildOf 'Small]                  -> HTML Small
--Tag_Source                 :: [ValidAttributeOf 'Source]                                                           -> HTML Source
--Tag_Span                   :: [ValidAttributeOf 'Span]                   -> [ValidChildOf 'Span]                   -> HTML Span
--Tag_Strikethrough          :: [ValidAttributeOf 'Strikethrough]          -> [ValidChildOf 'Strikethrough]          -> HTML Strikethrough
--Tag_Strong                 :: [ValidAttributeOf 'Strong]                 -> [ValidChildOf 'Strong]                 -> HTML Strong
--Tag_Style                  :: [ValidAttributeOf 'Style]                  -> [ValidChildOf 'Style]                  -> HTML Style
--Tag_Subscript              :: [ValidAttributeOf 'Subscript]              -> [ValidChildOf 'Subscript]              -> HTML Subscript
--Tag_Summary                :: [ValidAttributeOf 'Summary]                -> [ValidChildOf 'Summary]                -> HTML Summary
--Tag_Superscript            :: [ValidAttributeOf 'Superscript]            -> [ValidChildOf 'Superscript]            -> HTML Superscript
--Tag_Table                  :: [ValidAttributeOf 'Table]                  -> [ValidChildOf 'Table]                  -> HTML Table
--Tag_TableBody              :: [ValidAttributeOf 'TableBody]              -> [ValidChildOf 'TableBody]              -> HTML TableBody
--Tag_TableDataCell          :: [ValidAttributeOf 'TableDataCell]          -> [ValidChildOf 'TableDataCell]          -> HTML TableDataCell
--Tag_TableFooter            :: [ValidAttributeOf 'TableFooter]            -> [ValidChildOf 'TableFooter]            -> HTML TableFooter
--Tag_TableHeaderCell        :: [ValidAttributeOf 'TableHeaderCell]        -> [ValidChildOf 'TableHeaderCell]        -> HTML TableHeaderCell
--Tag_TableHeader            :: [ValidAttributeOf 'TableHeader]            -> [ValidChildOf 'TableHeader]            -> HTML TableHeader
--Tag_TableRow               :: [ValidAttributeOf 'TableRow]               -> [ValidChildOf 'TableRow]               -> HTML TableRow
--Tag_Template               :: [ValidAttributeOf 'Template]               -> [ValidChildOf 'Template]               -> HTML Template
--Tag_TextArea               :: [ValidAttributeOf 'TextArea]               -> [ValidChildOf 'TextArea]               -> HTML TextArea
--Tag_Time                   :: [ValidAttributeOf 'Time]                   -> [ValidChildOf 'Time]                   -> HTML Time
--Tag_Title                  :: [ValidAttributeOf 'Title]                  -> [ValidChildOf 'Title]                  -> HTML Title
--Tag_Track                  :: [ValidAttributeOf 'Track]                                                            -> HTML Track
--Tag_Underline              :: [ValidAttributeOf 'Underline]              -> [ValidChildOf 'Underline]              -> HTML Underline
--Tag_UnorderedList          :: [ValidAttributeOf 'UnorderedList]          -> [ValidChildOf 'UnorderedList]          -> HTML UnorderedList
--Tag_Variable               :: [ValidAttributeOf 'Variable]               -> [ValidChildOf 'Variable]               -> HTML Variable
--Tag_Video                  :: [ValidAttributeOf 'Video]                  -> [ValidChildOf 'Video]                  -> HTML Video
--Tag_WordBreakOpportunity   :: [ValidAttributeOf 'WordBreakOpportunity]                                             -> HTML WordBreakOpportunity
---}
