{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generation.Analysis
  ( totalNodes
  ) where

import Prelude hiding (div, head, map, span)

import Generation.Element (Element (..), ElementType (..))

newtype Count =
  Count
    { unCount :: Int
    } deriving (Eq, Num, Ord)

instance Semigroup Count where
  (<>) = (+)

instance Monoid Count where
  mempty = 0

totalNodes :: Element -> Int
totalNodes =
  unCount . countNodes

countNodes :: Element -> Count
countNodes element =
  let
    handleContent :: Element -> Count
    handleContent =
      either (const $ Count 1) (foldMap countNodes) . elementChildren
  in
    case elementType element of
      Text -> Count 1
      Comment -> Count 1
      Anchor -> Count 1 + handleContent element
      Abbreviation -> Count 1 + handleContent element
      ContactAddress -> Count 1 + handleContent element
      Area -> Count 1
      Article -> Count 1 + handleContent element
      Aside -> Count 1 + handleContent element
      Audio -> Count 1 + handleContent element
      BringAttentionTo -> Count 1 + handleContent element
      Base -> Count 1
      BidirectionalIsolation -> Count 1 + handleContent element
      BidirectionalOverride -> Count 1 + handleContent element
      Blockquote -> Count 1 + handleContent element
      Body -> Count 1 + handleContent element
      LineBreak -> Count 1
      Button -> Count 1 + handleContent element
      Canvas -> Count 1 + handleContent element
      TableCaption -> Count 1 + handleContent element
      Citation -> Count 1 + handleContent element
      Code -> Count 1 + handleContent element
      TableColumn -> Count 1
      TableColumnGroup -> Count 1 + handleContent element
      Data -> Count 1 + handleContent element
      DataList -> Count 1 + handleContent element
      DescriptionDetails -> Count 1 + handleContent element
      DeletedText -> Count 1 + handleContent element
      Details -> Count 1 + handleContent element
      Definition -> Count 1 + handleContent element
      Dialog -> Count 1 + handleContent element
      Division -> Count 1 + handleContent element
      DescriptionList -> Count 1 + handleContent element
      DescriptionTerm -> Count 1 + handleContent element
      Emphasis -> Count 1 + handleContent element
      Embed -> Count 1
      Fieldset -> Count 1 + handleContent element
      FigureCaption -> Count 1 + handleContent element
      Figure -> Count 1 + handleContent element
      Footer -> Count 1 + handleContent element
      Form -> Count 1 + handleContent element
      H1 -> Count 1 + handleContent element
      H2 -> Count 1 + handleContent element
      H3 -> Count 1 + handleContent element
      H4 -> Count 1 + handleContent element
      H5 -> Count 1 + handleContent element
      H6 -> Count 1 + handleContent element
      Head -> Count 1 + handleContent element
      Header -> Count 1 + handleContent element
      HeadingGroup -> Count 1 + handleContent element
      HorizontalRule -> Count 1
      Html -> Count 1 + handleContent element
      IdiomaticText -> Count 1 + handleContent element
      IFrame -> Count 1
      Image -> Count 1
      Input -> Count 1
      InsertedText -> Count 1 + handleContent element
      KeyboardInput -> Count 1 + handleContent element
      Label -> Count 1 + handleContent element
      Legend -> Count 1 + handleContent element
      ListItem -> Count 1 + handleContent element
      Link -> Count 1
      Main -> Count 1 + handleContent element
      Map -> Count 1 + handleContent element
      Mark -> Count 1 + handleContent element
      Menu -> Count 1 + handleContent element
      Meta -> Count 1
      Meter -> Count 1 + handleContent element
      Nav -> Count 1 + handleContent element
      NoScript -> Count 1 + handleContent element
      Object -> Count 1 + handleContent element
      OrderedList -> Count 1 + handleContent element
      OptionGroup -> Count 1 + handleContent element
      Option -> Count 1
      Output -> Count 1 + handleContent element
      Paragraph -> Count 1 + handleContent element
      Picture -> Count 1 + handleContent element
      PreformattedText -> Count 1 + handleContent element
      Progress -> Count 1 + handleContent element
      Quotation -> Count 1 + handleContent element
      RubyParenthesis -> Count 1
      RubyText -> Count 1 + handleContent element
      Ruby -> Count 1 + handleContent element
      Strikethrough -> Count 1 + handleContent element
      Sample -> Count 1 + handleContent element
      Script -> Count 1
      Search -> Count 1 + handleContent element
      Section -> Count 1 + handleContent element
      Select -> Count 1 + handleContent element
      Slot -> Count 1 + handleContent element
      SideComment -> Count 1 + handleContent element
      Source -> Count 1
      Span -> Count 1 + handleContent element
      Strong -> Count 1 + handleContent element
      Style -> Count 1
      Subscript -> Count 1 + handleContent element
      Summary -> Count 1 + handleContent element
      Superscript -> Count 1 + handleContent element
      Table -> Count 1 + handleContent element
      TableBody -> Count 1 + handleContent element
      TableDataCell -> Count 1 + handleContent element
      ContentTemplate -> Count 1 + handleContent element
      TextArea -> Count 1 + handleContent element
      TableFoot -> Count 1 + handleContent element
      TableHeader -> Count 1 + handleContent element
      TableHead -> Count 1 + handleContent element
      Time -> Count 1 + handleContent element
      Title -> Count 1
      TableRow -> Count 1 + handleContent element
      Track -> Count 1
      Underline -> Count 1 + handleContent element
      UnorderedList -> Count 1 + handleContent element
      Variable -> Count 1 + handleContent element
      Video -> Count 1 + handleContent element
      WordBreakOpportunity -> Count 1
