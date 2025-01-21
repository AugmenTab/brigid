{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HXML.Render.Text
  ( renderHXML
  , renderLazyHXML
  ) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

import Brigid.HXML.Elements.Internal (ChildHXML (..))
import Brigid.HXML.Types qualified as Types

renderHXML :: ChildHXML parent -> T.Text
renderHXML = TL.toStrict . renderLazyHXML

renderLazyHXML :: ChildHXML parent -> TL.Text
renderLazyHXML = toLazyText . renderTag

renderTag :: ChildHXML parent -> Builder
renderTag hxml =
  case hxml of
    Tag_NoElement ->
      fromText T.empty

    Tag_Comment comment ->
      fromText "<!-- " <> fromText comment <> fromText " -->"

    Tag_RawHXML content ->
      fromText content

    Tag_CustomHXML elemName eiCloserOrContent ->
      buildTag elemName eiCloserOrContent

    Tag_Behavior ->
      buildTag "behavior" $ Left Types.OmitTag

    Tag_Body content ->
      buildTag "body" $ closingTag content

    Tag_DateField ->
      buildTag "date-field" $ Left Types.OmitTag

    Tag_Document content ->
      buildTag "doc" $ closingTag content

    Tag_Form content ->
      buildTag "form" $ closingTag content

    Tag_Header content ->
      buildTag "header" $ closingTag content

    Tag_Image ->
      buildTag "image" $ Left Types.OmitTag

    Tag_Item content ->
      buildTag "item" $ closingTag content

    Tag_Items content ->
      buildTag "items" $ closingTag content

    Tag_List content ->
      buildTag "list" $ closingTag content

    Tag_Modifier content ->
      buildTag "modifier" $ closingTag content

    Tag_Navigator content ->
      buildTag "navigator" $ closingTag content

    Tag_NavRoute ->
      buildTag "nav-route" $ Left Types.OmitTag

    Tag_Option content ->
      buildTag "option" $ closingTag content

    Tag_PickerField content ->
      buildTag "picker-field" $ closingTag content

    Tag_PickerItem ->
      buildTag "picker-item" $ Left Types.OmitTag

    Tag_Screen content ->
      buildTag "screen" $ closingTag content

    Tag_Section ->
      buildTag "section" $ Left Types.OmitTag

    Tag_SectionList content ->
      buildTag "section-list" $ closingTag content

    Tag_SectionTitle content ->
      buildTag "section-title" $ closingTag content

    Tag_SelectMultiple content ->
      buildTag "select-multiple" $ closingTag content

    Tag_SelectSingle content ->
      buildTag "select-single" $ closingTag content

    Tag_Spinner ->
      buildTag "spinner" $ Left Types.OmitTag

    Tag_Style content ->
      buildTag "style" $ closingTag content

    Tag_Styles content ->
      buildTag "styles" $ closingTag content

    Tag_Switch ->
      buildTag "switch" $ Left Types.OmitTag

    Tag_Text ->
      buildTag "text" $ Left Types.OmitTag

    Tag_TextArea ->
      buildTag "text-area" $ Left Types.OmitTag

    Tag_TextField ->
      buildTag "text-field" $ Left Types.OmitTag

    Tag_View content ->
      buildTag "view" $ closingTag content

    Tag_WebView ->
      buildTag "web-view" $ Left Types.OmitTag

buildTag :: T.Text
         -> Either Types.NoContent [ChildHXML parent]
         -> Builder
buildTag tag content =
  mconcat
    [ fromText "<"
    , fromText tag
    , case content of
        Left  Types.OmitTag -> fromText "/>"
        Left  Types.WithTag -> fromText ">"
        Right _children     -> fromText ">"
    , case content of
        Left  _type    -> fromText T.empty
        Right children -> foldMap renderTag children
    , case content of
        Left Types.OmitTag -> fromText T.empty
        Left Types.WithTag -> fromText "</" <> fromText tag <> fromText ">"
        Right _children    -> fromText "</" <> fromText tag <> fromText ">"
    ]

closingTag :: [ChildHXML parent] -> Either Types.NoContent [ChildHXML parent]
closingTag content =
  if null content
    then Left Types.OmitTag
    else Right content
