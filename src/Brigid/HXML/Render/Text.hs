{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HXML.Render.Text
  ( renderHXML
  , renderLazyHXML
  ) where

import Prelude hiding (id)
import Data.Bool qualified as B
import Data.Containers.ListUtils (nubOrdOn)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

import Brigid.HTML.Internal.Render qualified as Render
import Brigid.HXML.Attributes.Internal (Attribute (..), attributeText)
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

    Tag_CustomHXML elemName attrs eiCloserOrContent ->
      buildTag elemName attrs eiCloserOrContent

    Tag_Behavior attrs ->
      buildTag "behavior" attrs $ Left Types.OmitTag

    Tag_Body attrs content ->
      buildTag "body" attrs $ closingTag content

    Tag_DateField attrs ->
      buildTag "date-field" attrs $ Left Types.OmitTag

    Tag_Document attrs content ->
      buildTag "doc" attrs $ closingTag content

    Tag_Form attrs content ->
      buildTag "form" attrs $ closingTag content

    Tag_Header attrs content ->
      buildTag "header" attrs $ closingTag content

    Tag_Image attrs ->
      buildTag "image" attrs $ Left Types.OmitTag

    Tag_Item attrs content ->
      buildTag "item" attrs $ closingTag content

    Tag_Items attrs content ->
      buildTag "items" attrs $ closingTag content

    Tag_List attrs content ->
      buildTag "list" attrs $ closingTag content

    Tag_Modifier attrs content ->
      buildTag "modifier" attrs $ closingTag content

    Tag_Navigator attrs content ->
      buildTag "navigator" attrs $ closingTag content

    Tag_NavRoute attrs ->
      buildTag "nav-route" attrs $ Left Types.OmitTag

    Tag_Option attrs content ->
      buildTag "option" attrs $ closingTag content

    Tag_PickerField attrs content ->
      buildTag "picker-field" attrs $ closingTag content

    Tag_PickerItem attrs ->
      buildTag "picker-item" attrs $ Left Types.OmitTag

    Tag_Screen attrs content ->
      buildTag "screen" attrs $ closingTag content

    Tag_Section attrs ->
      buildTag "section" attrs $ Left Types.OmitTag

    Tag_SectionList attrs content ->
      buildTag "section-list" attrs $ closingTag content

    Tag_SectionTitle attrs content ->
      buildTag "section-title" attrs $ closingTag content

    Tag_SelectMultiple attrs content ->
      buildTag "select-multiple" attrs $ closingTag content

    Tag_SelectSingle attrs content ->
      buildTag "select-single" attrs $ closingTag content

    Tag_Spinner attrs ->
      buildTag "spinner" attrs $ Left Types.OmitTag

    Tag_Style attrs content ->
      buildTag "style" attrs $ closingTag content

    Tag_Styles attrs content ->
      buildTag "styles" attrs $ closingTag content

    Tag_Switch attrs ->
      buildTag "switch" attrs $ Left Types.OmitTag

    Tag_Text attrs ->
      buildTag "text" attrs $ Left Types.OmitTag

    Tag_TextArea attrs ->
      buildTag "text-area" attrs $ Left Types.OmitTag

    Tag_TextField attrs ->
      buildTag "text-field" attrs $ Left Types.OmitTag

    Tag_View attrs content ->
      buildTag "view" attrs $ closingTag content

    Tag_WebView attrs ->
      buildTag "web-view" attrs $ Left Types.OmitTag

buildTag :: T.Text
         -> [Attribute tag]
         -> Either Types.NoContent [ChildHXML parent]
         -> Builder
buildTag tag attrs content =
  mconcat
    [ fromText "<"
    , fromText tag
    , fromText . B.bool " " T.empty $ L.null attrs
    , mconcat
        . L.intersperse (fromText " ")
        . mapMaybe renderAttribute
        $ nubOrdOn attributeText attrs
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

renderAttribute :: Attribute any -> Maybe Builder
renderAttribute attr =
  case attr of
    Attr_NoAttribute ->
      Just $ fromText T.empty

    Attr_Custom name value ->
      Just $ buildAttribute name value

    Attr_Hide hide ->
      Just . buildAttribute "hide" $ Render.enumBoolToText hide

    Attr_Id id ->
      Just . buildAttribute "id" $ Types.idToText id

    Attr_SafeArea safeArea ->
      Just . buildAttribute "safe-area" $ Render.enumBoolToText safeArea

    Attr_Style style ->
      Just $ buildAttribute "style" style

    Attr_XMLNS xmlns ->
      Just . buildAttribute "xmlns" $ Types.urlToText xmlns

buildAttribute :: T.Text -> T.Text -> Builder
buildAttribute attr value =
  fromText attr <> "=\"" <> fromText value <> fromText "\""

closingTag :: [ChildHXML parent] -> Either Types.NoContent [ChildHXML parent]
closingTag content =
  if null content
    then Left Types.OmitTag
    else Right content
