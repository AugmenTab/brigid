{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HXML.Render.ByteString
  ( renderHXML
  , renderLazyHXML
  ) where

import Data.Bool qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrdOn)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Brigid.HXML.Attributes.Internal (Attribute (..), attributeText)
import Brigid.HXML.Elements.Internal (ChildHXML (..))
import Brigid.HXML.Types qualified as Types

renderHXML :: ChildHXML parent -> BS.ByteString
renderHXML = LBS.toStrict . renderLazyHXML

renderLazyHXML :: ChildHXML parent -> LBS.ByteString
renderLazyHXML = toLazyByteString . renderTag

renderTag :: ChildHXML parent -> Builder
renderTag hxml =
  case hxml of
    Tag_NoElement ->
      lazyByteString LBS.empty

    Tag_Comment comment ->
      lazyByteString "<!-- "
        <> lazyByteString (toBytes comment)
        <> lazyByteString " -->"

    Tag_RawHXML content ->
      lazyByteString $ toBytes content

    Tag_CustomHXML elemName attrs eiCloserOrContent ->
      buildTag (toBytes elemName) attrs eiCloserOrContent

    Tag_Behavior attrs ->
      buildTag "behavior" attrs $ Left Types.OmitTag

    Tag_Body attrs content ->
      buildTag "body" attrs $ contentOrSelfClosing content

    Tag_DateField attrs ->
      buildTag "date-field" attrs $ Left Types.OmitTag

    Tag_Document attrs content ->
      buildTag "doc" attrs $ contentOrSelfClosing content

    Tag_Form attrs content ->
      buildTag "form" attrs $ contentOrClosingTag content

    Tag_Header attrs content ->
      buildTag "header" attrs $ contentOrSelfClosing content

    Tag_Image attrs ->
      buildTag "image" attrs $ Left Types.OmitTag

    Tag_Item attrs content ->
      buildTag "item" attrs $ contentOrClosingTag content

    Tag_Items attrs content ->
      buildTag "items" attrs $ contentOrSelfClosing content

    Tag_List attrs content ->
      buildTag "list" attrs $ contentOrClosingTag content

    Tag_Modifier attrs content ->
      buildTag "modifier" attrs $ contentOrSelfClosing content

    Tag_Navigator attrs content ->
      buildTag "navigator" attrs $ contentOrSelfClosing content

    Tag_NavRoute attrs ->
      buildTag "nav-route" attrs $ Left Types.OmitTag

    Tag_Option attrs content ->
      buildTag "option" attrs $ contentOrSelfClosing content

    Tag_PickerField attrs content ->
      buildTag "picker-field" attrs $ contentOrSelfClosing content

    Tag_PickerItem attrs ->
      buildTag "picker-item" attrs $ Left Types.OmitTag

    Tag_Screen attrs content ->
      buildTag "screen" attrs $ contentOrSelfClosing content

    Tag_Section attrs ->
      buildTag "section" attrs $ Left Types.OmitTag

    Tag_SectionList attrs content ->
      buildTag "section-list" attrs $ contentOrClosingTag content

    Tag_SectionTitle attrs content ->
      buildTag "section-title" attrs $ contentOrSelfClosing content

    Tag_SelectMultiple attrs content ->
      buildTag "select-multiple" attrs $ contentOrSelfClosing content

    Tag_SelectSingle attrs content ->
      buildTag "select-single" attrs $ contentOrSelfClosing content

    Tag_Spinner attrs ->
      buildTag "spinner" attrs $ Left Types.OmitTag

    Tag_Style attrs content ->
      buildTag "style" attrs $ contentOrSelfClosing content

    Tag_Styles attrs content ->
      buildTag "styles" attrs $ contentOrSelfClosing content

    Tag_Switch attrs ->
      buildTag "switch" attrs $ Left Types.OmitTag

    Tag_Text attrs ->
      buildTag "text" attrs $ Left Types.WithTag

    Tag_TextArea attrs ->
      buildTag "text-area" attrs $ Left Types.OmitTag

    Tag_TextField attrs ->
      buildTag "text-field" attrs $ Left Types.OmitTag

    Tag_View attrs content ->
      buildTag "view" attrs $ contentOrClosingTag content

    Tag_WebView attrs ->
      buildTag "web-view" attrs $ Left Types.OmitTag

buildTag :: LBS.ByteString
         -> [Attribute tag]
         -> Either Types.NoContent [ChildHXML parent]
         -> Builder
buildTag tag attrs content =
  mconcat
    [ lazyByteString "<"
    , lazyByteString tag
    , lazyByteString . B.bool " " LBS.empty $ L.null attrs
    , mconcat
        . L.intersperse (lazyByteString " ")
        . mapMaybe renderAttribute
        $ nubOrdOn attributeText attrs
    , case content of
        Left  Types.OmitTag -> lazyByteString "/>"
        Left  Types.WithTag -> lazyByteString ">"
        Right _children     -> lazyByteString ">"
    , case content of
        Left  _type    -> lazyByteString LBS.empty
        Right children -> foldMap renderTag children
    , case content of
        Left Types.OmitTag ->
          lazyByteString LBS.empty

        Left Types.WithTag ->
          lazyByteString "</" <> lazyByteString tag <> lazyByteString ">"

        Right _children ->
          lazyByteString "</" <> lazyByteString tag <> lazyByteString ">"
    ]

renderAttribute :: Attribute any -> Maybe Builder
renderAttribute attr =
  case attr of
    Attr_NoAttribute ->
      Just $ lazyByteString LBS.empty

    Attr_Custom name value ->
      Just $ buildAttribute (toBytes name) (toBytes value)

    Attr_XMLNS xmlns ->
      Just . buildAttribute "xmlns" $ Types.urlToBytes xmlns

buildAttribute :: LBS.ByteString -> LBS.ByteString -> Builder
buildAttribute attr value =
  lazyByteString attr <> "=\"" <> lazyByteString value <> lazyByteString "\""

contentOrClosingTag :: [ChildHXML parent]
                    -> Either Types.NoContent [ChildHXML parent]
contentOrClosingTag content =
  if null content
    then Left Types.WithTag
    else Right content

contentOrSelfClosing :: [ChildHXML parent]
                     -> Either Types.NoContent [ChildHXML parent]
contentOrSelfClosing content =
  if null content
    then Left Types.OmitTag
    else Right content

toBytes :: T.Text -> LBS.ByteString
toBytes = LBS.fromStrict . TE.encodeUtf8
