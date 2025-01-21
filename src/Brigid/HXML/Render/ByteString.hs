{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HXML.Render.ByteString
  ( renderHXML
  , renderLazyHXML
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

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

    Tag_CustomHXML elemName eiCloserOrContent ->
      buildTag (toBytes elemName) eiCloserOrContent

    Tag_Behavior ->
      buildTag "behavior" $ Left Types.OmitTag

    Tag_Body content ->
      buildTag "body" $ contentOrSelfClosing content

    Tag_DateField ->
      buildTag "date-field" $ Left Types.OmitTag

    Tag_Document content ->
      buildTag "doc" $ contentOrSelfClosing content

    Tag_Form content ->
      buildTag "form" $ contentOrClosingTag content

    Tag_Header content ->
      buildTag "header" $ contentOrSelfClosing content

    Tag_Image ->
      buildTag "image" $ Left Types.OmitTag

    Tag_Item content ->
      buildTag "item" $ contentOrClosingTag content

    Tag_Items content ->
      buildTag "items" $ contentOrSelfClosing content

    Tag_List content ->
      buildTag "list" $ contentOrClosingTag content

    Tag_Modifier content ->
      buildTag "modifier" $ contentOrSelfClosing content

    Tag_Navigator content ->
      buildTag "navigator" $ contentOrSelfClosing content

    Tag_NavRoute ->
      buildTag "nav-route" $ Left Types.OmitTag

    Tag_Option content ->
      buildTag "option" $ contentOrSelfClosing content

    Tag_PickerField content ->
      buildTag "picker-field" $ contentOrSelfClosing content

    Tag_PickerItem ->
      buildTag "picker-item" $ Left Types.OmitTag

    Tag_Screen content ->
      buildTag "screen" $ contentOrSelfClosing content

    Tag_Section ->
      buildTag "section" $ Left Types.OmitTag

    Tag_SectionList content ->
      buildTag "section-list" $ contentOrClosingTag content

    Tag_SectionTitle content ->
      buildTag "section-title" $ contentOrSelfClosing content

    Tag_SelectMultiple content ->
      buildTag "select-multiple" $ contentOrSelfClosing content

    Tag_SelectSingle content ->
      buildTag "select-single" $ contentOrSelfClosing content

    Tag_Spinner ->
      buildTag "spinner" $ Left Types.OmitTag

    Tag_Style content ->
      buildTag "style" $ contentOrSelfClosing content

    Tag_Styles content ->
      buildTag "styles" $ contentOrSelfClosing content

    Tag_Switch ->
      buildTag "switch" $ Left Types.OmitTag

    Tag_Text ->
      buildTag "text" $ Left Types.WithTag

    Tag_TextArea ->
      buildTag "text-area" $ Left Types.OmitTag

    Tag_TextField ->
      buildTag "text-field" $ Left Types.OmitTag

    Tag_View content ->
      buildTag "view" $ contentOrClosingTag content

    Tag_WebView ->
      buildTag "web-view" $ Left Types.OmitTag

buildTag :: LBS.ByteString
         -> Either Types.NoContent [ChildHXML parent]
         -> Builder
buildTag tag content =
  mconcat
    [ lazyByteString "<"
    , lazyByteString tag
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
