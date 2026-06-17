{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HXML.Render.ByteString
  ( renderHXML
  , renderLazyHXML
  ) where

import Prelude hiding (id)
import Data.Bool qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, string8, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as L
import Data.Maybe (mapMaybe)

import Brigid.HXML.Attributes.Internal (Attribute (..))
import Brigid.HXML.Elements.Internal (ChildHXML (..))
import Brigid.HXML.Types qualified as Types
import Brigid.Internal.Escape qualified as Escape
import Brigid.Internal.Render qualified as Render
import Brigid.Types qualified as Types

renderHXML :: ChildHXML parent -> BS.ByteString
renderHXML = LBS.toStrict . renderLazyHXML

renderLazyHXML :: ChildHXML parent -> LBS.ByteString
renderLazyHXML = toLazyByteString . renderTag

renderTag :: ChildHXML parent -> Builder
renderTag hxml =
  case hxml of
    Tag_NoElement ->
      mempty

    Tag_Comment comment ->
      "<!-- " <> Render.textToBytesBuilder comment <> " -->"

    Tag_Content content ->
      Render.textToBytesBuilder content

    Tag_Entity entity ->
      string8 entity

    Tag_RawHXML content ->
      Render.textToBytesBuilder content

    Tag_CustomHXML elemName attrs eiCloserOrContent ->
      buildTag (Render.textToBytesBuilder elemName) attrs eiCloserOrContent

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

    Tag_Image attrs content ->
      buildTag "image" attrs $ contentOrSelfClosing content

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

    Tag_NavRoute attrs content ->
      buildTag "nav-route" attrs $ contentOrSelfClosing content

    Tag_Option attrs content ->
      buildTag "option" attrs $ contentOrSelfClosing content

    Tag_PickerField attrs content ->
      buildTag "picker-field" attrs $ contentOrSelfClosing content

    Tag_PickerItem attrs ->
      buildTag "picker-item" attrs $ Left Types.OmitTag

    Tag_Screen attrs content ->
      buildTag "screen" attrs $ contentOrSelfClosing content

    Tag_Section attrs content ->
      buildTag "section" attrs $ contentOrSelfClosing content

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

    Tag_Styles content ->
      buildTag "styles" [] $ contentOrSelfClosing content

    Tag_Switch attrs ->
      buildTag "switch" attrs $ Left Types.OmitTag

    Tag_Text attrs content ->
      buildTag "text" attrs $ contentOrClosingTag content

    Tag_TextArea attrs ->
      buildTag "text-area" attrs $ Left Types.OmitTag

    Tag_TextField attrs ->
      buildTag "text-field" attrs $ Left Types.OmitTag

    Tag_View attrs content ->
      buildTag "view" attrs $ contentOrClosingTag content

    Tag_WebView attrs ->
      buildTag "web-view" attrs $ Left Types.OmitTag

buildTag :: Builder
         -> [Attribute tag]
         -> Either Types.NoContent [ChildHXML parent]
         -> Builder
buildTag tag attrs content =
  mconcat
    [ "<"
    , tag
    , B.bool " " mempty $ L.null attrs
    , mconcat
        . L.intersperse " "
        $ mapMaybe renderAttribute attrs
    , case content of
        Left  Types.OmitTag -> "/>"
        Left  Types.WithTag -> ">"
        Right _children     -> ">"
    , case content of
        Left  _type    -> mempty
        Right children -> foldMap renderTag children
    , case content of
        Left Types.OmitTag ->
          mempty

        Left Types.WithTag ->
          "</" <> tag <> ">"

        Right _children ->
          "</" <> tag <> ">"
    ]

renderAttribute :: Attribute any -> Maybe Builder
renderAttribute attr =
  case attr of
    Attr_NoAttribute ->
      Just mempty

    Attr_Custom name value ->
      Just $ buildAttribute (Render.textToBytesBuilder name) (Render.textToBytesBuilder value)

    Attr_ActivityIndicatorColor activityIndicatorColor ->
      Just
        . buildAttribute "activity-indicator-color"
        $ Types.colorToBytesBuilder activityIndicatorColor

    Attr_AdjustsFontSizeToFit adjustsFontSizeToFit ->
      Just
        . buildAttribute "adjustsFontSizeToFit"
        $ Render.enumBoolToBytesBuilder adjustsFontSizeToFit

    Attr_AllowDeselect allowDeselect ->
      Just
        . buildAttribute "allow-deselect"
        $ Render.enumBoolToBytesBuilder allowDeselect

    Attr_AutoFocus autoFocus ->
      Just . buildAttribute "auto-focus" $ Render.enumBoolToBytesBuilder autoFocus

    Attr_AvoidKeyboard avoidKeyboard ->
      Just
        . buildAttribute "avoid-keyboard"
        $ Render.enumBoolToBytesBuilder avoidKeyboard

    Attr_CancelLabel cancelLabel ->
      Just . buildAttribute "cancel-label" $ Render.textToBytesBuilder cancelLabel

    Attr_Color color ->
      Just . buildAttribute "color" $ Types.hexColorToBytesBuilder color

    Attr_ContentContainerStyle contentContainerStyle ->
      Just
        . buildAttribute "content-container-style"
        . Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " "
        $ contentContainerStyle

    Attr_CursorColor cursorColor ->
      Just . buildAttribute "cursorColor" $ Types.colorToBytesBuilder cursorColor

    Attr_DoneLabel doneLabel ->
      Just . buildAttribute "done-label" $ Render.textToBytesBuilder doneLabel

    Attr_FieldStyle fieldStyle ->
      Just
        . buildAttribute "field-style"
        . Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " "
        $ fieldStyle

    Attr_FieldTextStyle fieldTextStyle ->
      Just
        . buildAttribute "field-text-style"
        . Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " "
        $ fieldTextStyle

    Attr_Focused focused ->
      Just . buildAttribute "focused" $ Render.enumBoolToBytesBuilder focused

    Attr_Hide hide ->
      Just . buildAttribute "hide" $ Render.enumBoolToBytesBuilder hide

    Attr_Href href ->
      Just . buildAttribute "href" $ Types.urlToBytesBuilder href

    Attr_Html html ->
      Just . buildAttribute "html" $ Escape.lazyBytesAttributeBytesBuilder html

    Attr_Id id ->
      Just . buildAttribute "id" $ Types.idToBytesBuilder id

    Attr_InjectedJavaScript injectedJavaScript ->
      Just
        . buildAttribute "injected-java-script"
        $ Types.rawJavaScriptToBytesBuilder injectedJavaScript

    Attr_ItemHeight itemHeight ->
      Just . buildAttribute "itemHeight" $ Render.showBytesBuilder itemHeight

    Attr_Key key ->
      Just . buildAttribute "key" $ Types.keyToBytesBuilder key

    Attr_KeyboardDismissMode keyboardDismissMode ->
      Just
        . buildAttribute "keyboard-dismiss-mode"
        $ Types.keyboardDismissModeToBytesBuilder keyboardDismissMode

    Attr_KeyboardShouldPersistTaps keyboardShouldPersistTaps ->
      Just
        . buildAttribute "keyboard-should-persist-taps"
        $ Types.keyboardShouldPersistTapsToBytesBuilder keyboardShouldPersistTaps

    Attr_KeyboardType keyboardType ->
      Just
        . buildAttribute "keyboard-type"
        $ Types.keyboardTypeToBytesBuilder keyboardType

    Attr_Label label ->
      Just . buildAttribute "label" $ Render.textToBytesBuilder label

    Attr_Mask mask ->
      Just . buildAttribute "mask" $ Types.maskToBytesBuilder mask

    Attr_Merge merge ->
      Just . buildAttribute "merge" $ Render.enumBoolToBytesBuilder merge

    Attr_Modal modal ->
      Just . buildAttribute "modal" $ Render.enumBoolToBytesBuilder modal

    Attr_ModalStyle modalStyle ->
      Just
        . buildAttribute "modal-style"
        . Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " "
        $ modalStyle

    Attr_ModalTextStyle modalTextStyle ->
      Just
        . buildAttribute "modal-text-style"
        . Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " "
        $ modalTextStyle

    Attr_Multiline multiline ->
      Just . buildAttribute "multiline" $ Render.enumBoolToBytesBuilder multiline

    Attr_Name name ->
      Just . buildAttribute "name" $ Types.nameToBytesBuilder name

    Attr_NumberOfLines numberOfLines ->
      Just
        . buildAttribute "numberOfLines"
        $ Render.showBytesBuilder numberOfLines

    Attr_Placeholder placeholder ->
      Just . buildAttribute "placeholder" $ Render.textToBytesBuilder placeholder

    Attr_PlaceholderTextColor placeholderTextColor ->
      Just
        . buildAttribute "placeholderTextColor"
        $ Types.colorToBytesBuilder placeholderTextColor

    Attr_Preformatted preformatted ->
      Just
        . buildAttribute "preformatted"
        $ Render.enumBoolToBytesBuilder preformatted

    Attr_Pressed pressed ->
      Just . buildAttribute "pressed" $ Render.enumBoolToBytesBuilder pressed

    Attr_SafeArea safeArea ->
      Just . buildAttribute "safe-area" $ Render.enumBoolToBytesBuilder safeArea

    Attr_Scroll scroll ->
      Just . buildAttribute "scroll" $ Render.enumBoolToBytesBuilder scroll

    Attr_ScrollOrientation scrollOrientation ->
      Just
        . buildAttribute "scroll-orientation"
        $ Types.scrollOrientationToBytesBuilder scrollOrientation

    Attr_ScrollToInputOffset scrollToInputOffset ->
      Just
        . buildAttribute "scroll-orientation"
        $ Render.showBytesBuilder scrollToInputOffset

    Attr_SecureText secureText ->
      Just . buildAttribute "secure-text" $ Render.enumBoolToBytesBuilder secureText

    Attr_Selectable selectable ->
      Just . buildAttribute "selectable" $ Render.enumBoolToBytesBuilder selectable

    Attr_Selected selected ->
      Just . buildAttribute "selected" $ Render.enumBoolToBytesBuilder selected

    Attr_SelectionColor selectionColor ->
      Just
        . buildAttribute "selectionColor"
        $ Types.colorToBytesBuilder selectionColor

    Attr_SelectionHandleColor selectionHandleColor ->
      Just
        . buildAttribute "selectionHandleColor"
        $ Types.colorToBytesBuilder selectionHandleColor

    Attr_ShowLoadingIndicator showLoadingIndicator ->
      Just
        . buildAttribute "show-loading-indicator"
        $ Types.showLoadingIndicatorToBytesBuilder showLoadingIndicator

    Attr_ShowsScrollIndicator showsScrollIndicator ->
      Just
        . buildAttribute "shows-scroll-indicator"
        $ Render.enumBoolToBytesBuilder showsScrollIndicator

    Attr_Source source ->
      Just . buildAttribute "source" $ Types.urlToBytesBuilder source

    Attr_Sticky sticky ->
      Just . buildAttribute "sticky" $ Render.enumBoolToBytesBuilder sticky

    Attr_StickySectionTitles stickySectionTitles ->
      Just
        . buildAttribute "sticky-section-titles"
        $ Render.enumBoolToBytesBuilder stickySectionTitles

    Attr_Style style ->
      Just
        . buildAttribute "style"
        $ Render.foldToBytesBuilderWithSeparator Types.idToBytesBuilder " " style

    Attr_Type type_ ->
      Just . buildAttribute "type" $ Types.navigatorTypeToBytesBuilder type_

    Attr_Url url ->
      Just . buildAttribute "url" $ Types.urlToBytesBuilder url

    Attr_Value value ->
      Just . buildAttribute "value" $ Render.textToBytesBuilder value

    Attr_XMLNS xmlns ->
      Just . buildAttribute "xmlns" $ Types.urlToBytesBuilder xmlns

buildAttribute :: Builder -> Builder -> Builder
buildAttribute attr value =
  attr <> "=\"" <> value <> "\""

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
