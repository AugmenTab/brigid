{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HXML.Render.Text
  ( renderHXML
  , renderLazyHXML
  ) where

import Prelude hiding (id)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Builder.Linear (Builder, fromText, runBuilder)

import Brigid.HXML.Attributes.Internal (Attribute (..))
import Brigid.HXML.Elements.Internal (ChildHXML (..))
import Brigid.HXML.Types qualified as Types
import Brigid.Internal.Escape qualified as Escape
import Brigid.Internal.Render qualified as Render
import Brigid.Types qualified as Types

renderHXML :: ChildHXML parent -> T.Text
renderHXML = runBuilder . renderTag

renderLazyHXML :: ChildHXML parent -> TL.Text
renderLazyHXML = TL.fromStrict . runBuilder . renderTag

renderTag :: ChildHXML parent -> Builder
renderTag hxml =
  case hxml of
    Tag_NoElement ->
      mempty

    Tag_Comment comment ->
      fromText "<!-- " <> fromText comment <> fromText " -->"

    Tag_Content content ->
      fromText content

    Tag_Entity entity ->
      fromText $ T.pack entity

    Tag_RawHXML content ->
      fromText content

    Tag_CustomHXML elemName attrs eiCloserOrContent ->
      buildTag elemName attrs eiCloserOrContent

    Tag_Behavior attrs ->
      buildVoidTag "behavior" attrs Types.OmitTag

    Tag_Body attrs content ->
      buildSelfClosingOrContentTag "body" attrs content

    Tag_DateField attrs ->
      buildVoidTag "date-field" attrs Types.OmitTag

    Tag_Document attrs content ->
      buildSelfClosingOrContentTag "doc" attrs content

    Tag_Form attrs content ->
      buildSelfClosingOrContentTag "form" attrs content

    Tag_Header attrs content ->
      buildSelfClosingOrContentTag "header" attrs content

    Tag_Image attrs content ->
      buildSelfClosingOrContentTag "image" attrs content

    Tag_Item attrs content ->
      buildSelfClosingOrContentTag "item" attrs content

    Tag_Items attrs content ->
      buildSelfClosingOrContentTag "items" attrs content

    Tag_List attrs content ->
      buildSelfClosingOrContentTag "list" attrs content

    Tag_Modifier attrs content ->
      buildSelfClosingOrContentTag "modifier" attrs content

    Tag_Navigator attrs content ->
      buildSelfClosingOrContentTag "navigator" attrs content

    Tag_NavRoute attrs content ->
      buildSelfClosingOrContentTag "nav-route" attrs content

    Tag_Option attrs content ->
      buildSelfClosingOrContentTag "option" attrs content

    Tag_PickerField attrs content ->
      buildSelfClosingOrContentTag "picker-field" attrs content

    Tag_PickerItem attrs ->
      buildVoidTag "picker-item" attrs Types.OmitTag

    Tag_Screen attrs content ->
      buildSelfClosingOrContentTag "screen" attrs content

    Tag_Section attrs content ->
      buildSelfClosingOrContentTag "section" attrs content

    Tag_SectionList attrs content ->
      buildSelfClosingOrContentTag "section-list" attrs content

    Tag_SectionTitle attrs content ->
      buildSelfClosingOrContentTag "section-title" attrs content

    Tag_SelectMultiple attrs content ->
      buildSelfClosingOrContentTag "select-multiple" attrs content

    Tag_SelectSingle attrs content ->
      buildSelfClosingOrContentTag "select-single" attrs content

    Tag_Spinner attrs ->
      buildVoidTag "spinner" attrs Types.OmitTag

    Tag_Style attrs content ->
      buildSelfClosingOrContentTag "style" attrs content

    Tag_Styles content ->
      buildSelfClosingOrContentTag "styles" [] content

    Tag_Switch attrs ->
      buildVoidTag "switch" attrs Types.OmitTag

    Tag_Text attrs content ->
      buildClosingOrContentTag "text" attrs content

    Tag_TextArea attrs ->
      buildVoidTag "text-area" attrs Types.OmitTag

    Tag_TextField attrs ->
      buildVoidTag "text-field" attrs Types.OmitTag

    Tag_View attrs content ->
      buildSelfClosingOrContentTag "view" attrs content

    Tag_WebView attrs ->
      buildVoidTag "web-view" attrs Types.OmitTag

buildTag :: T.Text
         -> [Attribute tag]
         -> Either Types.NoContent [ChildHXML parent]
         -> Builder
buildTag tag attrs eiCloserOrContent =
  case eiCloserOrContent of
    Left  closer   -> buildVoidTag tag attrs closer
    Right children -> buildContentTag tag attrs children

buildVoidTag :: T.Text -> [Attribute tag] -> Types.NoContent -> Builder
buildVoidTag tag attrs closer =
  fromText "<"
    <> fromText tag
    <> foldMap (\attr -> maybe mempty (fromText " " <>) (renderAttribute attr)) attrs
    <> case closer of
         Types.OmitTag -> fromText "/>"
         Types.WithTag -> fromText ">" <> fromText "</" <> fromText tag <> fromText ">"

buildContentTag :: T.Text -> [Attribute tag] -> [ChildHXML parent] -> Builder
buildContentTag tag attrs children =
  fromText "<"
    <> fromText tag
    <> foldMap (\attr -> maybe mempty (fromText " " <>) (renderAttribute attr)) attrs
    <> fromText ">"
    <> foldMap renderTag children
    <> fromText "</"
    <> fromText tag
    <> fromText ">"

buildSelfClosingOrContentTag :: T.Text -> [Attribute tag] -> [ChildHXML parent] -> Builder
buildSelfClosingOrContentTag tag attrs children
  | null children = buildVoidTag tag attrs Types.OmitTag
  | otherwise     = buildContentTag tag attrs children

buildClosingOrContentTag :: T.Text -> [Attribute tag] -> [ChildHXML parent] -> Builder
buildClosingOrContentTag tag attrs children
  | null children = buildVoidTag tag attrs Types.WithTag
  | otherwise     = buildContentTag tag attrs children

renderAttribute :: Attribute any -> Maybe Builder
renderAttribute attr =
  case attr of
    Attr_NoAttribute ->
      Nothing

    Attr_Custom name value ->
      Just $ buildAttribute name (fromText value)

    Attr_ActivityIndicatorColor activityIndicatorColor ->
      Just
        . buildAttribute "activity-indicator-color"
        $ Types.colorToTextBuilder activityIndicatorColor

    Attr_AdjustsFontSizeToFit adjustsFontSizeToFit ->
      Just
        . buildAttribute "adjustsFontSizeToFit"
        $ Render.enumBoolToTextBuilder adjustsFontSizeToFit

    Attr_AllowDeselect allowDeselect ->
      Just
        . buildAttribute "allow-deselect"
        $ Render.enumBoolToTextBuilder allowDeselect

    Attr_AutoFocus autoFocus ->
      Just . buildAttribute "auto-focus" $ Render.enumBoolToTextBuilder autoFocus

    Attr_AvoidKeyboard avoidKeyboard ->
      Just
        . buildAttribute "avoid-keyboard"
        $ Render.enumBoolToTextBuilder avoidKeyboard

    Attr_CancelLabel cancelLabel ->
      Just $ buildAttribute "cancel-label" (fromText cancelLabel)

    Attr_Color color ->
      Just . buildAttribute "color" $ Types.hexColorToTextBuilder color

    Attr_ContentContainerStyle contentContainerStyle ->
      Just
        . buildAttribute "content-container-style"
        $ Render.foldToBuilderWithSeparator Types.idToTextBuilder (fromText " ") contentContainerStyle

    Attr_CursorColor cursorColor ->
      Just . buildAttribute "cursorColor" $ Types.colorToTextBuilder cursorColor

    Attr_DoneLabel doneLabel ->
      Just $ buildAttribute "done-label" (fromText doneLabel)

    Attr_FieldStyle fieldStyle ->
      Just
        . buildAttribute "field-style"
        $ Render.foldToBuilderWithSeparator Types.idToTextBuilder (fromText " ") fieldStyle

    Attr_FieldTextStyle fieldTextStyle ->
      Just
        . buildAttribute "field-text-style"
        $ Render.foldToBuilderWithSeparator Types.idToTextBuilder (fromText " ") fieldTextStyle

    Attr_Focused focused ->
      Just . buildAttribute "focused" $ Render.enumBoolToTextBuilder focused

    Attr_Hide hide ->
      Just . buildAttribute "hide" $ Render.enumBoolToTextBuilder hide

    Attr_Href href ->
      Just . buildAttribute "href" $ Types.urlToTextBuilder href

    Attr_Html html ->
      Just
        . buildAttribute "html"
        . fromText
        . Render.lazyBytesToText
        $ Escape.attributeBytes html

    Attr_Id id ->
      Just . buildAttribute "id" $ Types.idToTextBuilder id

    Attr_InjectedJavaScript injectedJavaScript ->
      Just
        . buildAttribute "injected-java-script"
        $ Types.rawJavaScriptToTextBuilder injectedJavaScript

    Attr_ItemHeight itemHeight ->
      Just . buildAttribute "itemHeight" $ Render.showIntegerToTextBuilder itemHeight

    Attr_Key key ->
      Just . buildAttribute "key" $ Types.keyToTextBuilder key

    Attr_KeyboardDismissMode keyboardDismissMode ->
      Just
        . buildAttribute "keyboard-dismiss-mode"
        $ Types.keyboardDismissModeToTextBuilder keyboardDismissMode

    Attr_KeyboardShouldPersistTaps keyboardShouldPersistTaps ->
      Just
        . buildAttribute "keyboard-should-persist-taps"
        $ Types.keyboardShouldPersistTapsToTextBuilder keyboardShouldPersistTaps

    Attr_KeyboardType keyboardType ->
      Just
        . buildAttribute "keyboard-type"
        $ Types.keyboardTypeToTextBuilder keyboardType

    Attr_Label label ->
      Just $ buildAttribute "label" (fromText label)

    Attr_Mask mask ->
      Just . buildAttribute "mask" $ Types.maskToTextBuilder mask

    Attr_Merge merge ->
      Just . buildAttribute "merge" $ Render.enumBoolToTextBuilder merge

    Attr_Modal modal ->
      Just . buildAttribute "modal" $ Render.enumBoolToTextBuilder modal

    Attr_ModalStyle modalStyle ->
      Just
        . buildAttribute "modal-style"
        $ Render.foldToBuilderWithSeparator Types.idToTextBuilder (fromText " ") modalStyle

    Attr_ModalTextStyle modalTextStyle ->
      Just
        . buildAttribute "modal-text-style"
        $ Render.foldToBuilderWithSeparator Types.idToTextBuilder (fromText " ") modalTextStyle

    Attr_Multiline multiline ->
      Just . buildAttribute "multiline" $ Render.enumBoolToTextBuilder multiline

    Attr_Name name ->
      Just . buildAttribute "name" $ Types.nameToTextBuilder name

    Attr_NumberOfLines numberOfLines ->
      Just
        . buildAttribute "numberOfLines"
        $ Render.showIntegerToTextBuilder numberOfLines

    Attr_Placeholder placeholder ->
      Just $ buildAttribute "placeholder" (fromText placeholder)

    Attr_PlaceholderTextColor placeholderTextColor ->
      Just
        . buildAttribute "placeholderTextColor"
        $ Types.colorToTextBuilder placeholderTextColor

    Attr_Preformatted preformatted ->
      Just
        . buildAttribute "preformatted"
        $ Render.enumBoolToTextBuilder preformatted

    Attr_Pressed pressed ->
      Just . buildAttribute "pressed" $ Render.enumBoolToTextBuilder pressed

    Attr_SafeArea safeArea ->
      Just . buildAttribute "safe-area" $ Render.enumBoolToTextBuilder safeArea

    Attr_Scroll scroll ->
      Just . buildAttribute "scroll" $ Render.enumBoolToTextBuilder scroll

    Attr_ScrollOrientation scrollOrientation ->
      Just
        . buildAttribute "scroll-orientation"
        $ Types.scrollOrientationToTextBuilder scrollOrientation

    Attr_ScrollToInputOffset scrollToInputOffset ->
      Just
        . buildAttribute "scroll-orientation"
        $ Render.showIntegerToTextBuilder scrollToInputOffset

    Attr_SecureText secureText ->
      Just . buildAttribute "secure-text" $ Render.enumBoolToTextBuilder secureText

    Attr_Selectable selectable ->
      Just . buildAttribute "selectable" $ Render.enumBoolToTextBuilder selectable

    Attr_Selected selected ->
      Just . buildAttribute "selected" $ Render.enumBoolToTextBuilder selected

    Attr_SelectionColor selectionColor ->
      Just
        . buildAttribute "selectionColor"
        $ Types.colorToTextBuilder selectionColor

    Attr_SelectionHandleColor selectionHandleColor ->
      Just
        . buildAttribute "selectionHandleColor"
        $ Types.colorToTextBuilder selectionHandleColor

    Attr_ShowLoadingIndicator showLoadingIndicator ->
      Just
        . buildAttribute "show-loading-indicator"
        $ Types.showLoadingIndicatorToTextBuilder showLoadingIndicator

    Attr_ShowsScrollIndicator showsScrollIndicator ->
      Just
        . buildAttribute "shows-scroll-indicator"
        $ Render.enumBoolToTextBuilder showsScrollIndicator

    Attr_Source source ->
      Just . buildAttribute "source" $ Types.urlToTextBuilder source

    Attr_Sticky sticky ->
      Just . buildAttribute "sticky" $ Render.enumBoolToTextBuilder sticky

    Attr_StickySectionTitles stickySectionTitles ->
      Just
        . buildAttribute "sticky-section-titles"
        $ Render.enumBoolToTextBuilder stickySectionTitles

    Attr_Style style ->
      Just
        . buildAttribute "style"
        $ Render.foldToBuilderWithSeparator Types.idToTextBuilder (fromText " ") style

    Attr_Type type_ ->
      Just . buildAttribute "type" $ Types.navigatorTypeToTextBuilder type_

    Attr_Url url ->
      Just . buildAttribute "url" $ Types.urlToTextBuilder url

    Attr_Value value ->
      Just $ buildAttribute "value" (fromText value)

    Attr_XMLNS xmlns ->
      Just . buildAttribute "xmlns" $ Types.urlToTextBuilder xmlns

buildAttribute :: T.Text -> Builder -> Builder
{-# INLINE buildAttribute #-}
buildAttribute attr value =
  fromText attr <> fromText "=\"" <> value <> fromText "\""
