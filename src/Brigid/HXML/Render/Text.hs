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
      Just $ buildAttribute name value

    Attr_ActivityIndicatorColor activityIndicatorColor ->
      Just
        . buildAttribute "activity-indicator-color"
        $ Types.colorToText activityIndicatorColor

    Attr_AdjustsFontSizeToFit adjustsFontSizeToFit ->
      Just
        . buildAttribute "adjustsFontSizeToFit"
        $ Render.enumBoolToText adjustsFontSizeToFit

    Attr_AllowDeselect allowDeselect ->
      Just
        . buildAttribute "allow-deselect"
        $ Render.enumBoolToText allowDeselect

    Attr_AutoFocus autoFocus ->
      Just . buildAttribute "auto-focus" $ Render.enumBoolToText autoFocus

    Attr_AvoidKeyboard avoidKeyboard ->
      Just
        . buildAttribute "avoid-keyboard"
        $ Render.enumBoolToText avoidKeyboard

    Attr_CancelLabel cancelLabel ->
      Just $ buildAttribute "cancel-label" cancelLabel

    Attr_Color color ->
      Just . buildAttribute "color" $ Types.hexColorToText color

    Attr_ContentContainerStyle contentContainerStyle ->
      Just
        . buildAttributeB "content-container-style"
        $ Render.foldToBuilderWithSeparator (fromText . Types.idToText) (fromText " ") contentContainerStyle

    Attr_CursorColor cursorColor ->
      Just . buildAttribute "cursorColor" $ Types.colorToText cursorColor

    Attr_DoneLabel doneLabel ->
      Just $ buildAttribute "done-label" doneLabel

    Attr_FieldStyle fieldStyle ->
      Just
        . buildAttributeB "field-style"
        $ Render.foldToBuilderWithSeparator (fromText . Types.idToText) (fromText " ") fieldStyle

    Attr_FieldTextStyle fieldTextStyle ->
      Just
        . buildAttributeB "field-text-style"
        $ Render.foldToBuilderWithSeparator (fromText . Types.idToText) (fromText " ") fieldTextStyle

    Attr_Focused focused ->
      Just . buildAttribute "focused" $ Render.enumBoolToText focused

    Attr_Hide hide ->
      Just . buildAttribute "hide" $ Render.enumBoolToText hide

    Attr_Href href ->
      Just . buildAttribute "href" $ Types.urlToText href

    Attr_Html html ->
      Just
        . buildAttribute "html"
        . Render.lazyBytesToText
        $ Escape.attributeBytes html

    Attr_Id id ->
      Just . buildAttribute "id" $ Types.idToText id

    Attr_InjectedJavaScript injectedJavaScript ->
      Just
        . buildAttribute "injected-java-script"
        $ Types.rawJavaScriptToText injectedJavaScript

    Attr_ItemHeight itemHeight ->
      Just . buildAttribute "itemHeight" $ Render.showText itemHeight

    Attr_Key key ->
      Just . buildAttribute "key" $ Types.keyToText key

    Attr_KeyboardDismissMode keyboardDismissMode ->
      Just
        . buildAttribute "keyboard-dismiss-mode"
        $ Types.keyboardDismissModeToText keyboardDismissMode

    Attr_KeyboardShouldPersistTaps keyboardShouldPersistTaps ->
      Just
        . buildAttribute "keyboard-should-persist-taps"
        $ Types.keyboardShouldPersistTapsToText keyboardShouldPersistTaps

    Attr_KeyboardType keyboardType ->
      Just
        . buildAttribute "keyboard-type"
        $ Types.keyboardTypeToText keyboardType

    Attr_Label label ->
      Just $ buildAttribute "label" label

    Attr_Mask mask ->
      Just . buildAttribute "mask" $ Types.maskToText mask

    Attr_Merge merge ->
      Just . buildAttribute "merge" $ Render.enumBoolToText merge

    Attr_Modal modal ->
      Just . buildAttribute "modal" $ Render.enumBoolToText modal

    Attr_ModalStyle modalStyle ->
      Just
        . buildAttributeB "modal-style"
        $ Render.foldToBuilderWithSeparator (fromText . Types.idToText) (fromText " ") modalStyle

    Attr_ModalTextStyle modalTextStyle ->
      Just
        . buildAttributeB "modal-text-style"
        $ Render.foldToBuilderWithSeparator (fromText . Types.idToText) (fromText " ") modalTextStyle

    Attr_Multiline multiline ->
      Just . buildAttribute "multiline" $ Render.enumBoolToText multiline

    Attr_Name name ->
      Just . buildAttribute "name" $ Types.nameToText name

    Attr_NumberOfLines numberOfLines ->
      Just
        . buildAttribute "numberOfLines"
        $ Render.showText numberOfLines

    Attr_Placeholder placeholder ->
      Just $ buildAttribute "placeholder" placeholder

    Attr_PlaceholderTextColor placeholderTextColor ->
      Just
        . buildAttribute "placeholderTextColor"
        $ Types.colorToText placeholderTextColor

    Attr_Preformatted preformatted ->
      Just
        . buildAttribute "preformatted"
        $ Render.enumBoolToText preformatted

    Attr_Pressed pressed ->
      Just . buildAttribute "pressed" $ Render.enumBoolToText pressed

    Attr_SafeArea safeArea ->
      Just . buildAttribute "safe-area" $ Render.enumBoolToText safeArea

    Attr_Scroll scroll ->
      Just . buildAttribute "scroll" $ Render.enumBoolToText scroll

    Attr_ScrollOrientation scrollOrientation ->
      Just
        . buildAttribute "scroll-orientation"
        $ Types.scrollOrientationToText scrollOrientation

    Attr_ScrollToInputOffset scrollToInputOffset ->
      Just
        . buildAttribute "scroll-orientation"
        $ Render.showText scrollToInputOffset

    Attr_SecureText secureText ->
      Just . buildAttribute "secure-text" $ Render.enumBoolToText secureText

    Attr_Selectable selectable ->
      Just . buildAttribute "selectable" $ Render.enumBoolToText selectable

    Attr_Selected selected ->
      Just . buildAttribute "selected" $ Render.enumBoolToText selected

    Attr_SelectionColor selectionColor ->
      Just
        . buildAttribute "selectionColor"
        $ Types.colorToText selectionColor

    Attr_SelectionHandleColor selectionHandleColor ->
      Just
        . buildAttribute "selectionHandleColor"
        $ Types.colorToText selectionHandleColor

    Attr_ShowLoadingIndicator showLoadingIndicator ->
      Just
        . buildAttribute "show-loading-indicator"
        $ Types.showLoadingIndicatorToText showLoadingIndicator

    Attr_ShowsScrollIndicator showsScrollIndicator ->
      Just
        . buildAttribute "shows-scroll-indicator"
        $ Render.enumBoolToText showsScrollIndicator

    Attr_Source source ->
      Just . buildAttribute "source" $ Types.urlToText source

    Attr_Sticky sticky ->
      Just . buildAttribute "sticky" $ Render.enumBoolToText sticky

    Attr_StickySectionTitles stickySectionTitles ->
      Just
        . buildAttribute "sticky-section-titles"
        $ Render.enumBoolToText stickySectionTitles

    Attr_Style style ->
      Just
        . buildAttributeB "style"
        $ Render.foldToBuilderWithSeparator (fromText . Types.idToText) (fromText " ") style

    Attr_Type type_ ->
      Just . buildAttribute "type" $ Types.navigatorTypeToText type_

    Attr_Url url ->
      Just . buildAttribute "url" $ Types.urlToText url

    Attr_Value value ->
      Just $ buildAttribute "value" value

    Attr_XMLNS xmlns ->
      Just . buildAttribute "xmlns" $ Types.urlToText xmlns

buildAttribute :: T.Text -> T.Text -> Builder
buildAttribute attr value =
  fromText attr <> fromText "=\"" <> fromText value <> fromText "\""

buildAttributeB :: T.Text -> Builder -> Builder
buildAttributeB attr value =
  fromText attr <> fromText "=\"" <> value <> fromText "\""

