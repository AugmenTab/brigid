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

import Brigid.HXML.Attributes.Internal (Attribute (..), attributeText)
import Brigid.HXML.Elements.Internal (ChildHXML (..))
import Brigid.HXML.Types qualified as Types
import Brigid.Internal.Escape qualified as Escape
import Brigid.Internal.Render qualified as Render

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

    Tag_Content content ->
      fromText content

    Tag_Entity entity ->
      fromText $ T.pack entity

    Tag_RawHXML content ->
      fromText content

    Tag_CustomHXML elemName attrs eiCloserOrContent ->
      buildTag elemName attrs eiCloserOrContent

    Tag_Behavior attrs ->
      buildTag "behavior" attrs $ Left Types.OmitTag

    Tag_Body attrs content ->
      buildTag "body" attrs $ contentOrSelfClosing content

    Tag_DateField attrs ->
      buildTag "date-field" attrs $ Left Types.OmitTag

    Tag_Document attrs content ->
      buildTag "doc" attrs $ contentOrSelfClosing content

    Tag_Form attrs content ->
      buildTag "form" attrs $ contentOrSelfClosing content

    Tag_Header attrs content ->
      buildTag "header" attrs $ contentOrSelfClosing content

    Tag_Image attrs ->
      buildTag "image" attrs $ Left Types.OmitTag

    Tag_Item attrs content ->
      buildTag "item" attrs $ contentOrSelfClosing content

    Tag_Items attrs content ->
      buildTag "items" attrs $ contentOrSelfClosing content

    Tag_List attrs content ->
      buildTag "list" attrs $ contentOrSelfClosing content

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

    Tag_Section attrs content ->
      buildTag "section" attrs $ contentOrSelfClosing content

    Tag_SectionList attrs content ->
      buildTag "section-list" attrs $ contentOrSelfClosing content

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
      buildTag "view" attrs $ contentOrSelfClosing content

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
        . buildAttribute "content-container-style"
        . Render.foldToTextWithSeparator Types.idToText " "
        $ contentContainerStyle

    Attr_CursorColor cursorColor ->
      Just . buildAttribute "cursorColor" $ Types.colorToText cursorColor

    Attr_DoneLabel doneLabel ->
      Just $ buildAttribute "done-label" doneLabel

    Attr_FieldStyle fieldStyle ->
      Just
        . buildAttribute "field-style"
        . Render.foldToTextWithSeparator Types.idToText " "
        $ fieldStyle

    Attr_FieldTextStyle fieldTextStyle ->
      Just
        . buildAttribute "field-text-style"
        . Render.foldToTextWithSeparator Types.idToText " "
        $ fieldTextStyle

    Attr_Focused focused ->
      Just . buildAttribute "focused" $ Render.enumBoolToText focused

    Attr_Hide hide ->
      Just . buildAttribute "hide" $ Render.enumBoolToText hide

    Attr_Html html ->
      Just
        . buildAttribute "html"
        . Render.bytesToText
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

    Attr_Mask mask ->
      Just . buildAttribute "mask" $ Types.maskToText mask

    Attr_Merge merge ->
      Just . buildAttribute "merge" $ Render.enumBoolToText merge

    Attr_Modal modal ->
      Just . buildAttribute "modal" $ Render.enumBoolToText modal

    Attr_ModalStyle modalStyle ->
      Just
        . buildAttribute "modal-style"
        . Render.foldToTextWithSeparator Types.idToText " "
        $ modalStyle

    Attr_ModalTextStyle modalTextStyle ->
      Just
        . buildAttribute "modal-text-style"
        . Render.foldToTextWithSeparator Types.idToText " "
        $ modalTextStyle

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
        . buildAttribute "style"
        $ Render.foldToTextWithSeparator Types.idToText " " style

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
  fromText attr <> "=\"" <> fromText value <> fromText "\""

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
