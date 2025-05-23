{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HXML.Attributes
  ( noAttribute
  , customAttribute
  , activityIndicatorColor
  , adjustsFontSizeToFit
  , allowDeselect
  , autoFocus
  , avoidKeyboard
  , cancelLabel
  , color
  , contentContainerStyle
  , cursorColor
  , doneLabel
  , fieldStyle
  , fieldTextStyle
  , focused
  , hide
  , href
  , html
  , id
  , injectedJavaScript
  , itemHeight
  , key
  , keyboardDismissMode
  , keyboardShouldPersistTaps
  , keyboardType
  , label
  , mask
  , merge
  , modal
  , modalStyle
  , modalTextStyle
  , multiline
  , name
  , numberOfLines
  , placeholder
  , placeholderTextColor
  , preformatted
  , pressed
  , safeArea
  , scroll
  , scrollOrientation
  , scrollToInputOffset
  , secureText
  , selectable
  , selected
  , selectionColor
  , selectionHandleColor
  , showLoadingIndicator
  , showsScrollIndicator
  , source
  , sticky
  , stickySectionTitles
  , style
  , type_
  , url
  , value
  , xmlns
  ) where

import Prelude hiding (id)
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Integer (Positive)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Elements (ChildHTML)
import Brigid.HTML.Render.ByteString (renderLazyHTML)
import Brigid.HXML.Attributes.AttributeType (AttributeType (..))
import Brigid.HXML.Attributes.Internal (Attribute (..))
import Brigid.HXML.Attributes.Elements (ValidAttribute)
import Brigid.HXML.Types qualified as Types
import Brigid.Types qualified as Types

noAttribute :: Attribute tag
noAttribute = Attr_NoAttribute

customAttribute :: T.Text -> T.Text -> Attribute tag
customAttribute = Attr_Custom

activityIndicatorColor :: ( KnownNat branchIndex
                          , branchIndex ~ FirstIndexOf color Types.ColorTypes
                          , ValidAttribute 'ActivityIndicatorColor tag
                          )
                       => color -> Attribute tag
activityIndicatorColor =
  Attr_ActivityIndicatorColor . Types.mkColor

adjustsFontSizeToFit :: ValidAttribute 'AdjustsFontSizeToFit tag
                     => Bool -> Attribute tag
adjustsFontSizeToFit = Attr_AdjustsFontSizeToFit

allowDeselect :: ValidAttribute 'AllowDeselect tag => Bool -> Attribute tag
allowDeselect = Attr_AllowDeselect

autoFocus :: ValidAttribute 'AutoFocus tag => Bool -> Attribute tag
autoFocus = Attr_AutoFocus

avoidKeyboard :: ValidAttribute 'AvoidKeyboard tag => Bool -> Attribute tag
avoidKeyboard = Attr_AvoidKeyboard

cancelLabel :: ValidAttribute 'CancelLabel tag => T.Text -> Attribute tag
cancelLabel = Attr_CancelLabel

color :: ValidAttribute 'Color tag => Types.HexColor -> Attribute tag
color = Attr_Color

contentContainerStyle :: ValidAttribute 'ContentContainerStyle tag
                      => [Types.Id] -> Attribute tag
contentContainerStyle = Attr_ContentContainerStyle

cursorColor :: ( KnownNat branchIndex
               , branchIndex ~ FirstIndexOf color Types.ColorTypes
               , ValidAttribute 'CursorColor tag
               )
            => color -> Attribute tag
cursorColor =
  Attr_CursorColor . Types.mkColor

doneLabel :: ValidAttribute 'DoneLabel tag => T.Text -> Attribute tag
doneLabel = Attr_DoneLabel

fieldStyle :: ValidAttribute 'FieldStyle tag => [Types.Id] -> Attribute tag
fieldStyle = Attr_FieldStyle

fieldTextStyle :: ValidAttribute 'FieldTextStyle tag
               => [Types.Id] -> Attribute tag
fieldTextStyle = Attr_FieldTextStyle

focused :: ValidAttribute 'Focused tag => Bool -> Attribute tag
focused = Attr_Focused

hide :: ValidAttribute 'Hide tag => Bool -> Attribute tag
hide = Attr_Hide

href :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf url Types.URLTypes
        , ValidAttribute 'Href tag
        )
     => url -> Attribute tag
href =
  Attr_Href . Types.mkURL

html :: ValidAttribute 'Html tag
     => ChildHTML parent grandparent -> Attribute tag
html = Attr_Html . renderLazyHTML

id :: ValidAttribute 'Id tag => Types.Id -> Attribute tag
id = Attr_Id

injectedJavaScript :: ValidAttribute 'InjectedJavaScript tag
                   => Types.RawJavaScript -> Attribute tag
injectedJavaScript = Attr_InjectedJavaScript

itemHeight :: ValidAttribute 'ItemHeight tag => Positive -> Attribute tag
itemHeight = Attr_ItemHeight

key :: ValidAttribute 'Key tag => Types.Key -> Attribute tag
key = Attr_Key

keyboardDismissMode :: ValidAttribute 'KeyboardDismissMode tag
                    => Types.KeyboardDismissMode -> Attribute tag
keyboardDismissMode = Attr_KeyboardDismissMode

keyboardShouldPersistTaps :: ValidAttribute 'KeyboardShouldPersistTaps tag
                          => Types.KeyboardShouldPersistTaps -> Attribute tag
keyboardShouldPersistTaps = Attr_KeyboardShouldPersistTaps

keyboardType :: ValidAttribute 'KeyboardType tag
             => Types.KeyboardType -> Attribute tag
keyboardType = Attr_KeyboardType

label :: ValidAttribute 'Label tag => T.Text -> Attribute tag
label = Attr_Label

mask :: ValidAttribute 'Mask tag => Types.Mask -> Attribute tag
mask = Attr_Mask

merge :: ValidAttribute 'Merge tag => Bool -> Attribute tag
merge = Attr_Merge

modal :: ValidAttribute 'Modal tag => Bool -> Attribute tag
modal = Attr_Modal

modalStyle :: ValidAttribute 'ModalStyle tag => [Types.Id] -> Attribute tag
modalStyle = Attr_ModalStyle

modalTextStyle :: ValidAttribute 'ModalTextStyle tag
               => [Types.Id] -> Attribute tag
modalTextStyle = Attr_ModalTextStyle

multiline :: ValidAttribute 'Multiline tag => Bool -> Attribute tag
multiline = Attr_Multiline

name :: ValidAttribute 'Name tag => Types.Name -> Attribute tag
name = Attr_Name

numberOfLines :: ValidAttribute 'NumberOfLines tag => Positive -> Attribute tag
numberOfLines = Attr_NumberOfLines

placeholder :: ValidAttribute 'Placeholder tag => T.Text -> Attribute tag
placeholder = Attr_Placeholder

placeholderTextColor :: ( KnownNat branchIndex
                        , branchIndex ~ FirstIndexOf color Types.ColorTypes
                        , ValidAttribute 'PlaceholderTextColor tag
                        )
                     => color -> Attribute tag
placeholderTextColor =
  Attr_PlaceholderTextColor . Types.mkColor

preformatted :: ValidAttribute 'Preformatted tag => Bool -> Attribute tag
preformatted = Attr_Preformatted

pressed :: ValidAttribute 'Pressed tag => Bool -> Attribute tag
pressed = Attr_Pressed

safeArea :: ValidAttribute 'SafeArea tag => Bool -> Attribute tag
safeArea = Attr_SafeArea

scroll :: ValidAttribute 'Scroll tag => Bool -> Attribute tag
scroll = Attr_Scroll

scrollOrientation :: ValidAttribute 'ScrollOrientation tag
                  => Types.ScrollOrientation -> Attribute tag
scrollOrientation = Attr_ScrollOrientation

scrollToInputOffset :: ValidAttribute 'ScrollToInputOffset tag
                    => Integer -> Attribute tag
scrollToInputOffset = Attr_ScrollToInputOffset

secureText :: ValidAttribute 'SecureText tag => Bool -> Attribute tag
secureText = Attr_SecureText

selectable :: ValidAttribute 'Selectable tag => Bool -> Attribute tag
selectable = Attr_Selectable

selected :: ValidAttribute 'Selected tag => Bool -> Attribute tag
selected = Attr_Selected

selectionColor :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf color Types.ColorTypes
                  , ValidAttribute 'SelectionColor tag
                  )
               => color -> Attribute tag
selectionColor =
  Attr_SelectionColor . Types.mkColor

selectionHandleColor :: ( KnownNat branchIndex
                        , branchIndex ~ FirstIndexOf color Types.ColorTypes
                        , ValidAttribute 'SelectionHandleColor tag
                        )
                     => color -> Attribute tag
selectionHandleColor =
  Attr_SelectionHandleColor . Types.mkColor

showLoadingIndicator :: ValidAttribute 'ShowLoadingIndicator tag
                     => Types.ShowLoadingIndicator -> Attribute tag
showLoadingIndicator = Attr_ShowLoadingIndicator

showsScrollIndicator :: ValidAttribute 'ShowsScrollIndicator tag
                     => Bool -> Attribute tag
showsScrollIndicator = Attr_ShowsScrollIndicator

source :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf url Types.URLTypes
          , ValidAttribute 'Source tag
          )
       => url -> Attribute tag
source =
  Attr_Source . Types.mkURL

sticky :: ValidAttribute 'Sticky tag => Bool -> Attribute tag
sticky = Attr_Sticky

stickySectionTitles :: ValidAttribute 'StickySectionTitles tag
                    => Bool -> Attribute tag
stickySectionTitles = Attr_StickySectionTitles

style :: ValidAttribute 'Style tag => [Types.Id] -> Attribute tag
style = Attr_Style

type_ :: ValidAttribute 'Type tag => Types.NavigatorType -> Attribute tag
type_ = Attr_Type

url :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf url Types.URLTypes
       , ValidAttribute 'Url tag
       )
    => url -> Attribute tag
url =
  Attr_Url . Types.mkURL

value :: ValidAttribute 'Value tag => T.Text -> Attribute tag
value = Attr_Value

xmlns :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf url Types.URLTypes
         , ValidAttribute 'XMLNS tag
         )
      => url -> Attribute tag
xmlns =
  Attr_XMLNS . Types.mkURL
