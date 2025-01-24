{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HXML.Attributes
  ( noAttribute
  , customAttribute
  , activityIndicatorColor
  , adjustsFontSizeToFit
  , avoidKeyboard
  , color
  , contentContainerStyle
  , hide
  , html
  , id
  , injectedJavaScript
  , itemHeight
  , key
  , keyboardDismissMode
  , keyboardShouldPersistTaps
  , numberOfLines
  , preformatted
  , safeArea
  , scroll
  , scrollOrientation
  , scrollToInputOffset
  , selectable
  , showLoadingIndicator
  , showsScrollIndicator
  , source
  , sticky
  , stickySectionTitles
  , style
  , styles
  , url
  , xmlns
  ) where

import Prelude hiding (id)
import Data.List qualified as L
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

avoidKeyboard :: ValidAttribute 'AvoidKeyboard tag => Bool -> Attribute tag
avoidKeyboard = Attr_AvoidKeyboard

color :: ValidAttribute 'Color tag => Types.HexColor -> Attribute tag
color = Attr_Color

contentContainerStyle :: ValidAttribute 'ContentContainerStyle tag
                      => [Types.Id] -> Attribute tag
contentContainerStyle = Attr_ContentContainerStyle

hide :: ValidAttribute 'Hide tag => Bool -> Attribute tag
hide = Attr_Hide

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

numberOfLines :: ValidAttribute 'NumberOfLines tag => Positive -> Attribute tag
numberOfLines = Attr_NumberOfLines

preformatted :: ValidAttribute 'Preformatted tag => Bool -> Attribute tag
preformatted = Attr_Preformatted

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

selectable :: ValidAttribute 'Selectable tag => Bool -> Attribute tag
selectable = Attr_Selectable

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

style :: ValidAttribute 'Style tag => Types.Id -> Attribute tag
style = styles . L.singleton

styles :: ValidAttribute 'Style tag => [Types.Id] -> Attribute tag
styles = Attr_Style

url :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf url Types.URLTypes
       , ValidAttribute 'Url tag
       )
    => url -> Attribute tag
url =
  Attr_Url . Types.mkURL

xmlns :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf url Types.URLTypes
         , ValidAttribute 'XMLNS tag
         )
      => url -> Attribute tag
xmlns =
  Attr_XMLNS . Types.mkURL
