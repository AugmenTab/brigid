{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HXML.Attributes.Elements
  ( ValidAttribute
  ) where

import Brigid.HXML.Attributes.AttributeType (AttributeType (..))
import Brigid.HXML.Elements.Tags qualified as Tags
import Brigid.HXML.Elements.TagType (TagType)
import Brigid.HXML.Internal.TagOperations (AlertAttribute, Elem)

type ValidAttribute attr tag =
  AlertAttribute (Elem tag (ValidElementsFor attr)) attr tag ~ 'True

type family ValidElementsFor (attribute :: AttributeType) :: [TagType] where
  ValidElementsFor 'ActivityIndicatorColor    = '[ Tags.WebView ]
  ValidElementsFor 'AdjustsFontSizeToFit      = '[ Tags.Text ]
  ValidElementsFor 'AvoidKeyboard             = '[ Tags.View ]
  ValidElementsFor 'Color                     = '[ Tags.Spinner ]
  ValidElementsFor 'ContentContainerStyle     = '[ Tags.View ]
  ValidElementsFor 'Hide                      = [ Tags.Form, Tags.Header, Tags.Image, Tags.Item, Tags.List, Tags.Section, Tags.SectionList, Tags.SectionTitle, Tags.Text, Tags.View ]
  ValidElementsFor 'Html                      = '[ Tags.WebView ]
  ValidElementsFor 'Id                        = [ Tags.Body, Tags.Form, Tags.Header, Tags.Image, Tags.Item, Tags.List, Tags.Screen, Tags.Section, Tags.SectionList, Tags.SectionTitle, Tags.Text, Tags.View, Tags.WebView ]
  ValidElementsFor 'InjectedJavaScript        = '[ Tags.WebView ]
  ValidElementsFor 'ItemHeight                = [ Tags.List, Tags.SectionList ]
  ValidElementsFor 'Key                       = '[ Tags.Item ]
  ValidElementsFor 'KeyboardDismissMode       = [ Tags.List, Tags.View ]
  ValidElementsFor 'KeyboardShouldPersistTaps = [ Tags.List, Tags.SectionList ]
  ValidElementsFor 'NumberOfLines             = '[ Tags.Text ]
  ValidElementsFor 'Preformatted              = '[ Tags.Text ]
  ValidElementsFor 'SafeArea                  = [ Tags.Body, Tags.Header, Tags.View ]
  ValidElementsFor 'Scroll                    = [ Tags.Body, Tags.Form, Tags.View ]
  ValidElementsFor 'ScrollOrientation         = [ Tags.Body, Tags.Form, Tags.List, Tags.View ]
  ValidElementsFor 'ScrollToInputOffset       = '[ Tags.View ]
  ValidElementsFor 'Selectable                = '[ Tags.Text ]
  ValidElementsFor 'ShowLoadingIndicator      = '[ Tags.WebView ]
  ValidElementsFor 'ShowsScrollIndicator      = [ Tags.Body, Tags.Form, Tags.List, Tags.View ]
  ValidElementsFor 'Source                    = '[ Tags.Image ]
  ValidElementsFor 'Sticky                    = '[ Tags.Item, Tags.View ]
  ValidElementsFor 'StickySectionTitles       = '[ Tags.SectionList ]
  ValidElementsFor 'Style                     = [ Tags.Body, Tags.Form, Tags.Header, Tags.Image, Tags.Item, Tags.List, Tags.SectionList, Tags.SectionTitle, Tags.Text, Tags.View ]
  ValidElementsFor 'Url                       = '[ Tags.WebView ]
  ValidElementsFor 'XMLNS                     = '[ Tags.Document ]
