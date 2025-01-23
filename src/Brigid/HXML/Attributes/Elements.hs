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
  ValidElementsFor 'AdjustsFontSizeToFit      = '[ Tags.Text ]
  ValidElementsFor 'AvoidKeyboard             = '[ Tags.View ]
  ValidElementsFor 'ContentContainerStyle     = '[ Tags.View ]
  ValidElementsFor 'Hide                      = [ Tags.Header, Tags.Image, Tags.List, Tags.Text, Tags.View ]
  ValidElementsFor 'Id                        = [ Tags.Body, Tags.Header, Tags.Image, Tags.List, Tags.Screen, Tags.Text, Tags.View ]
  ValidElementsFor 'ItemHeight                = ' [ Tags.List ]
  ValidElementsFor 'KeyboardDismissMode       = '[ Tags.List, Tags.View ]
  ValidElementsFor 'KeyboardShouldPersistTaps = ' [ Tags.List ]
  ValidElementsFor 'NumberOfLines             = '[ Tags.Text ]
  ValidElementsFor 'Preformatted              = '[ Tags.Text ]
  ValidElementsFor 'SafeArea                  = [ Tags.Body, Tags.Header, Tags.View ]
  ValidElementsFor 'Scroll                    = [ Tags.Body, Tags.View ]
  ValidElementsFor 'ScrollOrientation         = [ Tags.Body, Tags.List, Tags.View ]
  ValidElementsFor 'ScrollToInputOffset       = '[ Tags.View ]
  ValidElementsFor 'Selectable                = '[ Tags.Text ]
  ValidElementsFor 'ShowsScrollIndicator      = [ Tags.Body, Tags.List, Tags.View ]
  ValidElementsFor 'Source                    = '[ Tags.Image ]
  ValidElementsFor 'Sticky                    = '[ Tags.View ]
  ValidElementsFor 'Style                     = [ Tags.Body, Tags.Header, Tags.Image, Tags.List, Tags.Text, Tags.View ]
  ValidElementsFor 'XMLNS                     = '[ Tags.Document ]
