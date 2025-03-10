{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This is required in order for `Add` to work. Using this language extension
-- is always risky, but given that we can be sure that every argument that will
-- be passed to `Add` will be a finite list, and that `Add` have base cases
-- that will resolve, it's a relatively safe use case for it. If a better
-- solution ever comes along that allows for the elimination of this extension,
-- we shouldn't hesitate to jump on the opportunity.
--
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HXML.Elements.Children
  ( ValidChild
  ) where

import Brigid.HXML.Elements.TagGroups qualified as TagGroups
import Brigid.HXML.Internal.TagOperations (AlertElement, Elem)
import Brigid.HXML.Elements.TagType (TagType (..))

type ValidChild tag parent =
  AlertElement (Elem tag (ValidChildrenFor parent)) tag parent ~ 'True

type family ValidChildrenFor (parent :: TagType) :: [TagType] where
  ValidChildrenFor 'HXML           = '[ 'Document ]
  ValidChildrenFor 'Body           = TagGroups.BodyTags
  ValidChildrenFor 'Document       = [ 'Navigator, 'NavigatorStack, 'NavigatorTab, 'Screen ]
  ValidChildrenFor 'Form           = TagGroups.FormTags
  ValidChildrenFor 'Header         = [ 'Image, 'Text, 'View ]
  ValidChildrenFor 'Image          = '[ 'Behavior ]
  ValidChildrenFor 'Item           = [ 'Behavior, 'Image, 'Text, 'View ]
  ValidChildrenFor 'Items          = [ 'Item, 'List ]
  ValidChildrenFor 'List           = [ 'Behavior, 'Items, 'Item ]
  ValidChildrenFor 'Modifier       = '[ 'Style ]
  ValidChildrenFor 'Navigator      = [ 'Behavior, 'NavRoute ]
  ValidChildrenFor 'NavigatorStack = [ 'Behavior, 'NavRoute ]
  ValidChildrenFor 'NavigatorTab   = [ 'Behavior, 'NavRoute ]
  ValidChildrenFor 'NavRoute       = '[ 'Navigator ]
  ValidChildrenFor 'Option         = [ 'Behavior, 'Image, 'Text, 'View ]
  ValidChildrenFor 'PickerField    = '[ 'PickerItem ]
  ValidChildrenFor 'Screen         = [ 'Body, 'Header, 'Styles ]
  ValidChildrenFor 'Section        = [ 'Items, 'Item, 'SectionTitle ]
  ValidChildrenFor 'SectionList    = [ 'Behavior, 'Item, 'Section, 'SectionTitle ]
  ValidChildrenFor 'SectionTitle   = [ 'Image, 'Text, 'View ]
  ValidChildrenFor 'SelectMultiple = '[ 'Option ]
  ValidChildrenFor 'SelectSingle   = '[ 'Option ]
  ValidChildrenFor 'Style          = '[ 'Modifier ]
  ValidChildrenFor 'Styles         = '[ 'Style ]
  ValidChildrenFor 'Text           = '[ 'Behavior, 'Content ]
  ValidChildrenFor 'View           = TagGroups.ViewTags
