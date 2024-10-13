{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.Source
  ( ValidSource
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage(..), TypeError)

import Brigid.HTML.Attributes.Href (HrefTypeErrorMessage)
import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem)
import Brigid.HTML.Types qualified as Types

type ValidSource url tag =
  AlertSource (Elem tag (ValidSourcesFor url)) url tag ~ 'True

type family ValidSourcesFor (url :: Type) :: [TagType] where
  ValidSourcesFor Types.AbsoluteURL             = TagGroups.SrcTags
  ValidSourcesFor (Types.RelativeURL Types.Get) = TagGroups.SrcTags
  ValidSourcesFor (Types.RelativeURL method)    = TagGroups.SrcTags
  ValidSourcesFor Types.RawURL                  = TagGroups.SrcTags

type family AlertSource (member :: Bool) (url :: Type) (tag :: TagType) :: Bool where
  AlertSource 'True url tag =
    'True

  AlertSource 'False url tag =
    TypeError
      ( 'Text "The "
          ':<>: HrefTypeErrorMessage url
          ':<>: 'Text " type is not valid for the src attribute on the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )
