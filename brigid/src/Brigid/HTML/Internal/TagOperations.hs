{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This is required in order for `Filter` to work. Using this language
-- extension is always risky, but given that we can be sure that every argument
-- that will be passed to `Filter` will be a finite list, and that both
-- `Remove` and `Filter` have base cases that will resolve, it's a relatively
-- safe use case for it. If a better solution ever comes along that allows for
-- the elimination of this extension, we shouldn't hesitate to jump on the
-- opportunity.
--
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Internal.TagOperations
  ( Add
  , AlertAttribute
  , AlertElement
  , Elem
  , Filter
  , Remove
  , Union
  ) where

import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HTML.Attributes.AttributeType (AttributeErrorMessage, AttributeType)
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)

type family Add (tag :: TagType) (tags :: [TagType]) :: [TagType] where
  Add e '[]       = '[e]
  Add e (e ': ts) = e ': ts
  Add e (t ': ts) = t ': Add e ts

type family AlertAttribute (member :: Bool) (attr :: AttributeType) (tag :: TagType) :: Bool where
  AlertAttribute 'True attr tag =
    'True

  AlertAttribute 'False attr tag =
    TypeError
      ( 'Text "The "
          ':<>: AttributeErrorMessage attr
          ':<>: 'Text " attribute is not a valid attribute for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family AlertElement (member :: Bool) (tag :: TagType) (parent :: TagType) :: Bool where
  AlertElement 'True tag parent =
    'True

  AlertElement 'False tag parent =
    TypeError
      ( 'Text "The "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element is not a valid child for the "
          ':<>: TagErrorMessage parent
          ':<>: 'Text " element."
      )

type family Elem (tag :: TagType) (tags :: [TagType]) :: Bool where
  Elem e '[]       = 'False
  Elem e (e ': ts) = 'True
  Elem e (t ': ts) = Elem e ts

type family Filter (deletes :: [TagType]) (keeps :: [TagType]) :: [TagType] where
  Filter '[]       keeps = keeps
  Filter (d ': ds) keeps = Filter ds (Remove d keeps)

type family Remove (tag :: TagType) (tags :: [TagType]) :: [TagType] where
  Remove d '[]       = '[]
  Remove d (d ': ks) = Remove d ks
  Remove d (k ': ks) = k ': Remove d ks

type family Union (adds :: [TagType]) (tags :: [TagType]) :: [TagType] where
  Union '[]       tags = tags
  Union (a ': as) tags = Union as (Add a tags)
