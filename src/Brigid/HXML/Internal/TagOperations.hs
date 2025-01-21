{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HXML.Internal.TagOperations
  ( Add
  , AlertElement
  , Elem
  ) where

import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HXML.Elements.TagType (TagErrorMessage, TagType)

type family Add (tag :: TagType) (tags :: [TagType]) :: [TagType] where
  Add e '[]       = '[e]
  Add e (e ': ts) = e ': ts
  Add e (t ': ts) = t ': Add e ts

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
