{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This is required in order for `Filter` to work. Using this language
-- extension is always risky, but given that we can be sure that every argument
-- that will be passed to `Filter` will be a finite list, and that both
-- `Remove` and `Filter` have base cases that will resolve, it's a relatively
-- safe use case for it. If a better solution ever comes along that allows for
-- the elimination of this extension, we shouldn't hesitate to jump on the
-- opportunity.
{-# LANGUAGE UndecidableInstances #-}

-- This is to prevent warnings for the non-matching case in the third
-- `Contains` instance. GHC claims that this is a redundant constraint, but
-- attributes will fail to compile without it. I believe this is related to a
-- known GHC bug.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module HTML.Internal.TagOperations
  ( Contains
  , Filter
  , Remove
  , Union
  ) where

import HTML.Elements.TagType (TagType)

class Contains (list :: [TagType]) (eType :: TagType)

instance Contains (eType ': es) eType
instance {-# OVERLAPPABLE #-} Contains es eType => Contains (e ': es) eType

type family Add (tag :: TagType) (tags :: [TagType]) :: [TagType] where
  Add e '[]       = '[e]
  Add e (e ': ts) = e ': ts
  Add e (t ': ts) = t ': Add e ts

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
