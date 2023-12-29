{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- This is to prevent warnings for the non-matching case in the third
-- `Contains` instance. GHC claims that this is a redundant constraint, but
-- attributes will fail to compile without it. I believe this is related to a
-- known GHC bug.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module HTML.Contains
  ( Contains
  ) where

import HTML.Elements.TagType (TagType)

class Contains (list :: [TagType]) (eType :: TagType)

instance Contains (eType ': es) eType
instance {-# OVERLAPPABLE #-} Contains es eType => Contains (e ': es) eType
