{-# LANGUAGE DataKinds #-}

-- This is to prevent warnings for the non-matching case in the third `Elem`
-- instance. GHC claims that this is a redundant constraint, but attributes
-- will fail to compile without it. I believe this is related to a known GHC
-- bug.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Data.HTML.Types
  ( Elem
  ) where

import           Data.Kind (Type)

class Elem (a :: Type) (list :: [Type]) where

instance Elem a '[] where
instance {-# OVERLAPPING #-} Elem a (a ': rest) where
instance {-# OVERLAPPABLE #-} Elem a (b ': rest) where
