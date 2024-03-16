module HTML.Types.Negatable
  ( Negatable
  , not
  ) where

import Prelude hiding (not)

class Negatable a where
  not :: a -> a
