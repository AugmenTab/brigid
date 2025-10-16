{-# LANGUAGE RankNTypes #-}

module Brigid.Internal.Use
  ( use
  ) where

import Data.Constraint (Dict (..))

use :: forall c r. c => (Dict c -> r) -> r
use r = r Dict
