{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generation.Analysis
  ( totalNodes
  , maxDepth
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty.Extra (maximum1)

import Generation.Types qualified as GT

newtype Count =
  Count
    { unCount :: Int
    } deriving (Eq, Num, Ord)

instance Semigroup Count where
  (<>) = (+)

instance Monoid Count where
  mempty = 0

totalNodes :: GT.Element -> Int
totalNodes =
  unCount . countNodes

countNodes :: GT.Element -> Count
countNodes element =
  case GT.elementChildren element of
    GT.Branch nodes -> Count 1 + foldMap countNodes nodes
    GT.Leaf _net -> Count 1
    GT.Void -> Count 1

maxDepth :: GT.Element -> Int
maxDepth element =
  (+ 1) $
    case GT.elementChildren element of
      GT.Branch nodes -> maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty nodes)
      GT.Leaf _net -> 0
      GT.Void -> 0
