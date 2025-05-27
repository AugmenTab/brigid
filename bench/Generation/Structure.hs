module Generation.Structure
  ( Node (..)
  , buildHtmlTree
  , countTotalNodes
  ) where

import Data.List (sort)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

data Node
  = Root Node Node
  | Branch [Node]
  | Leaf
  | Void
  deriving Show

buildHtmlTree :: Int -> Int -> Int -> Gen Node
buildHtmlTree totalNodes maxDepth maxChildren
  | totalNodes < 3 = error "At least 3 nodes required: html, head, body"
  | maxDepth < 1 = error "Max depth must be >= 1"
  | otherwise = do
      let
        remaining = totalNodes - 3

      headCount <- Gen.int . Range.linear 1 $ remaining - 1

      let
        bodyCount = remaining - headCount

      headTree <- buildSubtree (1 + headCount) (maxDepth - 1) maxChildren
      bodyTree <- buildSubtree (1 + bodyCount) (maxDepth - 1) maxChildren

      pure $ Root headTree bodyTree

suggestBranching :: Int -> Int -> Int -> (Int, Int)
suggestBranching n d maxChildren
  | d <= 0 || n <= 1 = (1, 1)
  | otherwise =
      let
        base = max 1 $ n `div` (d * 2)
      in
        (max 1 base, min (n - 1) . min maxChildren $ base * 4)

buildSubtree :: Int -> Int -> Int -> Gen Node
buildSubtree n depth maxChildren
  | n < 1 =
      error $
        "buildSubtree: node budget must be positive - n="
          <> show n
          <> ", depth="
          <> show depth
          <> ", maxChildren="
          <> show maxChildren

  | n == 1 || depth == 0 =
      Gen.element [ Leaf, Void ]

  | otherwise = do
      let
        (minChildren, maxChildrenSuggested) =
          suggestBranching (n - 1) depth maxChildren

      let
        safeMin = max 1 minChildren
        safeMax = max safeMin maxChildrenSuggested

      numChildren <- Gen.int $ Range.linear safeMin safeMax
      sizes <- partitionExactly (n - 1) numChildren
      children <-
        traverse (\s -> buildSubtree s (depth - 1) maxChildren) sizes

      pure $ Branch children

partitionExactly :: Int -> Int -> Gen [Int]
partitionExactly total parts
  | parts < 1 = error "partitionExactly: number of parts must be >= 1"
  | total < parts = error "partitionExactly: total must be >= parts"
  | parts == 1 = pure [ total ]
  | otherwise = do
      cuts <- Gen.shuffle [ 1 .. (total - 1) ]

      let
        selected = take (parts - 1) cuts
        positions = sort (0 : selected <> [ total ])

      pure $ zipWith (-) (tail positions) positions

countTotalNodes :: Node -> Int
countTotalNodes node =
  case node of
    Root n1 n2 -> 1 + countTotalNodes n1 + countTotalNodes n2
    Branch nodes -> 1 + sum (countTotalNodes <$> nodes)
    Leaf -> 1
    Void -> 1
