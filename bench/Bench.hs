module Main
  ( main
  ) where

import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (nodeValue, runTree)
import Hedgehog.Range (Size (..))

import Generation.Structure (buildHtmlTree, countTotalNodes)

main :: IO ()
main = do
  let
    size = Size 30
    seed = from 0

  -- small:          100 |  20 |  20
  -- medium:        1000 |  25 |  50
  -- large:        10000 |  50 | 100
  -- stress test: 100000 | 250 | 500

  case evalGen size seed (buildHtmlTree 100 20 20) of
    Just tree -> do
      let
        node = nodeValue $ runTree tree

      print node
      print $ countTotalNodes node

    Nothing ->
      error "Structure generation failed."
