module Main
  ( main
  ) where

import Test.Tasty.Bench qualified as TB
import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (treeValue)
import Hedgehog.Range (Size (..))

import Generation.Analysis (totalNodes)
import Generation.Element qualified as E

main :: IO ()
main = do
  let
    size = Size 30
    seed = from 1000

  case evalGen size seed (E.html 8 10) of
    Nothing ->
      error "Failed to generate DOM tree."

    Just tree -> do
      let
        html = treeValue tree

      print html
      print $ totalNodes html
      TB.defaultMain
        [
        ]
