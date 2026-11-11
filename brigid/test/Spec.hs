module Main
  ( main
  ) where

import Test.Tasty qualified as Tasty

import HTML.Render.Attributes qualified as Attrs
import HTML.Render.Elements qualified as Elements

main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup "Brigid tests"
      [ Elements.tests
      , Attrs.tests
      ]
