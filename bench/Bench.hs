module Main
  ( main
  ) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (nodeValue, runTree)
import Hedgehog.Range (Size (..))
import Lucid.Base (renderBS)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Brigid.HTML.Generation qualified as G
import Brigid.HTML.Generation.Elements (ElementType (Html))
import Generation.Convert (toBlaze, toLucid)

main :: IO ()
main = do
  let
    size = Size 30
    seed = from 0

    small =
      G.GeneratorParams
        { G.startingElement = Html
        , G.maximumTotalNodes = 100
        , G.maximumDepth = 7
        , G.childrenPerNode = G.mkRange 0 5
        , G.attributesPerNode = G.mkRange 0 10
        }

    _medium =
      G.GeneratorParams
        { G.startingElement = Html
        , G.maximumTotalNodes = 1000
        , G.maximumDepth = 9
        , G.childrenPerNode = G.mkRange 0 6
        , G.attributesPerNode = G.mkRange 0 10
        }

    _large =
      G.GeneratorParams
        { G.startingElement = Html
        , G.maximumTotalNodes = 10000
        , G.maximumDepth = 10
        , G.childrenPerNode = G.mkRange 0 7
        , G.attributesPerNode = G.mkRange 0 10
        }

    _stress =
      G.GeneratorParams
        { G.startingElement = Html
        , G.maximumTotalNodes = 100000
        , G.maximumDepth = 11
        , G.childrenPerNode = G.mkRange 0 8
        , G.attributesPerNode = G.mkRange 0 10
        }

    _absurd =
      G.GeneratorParams
        { G.startingElement = Html
        , G.maximumTotalNodes = 1000000
        , G.maximumDepth = 12
        , G.childrenPerNode = G.mkRange 0 10
        , G.attributesPerNode = G.mkRange 0 10
        }

    testing =
      small

  case evalGen size seed (G.generateDOM testing) of
    Just nodeTree -> do
      let
        node = nodeValue $ runTree nodeTree

   -- putStrLn $ G.prettyPrint node
   -- putStrLn "\n"

      putStrLn "Blaze:"
      LBS8.putStrLn . renderHtml $ toBlaze node
      putStrLn "\n"

      putStrLn "Lucid:"
      LBS8.putStrLn . renderBS $ toLucid node
      putStrLn "\n"

      putStrLn $
        "Total Nodes: "
          <> show (G.totalNodes node)
          <> " / "
          <> show (G.maximumTotalNodes testing)

      putStrLn $
        "Depth: "
          <> show (G.maxDepth node)
          <> " / "
          <> show (G.maximumDepth testing)

      putStrLn "\n"

    Nothing ->
      error "Structure generation failed."
