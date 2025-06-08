module Main
  ( main
  ) where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (nodeValue, runTree)
import Hedgehog.Range (Size (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Generation.Analysis qualified as A
import Generation.Build (generateDOM)
import Generation.Convert (toBlaze)
import Generation.Types qualified as Types

main :: IO ()
main = do
  let
    size = Size 30
    seed = from 0

    small =
      Types.GeneratorParams
        { Types.startingElement = Types.Html
        , Types.maxTotalNodes = 100
        , Types.maxDepth = 7
        , Types.childrenPerNode = Types.mkRange 0 5
        , Types.attributesPerNode = Types.mkRange 0 10
        , Types.enableLogging = True
        }

    _medium =
      Types.GeneratorParams
        { Types.startingElement = Types.Html
        , Types.maxTotalNodes = 1000
        , Types.maxDepth = 9
        , Types.childrenPerNode = Types.mkRange 0 6
        , Types.attributesPerNode = Types.mkRange 0 10
        , Types.enableLogging = False
        }

    _large =
      Types.GeneratorParams
        { Types.startingElement = Types.Html
        , Types.maxTotalNodes = 10000
        , Types.maxDepth = 10
        , Types.childrenPerNode = Types.mkRange 0 7
        , Types.attributesPerNode = Types.mkRange 0 10
        , Types.enableLogging = False
        }

    _stress =
      Types.GeneratorParams
        { Types.startingElement = Types.Html
        , Types.maxTotalNodes = 100000
        , Types.maxDepth = 11
        , Types.childrenPerNode = Types.mkRange 0 8
        , Types.attributesPerNode = Types.mkRange 0 10
        , Types.enableLogging = False
        }

    _absurd =
      Types.GeneratorParams
        { Types.startingElement = Types.Html
        , Types.maxTotalNodes = 1000000
        , Types.maxDepth = 12
        , Types.childrenPerNode = Types.mkRange 0 10
        , Types.attributesPerNode = Types.mkRange 0 10
        , Types.enableLogging = False
        }

    testing =
      small

  case evalGen size seed (generateDOM testing) of
    Just nodeTree -> do
      let
        node = nodeValue $ runTree nodeTree

      when (Types.enableLogging testing) $ do
        putStrLn $ Types.prettyPrint node
        putStrLn "\n\n"

      LBS8.putStrLn . renderHtml $ toBlaze node
      LBS8.putStrLn "\n\n"

      putStrLn $
        "Total Nodes: "
          <> show (A.totalNodes node)
          <> " / "
          <> show (Types.maxTotalNodes testing)

      putStrLn $
        "Depth: "
          <> show (A.maxDepth node)
          <> " / "
          <> show (Types.maxDepth testing)

    Nothing ->
      error "Structure generation failed."
