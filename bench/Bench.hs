{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Monad.Reader (liftIO, runReaderT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.IORef (newIORef)
import Hedgehog.Internal.Gen (runGenT)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (nodeValue, runTreeT)
import Hedgehog.Range (Size (..))

import Generation.Analysis qualified as A
import Generation.Element qualified as E
import Generation.Types qualified as Types

main :: IO ()
main = do
  element <-
    generate E.Html $
      Types.GeneratorParams
        { Types.maxTotalNodesParam = 1000
        , Types.maxDepthParam = 20
        , Types.maxChildrenPerNodeParam = 5
        , Types.maxAttributesPerElementParam = 8
        , Types.sizeParam = Size 1000
        , Types.seedParam = from 1000
        }

  print element
  print $ A.totalNodes element

generate :: E.ElementType -> Types.GeneratorParams -> IO E.Element
generate element params = do
  let
    size = Types.sizeParam params
    seed = Types.seedParam params

  mbNode <-
    runMaybeT . runTreeT . runGenT size seed $ do
      ref <- liftIO . newIORef $ Types.maxTotalNodesParam params

      runReaderT (E.elementGenerator element) $
        Types.GenContext
          { Types.remainingNodes = ref
          , Types.currentDepth = 0
          , Types.maxDepth = Types.maxDepthParam params
          , Types.maxChildrenPerNode = Types.maxChildrenPerNodeParam params
          , Types.maxAttributesPerNode =
              Types.maxAttributesPerElementParam params
          }

  case mbNode of
    Just node -> pure $ nodeValue node
    Nothing -> fail "Failed to generate HTML document."
