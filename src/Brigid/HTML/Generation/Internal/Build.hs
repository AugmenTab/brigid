{-# LANGUAGE TupleSections #-}

module Brigid.HTML.Generation.Internal.Build
  ( generateDOM
  ) where

import Control.Monad (foldM, forM, replicateM)
import Control.Monad.State (StateT, execStateT, get, lift, modify)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Prelude hiding (head)

import Brigid.HTML.Generation.Internal.Elements qualified as E
import Brigid.HTML.Generation.Internal.Generators qualified as Generators
import Brigid.HTML.Generation.Internal.Types qualified as Types

type LayerMap = IntMap [Int]
type ChildMap = IntMap [Int]
type NodeInfoMap = IntMap (Types.ElementType, Types.NodeType)

type GenM a = StateT Context Gen a

data Context =
  Context
    { genParams :: Types.GeneratorParams
    , nextId    :: Int
    , layerMap  :: LayerMap
    , childMap  :: ChildMap
    , nodeInfo  :: NodeInfoMap
    } deriving Show

data LayerItem =
  LayerItem
    { nodeId :: Int
    , parentId :: Maybe Int
    , layerDepth :: Int
    } deriving Show

generateDOM :: Types.GeneratorParams -> Gen Types.Element
generateDOM params =
  case Types.startingElement params of
    Types.Html -> do
      let
        headNodes = 10

      body <-
        generateElement $
          params
            { Types.startingElement = Types.Body
            , Types.maximumTotalNodes =
                Types.maximumTotalNodes params - headNodes - 1
            , Types.maximumDepth = Types.maximumDepth params - 1
            }

      head <-
        generateElement $
          params
            { Types.startingElement = Types.Head
            , Types.maximumTotalNodes = headNodes
            , Types.maximumDepth = Types.maximumDepth params - 1
            , Types.childrenPerNode = Types.mkRange headNodes headNodes
            }

      pure . Types.Element Types.Html [] $ Types.Branch [ head, body ]

    _element ->
      generateElement params

generateElement :: Types.GeneratorParams -> Gen Types.Element
generateElement params = do
  let
    rootId = 0

  finalState <-
    execStateT (buildLayer [ LayerItem rootId Nothing 1 ]) $
      let
        startingElement = Types.startingElement params
      in
        Context
          { genParams = params
          , nextId = rootId + 1
          , layerMap = IntMap.singleton 1 [ rootId ]
          , childMap = IntMap.empty
          , nodeInfo =
              IntMap.singleton
                rootId
                ( startingElement
                , E.elementNodeType startingElement
                )
          }

  buildTree finalState rootId

buildLayer :: [LayerItem] -> GenM ()
buildLayer [] = pure ()
buildLayer queue = do
  ctx <- get

  let
    params = genParams ctx
    nodeCount = IntMap.size $ nodeInfo ctx

  if nodeCount >= Types.maximumTotalNodes params
    then pure ()
    else do
      nextQueue <-
        fmap concat . forM queue $ \(LayerItem nid _parent depth) -> do
          if depth >= Types.maximumDepth params
            then pure []
            else do
              updatedCtx <- get

              let
                currentCount = IntMap.size (nodeInfo updatedCtx)
                remaining = Types.maximumTotalNodes params - currentCount
                range = Types.childrenPerNode params
                maxAllowed = min (Types.maxRange range) remaining
                minAllowed = min (Types.minRange range) maxAllowed

              rangeList <- Gen.shuffle [ minAllowed .. maxAllowed ]
              childCount <- lift $ Gen.element rangeList
              children <-
                fmap catMaybes
                  . replicateM (min remaining childCount)
                  . genChild nid
                  $ depth + 1

              ctxAfterChildren <- get
              pure
                [ child
                | child@(LayerItem cid _parent _depth) <- children
                , case IntMap.lookup cid (nodeInfo ctxAfterChildren) of
                    Just (_elementType, Types.BranchNode) -> True
                    _ -> False
                ]

      Gen.small $ buildLayer nextQueue

genChild :: Int -> Int -> GenM (Maybe LayerItem)
genChild pid depth = do
  ctx <- get

  case IntMap.lookup pid (nodeInfo ctx) of
    Just (parentType, parentNodeType) ->
      case parentNodeType of
        Types.BranchNode ->
          let
            allValidChildren = Set.toList $ E.elementValidChildren parentType
            weightedChildren =
              flip mapMaybe allValidChildren $ \child ->
                (, pure child) <$> E.elementWeight child

          in
            if null weightedChildren
              then pure Nothing
              else do
                let
                  cid = nextId ctx

                childType <- Gen.frequency weightedChildren

                modify $ \s ->
                  s { nextId = cid + 1
                    , nodeInfo =
                        IntMap.insert
                          cid
                          (childType, E.elementNodeType childType)
                          (nodeInfo s)
                    , childMap =
                        IntMap.insertWith (<>) pid [ cid ] (childMap s)
                    , layerMap =
                        IntMap.insertWith (<>) depth [ cid ] (layerMap s)
                    }

                pure . Just $ LayerItem cid (Just pid) depth

        Types.LeafNode ->
          pure Nothing

        Types.VoidNode ->
          pure Nothing

    Nothing ->
      fail $ "Could not find element with UUID " <> show pid

buildTree :: Context -> Int -> Gen Types.Element
buildTree ctx rootId = do
  let
    maxLayer =
      if IntMap.null (layerMap ctx)
        then 0
        else fst (IntMap.findMax $ layerMap ctx)

    buildLayerNodes :: IntMap Types.Element
                    -> Int
                    -> Gen (IntMap Types.Element)
    buildLayerNodes acc depth =
      maybe (pure acc) (foldM buildOne acc)
        . IntMap.lookup depth
        $ layerMap ctx

    buildOne :: IntMap Types.Element -> Int -> Gen (IntMap Types.Element)
    buildOne acc nid =
      case IntMap.lookup nid (nodeInfo ctx) of
        Just (elementType, nodeType) -> do
          attrs <- E.withGlobalAttrs (genParams ctx) elementType

          case nodeType of
            Types.BranchNode -> do
              let
                children =
                  mapMaybe (`IntMap.lookup` acc)
                    . IntMap.findWithDefault [] nid
                    $ childMap ctx

              content <-
                if null children && Set.member elementType E.leafBranchElements
                  then Types.Leaf <$> Generators.nonEmptyText
                  else pure $ Types.Branch children

              pure $
                IntMap.insert
                  nid
                  (Types.Element elementType attrs content)
                  acc

            Types.LeafNode -> do
              text <- Generators.nonEmptyText
              pure $
                IntMap.insert
                  nid
                  (Types.Element elementType attrs $ Types.Leaf text)
                  acc

            Types.VoidNode ->
              pure $
                IntMap.insert
                  nid
                  (Types.Element elementType attrs Types.Void)
                  acc

        Nothing ->
          pure acc

  builtMap <-
    foldM buildLayerNodes IntMap.empty [ maxLayer, maxLayer - 1 .. 0 ]

  case IntMap.lookup rootId builtMap of
    Just node -> pure node
    Nothing -> fail "Failed to build root node"
