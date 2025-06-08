{-# LANGUAGE TupleSections #-}

module Generation.Build
  ( generateDOM
  ) where

import Control.Monad (foldM, forM, replicateM)
import Control.Monad.State (StateT, execStateT, get, lift, modify)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.UUID (UUID)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Prelude hiding (head)

import Generation.Element qualified as E
import Generation.Generators qualified as Generators
import Generation.Types qualified as Types

type LayerMap = IntMap [UUID]
type ChildMap = Map UUID [UUID]
type NodeInfoMap = Map UUID (Types.ElementType, Types.NodeType)

type GenM a = StateT Context Gen a

data Context =
  Context
    { genParams :: Types.GeneratorParams
    , layerMap  :: LayerMap
    , childMap  :: ChildMap
    , nodeInfo  :: NodeInfoMap
    } deriving Show

data LayerItem =
  LayerItem
    { nodeId :: UUID
    , parentId :: Maybe UUID
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
            , Types.maxTotalNodes = Types.maxTotalNodes params - headNodes - 1
            , Types.maxDepth = Types.maxDepth params - 1
            }

      head <-
        generateElement $
          params
            { Types.startingElement = Types.Head
            , Types.maxTotalNodes = headNodes
            , Types.maxDepth = Types.maxDepth params - 1
            , Types.childrenPerNode = Types.mkRange headNodes headNodes
            }

      pure . Types.Element Types.Html [] $ Types.Branch [ head, body ]

    _element ->
      generateElement params

generateElement :: Types.GeneratorParams -> Gen Types.Element
generateElement params = do
  rootId <- Generators.uuid
  finalState <-
    execStateT (buildLayer [ LayerItem rootId Nothing 1 ]) $
      Context
        { genParams = params
        , layerMap = IntMap.singleton 1 [ rootId ]
        , childMap = Map.empty
        , nodeInfo =
            Map.singleton
              rootId
              (Types.startingElement params, Types.BranchNode)
        }

  buildTree finalState rootId

buildLayer :: [LayerItem] -> GenM ()
buildLayer [] = pure ()
buildLayer queue = do
  ctx <- get

  let
    params = genParams ctx
    nodeCount = Map.size $ nodeInfo ctx

  if nodeCount >= Types.maxTotalNodes params
    then pure ()
    else do
      nextQueue <-
        fmap concat . forM queue $ \(LayerItem nid _parent depth) -> do
          if depth >= Types.maxDepth params
            then pure []
            else do
              updatedCtx <- get

              let
                currentCount = Map.size (nodeInfo updatedCtx)
                remaining = Types.maxTotalNodes params - currentCount
                range = Types.childrenPerNode params
                maxAllowed = min (Types.maxRange range) remaining
                minAllowed = min (Types.minRange range) maxAllowed

              rangeList <- Gen.shuffle [ minAllowed .. maxAllowed ]
              childCount <- lift $ Gen.element rangeList
              children <-
                replicateM (min remaining childCount)
                  . genChild nid
                  $ depth + 1

              ctxAfterChildren <- get
              pure
                [ child
                | child@(LayerItem cid _parent _depth) <- children
                , case Map.lookup cid (nodeInfo ctxAfterChildren) of
                    Just (_elementType, Types.BranchNode) -> True
                    _ -> False
                ]

      buildLayer nextQueue

genChild :: UUID -> Int -> GenM LayerItem
genChild pid depth = do
  ctx <- get
  cid <- lift Generators.uuid

  case Map.lookup pid (nodeInfo ctx) of
    Just (parentType, _parentNodeType) -> do
      let
        allValidChildren = E.elementValidChildren parentType
        branchable = Set.intersection E.branchElements allValidChildren
        validChildren =
          Set.toList $
            if Set.null branchable
              then allValidChildren
              else branchable

        weightedChildren =
          flip mapMaybe validChildren $ \child ->
            (,pure child) <$> E.elementWeight child

      childType <- lift $ Gen.frequency weightedChildren
      modify $ \s ->
        s { nodeInfo =
              Map.insert
                cid
                (childType, E.elementNodeType childType)
                (nodeInfo s)
          , childMap = Map.insertWith (<>) pid [ cid ] (childMap s)
          , layerMap = IntMap.insertWith (<>) depth [ cid ] (layerMap s)
          }

      pure $ LayerItem cid (Just pid) depth

    Nothing ->
      fail $ "No parent for UUID " <> show cid

buildTree :: Context -> UUID -> Gen Types.Element
buildTree ctx rootId = do
  let
    maxLayer =
      if IntMap.null (layerMap ctx)
        then 0
        else fst (IntMap.findMax $ layerMap ctx)

    buildLayerNodes :: Map UUID Types.Element -> Int -> Gen (Map UUID Types.Element)
    buildLayerNodes acc depth =
      maybe (pure acc) (foldM buildOne acc)
        . IntMap.lookup depth
        $ layerMap ctx

    buildOne :: Map UUID Types.Element -> UUID -> Gen (Map UUID Types.Element)
    buildOne acc nid =
      case Map.lookup nid (nodeInfo ctx) of
        Just (elementType, nodeType) -> do
          attrs <- E.withGlobalAttrs (genParams ctx) elementType

          case nodeType of
            Types.BranchNode -> do
              let
                children =
                  mapMaybe (`Map.lookup` acc)
                    . Map.findWithDefault [] nid
                    $ childMap ctx

              content <-
                if null children && Set.member elementType E.leafBranchElements
                  then Types.Leaf <$> Generators.nonEmptyText
                  else pure $ Types.Branch children

              pure $
                Map.insert
                  nid
                  (Types.Element elementType attrs content)
                  acc

            Types.LeafNode -> do
              text <- Generators.nonEmptyText
              pure $
                Map.insert
                  nid
                  (Types.Element elementType attrs $ Types.Leaf text)
                  acc

            Types.VoidNode ->
              pure $
                Map.insert
                  nid
                  (Types.Element elementType attrs Types.Void)
                  acc

        Nothing ->
          pure acc

  builtMap <- foldM buildLayerNodes Map.empty [ maxLayer, maxLayer - 1 .. 0 ]

  case Map.lookup rootId builtMap of
    Just node -> pure node
    Nothing -> fail "Failed to build root node"
