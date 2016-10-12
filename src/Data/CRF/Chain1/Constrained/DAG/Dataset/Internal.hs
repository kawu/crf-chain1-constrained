{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Data.CRF.Chain1.Constrained.DAG.Dataset.Internal
(
-- * Types
  DAG
, NodeID
, EdgeID

-- * Primitive Operations
, begsWith
, endsWith
, ingoingEdges
, outgoingEdges
, nodeLabel
, edgeLabel

-- * Intermediate Operations
, prevEdges
, isInitialEdge
-- , isInitialNode
, nextEdges
, isFinalEdge

, minEdge
, maxEdge

, mapE
, zipE

-- * Advanced Operations
, dagNodes
, dagEdges

-- * Conversion
, fromList
-- ** Provisional
, toListProv

-- * Check
, isOK
) where


import qualified Data.Foldable as F
import qualified Data.Array as A
import qualified Data.Vector as V

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Binary (Binary, get, put, putWord8, getWord8)
import Data.Vector.Binary ()
-- import qualified Data.Binary as B


------------------------------------------------------------------
-- Types
------------------------------------------------------------------


-- | A directed acyclic graph (DAG) with nodes of type `a` and
-- edges of type `b`.
data DAG a b = DAG
  { nodeMap :: M.Map NodeID (Node a)
  , edgeMap :: M.Map EdgeID (Edge b)
  } deriving (Functor, F.Foldable)

instance (Binary a, Binary b) => Binary (DAG a b) where
  put = undefined
  get = undefined


-- | Node ID.
newtype NodeID = NodeID {_unNodeID :: Int}
  deriving (Show, Eq, Ord)


-- | Node of the DAG.
data Node a = Node
  { ingoSet :: S.Set EdgeID
  , outgoSet :: S.Set EdgeID
  , ndLabel :: a }
  deriving (Show, Eq, Ord)


-- | ID of an edge. The following properties must be satisfied by `EdgeID`:
--
--   * The ordering of edge IDs (`Ord` instance) is consistent with the
--     topological ordering of the edges.
--   * The smallest `EdgeID` of a given DAG, `minEdge`, is equal
--     to `0` (`EdgeID 0`).
--
-- Additional important property, which guarantees that inference computations
-- over the DAG, based on dynamic programming, are efficient:
--
--   * Let `e` be the greatest `EdgeID` in the DAG. Then, the set of `EdgeID`s
--     in the DAG is equal to {0 .. e}.
--
-- However, this last property is not required for the correcntess of the
-- inference computations, only for their memory complexity.
newtype EdgeID = EdgeID {_unEdgeID :: Int}
  deriving (Show, Eq, Ord, Num, A.Ix)


-- | Edge of the DAG.
data Edge a = Edge
  { tailNode :: NodeID
  , headNode :: NodeID
  , edLabel  :: a }
  deriving (Show, Eq, Ord, Functor, F.Foldable)


------------------------------------------------------------------
-- Primitive Operations
------------------------------------------------------------------


-- | Return the tail node of the given edge.
begsWith :: EdgeID -> DAG a b -> NodeID
begsWith i DAG{..} = case M.lookup i edgeMap of
  Nothing -> error "begsWith: incorrent edge ID"
  Just Edge{..} -> tailNode


-- | Return the head node of the given edge.
endsWith :: EdgeID -> DAG a b -> NodeID
endsWith i DAG{..} = case M.lookup i edgeMap of
  Nothing -> error "endsWith: incorrent edge ID"
  Just Edge{..} -> headNode


-- | The list of outgoint edges from the given node.
ingoingEdges :: NodeID -> DAG a b -> [EdgeID]
ingoingEdges i DAG{..} = case M.lookup i nodeMap of
  Nothing -> error "ingoingEdges: incorrect ID"
  Just Node{..} -> S.toList ingoSet


-- | The list of outgoint edges from the given node.
outgoingEdges :: NodeID -> DAG a b -> [EdgeID]
outgoingEdges i DAG{..} = case M.lookup i nodeMap of
  Nothing -> error "outgoingEdges: incorrect ID"
  Just Node{..} -> S.toList outgoSet


-- | The label assigned to the given node.
nodeLabel :: NodeID -> DAG a b -> a
nodeLabel i DAG{..} = case M.lookup i nodeMap of
  Nothing -> error "nodeLabel: incorrect ID"
  Just Node{..} -> ndLabel


-- | The label assigned to the given node.
edgeLabel :: EdgeID -> DAG a b -> b
edgeLabel i DAG{..} = case M.lookup i edgeMap of
  Nothing -> error "edgeLabel: incorrent ID"
  Just Edge{..} -> edLabel


-- | The greatest `EdgeID` in the DAG.
minEdge :: DAG a b -> EdgeID
minEdge = fst . M.findMin . edgeMap


-- | The greatest `EdgeID` in the DAG.
maxEdge :: DAG a b -> EdgeID
maxEdge = fst . M.findMax . edgeMap


------------------------------------------------------------------
-- Not-so-primitive ops, but still looking at the implementation
------------------------------------------------------------------


-- | The list of DAG nodes.
dagNodes :: DAG a b -> [NodeID]
dagNodes = M.keys . nodeMap


-- | The list of DAG edges.
dagEdges :: DAG a b -> [EdgeID]
dagEdges = M.keys . edgeMap


-- | Similar to `fmap` but the mapping function has access to IDs of the
-- individual edges.
mapE :: (EdgeID -> b -> c) -> DAG a b -> DAG a c
mapE f dag =
  dag {edgeMap = edgeMap'}
  where
    edgeMap' = M.fromList
      [ (edgeID, edge {edLabel = newLabel})
      | (edgeID, edge) <- M.toList (edgeMap dag)
      , let newLabel = f edgeID (edLabel edge) ]


-- | Zip labels assigned to the same edges in the two input DAGs. Node labels
-- from the first DAG are preserved. The function fails if the input DAGs
-- contain different sets of edge IDs or node IDs.
zipE :: DAG a b -> DAG x c -> DAG a (b, c)
zipE dagL dagR
  | M.keysSet (nodeMap dagL) /= M.keysSet (nodeMap dagR) =
      error "zipE: different sets of node IDs"
  | M.keysSet (edgeMap dagL) /= M.keysSet (edgeMap dagR) =
      error "zipE: different sets of edge IDs"
  | otherwise = DAG
      { nodeMap = newNodeMap
      , edgeMap = newEdgeMap }
  where
    newNodeMap = nodeMap dagL
    newEdgeMap = M.fromList
      [ (edgeID, newEdge)
      | edgeID <- M.keys (edgeMap dagL)
      , let newEdge = mergeEdges
              (edgeMap dagL M.! edgeID)
              (edgeMap dagR M.! edgeID) ]
    mergeEdges e1 e2
      | tailNode e1 /= tailNode e2 =
          error "zipE.mergEdges: different tail nodes"
      | headNode e1 /= headNode e2 =
          error "zipE.mergEdges: different head nodes"
      | otherwise =
          let newLabel = (edLabel e1, edLabel e2)
          in  e1 {edLabel = newLabel}


------------------------------------------------------------------
-- Intermediate Operations
------------------------------------------------------------------


-- | The list of the preceding edges of the given edge.
prevEdges :: EdgeID -> DAG a b -> [EdgeID]
prevEdges edgeID dag =
  let tailNodeID = begsWith edgeID dag
  in  ingoingEdges tailNodeID dag


-- | Is the given edge initial?
isInitialEdge :: EdgeID -> DAG a b -> Bool
isInitialEdge edgeID = null . prevEdges edgeID


-- -- | Is the given node initial?
-- isInitialNode :: NodeID -> DAG a b -> Bool
-- isInitialNode nodeID = null . ingoingEdges nodeID


-- | The list of the succeding edges of the given edge.
nextEdges :: EdgeID -> DAG a b -> [EdgeID]
nextEdges edgeID dag =
  let headNodeID = endsWith edgeID dag
  in  outgoingEdges headNodeID dag


-- | Is the given edge initial?
isFinalEdge :: EdgeID -> DAG a b -> Bool
isFinalEdge edgeID = null . nextEdges edgeID


------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------


-- | Convert a sequence of items to a trivial DAG.
_fromList :: [a] -> DAG () a
_fromList xs = DAG
  { nodeMap = M.unions [begNodeMap, middleNodeMap, endNodeMap]
  , edgeMap = newEdgeMap }
  where

    begNodeMap =
      let node = Node
            { ingoSet  = S.empty
            , outgoSet = S.singleton $ EdgeID 0
            , ndLabel = () }
      in  M.singleton (NodeID 0) node
    middleNodeMap = M.fromList $ do
      i <- [1 .. length xs - 1]
      let node = Node
            { ingoSet  = S.singleton $ EdgeID (i-1)
            , outgoSet = S.singleton $ EdgeID i
            , ndLabel = () }
      return (NodeID i, node)
    endNodeMap =
      let n = length xs
          node = Node
            { ingoSet  = S.singleton $ EdgeID (n-1)
            , outgoSet = S.empty
            , ndLabel = () }
      in  M.singleton (NodeID n) node

    newEdgeMap = M.fromList $ do
      (i, x) <- zip [0..] xs
      let edge = Edge
            { tailNode = NodeID i
            , headNode = NodeID (i+1)
            , edLabel  = x }
      return (EdgeID i, edge)


-- | Convert a sequence of items to a trivial DAG. Afterwards, check if the
-- resulting DAG is well-structured and throw error if not.
fromList :: [a] -> DAG () a
fromList xs =
  if isOK dag
  then dag
  else error "fromList: resulting DAG not `isOK`"
  where
    dag = _fromList xs


------------------------------------------------------------------
-- Provisional
------------------------------------------------------------------


-- | Convert the DAG to a list, provided that it was constructed from a list,
-- which is not checked.
toListProv :: DAG () a -> [a]
toListProv DAG{..} =
  [ edLabel edge
  | (_edgeID, edge) <- M.toAscList edgeMap ]


------------------------------------------------------------------
-- Check
------------------------------------------------------------------


-- | Check if the DAG is well-structured.
isOK :: DAG a b -> Bool
isOK DAG{..} =
  nodeMapOK && edgeMapOK
  where
    nodeMapOK = and
      [ M.member edgeID edgeMap
      | (_nodeID, Node{..}) <- M.toList nodeMap
      , edgeID <- S.toList (S.union ingoSet outgoSet) ]
    edgeMapOK = and
      [ M.member nodeID nodeMap
      | (_edgeID, Edge{..}) <- M.toList edgeMap
      , nodeID <- [tailNode, headNode] ]
