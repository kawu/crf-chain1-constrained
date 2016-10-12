{-# LANGUAGE DeriveFunctor #-}
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
, isInitialNode
, nextEdges

, minEdge
, maxEdge

-- * Advanced Operations
, dagNodes
, dagEdges
) where


import qualified Data.Array as A


------------------------------------------------------------------
-- Types
------------------------------------------------------------------


-- | A directed acyclic graph (DAG) with nodes of type `a` and
-- edges of type `b`.
data DAG a b = DAG
  deriving (Functor)


-- | Node ID.
data NodeID = NodeID
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


------------------------------------------------------------------
-- Primitive Operations
------------------------------------------------------------------


-- | Return the tail node of the given edge.
begsWith :: EdgeID -> DAG a b -> NodeID
begsWith = undefined


-- | Return the head node of the given edge.
endsWith :: EdgeID -> DAG a b -> NodeID
endsWith = undefined


-- | The list of outgoint edges from the given node.
ingoingEdges :: NodeID -> DAG a b -> [EdgeID]
ingoingEdges = undefined


-- | The list of outgoint edges from the given node.
outgoingEdges :: NodeID -> DAG a b -> [EdgeID]
outgoingEdges = undefined


-- | The label assigned to the given node.
nodeLabel :: NodeID -> DAG a b -> a
nodeLabel = undefined


-- | The label assigned to the given node.
edgeLabel :: EdgeID -> DAG a b -> b
edgeLabel = undefined


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


-- | Is the given node initial?
isInitialNode :: NodeID -> DAG a b -> Bool
isInitialNode nodeID = null . ingoingEdges nodeID


-- | The list of the succeding edges of the given edge.
nextEdges :: EdgeID -> DAG a b -> [EdgeID]
nextEdges _edgeID _dag = undefined


-- | The greatest `EdgeID` in the DAG.
minEdge :: DAG a b -> EdgeID
minEdge _ = 0


-- | The greatest `EdgeID` in the DAG.
-- TODO: This must be computed quickly.
maxEdge :: DAG a b -> EdgeID
maxEdge = maximum . dagEdges

------------------------------------------------------------------
-- Advanced Operations
------------------------------------------------------------------


-- | The list of DAG nodes.
dagNodes :: DAG a b -> [NodeID]
dagNodes = undefined


-- | The list of DAG edges.
dagEdges :: DAG a b -> [EdgeID]
dagEdges dag =
  [ edgeID
  | nodeID <- dagNodes dag
  , edgeID <- outgoingEdges nodeID dag ]
