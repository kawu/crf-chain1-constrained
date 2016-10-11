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

-- * Advanced Operations
, topoOrder
) where


------------------------------------------------------------------
-- Types
------------------------------------------------------------------


-- | A directed acyclic graph (DAG) with nodes of type `a` and
-- edges of type `b`.
data DAG a b = DAG


-- | Node ID.
data NodeID = NodeID


-- | Edge ID.
data EdgeID = EdgeID


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
-- Advanced Operations
------------------------------------------------------------------


-- | The list of DAG nodes in their topoligocal order.
topoOrder :: DAG a b -> [NodeID]
topoOrder = undefined
