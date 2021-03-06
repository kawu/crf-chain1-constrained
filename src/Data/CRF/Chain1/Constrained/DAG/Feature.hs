module Data.CRF.Chain1.Constrained.DAG.Feature
( featuresIn
, features
) where


import qualified Data.Number.LogFloat as L

-- import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (EdgeID, DAG)
-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
import           Data.DAG (EdgeID, DAG)
import qualified Data.DAG as DAG

import           Data.CRF.Chain1.Constrained.Core (X, Y, Feature)
import qualified Data.CRF.Chain1.Constrained.Core as C


-- | Transition features with assigned probabilities for given position.
trFeats :: EdgeID -> DAG a (X, Y) -> [(Feature, L.LogFloat)]
trFeats edgeID dag =
  doit
  where
    edgeLabel = DAG.edgeLabel edgeID dag
    prevEdges = DAG.prevEdges edgeID dag
    doit
      | null prevEdges =
        [ (C.SFeature x, L.logFloat px)
        | (x, px) <- C.unY (snd edgeLabel) ]
      | otherwise =
        [ (C.TFeature x y, L.logFloat px * L.logFloat py)
        | (x, px) <- C.unY (snd edgeLabel)
        , prevEdgeID <- prevEdges
        , let prevEdgeLabel = DAG.edgeLabel prevEdgeID dag
        , (y, py) <- C.unY (snd prevEdgeLabel) ]


-- | Observation features with assigned probabilities for a given position.
obFeats :: EdgeID -> DAG a (X, Y) -> [(Feature, L.LogFloat)]
obFeats edgeID dag =
    [ (C.OFeature o x, L.logFloat px)
    | let edgeLabel = DAG.edgeLabel edgeID dag
    , (x, px) <- C.unY (snd edgeLabel)
    , o       <- C.unX (fst edgeLabel) ]


-- | Return the list of features with the corresponding probabilities
-- corresponding to the given DAG edge.
features :: EdgeID -> DAG a (X, Y) -> [(Feature, L.LogFloat)]
features edgeID dag = trFeats edgeID dag ++ obFeats edgeID dag


-- | Return the list of features, together with the corresponding probabilities
-- (specified in the dataset), in the labeled DAG.
--
-- WARNING: this function is unsuitable to compute the potential of a given (X,
-- Y) pair w.r.t. to a CRF model for at least two reasons:
-- * The parameters (second elements of the output list) are not from the model.
-- * More importantly, the function does not take into account the potential labels
--   of the OOV words.
featuresIn :: DAG a (X, Y) -> [(Feature, L.LogFloat)]
featuresIn dag = concat
  [ features edgeID dag
  | edgeID <- DAG.dagEdges dag ]
