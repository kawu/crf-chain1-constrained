-- | The module provides feature selection functions which extract
-- features present in the dataset, i.e. features which directly occure
-- the dataset.


module Data.CRF.Chain1.Constrained.DAG.Feature.Present
( presentFeats
, presentOFeats
, presentTFeats
, presentSFeats
) where



import           Data.CRF.Chain1.Constrained.Core (X, Y, Lb, Feature)
import qualified Data.CRF.Chain1.Constrained.Core as C
import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (DAG)
import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG


-- | 'OFeature's which occur in the dataset.
presentOFeats :: [DAG a (X, Y)] -> [Feature]
presentOFeats =
  concatMap sentOFeats
  where
    sentOFeats dag =
      [ C.OFeature o x
      | edgeID <- DAG.topoOrder' dag
      , let edgeLabel = DAG.edgeLabel edgeID dag
      , o <- C.unX (fst edgeLabel)
      , x <- lbs (snd edgeLabel) ]


-- | 'TFeature's which occur in the dataset.
presentTFeats :: [DAG a Y] -> [Feature]
presentTFeats =
  concatMap sentTFeats
  where
    sentTFeats dag =
      [ C.TFeature x y
      | edgeID <- DAG.topoOrder' dag
      , x <- lbs (DAG.edgeLabel edgeID dag)
      , prevEdgeID <- DAG.prevEdges edgeID dag
      , y <- lbs (DAG.edgeLabel prevEdgeID dag) ]


-- | 'SFeature's which occur in the given dataset.
presentSFeats :: [DAG a Y] -> [Feature]
presentSFeats =
  concatMap sentSFeats
  where
    sentSFeats dag =
      [ C.SFeature x
      | edgeID <- DAG.topoOrder' dag
      , DAG.isInitialEdge edgeID dag
      , x <- lbs (DAG.edgeLabel edgeID dag) ]


-- | 'Feature's of all kinds which occur in the given dataset.
presentFeats :: [DAG a (X, Y)] -> [Feature]
presentFeats ds
    =  presentOFeats ds
    ++ presentTFeats (map (fmap snd) ds)
    ++ presentSFeats (map (fmap snd) ds)


-- | Retrieve the domain of the given probability distribution represented.
lbs :: Y -> [Lb]
lbs = map fst . C.unY
