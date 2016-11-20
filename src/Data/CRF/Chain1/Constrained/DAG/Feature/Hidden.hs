-- | The module provides feature selection functions which extract
-- hidden features, i.e. all features which can be constructed
-- on the basis of observations and potential labels (constraints)
-- corresponding to individual words.
--
-- You can mix functions defined here with the selection functions
-- from the "Data.CRF.Chain1.Constrained.Feature.Present" module.

module Data.CRF.Chain1.Constrained.DAG.Feature.Hidden
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
) where

import           Data.DAG (DAG)
import qualified Data.DAG as DAG

import           Data.CRF.Chain1.Constrained.Core (AVec, X, Y, Lb, Feature)
import qualified Data.CRF.Chain1.Constrained.Core as C

-- import Data.CRF.Chain1.Constrained.Dataset.Internal
-- import Data.CRF.Chain1.Constrained.Feature

-- | Hidden 'OFeature's which can be constructed based on the dataset.
-- The default set of potential interpretations is used for all unknown words.
hiddenOFeats :: AVec Lb -> [DAG a X] -> [Feature]
hiddenOFeats r0 ds =
  concatMap oFeats ds
  where
    oFeats dag = concatMap (oFeatsOn dag) (DAG.dagEdges dag)
    oFeatsOn dag edgeID =
      [ C.OFeature o x
      | let label = DAG.edgeLabel edgeID dag
      , o <- C.unX label
      , x <- C.unR r0 label ]


-- | Hidden 'TFeature's which can be constructed based on the dataset.
-- The default set of potential interpretations is used for all unknown words.
hiddenTFeats :: AVec Lb -> [DAG a X] -> [Feature]
hiddenTFeats r0 ds =
  concatMap tFeats ds
  where
    tFeats dag =
      [ C.TFeature x y
      | i <- DAG.dagEdges dag
      , x <- C.unR r0 $ DAG.edgeLabel i dag
      , j <- DAG.prevEdges i dag
      , y <- C.unR r0 $ DAG.edgeLabel j dag ]


-- | Hidden 'SFeature's which can be constructed based on the dataset.
-- The default set of potential interpretations is used for all unknown words.
hiddenSFeats :: AVec Lb -> [DAG a X] -> [Feature]
hiddenSFeats r0 ds =
  concatMap sFeats ds
  where
    sFeats dag =
      [ C.SFeature x
      | i <- DAG.dagEdges dag
      , x <- C.unR r0 $ DAG.edgeLabel i dag ]


-- | Hidden 'Feature's of all types which can be constructed
-- on the basis of the dataset.  The default set of potential
-- interpretations is used for all unknown words.
hiddenFeats :: AVec Lb -> [DAG a X] -> [Feature]
hiddenFeats r0 ds
    =  hiddenOFeats r0 ds
    ++ hiddenTFeats r0 ds
    ++ hiddenSFeats r0 ds
