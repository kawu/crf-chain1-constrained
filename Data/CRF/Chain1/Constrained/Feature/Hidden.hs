-- | The module provides feature selection functions which extract
-- hidden features, i.e. all features which can be constructed 
-- on the basis of observations and potential labels (constraints)
-- corresponding to individual words.
--
-- You can mix functions defined here with the selection functions
-- from the "Data.CRF.Chain1.Constrained.Feature.Present" module.

module Data.CRF.Chain1.Constrained.Feature.Hidden
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
) where

import qualified Data.Vector as V

import Data.CRF.Chain1.Constrained.Dataset.Internal
import Data.CRF.Chain1.Constrained.Feature

-- | Hidden 'OFeature's which can be constructed based on the dataset.
hiddenOFeats :: [(Xs, b)] -> [Feature]
hiddenOFeats ds =
    concatMap f ds
  where
    f = concatMap oFeats . V.toList . fst
    oFeats x =
        [ OFeature o y
        | o <- unX x
        , y <- unR x ]

-- | Hidden 'TFeature's which can be constructed based on the dataset.
hiddenTFeats :: [(Xs, b)] -> [Feature]
hiddenTFeats ds =
    concatMap (tFeats . fst) ds
  where
    tFeats xs = concatMap (tFeatsOn xs) [1 .. V.length xs - 1]
    tFeatsOn xs k =
        [ TFeature x y
        | x <- unR (xs V.! k)
        , y <- unR (xs V.! (k-1)) ]

-- | Hidden 'SFeature's which can be constructed based on the dataset.
hiddenSFeats :: [(Xs, b)] -> [Feature]
hiddenSFeats ds =
    let sFeats xs = [SFeature x | x <- unR (xs V.! 0)]
    in  concatMap (sFeats . fst) ds

-- | Hidden 'Feature's of all types which can be constructed
-- based on the dataset.
hiddenFeats :: [(Xs, b)] -> [Feature]
hiddenFeats ds
    =  hiddenOFeats ds
    ++ hiddenTFeats ds
    ++ hiddenSFeats ds
