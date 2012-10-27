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
-- The default set of potential interpretations is used for all unknown words.
hiddenOFeats :: AVec Lb -> [(Xs, b)] -> [Feature]
hiddenOFeats r0 ds =
    concatMap f ds
  where
    f = concatMap oFeats . V.toList . fst
    oFeats x =
        [ OFeature o y
        | o <- unX x
        , y <- unR r0 x ]

-- | Hidden 'TFeature's which can be constructed based on the dataset.
-- The default set of potential interpretations is used for all unknown words.
hiddenTFeats :: AVec Lb -> [(Xs, b)] -> [Feature]
hiddenTFeats r0 ds =
    concatMap (tFeats . fst) ds
  where
    tFeats xs = concatMap (tFeatsOn xs) [1 .. V.length xs - 1]
    tFeatsOn xs k =
        [ TFeature x y
        | x <- unR r0 (xs V.! k)
        , y <- unR r0 (xs V.! (k-1)) ]

-- | Hidden 'SFeature's which can be constructed based on the dataset.
-- The default set of potential interpretations is used for all unknown words.
hiddenSFeats :: AVec Lb -> [(Xs, b)] -> [Feature]
hiddenSFeats r0 ds =
    let sFeats xs = [SFeature x | x <- unR r0 (xs V.! 0)]
    in  concatMap (sFeats . fst) ds

-- | Hidden 'Feature's of all types which can be constructed
-- on the basis of the dataset.  The default set of potential
-- interpretations is used for all unknown words.
hiddenFeats :: AVec Lb -> [(Xs, b)] -> [Feature]
hiddenFeats r0 ds
    =  hiddenOFeats r0 ds
    ++ hiddenTFeats r0 ds
    ++ hiddenSFeats r0 ds
