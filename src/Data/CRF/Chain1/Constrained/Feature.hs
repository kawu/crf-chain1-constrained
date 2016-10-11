module Data.CRF.Chain1.Constrained.Feature
( Feature (..)
, isSFeat
, isTFeat
, isOFeat
, featuresIn
) where


-- import Data.Binary (Binary, Get, put, get)
-- import Control.Applicative ((<*>), (<$>))
import qualified Data.Vector as V
import qualified Data.Number.LogFloat as L

import Data.CRF.Chain1.Constrained.Dataset.Internal


-- | Transition features with assigned probabilities for given position.
trFeats :: Ys -> Int -> [(Feature, L.LogFloat)]
trFeats ys 0 =
    [ (SFeature x, L.logFloat px)
    | (x, px) <- unY (ys V.! 0) ]
trFeats ys k =
    [ (TFeature x y, L.logFloat px * L.logFloat py)
    | (x, px) <- unY (ys V.! k)
    , (y, py) <- unY (ys V.! (k-1)) ]

-- | Observation features with assigned probabilities for a given position.
obFeats :: Xs -> Ys -> Int -> [(Feature, L.LogFloat)]
obFeats xs ys k =
    [ (OFeature o x, L.logFloat px)
    | (x, px) <- unY (ys V.! k)
    , o       <- unX (xs V.! k) ]

-- | All features with assigned probabilities for given position.
features :: Xs -> Ys -> Int -> [(Feature, L.LogFloat)]
features xs ys k = trFeats ys k ++ obFeats xs ys k

-- | All features with assigned probabilities in given labeled sentence.
featuresIn :: Xs -> Ys -> [(Feature, L.LogFloat)]
featuresIn xs ys = concatMap (features xs ys) [0 .. V.length xs - 1]
