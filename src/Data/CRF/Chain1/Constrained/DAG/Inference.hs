{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}


-- | Inference with CRFs.


module Data.CRF.Chain1.Constrained.DAG.Inference
( tag
, tagK
, marginals
, accuracy
, expectedFeaturesIn
, zx
, zx'

-- , probability
-- , likelihood

-- * Internals
, computePsi
) where


import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.List (maximumBy, sort, sortBy)
import Data.Function (on)
import qualified Data.Set as S
import qualified Data.Array as A
-- import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Foldable as F

import Control.Parallel.Strategies (rseq, parMap)
import Control.Parallel (par, pseq)
import GHC.Conc (numCapabilities)
import qualified Data.Number.LogFloat as L

-- import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (EdgeID, DAG)
-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
import           Data.DAG (EdgeID, DAG)
import qualified Data.DAG as DAG

import qualified Data.CRF.Chain1.Constrained.DP as DP
import           Data.CRF.Chain1.Constrained.Util (partition)
import qualified Data.CRF.Chain1.Constrained.Model as Md

import           Data.CRF.Chain1.Constrained.Core (X, Y, Lb, AVec)
import qualified Data.CRF.Chain1.Constrained.Core as C
import qualified Data.CRF.Chain1.Constrained.Intersect as I

import           Data.CRF.Chain1.Constrained.DAG.Feature (featuresIn)

import Debug.Trace (trace)


---------------------------------------------
-- Util Types
---------------------------------------------


-- | TODO.
type LbIx       = Int


-- | The probability array assigns some probability to each label (represented
-- by its index) which can be assigned to a given edge (represented by its
-- `EdgeID`).
type ProbArray  = EdgeID -> LbIx -> L.LogFloat


---------------------------------------------
-- Summing
---------------------------------------------


-- -- | Numerically safer summing.
-- safeSum :: (Ord a, Num a) => [a] -> a
-- -- safeSum = sum . sort
-- safeSum = sum
-- {-#INLINE safeSum #-}


-- | Numerically safer summing.
safeSum :: [L.LogFloat] -> L.LogFloat
safeSum [] = 0
safeSum xs = L.sum xs
{-#INLINE safeSum #-}


---------------------------------------------
-- Some basic functions.
---------------------------------------------


-- | Vector of potential labels on the given edge of the sentence.
lbVec :: Md.Model -> DAG a X -> EdgeID -> AVec Lb
lbVec crf dag edgeID = case DAG.edgeLabel edgeID dag of
  C.X _   -> (Md.r0 crf)
  C.R _ r -> r
{-# INLINE lbVec #-}


-- | Number of potential labels on the given edge of the sentence.
lbNum :: Md.Model -> DAG a X -> EdgeID -> Int
lbNum crf dag = (U.length . C.unAVec) . lbVec crf dag
{-# INLINE lbNum #-}


-- | Potential label on the given vector position.
lbOn :: Md.Model -> X -> LbIx -> Lb
lbOn crf (C.X _)   = (C.unAVec (Md.r0 crf) U.!)
lbOn _   (C.R _ r) = (C.unAVec r U.!)
{-# INLINE lbOn #-}


-- | Potential labels on the given sentence edge (as in `lbVec`), accompanied
-- with the corresponding indexes. I.e., each label `Lb` is accompanied with a
-- number, from [0..], corresponding to its index in the vector of labels
-- obtained with `lbVec`.
lbIxs :: Md.Model -> DAG a X -> EdgeID -> [(LbIx, Lb)]
lbIxs crf dag = zip [0..] . U.toList . C.unAVec . lbVec crf dag
{-# INLINE lbIxs #-}


---------------------------------------------
-- A bit more complex stuff.
---------------------------------------------


-- | Compute the table of potential products associated with observation
-- features for the given sentence edge.
computePsi :: Md.Model -> DAG a X -> EdgeID -> LbIx -> L.LogFloat
computePsi crf dag i = (A.!) $ A.accumArray (*) 1 bounds
    [ (k, Md.valueL crf ix)
    | ob <- C.unX (DAG.edgeLabel i dag)
    , (k, ix) <- I.intersect (Md.obIxs crf ob) (lbVec crf dag i) ]
  where
    bounds = (0, lbNum crf dag i - 1)


-- | Equivalent to `computePsi`, but memoizes additionally on `EdgeID`s.
computePsi'
  :: Md.Model -> DAG a X
  -> EdgeID -> LbIx
  -> L.LogFloat
computePsi' crf dag =
  (array A.!)
  where
    bounds = (DAG.minEdge dag, DAG.maxEdge dag)
    array = A.array bounds
      [ (i, computePsi crf dag i)
      | i <- A.range bounds ]


-- | Forward table computation.
forward :: Md.Model -> DAG a X -> ProbArray
forward crf dag = alpha where
  alpha = DP.flexible2 bounds boundsOn
    (\t i -> withMem (computePsi crf dag i) t i)
  bounds = (DAG.minEdge dag, DAG.maxEdge dag + 1)
  boundsOn i
    | i == snd bounds = (0, 0)
    | otherwise = (0, lbNum crf dag i - 1)
  -- set of initial edges
  initialSet = S.fromList
    [ i
    | i <- DAG.dagEdges dag
    , DAG.isInitialEdge i dag ]
  withMem psi alpha i
    | i == snd bounds = const u'
    | i `S.member` initialSet = \j ->
        let x = lbOn crf (DAG.edgeLabel i dag) j
        in  psi j * Md.sgValue crf x
    | otherwise = \j ->
        let x = lbOn crf (DAG.edgeLabel i dag) j
        in  psi j * ((u - v x) + w x)
    where
      u = safeSum
        [ alpha iMinus1 k
        | iMinus1 <- DAG.prevEdges i dag
        , (k, _) <- lbIxs crf dag iMinus1 ]
      v x = safeSum
        [ alpha iMinus1 k
        | iMinus1 <- DAG.prevEdges i dag
        , (k, _) <- I.intersect (Md.prevIxs crf x) (lbVec crf dag iMinus1) ]
      w x = safeSum
        [ alpha iMinus1 k * Md.valueL crf ix
        | iMinus1 <- DAG.prevEdges i dag
        , (k, ix) <- I.intersect (Md.prevIxs crf x) (lbVec crf dag iMinus1) ]
      -- Note that if `i == snd bounds` then `i` does not refer to any existing
      -- edge, hence the need to introduce `u'` which does almost the same thing
      -- as `u`.
      u' = safeSum
        [ alpha iMinus1 k
        | iMinus1 <- DAG.dagEdges dag
        , DAG.isFinalEdge iMinus1 dag
        , (k, _) <- lbIxs crf dag iMinus1 ]


-- | Backward table computation.
backward :: Md.Model -> DAG a X -> ProbArray
backward crf dag = beta where
  beta = DP.flexible2 bounds boundsOn withMem
  bounds = (DAG.minEdge dag - 1, DAG.maxEdge dag)
  boundsOn i
    | i == fst bounds = (0, 0)
    | otherwise = (0, lbNum crf dag i - 1)
  psi = computePsi' crf dag
  -- set of final edges
  finalSet = S.fromList
    [ i
    | i <- DAG.dagEdges dag
    , DAG.isFinalEdge i dag ]
  withMem beta i
    | i `S.member` finalSet = const 1
    | i == fst bounds = const $ safeSum
      [ beta iPlus1 k * psi iPlus1 k
        * Md.sgValue crf (lbOn crf (DAG.edgeLabel iPlus1 dag) k)
      | iPlus1 <- DAG.dagEdges dag
      , DAG.isInitialEdge iPlus1 dag
      , (k, _) <- lbIxs crf dag iPlus1 ]
    | otherwise = \j ->
        let y = lbOn crf (DAG.edgeLabel i dag) j
        in  (u - v y) + w y
    where
      -- Note that here `i` is an identifier of the current DAG edge.
      -- Instead of simply adding `1` to `i` (i.e., `i + 1`),
      -- we need to find the identifiers of the succeeding edges.
      u = safeSum
        [ beta iPlus1 k * psi iPlus1 k
        | iPlus1 <- DAG.nextEdges i dag
        , (k, _ ) <- lbIxs crf dag iPlus1 ]
      -- `y` is the label on position `i`, we are looking for
      -- matching labels on the position `i+1`.
      v y = safeSum
        [ beta iPlus1 k * psi iPlus1 k
        | iPlus1 <- DAG.nextEdges i dag
        , (k, _ ) <- I.intersect (Md.nextIxs crf y) (lbVec crf dag iPlus1) ]
      -- `y` is the label on position `i`, we are looking for
      -- matching labels on the position `i+1`.
      w y = safeSum
        [ beta iPlus1 k * psi iPlus1 k * Md.valueL crf ix
        | iPlus1 <- DAG.nextEdges i dag
        , (k, ix) <- I.intersect (Md.nextIxs crf y) (lbVec crf dag iPlus1) ]


-- | Normalization factor computed for the 'Xs' sentence using the
-- forward computation.
zx' :: Md.Model -> DAG a X -> L.LogFloat
zx' crf dag = zxAlpha dag (forward crf dag)

zxAlpha :: DAG a b -> ProbArray -> L.LogFloat
zxAlpha dag alpha = alpha (DAG.maxEdge dag + 1) 0


-- | Normalization factor computed for the 'Xs' sentence using the
-- backward computation.
zx :: Md.Model -> DAG a X -> L.LogFloat
zx crf dag = zxBeta dag (backward crf dag)

zxBeta :: DAG a b -> ProbArray -> L.LogFloat
zxBeta dag beta = beta (DAG.minEdge dag - 1) 0


-- prob1 :: ProbArray -> ProbArray -> Int -> LbIx -> L.LogFloat
-- prob1 alpha beta k x =
--     alpha k x * beta (k + 1) x / zxBeta beta
-- {-# INLINE prob1 #-}

-- | Probability of chosing the given edge and the corresponding label.
edgeProb1
  :: DAG a b
  -- ^ The underlying sentence DAG
  -> ProbArray
  -- ^ Forward probability table
  -> ProbArray
  -- ^ Backward probability table
  -> EdgeID
  -- ^ ID of the edge in the underlying DAG
  -> LbIx
  -- ^ Index of the label of the edge represented by the `EdgeID`
  -> L.LogFloat
edgeProb1 dag alpha beta k x
  -- alpha k x * beta k x / zxBeta dag beta
  | any isInf [up1, up2, down] =
      error $ "edgeProb1: infinite -- " ++ show [up1, up2, down, down'] -- ++ "; " ++ show (k, x)
  | otherwise = up1 * up2 / down
  where
    isInf x = isInfinite (L.logFromLogFloat x :: Double)
    up1 = alpha k x
    up2 = beta k x
    down = zxBeta dag beta
    down' = zxAlpha dag alpha
{-# INLINE edgeProb1 #-}


-- | Probability of chosing the given pair of edges and the corresponding labels.
edgeProb2
  :: Md.Model
  -- ^ CRF model
  -> DAG a b
  -- ^ The underlying sentence DAG
  -> ProbArray
  -- ^ Forward computation table
  -> ProbArray
  -- ^ Backward computation table
  -> (EdgeID -> LbIx -> L.LogFloat)
  -- ^ Psi computation
  -> (EdgeID, LbIx)
  -- ^ First edge and the corresponding label index
  -> (EdgeID, LbIx)
  -- ^ Succeeding edge and the corresponding label index
  -> Md.FeatIx
  -- ^ TODO (NO IDEA!); Hypo: index of the transition feature corresponding
  -- to the transition between the first and the succeeding edge
  -> L.LogFloat
edgeProb2 crf dag alpha beta psi (kEdgeID, xLbIx) (lEdgeID, yLbIx) ix
  -- = alpha kEdgeID xLbIx * beta lEdgeID yLbIx
  -- -- * psi lEdgeID yLbIx * Md.valueL crf ix / zxBeta dag beta
  | any isInf [up1, up2, up3, up4, down] =
      error $ "edgeProb2: infinite -- " ++ show [up1, up2, up3, up4, down, down']
  | otherwise = up1 * up2 * up3 * up4 / down
  where
    isInf x = isInfinite (L.logFromLogFloat x :: Double)
    up1 = alpha kEdgeID xLbIx
    up2 = beta lEdgeID yLbIx
    up3 = psi lEdgeID yLbIx
    up4 = Md.valueL crf ix
    down = zxBeta dag beta
    down' = zxAlpha dag alpha
{-# INLINE edgeProb2 #-}


-- prob2 :: Model -> ProbArray -> ProbArray -> Int -> (LbIx -> L.LogFloat)
--       -> LbIx -> LbIx -> FeatIx -> L.LogFloat
-- prob2 crf alpha beta k psi x y ix
--     = alpha (k - 1) y * beta (k + 1) x
--     * psi x * valueL crf ix / zxBeta beta
-- {-# INLINE prob2 #-}


-- | Tag potential labels with marginal distributions.
-- marginals :: Md.Model -> DAG a X -> [[(Lb, L.LogFloat)]]
marginals :: Md.Model -> DAG a X -> DAG a [(Lb, L.LogFloat)]
marginals crf dag
  | not (zx1 `almostEq` zx2) = trace warning margs
  | otherwise = margs
  where
    margs = DAG.mapE label dag
    warning =
      "[marginals] normalization factors differ significantly: "
      ++ show (L.logFromLogFloat zx1, L.logFromLogFloat zx2)
    label edgeID _ =
      [ (lab, prob1 edgeID labID)
      | (labID, lab) <- lbIxs crf dag edgeID ]
    prob1 = edgeProb1 dag alpha beta
    alpha = forward crf dag
    beta = backward crf dag
    zx1 = zxAlpha dag alpha
    zx2 = zxBeta dag beta


-- | Get (at most) k best tags for each word and return them in
-- descending order.  TODO: Tagging with respect to marginal
-- distributions might not be the best idea.  Think of some
-- more elegant method.
tagK :: Int -> Md.Model -> DAG a X -> DAG a [(Lb, L.LogFloat)]
tagK k crf dag = fmap
    ( take k
    . reverse
    . sortBy (compare `on` snd)
    ) (marginals crf dag)


-- | Find the most probable label sequence (with probabilities of individual
-- lables determined with respect to marginal distributions) satisfying the
-- constraints imposed over label values.
tag :: Md.Model -> DAG a X -> DAG a Lb
tag crf = fmap (fst . head) . (tagK 1 crf)


expectedFeaturesOn
  :: Md.Model
  -- ^ CRF model
  -> DAG a X
  -- ^ The underlying sentence DAG
  -> ProbArray
  -- ^ Forward computation table
  -> ProbArray
  -- ^ Backward computation table
  -> EdgeID
  -- ^ ID of an edge of the underlying DAG
  -> [(Md.FeatIx, L.LogFloat)]
expectedFeaturesOn crf dag alpha beta iEdgeID =
  tFeats ++ oFeats
  where
    prob1 = edgeProb1 dag alpha beta iEdgeID
    oFeats = [ (ix, prob1 k)
             | ob <- C.unX (DAG.edgeLabel iEdgeID dag)
             , (k, ix) <- I.intersect (Md.obIxs crf ob) (lbVec crf dag iEdgeID) ]

    -- TODO: Move `psi` to `expectedFeatureIn`
    psi = computePsi' crf dag -- iEdgeID
    prob2 = edgeProb2 crf dag alpha beta psi
    tFeats
        | DAG.isInitialEdge iEdgeID dag = catMaybes
          [ (, prob1 k) <$> Md.featToIx crf (C.SFeature x)
          | (k, x) <- lbIxs crf dag iEdgeID ]
        | otherwise =
          [ (ix, prob2 (iMinus1, l) (iEdgeID, k) ix)
          | (k,  x) <- lbIxs crf dag iEdgeID
          , iMinus1 <- DAG.prevEdges iEdgeID dag
          , (l, ix) <- I.intersect (Md.prevIxs crf x) (lbVec crf dag iMinus1) ]


-- | A list of features (represented by feature indices) defined within
-- the context of the sentence accompanied by expected probabilities
-- determined on the basis of the model.
--
-- One feature can occur multiple times in the output list.
expectedFeaturesIn
  :: Md.Model
  -> DAG a X
  -> [(Md.FeatIx, L.LogFloat)]
expectedFeaturesIn crf dag = zxF `par` zxB `pseq` zxF `pseq`
    -- concat [expectedOn k | k <- [0 .. V.length xs - 1] ]
    concat [expectedOn edgeID | edgeID <- DAG.dagEdges dag]
  where
    expectedOn = expectedFeaturesOn crf dag alpha beta
    alpha = forward crf dag
    beta = backward crf dag
    zxF = zxAlpha dag alpha
    zxB = zxBeta dag beta


-- goodAndBad :: Md.Model -> DAG a X -> DAG b Y -> (Int, Int)
goodAndBad :: Md.Model -> DAG a (X, Y) -> (Int, Int)
goodAndBad crf dag =
    F.foldl' gather (0, 0) $ DAG.zipE labels labels'
  where
    xs = fmap fst dag
    ys = fmap snd dag
    labels = fmap (best . C.unY) ys
    best zs
        | null zs   = Nothing
        | otherwise = Just . fst $ maximumBy (compare `on` snd) zs
    labels' = fmap Just $ tag crf xs
    gather (good, bad) (x, y)
        | x == y = (good + 1, bad)
        | otherwise = (good, bad + 1)


goodAndBad' :: Md.Model -> [DAG a (X, Y)] -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  F.foldl' add (0, 0) [goodAndBad crf x | x <- dataset]


-- | Compute the accuracy of the model with respect to the labeled dataset.
accuracy :: Md.Model -> [DAG a (X, Y)] -> Double
accuracy crf dataset =
    let k = numCapabilities
    	parts = partition k dataset
        xs = parMap rseq (goodAndBad' crf) parts
        (good, bad) = F.foldl' add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)


---------------------------------------------
-- Probability and likelihood
---------------------------------------------


-- -- | Log-likelihood of the given dataset.
-- likelihood :: Md.Model -> [DAG a (X, Y)] -> L.LogFloat
-- -- likelihood crf = L.product . map (probability crf)
-- -- likelihood crf = probability crf . head
-- likelihood crf = maximum . map (probability crf)
--
--
-- -- | The conditional probability of the dag in log-domain.
-- probability :: Md.Model -> DAG a (X, Y) -> L.LogFloat
-- probability crf dag = normFactor
-- --   | potential > normFactor =
-- --       error $ "[probability] potential greater than normFactor: "
-- --       ++ show (potential, normFactor)
-- --   | otherwise = potential / normFactor
--   where
--     potential = L.product
--       [ Md.valueL crf (Md.featToJustIx crf feat)
--       | (feat, _val) <- featuresIn dag ]
--     normFactor = zx crf (fmap fst dag)


-- -- | Features w.r.t. a given edge.
-- features
--   :: Md.Model
--   -> EdgeID  -- ^ ID of an edge of the DAG
--   -> DAG a (X, Y)
--   -> [Md.FeatIx]
-- features crf edgeID dag =
--   where
--     oFeats = [ (ix, prob1 k)
--              | ob <- C.unX (DAG.edgeLabel iEdgeID dag)
--              , (k, ix) <- I.intersect (Md.obIxs crf ob) (lbVec crf dag iEdgeID) ]


---------------------------------------------
-- Utils
---------------------------------------------


almostEq :: L.LogFloat -> L.LogFloat -> Bool
almostEq x0 y0
  | isZero x && isZero y = True
  | otherwise = 1.0 - eps < z && z < 1.0 + eps
  where
    x = L.logFromLogFloat x0
    y = L.logFromLogFloat y0
    z = x / y


isZero :: (Fractional t, Ord t) => t -> Bool
isZero x = abs x < eps


-- | A very small number.
eps :: Fractional t => t
eps = 0.000001
