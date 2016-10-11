{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}


-- | Inference with CRFs.


module Data.CRF.Chain1.Constrained.DAG.Inference
(
) where
-- ( tag
-- , tagK
-- , marginals
-- , accuracy
-- , expectedFeaturesIn
-- , zx
-- , zx'
-- ) where


import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.List (maximumBy, sortBy)
import Data.Function (on)
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Control.Parallel.Strategies (rseq, parMap)
import Control.Parallel (par, pseq)
import GHC.Conc (numCapabilities)
import qualified Data.Number.LogFloat as L

-- import qualified Data.CRF.Chain1.Constrained.DP as DP
-- import           Data.CRF.Chain1.Constrained.Util (partition)
-- import           Data.CRF.Chain1.Constrained.Dataset.Internal
-- import           Data.CRF.Chain1.Constrained.Feature (Feature(..))
-- import           Data.CRF.Chain1.Constrained.Model
-- import           Data.CRF.Chain1.Constrained.Intersect

import qualified Data.CRF.Chain1.Constrained.DP as DP
import           Data.CRF.Chain1.Constrained.Util (partition)
import qualified Data.CRF.Chain1.Constrained.Model as Md

import           Data.CRF.Chain1.Constrained.Core (X, Y, Lb, Feature, AVec)
import qualified Data.CRF.Chain1.Constrained.Core as C
import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (NodeID, EdgeID, DAG)
import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
import qualified Data.CRF.Chain1.Constrained.Intersect as I


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
-- Some basic functions.
---------------------------------------------


-- -- | Vector of potential labels on the given position of the sentence.
-- lbVec :: Model -> Xs -> Int -> AVec Lb
-- lbVec crf xs k = case xs V.! k of
--     X _     -> (r0 crf)
--     R _ r   -> r
-- {-# INLINE lbVec #-}

-- | Vector of potential labels on the given edge of the sentence.
lbVec :: Md.Model -> DAG a X -> EdgeID -> AVec Lb
lbVec crf dag edgeID = case DAG.edgeLabel edgeID dag of
  C.X _   -> (Md.r0 crf)
  C.R _ r -> r
{-# INLINE lbVec #-}


-- -- | Number of potential labels on the given position of the sentence.
-- lbNum :: Model -> Xs -> Int -> Int
-- lbNum crf xs = (U.length . unAVec) . lbVec crf xs
-- {-# INLINE lbNum #-}

-- | Number of potential labels on the given edge of the sentence.
lbNum :: Md.Model -> DAG a X -> EdgeID -> Int
lbNum crf dag = (U.length . C.unAVec) . lbVec crf dag
{-# INLINE lbNum #-}


-- -- | Potential label on the given vector position.
-- lbOn :: Model -> X -> Int -> Lb
-- lbOn crf (X _)   = (unAVec (r0 crf) U.!)
-- lbOn _   (R _ r) = (unAVec r U.!)
-- {-# INLINE lbOn #-}

-- | Potential label on the given vector position.
lbOn :: Md.Model -> X -> Int -> Lb
lbOn crf (C.X _)   = (C.unAVec (Md.r0 crf) U.!)
lbOn _   (C.R _ r) = (C.unAVec r U.!)
{-# INLINE lbOn #-}


-- lbIxs :: Model -> Xs -> Int -> [(Int, Lb)]
-- lbIxs crf xs = zip [0..] . U.toList . unAVec . lbVec crf xs
-- {-# INLINE lbIxs #-}

-- | Potential labels on the given sentence edge (as in `lbVec`), accompanied
-- with the corresponding indexes. I.e., each label `Lb` is accompanied with a
-- number, from [0..], corresponding to its index in the vector of labels
-- obtained with `lbVec`.
lbIxs :: Md.Model -> DAG a X -> EdgeID -> [(Int, Lb)]
lbIxs crf dag = zip [0..] . U.toList . C.unAVec . lbVec crf dag
{-# INLINE lbIxs #-}


---------------------------------------------
-- A bit more complex stuff.
---------------------------------------------


-- -- | Compute the table of potential products associated with 
-- -- observation features for the given sentence position.
-- computePsi :: Model -> Xs -> Int -> LbIx -> L.LogFloat
-- computePsi crf xs i = (A.!) $ A.accumArray (*) 1 bounds
--     [ (k, valueL crf ix)
--     | ob <- unX (xs V.! i)
--     , (k, ix) <- intersect (obIxs crf ob) (lbVec crf xs i) ]
--   where
--     bounds = (0, lbNum crf xs i - 1)

-- | Compute the table of potential products associated with observation
-- features for the given sentence edge.
computePsi :: Md.Model -> DAG a X -> EdgeID -> LbIx -> L.LogFloat
computePsi crf dag i = (A.!) $ A.accumArray (*) 1 bounds
    [ (k, Md.valueL crf ix)
    | ob <- C.unX (DAG.edgeLabel i dag)
    , (k, ix) <- I.intersect (Md.obIxs crf ob) (lbVec crf dag i) ]
  where
    bounds = (0, lbNum crf dag i - 1)


-- -- | Forward table computation.
-- forward :: Model -> Xs -> ProbArray
-- forward crf xs = alpha where
--     alpha = DP.flexible2 (0, V.length xs) bounds
--         (\t i -> withMem (computePsi crf xs i) t i)
--     bounds i
--         | i == V.length xs = (0, 0)
--         | otherwise = (0, lbNum crf xs i - 1)
--     withMem psi alpha i
--         | i == V.length xs = const u
--         | i == 0 = \j ->
--             let x = lbOn crf (xs V.! i) j
--             in  psi j * sgValue crf x
--         | otherwise = \j ->
--             let x = lbOn crf (xs V.! i) j
--             in  psi j * ((u - v x) + w x)
--       where
--         u = sum
--             [ alpha (i-1) k
--             | (k, _) <- lbIxs crf xs (i-1) ]
--         v x = sum
--             [ alpha (i-1) k
--             | (k, _) <- intersect (prevIxs crf x) (lbVec crf xs (i-1)) ]
--         w x = sum
--             [ alpha (i-1) k * valueL crf ix
--             | (k, ix) <- intersect (prevIxs crf x) (lbVec crf xs (i-1)) ]

-- | Forward table computation.
forward :: Md.Model -> DAG a X -> ProbArray
forward crf dag = alpha where
  alpha = DP.flexible2 (0, DAG.edgeMax dag + 1) bounds
    (\t i -> withMem (computePsi crf dag i) t i)
  bounds i
    | i == DAG.edgeMax dag + 1 = (0, 0)
    | otherwise = (0, lbNum crf dag i - 1)
  withMem psi alpha i
    | i == DAG.edgeMax dag + 1 = const u
    | i == 0 = \j ->
        let x = lbOn crf (DAG.edgeLabel i dag) j
        in  psi j * Md.sgValue crf x
    | otherwise = \j ->
        let x = lbOn crf (DAG.edgeLabel i dag) j
        in  psi j * ((u - v x) + w x)
    where
      -- Note that here `i` is an identifier of the current DAG edge.
      -- Instead of simply substracting `1` from `i` (i.e., `i - 1`),
      -- we need to find the identifiers of the preceding edges.
      u = sum
        [ alpha iMinus1 k
        | iMinus1 <- DAG.prevEdges i dag
        , (k, _) <- lbIxs crf dag iMinus1 ]
      v x = sum
        [ alpha iMinus1 k
        | iMinus1 <- DAG.prevEdges i dag
        , (k, _) <- I.intersect (Md.prevIxs crf x) (lbVec crf dag iMinus1) ]
      w x = sum
        [ alpha iMinus1 k * Md.valueL crf ix
        | iMinus1 <- DAG.prevEdges i dag
        , (k, ix) <- I.intersect (Md.prevIxs crf x) (lbVec crf dag iMinus1) ]


-- -- | Backward table computation.
-- backward :: Model -> Xs -> ProbArray
-- backward crf xs = beta where
--     beta = DP.flexible2 (0, V.length xs) bounds
--         (\t i -> withMem (computePsi crf xs i) t i)
--     bounds i
--         | i == 0    = (0, 0)
--         | otherwise = (0, lbNum crf xs (i-1) - 1)
--     withMem psi beta i
--         | i == V.length xs = const 1
--         | i == 0 = const $ sum
--             [ beta (i+1) k * psi k
--             * sgValue crf (lbOn crf (xs V.! i) k)
--             | (k, _) <- lbIxs crf xs i ]
--         | otherwise = \j ->
--             let y = lbOn crf (xs V.! (i-1)) j
--             in  (u - v y) + w y
--       where
--         u = sum
--             [ beta (i+1) k * psi k
--             | (k, _ ) <- lbIxs crf xs i ]
--         v y = sum
--             [ beta (i+1) k * psi k
--             | (k, _ ) <- intersect (nextIxs crf y) (lbVec crf xs i) ]
--         w y = sum
--             [ beta (i+1) k * psi k * valueL crf ix
--             | (k, ix) <- intersect (nextIxs crf y) (lbVec crf xs i) ]

-- | Backward table computation.
backward :: Md.Model -> DAG a X -> ProbArray
backward crf dag = beta where
  beta = DP.flexible2 (-1, DAG.edgeMax dag) bounds
    (\t i -> withMem (computePsi crf dag i) t i)
  bounds i
      | i == (-1) = (0, 0)
      | otherwise = (0, lbNum crf dag i - 1)
  withMem psi beta i
      | i == DAG.edgeMax dag = const 1
--       | i == (-1) = const $ sum
--           [ beta (i+1) k * psi k
--           * sgValue crf (lbOn crf (xs V.! i) k)
--           | (k, _) <- lbIxs crf xs i ]
      | otherwise = \j ->
          -- let y = lbOn crf (xs V.! i) j
          let y = lbOn crf (DAG.edgeLabel i dag) j
          in  (u - v y) + w y
    where
      u = sum
          [ beta iPlus1 k * psi k
          | (k, _ ) <- lbIxs crf dag i
          , iPlus1 <- DAG.nextEdges i dag ]
      v y = sum
          [ beta iPlus1 k * psi k
          | (k, _ ) <- I.intersect (Md.nextIxs crf y) (lbVec crf dag i)
          , iPlus1 <- DAG.nextEdges i dag ]
      w y = sum
          [ beta iPlus1 k * psi k * Md.valueL crf ix
          | (k, ix) <- I.intersect (Md.nextIxs crf y) (lbVec crf dag i)
          , iPlus1 <- DAG.nextEdges i dag ]


-- zxBeta :: ProbArray -> L.LogFloat
-- zxBeta beta = beta 0 0
-- 
-- zxAlpha :: Xs -> ProbArray -> L.LogFloat
-- zxAlpha xs alpha = alpha (V.length xs) 0
-- 
-- -- | Normalization factor computed for the 'Xs' sentence using the
-- -- backward computation.
-- zx :: Model -> Xs -> L.LogFloat
-- zx crf = zxBeta . backward crf
-- 
-- -- | Normalization factor computed for the 'Xs' sentence using the
-- -- forward computation.
-- zx' :: Model -> Xs -> L.LogFloat
-- zx' crf sent = zxAlpha sent (forward crf sent)
-- 
-- -- | Tag probabilities with respect to marginal distributions.
-- marginals :: Model -> Xs -> [[(Lb, L.LogFloat)]]
-- marginals crf xs =
--     let alpha = forward crf xs
--         beta = backward crf xs
--     in  [ [ (x, prob1 alpha beta i k)
--           | (k, x) <- lbIxs crf xs i ]
--         | i <- [0 .. V.length xs - 1] ]
-- 
-- -- | Get (at most) k best tags for each word and return them in
-- -- descending order.  TODO: Tagging with respect to marginal
-- -- distributions might not be the best idea.  Think of some
-- -- more elegant method.
-- tagK :: Int -> Model -> Xs -> [[(Lb, L.LogFloat)]]
-- tagK k crf xs = map
--     ( take k
--     . reverse
--     . sortBy (compare `on` snd)
--     ) (marginals crf xs)
-- 
-- -- | Find the most probable label sequence (with probabilities of individual
-- -- lables determined with respect to marginal distributions) satisfying the
-- -- constraints imposed over label values.
-- tag :: Model -> Xs -> [Lb]
-- tag crf = map (fst . head) . (tagK 1 crf)
-- 
-- prob1 :: ProbArray -> ProbArray -> Int -> LbIx -> L.LogFloat
-- prob1 alpha beta k x =
--     alpha k x * beta (k + 1) x / zxBeta beta
-- {-# INLINE prob1 #-}
-- 
-- prob2 :: Model -> ProbArray -> ProbArray -> Int -> (LbIx -> L.LogFloat)
--       -> LbIx -> LbIx -> FeatIx -> L.LogFloat
-- prob2 crf alpha beta k psi x y ix
--     = alpha (k - 1) y * beta (k + 1) x
--     * psi x * valueL crf ix / zxBeta beta
-- {-# INLINE prob2 #-}
-- 
-- goodAndBad :: Model -> Xs -> Ys -> (Int, Int)
-- goodAndBad crf xs ys =
--     foldl gather (0, 0) $ zip labels labels'
--   where
--     labels  = [ (best . unY) (ys V.! i)
--               | i <- [0 .. V.length ys - 1] ]
--     best zs
--         | null zs   = Nothing
--         | otherwise = Just . fst $ maximumBy (compare `on` snd) zs
--     labels' = map Just $ tag crf xs
--     gather (good, bad) (x, y)
--         | x == y = (good + 1, bad)
--         | otherwise = (good, bad + 1)
-- 
-- goodAndBad' :: Model -> [(Xs, Ys)] -> (Int, Int)
-- goodAndBad' crf dataset =
--     let add (g, b) (g', b') = (g + g', b + b')
--     in  foldl add (0, 0) [goodAndBad crf x y | (x, y) <- dataset]
-- 
-- -- | Compute the accuracy of the model with respect to the labeled dataset.
-- accuracy :: Model -> [(Xs, Ys)] -> Double
-- accuracy crf dataset =
--     let k = numCapabilities
--     	parts = partition k dataset
--         xs = parMap rseq (goodAndBad' crf) parts
--         (good, bad) = foldl add (0, 0) xs
--         add (g, b) (g', b') = (g + g', b + b')
--     in  fromIntegral good / fromIntegral (good + bad)
-- 
-- expectedFeaturesOn
--     :: Model -> ProbArray -> ProbArray -> Xs
--     -> Int -> [(FeatIx, L.LogFloat)]
-- expectedFeaturesOn crf alpha beta xs i =
--     tFeats ++ oFeats
--   where
--     psi = computePsi crf xs i
--     pr1 = prob1     alpha beta i
--     pr2 = prob2 crf alpha beta i psi
-- 
--     oFeats = [ (ix, pr1 k) 
--              | o <- unX (xs V.! i)
--              , (k, ix) <- intersect (obIxs crf o) (lbVec crf xs i) ]
-- 
--     tFeats
--         | i == 0 = catMaybes
--             [ (, pr1 k) <$> featToIx crf (SFeature x)
--             | (k, x) <- lbIxs crf xs i ]
--         | otherwise =
--             [ (ix, pr2 k l ix)
--             | (k,  x) <- lbIxs crf xs i
--             , (l, ix) <- intersect (prevIxs crf x) (lbVec crf xs (i-1)) ]
-- 
-- -- | A list of features (represented by feature indices) defined within
-- -- the context of the sentence accompanied by expected probabilities
-- -- determined on the basis of the model. 
-- --
-- -- One feature can occur multiple times in the output list.
-- expectedFeaturesIn :: Model -> Xs -> [(FeatIx, L.LogFloat)]
-- expectedFeaturesIn crf xs = zxF `par` zxB `pseq` zxF `pseq`
--     concat [expectedOn k | k <- [0 .. V.length xs - 1] ]
--   where
--     expectedOn = expectedFeaturesOn crf alpha beta xs
--     alpha = forward crf xs
--     beta = backward crf xs
--     zxF = zxAlpha xs alpha
--     zxB = zxBeta beta
