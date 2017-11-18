{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}


-- | For probability-related computations.


module Data.CRF.Chain1.Constrained.DAG.Probs
( probability
, likelihood
, parLikelihood
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
import qualified Data.CRF.Chain1.Constrained.DAG.Inference as Inf


---------------------------------------------
-- Util Types
---------------------------------------------


-- | Label index.
type LbIx = Int


-- | The probability array assigns some probability to each label (represented
-- by its index) which can be assigned to a given edge (represented by its
-- `EdgeID`).
type ProbArray  = EdgeID -> LbIx -> L.LogFloat


--------------------------------------------
-- Basic stuff
---------------------------------------------


-- | Vector of labels and the corresponding probabilities on the given edge of
-- the sentence.
lbVec :: Md.Model -> DAG a (X, Y) -> EdgeID -> AVec (Lb, Double)
lbVec crf dag edgeID =
  case DAG.edgeLabel edgeID dag of
    (_, y) -> C._unY y
{-# INLINE lbVec #-}


-- | Number of potential labels on the given edge of the sentence.
lbNum :: Md.Model -> DAG a (X, Y) -> EdgeID -> Int
lbNum crf dag = U.length . C.unAVec . lbVec crf dag
{-# INLINE lbNum #-}


-- | Label on the given edge and on the given position.
lbOn :: Md.Model -> DAG a (X, Y) -> EdgeID -> LbIx -> (Lb, Double)
lbOn crf dag = (U.!) . C.unAVec . lbVec crf dag
{-# INLINE lbOn #-}


-- | Labels on the given sentence edge (as in `lbVec`), accompanied with the
-- corresponding indexes. I.e., each label `Lb` (and its probability) is
-- accompanied with a number, from [0..], corresponding to its index in the
-- vector of labels obtained with `lbVec`.
lbIxs :: Md.Model -> DAG a (X, Y) -> EdgeID -> [(LbIx, (Lb, Double))]
lbIxs crf dag = zip [0..] . U.toList . C.unAVec . lbVec crf dag
{-# INLINE lbIxs #-}


---------------------------------------------
-- A bit more complex stuff
---------------------------------------------


-- | Compute the table of potential products associated with
-- * the observation features for the given sentence edge,
-- * the probabilities assigned to different labels.
computePsi :: Md.Model -> DAG a (X, Y) -> EdgeID -> LbIx -> L.LogFloat
computePsi crf dag edgeID
  = (A.!)
  . A.accumArray (*) 1 bounds
  $ proTab ++ obsTab
  where
    bounds = (0, lbNum crf dag edgeID - 1)
    obsTab =
      [ (lbIx, Md.valueL crf featIx)
      | ob <- (C.unX . fst) (DAG.edgeLabel edgeID dag)
      , (lbIx, featIx) <-
          I.intersect (Md.obIxs crf ob) (xify $ lbVec crf dag edgeID) ]
    proTab =
      [ (lbIx, L.logFloat prob)
      | (lbIx, (_lb, prob)) <- lbIxs crf dag edgeID ]


-- | An alternative forward computation which takes the probabilities assigned
-- to different lables into account (at the level of the `psi` function).
forward :: Md.Model -> DAG a (X, Y) -> ProbArray
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
    | i == snd bounds = const u'                    -- <= TO CHECK
    | i `S.member` initialSet = \j ->
        let (x, _) = lbOn crf dag i j
        in  psi j * Md.sgValue crf x
    | otherwise = \j ->
        let (x, _) = lbOn crf dag i j
        in  psi j * ((u - v x) + w x)
    where
      u = safeSum
        [ alpha iMinus1 k
        | iMinus1 <- DAG.prevEdges i dag
        , (k, _) <- lbIxs crf dag iMinus1 ]
      v x = safeSum
        [ alpha iMinus1 k
        | iMinus1 <- DAG.prevEdges i dag
        , (k, _) <-
            I.intersect (Md.prevIxs crf x) (xify $ lbVec crf dag iMinus1) ]
      w x = safeSum
        [ alpha iMinus1 k * Md.valueL crf ix
        | iMinus1 <- DAG.prevEdges i dag
        , (k, ix) <- I.intersect (Md.prevIxs crf x) (xify $ lbVec crf dag iMinus1) ]
      -- Note that if `i == snd bounds` then `i` does not refer to any existing
      -- edge, hence the need to introduce `u'` which does almost the same thing
      -- as `u`.
      u' = safeSum
        [ alpha iMinus1 k
        | iMinus1 <- DAG.dagEdges dag
        , DAG.isFinalEdge iMinus1 dag
        , (k, _) <- lbIxs crf dag iMinus1 ]


-- | Probability of the given DAG in the given model.
probability :: Md.Model -> DAG a (X, Y) -> L.LogFloat
probability crf dag =
  zxAlpha (forward crf dag) / normFactor
  where
    zxAlpha alpha = alpha (DAG.maxEdge dag + 1) 0
    normFactor = Inf.zx crf (fmap fst dag)


-- | Log-likelihood of the given dataset (parallelized version).
parLikelihood :: Md.Model -> [DAG a (X, Y)] -> L.LogFloat
parLikelihood crf dataset =
  let k = numCapabilities
      parts = partition k dataset
      probs = parMap rseq (likelihood crf) parts
  in  L.product probs


-- | Log-likelihood of the given dataset (no parallelization).
likelihood :: Md.Model -> [DAG a (X, Y)] -> L.LogFloat
likelihood crf = L.product . map (probability crf)


---------------------------------------------
-- Utils
---------------------------------------------


-- | X-ify the given ascending vector.
xify :: (U.Unbox x, U.Unbox y) => C.AVec (x, y) -> C.AVec x
xify = C.AVec . U.map fst . C.unAVec
{-# INLINE xify #-}


-- | Numerically safer summing.
safeSum :: [L.LogFloat] -> L.LogFloat
safeSum [] = 0
safeSum xs = L.sum xs
{-#INLINE safeSum #-}
