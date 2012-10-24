{-# LANGUAGE FlexibleContexts #-}

-- Inference with CRFs.

module Data.CRF.Chain1.Constrained.Inference
(
-- ( tag
-- , marginals
-- , accuracy
-- , expectedFeaturesIn
-- , zx
-- , zx'
) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes)
import Data.List (maximumBy)
import Data.Function (on)
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Control.Parallel.Strategies (rseq, parMap)
import Control.Parallel (par, pseq)
import GHC.Conc (numCapabilities)
import qualified Data.Number.LogFloat as L

import qualified Data.CRF.Chain1.Constrained.DP as DP
import Data.CRF.Chain1.Constrained.Util (partition)
import Data.CRF.Chain1.Constrained.Dataset.Internal
import Data.CRF.Chain1.Constrained.Model hiding (lbNum)
import Data.CRF.Chain1.Constrained.Intersect

type LbIx       = Int
type ProbArray  = Int -> LbIx -> L.LogFloat

-- Some basic definitions.

-- | Number of potential labels on the given position of the sentence.
lbNum :: Xs -> Int -> Int
lbNum xs k = (U.length . _unR) (xs V.! k)
{-# INLINE lbNum #-}

-- | Vector of potential labels on the given position of the sentence.
lbVec :: Xs -> Int -> U.Vector Lb
lbVec sent k = _unR (sent V.! k)
{-# INLINE lbVec #-}

-- | Potential label on the given vector position.
lbOn :: X -> Int -> Lb
lbOn r = (_unR r U.!)
{-# INLINE lbOn #-}

lbIxs :: Xs -> Int -> [(Int, Lb)]
lbIxs xs = zip [0..] . U.toList . lbVec xs
{-# INLINE lbIxs #-}

-- | Compute the table of potential products associated with 
-- observation features for the given sentence position.
computePsi :: Model -> Xs -> Int -> LbIx -> L.LogFloat
computePsi crf xs i = (A.!) $ A.accumArray (*) 1 bounds
    [ (k, valueL crf ix)
    | ob <- unX (xs V.! i)
    , (k, ix) <- intersect (obIxs crf ob) (lbVec xs i) ]
  where
    bounds = (0, lbNum xs i - 1)

-- | Forward table computation.
forward :: Model -> Xs -> ProbArray
forward crf xs = alpha where
    alpha = DP.flexible2 (0, V.length xs) bounds
        (\t i -> withMem (computePsi crf xs i) t i)
    bounds i
        | i == V.length xs = (0, 0)
        | otherwise = (0, lbNum xs i - 1)
    withMem psi alpha i
        | i == V.length xs = const u
        | i == 0 = \j ->
            let x = lbOn (xs V.! i) j
            in  psi j * sgValue crf x
        | otherwise = \j ->
            let x = lbOn (xs V.! i) j
            in  psi j * ((u - v x) + w x)
      where
        u = sum
            [ alpha (i-1) k
            | (k, _) <- lbIxs xs (i-1) ]
        v x = sum
            [ alpha (i-1) k
            | (k, _) <- intersect (prevIxs crf x) (lbVec xs (i-1)) ]
        w x = sum
            [ alpha (i-1) k * valueL crf ix
            | (k, ix) <- intersect (prevIxs crf x) (lbVec xs (i-1)) ]

-- | Backward table computation.
backward :: Model -> Xs -> ProbArray
backward crf xs = beta where
    beta = DP.flexible2 (0, V.length xs) bounds
        (\t i -> withMem (computePsi crf xs i) t i)
    bounds i
        | i == 0    = (0, 0)
        | otherwise = (0, lbNum xs (i-1) - 1)
    withMem psi beta i
        | i == V.length xs = const 0
        | i == 0 = const $ sum
            [ beta (i+1) k * psi k
            * sgValue crf (lbOn (xs V.! i) k)
            | (k, _) <- lbIxs xs i ]
        | otherwise = \j ->
            let y = lbOn (xs V.! (i-1)) j
            in  (u - v y) + w y
      where
        u = sum
            [ beta (i+1) k * psi k
            | (k, _ ) <- lbIxs xs i ]
        v y = sum
            [ beta (i+1) k * psi k
            | (k, _ ) <- intersect (nextIxs crf y) (lbVec xs i) ]
        w y = sum
            [ beta (i+1) k * psi k * valueL crf ix
            | (k, ix) <- intersect (nextIxs crf y) (lbVec xs i) ]

zxBeta :: ProbArray -> L.LogFloat
zxBeta beta = beta 0 0

zxAlpha :: Xs -> ProbArray -> L.LogFloat
zxAlpha xs alpha = alpha (V.length xs) 0

-- | Normalization factor computed for the 'Xs' sentence using the
-- backward computation.
zx :: Model -> Xs -> L.LogFloat
zx crf = zxBeta . backward crf

-- | Normalization factor computed for the 'Xs' sentence using the
-- forward computation.
zx' :: Model -> Xs -> L.LogFloat
zx' crf sent = zxAlpha sent (forward crf sent)

-- --------------------------------------------------------------
-- argmax :: Ord b => (a -> Maybe b) -> [a] -> Maybe (a, b)
-- argmax _ [] = Nothing
-- argmax f xs
--     | null ys   = Nothing
--     | otherwise = Just $ foldl1 choice ys
--   where
--     ys = catMaybes $ map (\x -> (,) <$> pure x <*> f x) xs
--     choice (x1, v1) (x2, v2)
--         | v1 > v2 = (x1, v1)
--         | otherwise = (x2, v2)
-- 
-- -- | Determine the most probable label sequence given the context of the
-- -- CRF model and the sentence.
-- tag :: Model -> Xs -> [Lb]
-- tag crf sent = collectMaxArg (0, 0) [] $ DP.flexible2
--     (0, V.length sent) wordBounds
--     (\t k -> withMem (computePsi crf sent k) t k)
--   where
--     n = V.length sent
-- 
--     wordBounds k
--         | k == 0    = (Lb 0, Lb 0)
--         | otherwise = (Lb 0, Lb $ lbNum crf - 1)
-- 
--     withMem psi mem k y
--         | k == n    = Just (-1, 1)  -- -1 is a dummy value
--         | k == 0    = prune <$> argmax eval (sgIxs crf)
--         | otherwise = prune <$> argmax eval (nextIxs crf y)
--       where
--         eval (x, ix) = do
--             v <- snd <$> mem (k + 1) x
--             return $ v * psi x * valueL crf ix
--         prune ((x, _ix), v) = (x, v)
-- 
--     collectMaxArg (i, j) acc mem
--         | i < n     = collect (mem i j)
--         | otherwise = reverse acc
--       where
--         collect (Just (h, _)) = collectMaxArg (i + 1, h) (h:acc) mem
--         collect Nothing       = error "tag.collect: Nothing"
-- 
-- -- | Tag probabilities with respect to marginal distributions.
-- marginals :: Model -> Xs -> [[(Lb, L.LogFloat)]]
-- marginals crf sent =
--     let alpha = forward sum crf sent
--         beta = backward sum crf sent
--     in  [ [ (x, prob1 alpha beta k x)
--           | x <- lbSet crf ]
--         | k <- [0 .. V.length sent - 1] ]
-- 
-- -- tagProbs :: Sent s => Model -> s -> [[Double]]
-- -- tagProbs crf sent =
-- --     let alpha = forward maximum crf sent
-- --         beta = backward maximum crf sent
-- --         normalize vs =
-- --             let d = - logSum vs
-- --             in map (+d) vs
-- --         m1 k x = alpha k x + beta (k + 1) x
-- --     in  [ map exp $ normalize [m1 i k | k <- interpIxs sent i]
-- --         | i <- [0 .. V.length sent - 1] ]
-- -- 
-- -- -- tag probabilities with respect to
-- -- -- marginal distributions
-- -- tagProbs' :: Sent s => Model -> s -> [[Double]]
-- -- tagProbs' crf sent =
-- --     let alpha = forward logSum crf sent
-- --         beta = backward logSum crf sent
-- --     in  [ [ exp $ prob1 crf alpha beta sent i k
-- --           | k <- interpIxs sent i ]
-- --         | i <- [0 .. V.length sent - 1] ]
-- 
-- goodAndBad :: Model -> Xs -> Ys -> (Int, Int)
-- goodAndBad crf sent labels =
--     foldl gather (0, 0) (zip labels' labels'')
--   where
--     labels' = [ fst . maximumBy (compare `on` snd) $ unY (labels V.! i)
--               | i <- [0 .. V.length labels - 1] ]
--     labels'' = tag crf sent
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
-- --------------------------------------------------------------
-- 
-- -- prob :: L.Vect t Int => Model -> Sent Int t -> Double
-- -- prob crf sent =
-- --     sum [ phiOn crf sent k
-- --         | k <- [0 .. (length sent) - 1] ]
-- --     - zx' crf sent
-- -- 
-- -- -- TODO: Wziac pod uwage "Regularization Variance" !
-- -- cll :: Model -> [Sentence] -> Double
-- -- cll crf dataset = sum [prob crf sent | sent <- dataset]
-- 
-- -- prob2 :: SentR s => Model -> ProbArray -> ProbArray -> s
-- --       -> Int -> Lb -> Lb -> Double
-- -- prob2 crf alpha beta sent k x y
-- --     = alpha (k - 1) y + beta (k + 1) x
-- --     + phi crf (observationsOn sent k) a b
-- --     - zxBeta beta
-- --   where
-- --     a = interp sent k       x
-- --     b = interp sent (k - 1) y
-- 
-- prob2 :: Model -> ProbArray -> ProbArray -> Int -> (Lb -> L.LogFloat)
--       -> Lb -> Lb -> FeatIx -> L.LogFloat
-- prob2 crf alpha beta k psi x y ix
--     = alpha (k - 1) y * beta (k + 1) x
--     * psi x * valueL crf ix / zxBeta beta
-- 
-- -- prob1 :: SentR s => Model -> ProbArray -> ProbArray
-- --       -> s -> Int -> Label -> Double
-- -- prob1 crf alpha beta sent k x = logSum
-- --     [ prob2 crf alpha beta sent k x y
-- --     | y <- interpIxs sent (k - 1) ]
-- 
-- prob1 :: ProbArray -> ProbArray -> Int -> Lb -> L.LogFloat
-- prob1 alpha beta k x =
--     alpha k x * beta (k + 1) x / zxBeta beta
-- 
-- expectedFeaturesOn
--     :: Model -> ProbArray -> ProbArray -> Xs
--     -> Int -> [(FeatIx, L.LogFloat)]
-- expectedFeaturesOn crf alpha beta sent k =
--     tFeats ++ oFeats
--   where
--     psi = computePsi crf sent k
--     pr1 = prob1     alpha beta k
--     pr2 = prob2 crf alpha beta k psi
-- 
--     oFeats = [ (ix, pr1 x) 
--              | o <- unX (sent V.! k)
--              , (x, ix) <- obIxs crf o ]
-- 
--     tFeats
--         | k == 0 = 
--             [ (ix, pr1 x) 
--             | (x, ix) <- sgIxs crf ]
--         | otherwise =
--             [ (ix, pr2 x y ix) 
--             | x <- lbSet crf
--             , (y, ix) <- prevIxs crf x ]
-- 
-- -- | A list of features (represented by feature indices) defined within
-- -- the context of the sentence accompanied by expected probabilities
-- -- determined on the basis of the model. 
-- --
-- -- One feature can occur multiple times in the output list.
-- expectedFeaturesIn :: Model -> Xs -> [(FeatIx, L.LogFloat)]
-- expectedFeaturesIn crf sent = zxF `par` zxB `pseq` zxF `pseq`
--     concat [expectedOn k | k <- [0 .. V.length sent - 1] ]
--   where
--     expectedOn = expectedFeaturesOn crf alpha beta sent
--     alpha = forward sum crf sent
--     beta = backward sum crf sent
--     zxF = zxAlpha sent alpha
--     zxB = zxBeta beta
