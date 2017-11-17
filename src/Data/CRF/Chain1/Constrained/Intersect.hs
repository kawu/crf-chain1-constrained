{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}


module Data.CRF.Chain1.Constrained.Intersect
( intersect
) where


import qualified Data.Vector.Unboxed as U

import Data.CRF.Chain1.Constrained.Dataset.Internal (Lb, AVec, unAVec)
import Data.CRF.Chain1.Constrained.Model (FeatIx)


-- | Assumption: both input list are given in an ascending order.
intersect
    :: AVec (Lb, FeatIx)    -- ^ Vector of (label, features index) pairs
    -> AVec Lb              -- ^ Vector of labels
    -- | Intersection of arguments: vector indices from the second list
    -- and feature indices from the first list.
    -> [(Int, FeatIx)]
intersect xs' ys'
    | n == 0 || m == 0 = []
    | otherwise = merge xs ys
  where
    xs = unAVec xs'
    ys = unAVec ys'
    n = U.length ys
    m = U.length xs


merge :: U.Vector (Lb, FeatIx) -> U.Vector Lb -> [(Int, FeatIx)]
merge xs ys = doIt 0 0
  where
    m = U.length xs
    n = U.length ys
    doIt i j
        | i >= m || j >= n = []
        | otherwise = case compare x y of
            EQ -> (j, ix) : doIt (i+1) (j+1)
            LT -> doIt (i+1) j
            GT -> doIt i (j+1)
      where
        (x, ix) = xs `U.unsafeIndex` i
        y = ys `U.unsafeIndex` j


---------------------------------------------
-- Alternative version
---------------------------------------------


-- -- | Assumption: both input list are given in an ascending order.
-- intersect'
--     :: AVec (Lb, FeatIx)    -- ^ Vector of (label, features index) pairs
--     -> AVec (Lb, Prob)      -- ^ Vector of labels
--     -- | Intersection of arguments: vector indices from the second list
--     -- and feature indices from the first list.
--     -> [(Int, FeatIx)]
-- intersect' xs' ys'
--     | n == 0 || m == 0 = []
--     | otherwise = merge xs ys
--   where
--     xs = unAVec xs'
--     ys = unAVec ys'
--     n = U.length ys
--     m = U.length xs
--
--
-- merge :: U.Vector (Lb, FeatIx) -> U.Vector Lb -> [(Int, FeatIx)]
-- merge xs ys = doIt 0 0
--   where
--     m = U.length xs
--     n = U.length ys
--     doIt i j
--         | i >= m || j >= n = []
--         | otherwise = case compare x y of
--             EQ -> (j, ix) : doIt (i+1) (j+1)
--             LT -> doIt (i+1) j
--             GT -> doIt i (j+1)
--       where
--         (x, ix) = xs `U.unsafeIndex` i
--         y = ys `U.unsafeIndex` j
