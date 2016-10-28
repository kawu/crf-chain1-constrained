module Data.CRF.Chain1.Constrained.Dataset.External
( Word (..)
, unknown
, Sent
, Prob (unProb)
, mkProb
, WordL (word, choice)
, mkWordL
, SentL
) where

import qualified Data.Set as S
import qualified Data.Map as M

-- | A Word is represented by a set of observations
-- and a set of potential interpretation labels.
-- When the set of potential labels is empty the word
-- is considered to be unknown and the default potential
-- set is used in its place.
data Word a b = Word
    { obs   :: S.Set a  -- ^ The set of observations
    , lbs   :: S.Set b  -- ^ The set of potential interpretations.
    } deriving (Show, Eq, Ord)

-- | The word is considered to be unknown when the set of potential
-- labels is empty.
unknown :: Word a b -> Bool
unknown x = S.size (lbs x) == 0
{-# INLINE unknown #-}

-- | A sentence of words.
type Sent a b = [Word a b]

-- | A probability distribution defined over elements of type a.
-- All elements not included in the map have probability equal
-- to 0.
newtype Prob a = Prob { unProb :: M.Map a Double }
    deriving (Show, Eq, Ord)


-- -- | Construct the probability distribution.
-- mkProb :: Ord a => [(a, Double)] -> Prob a
-- mkProb =
--     Prob . normalize . M.fromListWith (+) . filter ((>0).snd)
--   where
--     normalize dist
--         | M.null dist  =
--             error "mkProb: no elements with positive probability"
--         | otherwise     =
--             let z = sum (M.elems dist)
--             in  fmap (/z) dist


-- | Construct the probability distribution.
--
-- Normalization is not performed because, when working with DAGs, the
-- probability of a specific DAG edge can be lower than 1 (in particular, it can
-- be 0).
--
-- Elements with probability 0 cab be filtered out since information that a
-- given label is a potential interpretation of the given word/edge is preserved
-- at the level of the `Word`
mkProb :: Ord a => [(a, Double)] -> Prob a
mkProb = Prob . M.fromListWith (+) . filter ((>0).snd)


-- | A WordL is a labeled word, i.e. a word with probability distribution
-- defined over labels.  We assume that every label from the distribution
-- domain is a member of the set of potential labels corresponding to the
-- word.  Use the `mkWordL` smart constructor to build `WordL`.
data WordL a b = WordL
    { word      :: Word a b
    , choice    :: Prob b }


-- | Ensure, that every label from the distribution domain is a member of the
-- set of potential labels corresponding to the word.
mkWordL :: (Ord b) => Word a b -> Prob b -> WordL a b
mkWordL wd cs
--   | S.null chosen && S.null (lbs wd) =
--     error "mkWordL: no labels assigned to the word"
--     <- the above condition can actualy happen it the model does not
--        find any probable label for a given word.
  | S.null (lbs wd) = WordL wd cs
  | chosen `S.isSubsetOf` lbs wd = WordL wd cs
  | otherwise = error "mkWordL: chosen labels outside of `lbs`"
  where
    chosen = M.keysSet (unProb cs)


-- | A sentence of labeled words.
type SentL a b = [WordL a b]
