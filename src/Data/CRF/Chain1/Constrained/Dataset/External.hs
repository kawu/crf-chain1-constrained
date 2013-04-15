module Data.CRF.Chain1.Constrained.Dataset.External
( Word (..)
, unknown
, Sent
, Prob (unProb)
, mkProb
, WordL
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
unknown word = S.size (lbs word) == 0
{-# INLINE unknown #-}

-- | A sentence of words.
type Sent a b = [Word a b]

-- | A probability distribution defined over elements of type a.
-- All elements not included in the map have probability equal
-- to 0.
newtype Prob a = Prob { unProb :: M.Map a Double }
    deriving (Show, Eq, Ord)

-- | Construct the probability distribution.
mkProb :: Ord a => [(a, Double)] -> Prob a
mkProb =
    Prob . normalize . M.fromListWith (+) . filter ((>0).snd)
  where
    normalize dist 
        | M.null dist  =
            error "mkProb: no elements with positive probability"
        | otherwise     =
            let z = sum (M.elems dist)
            in  fmap (/z) dist

-- | A WordL is a labeled word, i.e. a word with probability distribution
-- defined over labels.  We assume that every label from the distribution
-- domain is a member of the set of potential labels corresponding to the
-- word.  TODO: Ensure the assumption using the smart constructor.
type WordL a b = (Word a b, Prob b)

-- | A sentence of labeled words.
type SentL a b = [WordL a b]
