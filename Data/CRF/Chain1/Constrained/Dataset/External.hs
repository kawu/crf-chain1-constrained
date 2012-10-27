module Data.CRF.Chain1.Constrained.Dataset.External
( Word (..)
, unknown
, Sent
, Dist (unDist)
, mkDist
, WordL
, annotate
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
newtype Dist a = Dist { unDist :: M.Map a Double }

-- | Construct the probability distribution.
mkDist :: Ord a => [(a, Double)] -> Dist a
mkDist =
    Dist . normalize . M.fromListWith (+)
  where
    normalize dist =
        let z = sum (M.elems dist)
        in  fmap (/z) dist

-- | A WordL is a labeled word, i.e. a word with probability distribution
-- defined over labels.  We assume that every label from the distribution
-- domain is a member of the set of potential labels corresponding to the
-- word.  TODO: Ensure the assumption using the smart constructor.
type WordL a b = (Word a b, Dist b)

-- | Annotate the word with the label.
annotate :: Ord b => Word a b -> b -> WordL a b
annotate w x
    | x `S.member` lbs w    = (w, Dist (M.singleton x 1))
    | otherwise             =
        error "annotate: label not in the set of potential interpretations"

-- | A sentence of labeled words.
type SentL a b = [WordL a b]
