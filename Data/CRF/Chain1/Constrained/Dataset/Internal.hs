{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.CRF.Chain1.Constrained.Dataset.Internal
( Ob (..)
, Lb (..)

, X (..)
, mkX
, unX
, unR
, Xs

, Y (..)
, mkY
, unY
, Ys
) where

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Binary (Binary)
import Data.Ix (Ix)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- | An observation.
newtype Ob = Ob { unOb :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A label.
newtype Lb = Lb { unLb :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox
	     , Num, Ix )

-- | A word represented by a list of its observations
-- and a list of its potential label interpretations.
-- TODO: Has the _unR be an ascending vector?
data X = X
    { _unX :: U.Vector Ob
    , _unR :: U.Vector Lb }
    deriving (Show, Read, Eq, Ord)

-- | X constructor with first argument beeing the default vector
-- of potential labels.  The default vector is used when the
-- set of potential labels given afterwards is empty and it
-- is provided mainly for the sake of memory efficiency).
-- TODO: Should we check, if the default vector is given in an
-- ascending order?
mkX :: U.Vector Lb -> [Ob] -> [Lb] -> X
mkX r0 x [] = X (U.fromList x) r0
mkX _  x r  = X (U.fromList x) (U.fromList . S.toList . S.fromList $ r)
{-# INLINE mkX #-}

-- | List of observations.
unX :: X -> [Ob]
unX = U.toList . _unX
{-# INLINE unX #-}

-- | List of potential labels.
unR :: X -> [Lb]
unR = U.toList . _unR
{-# INLINE unR #-}

-- | Sentence of words.
type Xs = V.Vector X

-- -- | Potential label on the given vector position or Nothing, if the
-- -- position is out of bounds.
-- safeLbOn :: X -> Int -> Maybe Lb
-- safeLbOn r = (_unR r U.!?)
-- {-# INLINE safeLbOn #-}

-- | Probability distribution over labels.  We assume, that when y is
-- a member of chosen labels list it is also a member of the list
-- potential labels for corresponding 'X' word.
-- TODO: Perhaps we should substitute 'Lb's with label indices
-- corresponding to labels from the vector of potential labels?
newtype Y = Y { _unY :: U.Vector (Lb, Double) }
    deriving (Show, Read, Eq, Ord)

-- | Y constructor.
mkY :: [(Lb, Double)] -> Y
mkY = Y . U.fromList
{-# INLINE mkY #-}

-- | Y deconstructor symetric to mkY.
unY :: Y -> [(Lb, Double)]
unY = U.toList . _unY
{-# INLINE unY #-}

-- | Sentence of Y (label choices).
type Ys = V.Vector Y
