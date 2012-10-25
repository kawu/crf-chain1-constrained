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

, AVec (unAVec)
, fromList
, fromSet
) where

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Binary (Binary)
import Data.Vector.Binary ()
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

-- | Ascending vector of unique interger elements.
newtype AVec a = AVec { unAVec :: U.Vector a }
    deriving (Show, Read, Eq, Ord, Binary)

-- | Smart AVec constructor which ensures that the
-- underlying vector satisfies the AVec properties.
fromList :: (Ord a, U.Unbox a) => [a] -> AVec a
fromList = fromSet . S.fromList 
{-# INLINE fromList #-}

-- | Smart AVec constructor which ensures that the
-- underlying vector satisfies the AVec properties.
fromSet :: (Ord a, U.Unbox a) => S.Set a -> AVec a
fromSet = AVec . U.fromList . S.toList 
{-# INLINE fromSet #-}

-- | A word represented by a list of its observations
-- and a list of its potential label interpretations.
-- TODO: Has the _unR be an ascending vector?
data X = X
    { _unX :: AVec Ob
    , _unR :: AVec Lb }
    deriving (Show, Read, Eq, Ord)

-- | X constructor with first argument beeing the default vector
-- of potential labels.  The default vector is used when the
-- set of potential labels given afterwards is empty and it
-- is provided mainly for the sake of memory efficiency).
mkX :: AVec Lb -> [Ob] -> [Lb] -> X
mkX r0 x [] = X (fromList x) r0
mkX _  x r  = X (fromList x) (fromList r)
{-# INLINE mkX #-}

-- | List of observations.
unX :: X -> [Ob]
unX = U.toList . unAVec . _unX
{-# INLINE unX #-}

-- | List of potential labels.
unR :: X -> [Lb]
unR = U.toList . unAVec . _unR
{-# INLINE unR #-}

-- | Sentence of words.
type Xs = V.Vector X

-- | Probability distribution over labels.  We assume, that when y is
-- a member of chosen labels list it is also a member of the list
-- potential labels for corresponding 'X' word.
-- TODO: Perhaps we should substitute 'Lb's with label indices
-- corresponding to labels from the vector of potential labels?
newtype Y = Y { _unY :: AVec (Lb, Double) }
    deriving (Show, Read, Eq, Ord)

-- | Y constructor.
mkY :: [(Lb, Double)] -> Y
mkY = Y . fromList
{-# INLINE mkY #-}

-- | Y deconstructor symetric to mkY.
unY :: Y -> [(Lb, Double)]
unY = U.toList . unAVec . _unY
{-# INLINE unY #-}

-- | Sentence of Y (label choices).
type Ys = V.Vector Y
