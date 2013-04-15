{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Applicative ((<$>), (<*>))
import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Binary (Binary, get, put, putWord8, getWord8)
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
data X
    -- | The word with default set of potential interpretations.
    = X { _unX :: AVec Ob }
    -- | The word with custom set of potential labels.
    | R { _unX :: AVec Ob
        , _unR :: AVec Lb }
    deriving (Show, Read, Eq, Ord)

instance Binary X where
    put X{..} = putWord8 0 >> put _unX
    put R{..} = putWord8 1 >> put _unX >> put _unR
    get = getWord8 >>= \i -> case i of
        0   -> X <$> get
        _   -> R <$> get <*> get

-- | X constructor.
mkX :: [Ob] -> [Lb] -> X
mkX x [] = X (fromList x)
mkX x r  = R (fromList x) (fromList r)
{-# INLINE mkX #-}

-- | List of observations.
unX :: X -> [Ob]
unX = U.toList . unAVec . _unX
{-# INLINE unX #-}

-- | List of potential labels.
unR :: AVec Lb -> X -> [Lb]
unR r0 X{..} = U.toList . unAVec $ r0
unR _  R{..} = U.toList . unAVec $ _unR
{-# INLINE unR #-}

-- | Sentence of words.
type Xs = V.Vector X

-- | Probability distribution over labels.  We assume, that when y is
-- a member of chosen labels list it is also a member of the list
-- potential labels for corresponding 'X' word.
-- TODO: Perhaps we should substitute 'Lb's with label indices
-- corresponding to labels from the vector of potential labels?
-- FIXME: The type definition is incorrect (see 'fromList' definition),
-- it should be something like AVec2.
newtype Y = Y { _unY :: AVec (Lb, Double) }
    deriving (Show, Read, Eq, Ord, Binary)

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
