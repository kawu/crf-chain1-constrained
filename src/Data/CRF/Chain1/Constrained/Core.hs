{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Data.CRF.Chain1.Constrained.Core
(
-- * Basic Types
  Ob (..)
, Lb (..)

, X (..)
, mkX
, unX
, unR

, Y (..)
, mkY
, unY

, AVec (..)
, fromList
, fromSet

-- * Features
, Feature (..)
, isSFeat
, isTFeat
, isOFeat
) where


import Control.Applicative ((<*>), (<$>))
-- import Data.Vector.Generic.Base
-- import Data.Vector.Generic.Mutable
import Data.Vector.Binary ()
import Data.Binary (Binary, Get, get, put, putWord8, getWord8)
import Data.Ix (Ix)
import qualified Data.Set as S
-- import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed.Deriving
-- import qualified Data.Number.LogFloat as L


----------------------------------------------
-- Basic Types
----------------------------------------------


-- | An observation.
newtype Ob = Ob { unOb :: Int }
    deriving ( Show, Read, Eq, Ord, Binary )
--           GeneralizedNewtypeDeriving doesn't work for this in 7.8.2:
--           , Vector U.Vector, MVector U.MVector, U.Unbox )
derivingUnbox "Ob" [t| Ob -> Int |] [| unOb |] [| Ob |]

-- | A label.
newtype Lb = Lb { unLb :: Int }
    deriving ( Show, Read, Eq, Ord, Binary, Num, Ix )
derivingUnbox "Lb" [t| Lb -> Int |] [| unLb |] [| Lb |]

-- | An ascending vector of unique elements.
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
fromSet = AVec . U.fromList . S.toAscList
{-# INLINE fromSet #-}

-- | A word represented by a list of its observations
-- and a list of its potential label interpretations.
data X
    -- | The word with default set of potential interpretations.
    = X { _unX :: AVec Ob }
    -- | The word with restricted set of potential labels.
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


----------------------------------------------
-- Features
----------------------------------------------


-- | A Feature is either an observation feature OFeature o x, which
-- models relation between observation o and label x assigned to
-- the same word, or a transition feature TFeature x y (SFeature x
-- for the first position in the sentence), which models relation
-- between two subsequent labels, x (on i-th position) and y
-- (on (i-1)-th positoin).
data Feature
    = SFeature
        {-# UNPACK #-} !Lb
    | TFeature
        {-# UNPACK #-} !Lb
        {-# UNPACK #-} !Lb
    | OFeature
        {-# UNPACK #-} !Ob
        {-# UNPACK #-} !Lb
    deriving (Show, Eq, Ord)

instance Binary Feature where
    put (SFeature x)   = put (0 :: Int) >> put x
    put (TFeature x y) = put (1 :: Int) >> put (x, y)
    put (OFeature o x) = put (2 :: Int) >> put (o, x)
    get = do
        k <- get :: Get Int
        case k of
            0 -> SFeature <$> get
            1 -> TFeature <$> get <*> get
            2 -> OFeature <$> get <*> get
	    _ -> error "Binary Feature: unknown identifier"


-- | Is it a 'SFeature'?
isSFeat :: Feature -> Bool
isSFeat (SFeature _) = True
isSFeat _            = False
{-# INLINE isSFeat #-}

-- | Is it an 'OFeature'?
isOFeat :: Feature -> Bool
isOFeat (OFeature _ _) = True
isOFeat _              = False
{-# INLINE isOFeat #-}

-- | Is it a 'TFeature'?
isTFeat :: Feature -> Bool
isTFeat (TFeature _ _) = True
isTFeat _              = False
{-# INLINE isTFeat #-}
