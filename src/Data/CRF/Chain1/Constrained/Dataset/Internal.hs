{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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
import           Data.Vector.Unboxed.Deriving

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
-- TODO: Change to DAG.
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


------------------------------------------------------------------
-- DAG representation
------------------------------------------------------------------
--
-- Naming proposals:
-- * lattice
-- * word-lattice       <- może za bardzo kojarzy się z czymś innym?
--                         ale z drugiej strony, chyba kojarz się
--                         właśnie z tym co trzeba, ew. sugeruje trochę
--                         inną funkcjonalność (jaką?) niż ja zamierzam
--                         udostępnić.
-- * word-dag
-- * edge-dag
--
-- First of all, nodes represent division places of the input text
-- (i.e., they point to places *between* two adjacent characters).
--
-- Therefore, segments (or tokens) are represented by graph edges.
-- An edge consists of:
-- * An analysis of the corresponding segment,
-- * Nodes (or their identifiers) between which the edge spans.
--
-- Query operations available on DAGs:
-- * Return nodes with respect to the topological order (in both
--   directions?)
-- * List all edges (in/out)going from the particular node.
--
-- It should be possible to make something like a context-sensitive
-- fmap, which will make it possible to:
-- * Translate individual edge values to feature vectors,
--   WARNING: there are several strategies of how observations for the,
--   previous edge (for example) are determined, since there may be
--   several candidates for a previous edge!
-- * Translate edge values to forward/backward probabilities.
--
-- Misc operations:
-- * Join DAG with a particular path of words (morphological analysis
--   will result with a DAG, while disambiguated sentence is just
--   a sequence of morphosyntactic tags refering to a particular path
--   in the DAG), or more general:
-- * Join DAG with a DAG.
--
-- Other considerations:
-- * Do we want to represent spaces on the level of word lattices?
--   Not necessarily.  On the other hand, it should be possible
--   to store a word-with-spaces in an edge -- why not? -- and
--   restore the original sentence from the lattice.  Wouldn't
--   that be nice?
--   Since node represents a position between two segments, it can
--   be also explicitly stated if there is a space between these two
--   segments, and what kind of space is that.  Yep, it makes sense!
-- * Potential morphosyntactic analyses of a sentence can be represented
--   as a DAG.  The same goes for chosen interpretations -- they can also
--   be represented as a DAG with potentials (probabilities?) assigned
--   to each label on every edge.
