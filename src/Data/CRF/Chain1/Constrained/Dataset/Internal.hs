-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeFamilies #-}

module Data.CRF.Chain1.Constrained.Dataset.Internal
( Xs
, Ys
, module Data.CRF.Chain1.Constrained.Core
) where

-- import Control.Applicative ((<$>), (<*>))
-- import Data.Vector.Generic.Base
-- import Data.Vector.Generic.Mutable
-- import Data.Binary (Binary, get, put, putWord8, getWord8)
-- import Data.Vector.Binary ()
-- import Data.Ix (Ix)
-- import qualified Data.Set as S
import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U
-- import           Data.Vector.Unboxed.Deriving

import Data.CRF.Chain1.Constrained.Core


-- | Sentence of words.
type Xs = V.Vector X

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
