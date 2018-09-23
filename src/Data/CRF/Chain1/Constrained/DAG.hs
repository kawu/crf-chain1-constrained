{-# LANGUAGE RecordWildCards #-}

-- | The module provides first-order, linear-chain conditional random fields
-- (CRFs) with position-wide constraints over label values.

module Data.CRF.Chain1.Constrained.DAG
(
-- * Data types
  Word (..)
, unknown
, Sent
, Prob (unProb)
, mkProb
, WordL (word, choice)
, mkWordL
, SentL

-- ** Tagging
, Config (..)
, tag
, marginals
-- , tagK

-- * Modules
, module Data.CRF.Chain1.Constrained.DAG.Train
, module Data.CRF.Chain1.Constrained.DAG.Feature.Present
, module Data.CRF.Chain1.Constrained.DAG.Feature.Hidden
) where

import           Prelude hiding (Word)
-- import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Number.LogFloat as L

-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
import qualified Data.DAG as DAG
import           Data.DAG (DAG)

import qualified Data.CRF.Chain1.Constrained.Core as Core
import qualified Data.CRF.Chain1.Constrained.Model as Model
-- import           Data.CRF.Chain1.Constrained.Dataset.External
import           Data.CRF.Chain1.Constrained.DAG.Dataset.External
-- import           Data.CRF.Chain1.Constrained.DAG.Dataset.External (WordL(..))
import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Codec as C
import           Data.CRF.Chain1.Constrained.Dataset.Codec (decodeLabel, unJust)
import           Data.CRF.Chain1.Constrained.DAG.Feature.Present
import           Data.CRF.Chain1.Constrained.DAG.Feature.Hidden
import           Data.CRF.Chain1.Constrained.DAG.Train
import qualified Data.CRF.Chain1.Constrained.DAG.Inference as I
-- import qualified Data.CRF.Chain1.Constrained.Dataset.Internal as Int


-- | Tagging configuration.
data Config a = Config
  { blackSet :: S.Set a
    -- ^ Blacklisted tags
  }


-- | Determine the most probable label sequence within the context of the
-- given sentence using the model provided by the 'CRF'.
tag 
  :: (Ord a, Ord b)
  => Config b
  -> CRF a b
  -> Sent a b
  -> DAG () b
tag Config{..} crf0 sent
    = onWords
    . fmap (decodeLabel codec)
    . I.tag model
    . C.encodeSent codec
    $ sent
  where
    CRF{..} = blackList blackSet crf0
    -- handle unknown labels; otherwise, the type of `tag`s result
    -- would be `DAG () (Maybe b)`
    onWords labeled =
      fmap f labeledSent
      where
        f = uncurry (unJust codec)
        labeledSent = DAG.zipE sent labeled


-- | Tag with marginal probabilities. For known words (i.e., with `lbs`
-- non-empty), their known potential interpretations are assigned some
-- probabilities (other interpretations are not considered.  For unknown
-- words (i.e., with empty `lbs`), all interpretations are considered
-- (up to the way the set of all interpretations is constructed).
-- In particular, if no interpretation with probability > 0 is found
-- for an unknown word, its set of chosen labels will remain empty.
marginals :: (Ord a, Ord b) => Config b -> CRF a b -> Sent a b -> SentL a b
marginals Config{..} crf0 sent
  = fmap decodeChosen
  . DAG.zipE sent
  . I.marginals model
  . C.encodeSent codec
  $ sent
  where
    CRF{..} = blackList blackSet crf0
    decodeChosen (word, chosen) =
      mkWordL word prob
      where
        prob = mkProb
          [ (decode word x, L.fromLogFloat p)
          |  (x, p) <- chosen ]
    decode word = unJust codec word . decodeLabel codec


-- -- | Determine the most probable label sets of the given size (at maximum)
-- -- for each position in the input sentence.
-- tagK :: (Ord a, Ord b) => Int -> CRF a b -> Sent a b -> [[b]]
-- tagK k CRF{..} sent
--     = onWords . map decodeChoice
--     . DAG.toListProv
--     . I.tagK k model
--     . dagSent
--     . encodeSent codec
--     $ sent
--   where
--     decodeChoice = decodeLabels codec . map fst
--     onWords xss =
--         [ take k $ unJusts codec word xs
--         | (word, xs) <- zip sent xss ]
--
--
-- ------------------------------------------------------
-- -- Dataset conversion (Provisional)
-- ------------------------------------------------------
--
--
-- -- | Convert the sequential representation to DAG-based one.
-- dagSent :: Int.Xs -> DAG.DAG () Int.X
-- dagSent = DAG.fromList . V.toList


------------------------------------------------------
-- Blacklisting tags
------------------------------------------------------


-- | Blacklist the given set of tags, so that they are never proposed for
-- OOV segments.
blackList :: (Ord b) => S.Set b -> CRF a b -> CRF a b
blackList blackSet CRF{..} = CRF
  { codec = codec 
  , model = newModel
  }
  where
    newModel = model {Model.r0 = newR0}
    r0Set = S.fromList . Core.toAscList $ Model.r0 model
    newR0 = Core.fromSet $ r0Set `S.difference` blackSetEncoded
    blackSetEncoded 
      = S.fromList
      . map (C.encodeLabel codec)
      . S.toList
      $ blackSet
