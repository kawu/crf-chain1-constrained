{-# LANGUAGE RecordWildCards #-}

-- | The module provides first-order, linear-chain conditional random fields
-- (CRFs) with position-wide constraints over label values.

module Data.CRF.Chain1.Constrained
(
-- * Data types
  Word (..)
, unknown
, Sent
, Prob (unProb)
, mkProb
, WordL
, SentL

-- ** Tagging
, tag
, tagK

-- * Modules
, module Data.CRF.Chain1.Constrained.Train
, module Data.CRF.Chain1.Constrained.Feature.Present
, module Data.CRF.Chain1.Constrained.Feature.Hidden
) where

import Data.CRF.Chain1.Constrained.Dataset.External
import Data.CRF.Chain1.Constrained.Dataset.Codec
import Data.CRF.Chain1.Constrained.Feature.Present
import Data.CRF.Chain1.Constrained.Feature.Hidden
import Data.CRF.Chain1.Constrained.Train
import qualified Data.CRF.Chain1.Constrained.Inference as I

-- | Determine the most probable label sequence within the context of the
-- given sentence using the model provided by the 'CRF'.
tag :: (Ord a, Ord b) => CRF a b -> Sent a b -> [b]
tag CRF{..} sent
    = onWords . decodeLabels codec
    . I.tag model . encodeSent codec
    $ sent
  where
    onWords xs =
        [ unJust codec word x
        | (word, x) <- zip sent xs ]

-- | Determine the most probable label sets of the given size (at maximum)
-- for each position in the input sentence.
tagK :: (Ord a, Ord b) => Int -> CRF a b -> Sent a b -> [[b]]
tagK k CRF{..} sent
    = onWords . map decodeChoice
    . I.tagK k model . encodeSent codec
    $ sent
  where
    decodeChoice = decodeLabels codec . map fst
    onWords xss =
        [ take k $ unJusts codec word xs
        | (word, xs) <- zip sent xss ]
