module Data.CRF.Chain1.Constrained.Dataset.Codec
( Codec
, CodecM

, encodeWord'Cu
, encodeWord'Cn
, encodeSent'Cu
, encodeSent'Cn
, encodeSent

, encodeWordL'Cu
, encodeWordL'Cn
, encodeSentL'Cu
, encodeSentL'Cn
, encodeSentL

, decodeLabel
, decodeLabels

, mkCodec
, encodeData
, encodeDataL
) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes)
import Data.Lens.Common (fstLens, sndLens)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C

import Data.CRF.Chain1.Constrained.Dataset.Internal
import Data.CRF.Chain1.Constrained.Dataset.External

-- | A codec.  The first component is used to encode observations
-- of type a, the second one is used to encode labels of type b.
type Codec a b = (C.AtomCodec a, C.AtomCodec b)

-- | Type synonym for the codec monad.  It is important to notice that by a
-- codec we denote here a structure of two 'C.AtomCodec's while in the
-- monad-codec package it denotes a monad.
type CodecM a b c = C.Codec (Codec a b) c

-- | Encode the labeled word and update the codec.
encodeWordL'Cu
    :: (Ord a, Ord b) => AVec Lb
    -> WordL a b -> CodecM a b (X, Y)
encodeWordL'Cu r0 (word, choice) = do
    x' <- map Ob <$> mapM (C.encode' fstLens) (S.toList (obs word))
    r' <- map Lb <$> mapM (C.encode  sndLens) (S.toList (lbs word))
    let x = mkX r0 x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> (Lb <$> C.encode sndLens lb) <*> pure pr
	| (lb, pr) <- (M.toList . unDist) choice ]
    return (x, y)

-- | Encodec the labeled word and do *not* update the codec.
-- If the label is not in the codec, use the default value.
encodeWordL'Cn
    :: (Ord a, Ord b) => AVec Lb -> Lb
    -> WordL a b -> CodecM a b (X, Y)
encodeWordL'Cn r0 defa (word, choice) = do
    x' <- map Ob . catMaybes <$>
        mapM (C.maybeEncode fstLens) (S.toList (obs word))
    r' <- mapM (withDefa defa) (S.toList (lbs word))
    let x = mkX r0 x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> withDefa defa lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) choice ]
    return (x, y)

-- | Encode the object with the corresponding codec label or
-- with the provided default label when object is unknown.
withDefa :: Ord b => Lb -> b -> CodecM a b Lb
withDefa defa x = maybe defa Lb <$> C.maybeEncode sndLens x
{-# INLINE withDefa #-}

-- | Encode the word and update the codec.
encodeWord'Cu :: (Ord a, Ord b) => AVec Lb -> Word a b -> CodecM a b X
encodeWord'Cu r0 word = do
    x' <- map Ob <$> mapM (C.encode' fstLens) (S.toList (obs word))
    r' <- map Lb <$> mapM (C.encode  sndLens) (S.toList (lbs word))
    return $ mkX r0 x' r'

-- | Encode the word and do *not* update the codec.
encodeWord'Cn :: (Ord a, Ord b) => AVec Lb -> Lb -> Word a b -> CodecM a b X
encodeWord'Cn r0 defa word = do
    x' <- map Ob . catMaybes <$>
        mapM (C.maybeEncode fstLens) (S.toList (obs word))
    r' <- mapM (withDefa defa) (S.toList (lbs word))
    return $ mkX r0 x' r'

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu
    :: (Ord a, Ord b) => AVec Lb
    -> SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cu r0 sent = do
    ps <- mapM (encodeWordL'Cu r0) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn
    :: (Ord a, Ord b) => AVec Lb -> Lb
    -> SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cn r0 defa sent = do
    ps <- mapM (encodeWordL'Cn r0 defa) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the label, do not update the codec and throw error
-- when the label is not a member of the codec.
encodeLb :: Ord b => b -> CodecM a b Lb
encodeLb x = do
    C.maybeEncode sndLens x >>= \my -> case my of
        Just x' -> return (Lb x')
        Nothing -> error "encodeLb: element not in the codec"

-- | Encode labels into an ascending vector of distinct label codes.
-- Do not update the codec and throw error when one of labels is not
-- a member of the codec.
encodeLbs :: Ord b => Codec a b -> [b] -> AVec Lb
encodeLbs codec = fromList . C.evalCodec codec . mapM encodeLb

encodeDefa :: Ord b => Codec a b -> b -> Lb
encodeDefa codec = C.evalCodec codec . encodeLb

encodeR0 :: Ord b => Codec a b -> S.Set b -> AVec Lb
encodeR0 codec = encodeLbs codec . S.toList

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL
    :: (Ord a, Ord b) => S.Set b -> b
    -> Codec a b -> SentL a b -> (Xs, Ys)
encodeSentL r0 defa codec =
    C.evalCodec codec . encodeSentL'Cn
        (encodeR0 codec r0)
        (encodeDefa codec defa)

-- | Encode the sentence and update the codec.
encodeSent'Cu :: (Ord a, Ord b) => AVec Lb -> Sent a b -> CodecM a b Xs
encodeSent'Cu r0 = fmap V.fromList . mapM (encodeWord'Cu r0)

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn :: (Ord a, Ord b) => AVec Lb -> Lb -> Sent a b -> CodecM a b Xs
encodeSent'Cn r0 defa = fmap V.fromList . mapM (encodeWord'Cn r0 defa)

-- | Encode the sentence using the given codec.
encodeSent :: (Ord a, Ord b) => S.Set b -> b -> Codec a b -> Sent a b -> Xs
encodeSent r0 defa codec =
    C.evalCodec codec . encodeSent'Cn
        (encodeR0 codec r0)
        (encodeDefa codec defa)

-- | Create the codec on the basis of the labeled dataset, return the
-- resultant codec and the encoded dataset.
mkCodec :: (Ord a, Ord b) => S.Set b -> [SentL a b] -> (Codec a b, [(Xs, Ys)])
mkCodec r00
    = swap
    . C.runCodec codec0
    . mapM (encodeSentL'Cu r0)
  where
    swap (x, y) = (y, x)
    (r0, codec0) = C.runCodec (C.empty, C.empty) $
        fromList . map Lb <$> mapM (C.encode sndLens) (S.toList r00)

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL
    :: (Ord a, Ord b) => S.Set b -> b
    -> Codec a b -> [SentL a b] -> [(Xs, Ys)]
encodeDataL r0 defa codec =
    C.evalCodec codec . mapM ( encodeSentL'Cn
        (encodeR0 codec r0)
        (encodeDefa codec defa) )

-- | Encode the dataset with the codec.
encodeData :: (Ord a, Ord b) => S.Set b -> b -> Codec a b -> [Sent a b] -> [Xs]
encodeData r0 defa codec = map (encodeSent r0 defa codec)

-- | Decode the label.
decodeLabel :: Ord b => Codec a b -> Lb -> b
decodeLabel codec x = C.evalCodec codec $ C.decode sndLens (unLb x)

-- | Decode the sequence of labels.
decodeLabels :: Ord b => Codec a b -> [Lb] -> [b]
decodeLabels codec xs = C.evalCodec codec $
    sequence [C.decode sndLens (unLb x) | x <- xs]
