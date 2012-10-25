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
import qualified Data.Vector.Unboxed as U
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
    :: (Ord a, Ord b) => U.Vector Lb
    -> WordL a b -> CodecM a b (X, Y)
encodeWordL'Cu r0 (word, choice) = do
    x' <- map Ob <$> mapM (C.encode' fstLens) (S.toList (obs word))
    r' <- map Lb <$> mapM (C.encode' sndLens) (S.toList (lbs word))
    x  <- mkX r0 x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> (Lb <$> C.encode sndLens lb) <*> pure pr
	| (lb, pr) <- (M.toList . unDist) choice ]
    return (x, y)

-- | Encodec the labeled word and do *not* update the codec.
-- If the label is not in the codec, use the default value.
encodeWordL'Cn
    :: (Ord a, Ord b) => U.Vector Lb -> Int
    -> WordL a b -> CodecM a b (X, Y)
encodeWordL'Cn r0 i (word, choice) = do
    x' <- map Ob . catMaybes <$>
        mapM (C.maybeEncode fstLens) (S.toList (obs word))
    r' <- map Lb <$> mapM (encodeL i) (S.toList (lbs word))
    x  <- mkX r0 x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeL i lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) choice ]
    return (x, y)
  where
    encodeL j y = Lb . maybe j id <$> C.maybeEncode sndLens y

-- | Encode the word and update the codec.
encodeWord'Cu :: Ord a => U.Vector Lb -> Word a -> CodecM a b X
encodeWord'Cu r0 word = do
    x' <- map Ob <$> mapM (C.encode' fstLens) (S.toList (obs word))
    r' <- map Lb <$> mapM (C.encode' sndLens) (S.toList (lbs word))
    mkX r0 x' r'

-- | Encode the word and do *not* update the codec.
encodeWord'Cn :: Ord a => U.Vector Lb -> Word a -> CodecM a b X
encodeWord'Cn r0 word = do
    x' <- map Ob . catMaybes <$>
        mapM (C.maybeEncode fstLens) (S.toList (obs word))
    -- FIXME: Ensure that the label vector is ascending
    -- and distinct.  Use the default value.  Use the newtype
    -- to ensure these properties.
    r' <- map Lb <$> mapM (encodeL i) (S.toList (lbs word))
    mkX r0 x' r'
  where
    encodeL j y = Lb . maybe j id <$> C.maybeEncode sndLens y

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu
    :: (Ord a, Ord b) => U.Vector Lb
    -> SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cu r0 sent = do
    ps <- mapM (encodeWordL'Cu r0) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn
    :: (Ord a, Ord b) => U.Vector Lb -> b
    -> SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cn r0 defa sent = do
    i <- C.maybeEncode sndLens defa >>= \mi -> case mi of
        Just _i -> return _i
        Nothing -> error "encodeWordL'Cn: default label not in the codec"
    ps <- mapM (encodeWordL'Cn r0 i) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the set of labels into an ascending vector of
-- distinct label codes.
encodeLbs :: Codec a b -> S.Set b -> U.Vector Lb
encodeLbs codec xs = U.fromList . sort . C.evalCode codec $
    forM (S.toList xs) $ \x -> do
        C.maybeEncode sndLens x >>= \my -> case my of
            Just  y -> return y
            Nothing -> error "encodeLbs: element not in the codec"

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
-- TODO: It should be checked that the default valus is a member
-- of the default set of potential labels.
encodeSentL
    :: (Ord a, Ord b) => S.Set b -> b
    -> Codec a b -> SentL a b -> (Xs, Ys)
encodeSentL r0 defa codec =
    C.evalCodec codec . encodeSentL'Cn (encodeLbs r0) defa

-- | Encode the sentence and update the codec.
encodeSent'Cu :: Ord a => U.Vector Lb -> Sent a -> CodecM a b Xs
encodeSent'Cu r0 = fmap V.fromList . mapM (encodeWord'Cu r0)

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn :: Ord a => U.Vector Lb -> Sent a -> CodecM a b Xs
encodeSent'Cn r0 = fmap V.fromList . mapM (encodeWord'Cn r0)

-- | Encode the sentence using the given codec.
encodeSent :: Ord a => S.Set b -> Codec a b -> Sent a -> Xs
encodeSent r0 codec = C.evalCodec codec . encodeSent'Cn (encodeLbs r0)

-- | Create the codec on the basis of the labeled dataset, return the
-- resultant codec and the encoded dataset.
mkCodec :: (Ord a, Ord b) => S.Set b -> [SentL a b] -> (Codec a b, [(Xs, Ys)])
mkCodec r00
    = swap
    . C.runCodec (C.empty, C.empty)
    . mapM (encodeSentL'Cu r0)
  where
    swap (x, y) = (y, x)
    r0 = encodeLbs r00

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
-- TODO: It should be checked that the default valus is a member
-- of the default set of potential labels.
encodeDataL
    :: (Ord a, Ord b) => S.Set b -> b
    -> Codec a b -> [SentL a b] -> [(Xs, Ys)]
encodeDataL r00 defa codec =
    let r0 = encodeLbs r00
    in  C.evalCodec codec . mapM (encodeSentL'Cn r0 defa)

-- | Encode the dataset with the codec.
encodeData :: Ord a => S.Set b -> Codec a b -> [Sent a] -> [Xs]
encodeData r0 codec = map (encodeSent r0 codec)

-- | Decode the label.
decodeLabel :: Ord b => Codec a b -> Lb -> b
decodeLabel codec x = C.evalCodec codec $ C.decode sndLens (unLb x)

-- | Decode the sequence of labels.
decodeLabels :: Ord b => Codec a b -> [Lb] -> [b]
decodeLabels codec xs = C.evalCodec codec $
    sequence [C.decode sndLens (unLb x) | x <- xs]
