module Data.CRF.Chain1.Constrained.Dataset.Codec
( Codec
, CodecM
, obMax
, lbMax

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

, encodeLabels
, decodeLabel
, decodeLabels

, mkCodec
, encodeData
, encodeDataL
, unJust
, unJusts
) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes, fromJust)
import Data.Lens.Common (fstLens, sndLens)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C

import Data.CRF.Chain1.Constrained.Dataset.Internal
import Data.CRF.Chain1.Constrained.Dataset.External

-- | A codec.  The first component is used to encode observations
-- of type a, the second one is used to encode labels of type b.
type Codec a b = (C.AtomCodec a, C.AtomCodec (Maybe b))

-- | The maximum internal observation included in the codec.
obMax :: Codec a b -> Ob
obMax =
    let idMax m = M.size m - 1
    in  Ob . idMax . C.to . fst

-- | The maximum internal label included in the codec.
lbMax :: Codec a b -> Lb
lbMax =
    let idMax m = M.size m - 1
    in  Lb . idMax . C.to . snd

-- | The empty codec.  The label part is initialized with Nothing
-- member, which represents unknown labels.  It is taken on account
-- in the model implementation because it is assigned to the
-- lowest label code and the model assumes that the set of labels
-- is of the {0, ..., 'lbMax'} form.
empty :: Ord b => Codec a b
empty =
    let withNo = C.execCodec C.empty (C.encode C.idLens Nothing)
    in  (C.empty, withNo)

-- | Type synonym for the codec monad.  It is important to notice that by a
-- codec we denote here a structure of two 'C.AtomCodec's while in the
-- monad-codec package it denotes a monad.
type CodecM a b c = C.Codec (Codec a b) c

-- | Encode the observation and update the codec (only in the encoding
-- direction).
encodeObU :: Ord a => a -> CodecM a b Ob
encodeObU = fmap Ob . C.encode' fstLens

-- | Encode the observation and do *not* update the codec.
encodeObN :: Ord a => a -> CodecM a b (Maybe Ob)
encodeObN = fmap (fmap Ob) . C.maybeEncode fstLens

-- | Encode the label and update the codec.
encodeLbU :: Ord b => b -> CodecM a b Lb
encodeLbU = fmap Lb . C.encode sndLens . Just

-- | Encode the label and do *not* update the codec.
encodeLbN :: Ord b => b -> CodecM a b Lb
encodeLbN x = do
    my <- C.maybeEncode sndLens (Just x)
    Lb <$> ( case my of
        Just y  -> return y
        Nothing -> fromJust <$> C.maybeEncode sndLens Nothing )

-- | Encode the labeled word and update the codec.
encodeWordL'Cu :: (Ord a, Ord b) => WordL a b -> CodecM a b (X, Y)
encodeWordL'Cu (word, choice) = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    let x = mkX x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeLbU lb <*> pure pr
	| (lb, pr) <- (M.toList . unProb) choice ]
    return (x, y)

-- | Encodec the labeled word and do *not* update the codec.
encodeWordL'Cn :: (Ord a, Ord b) => WordL a b -> CodecM a b (X, Y)
encodeWordL'Cn (word, choice) = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    let x = mkX x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeLbN lb <*> pure pr
	| (lb, pr) <- (M.toList . unProb) choice ]
    return (x, y)

-- | Encode the word and update the codec.
encodeWord'Cu :: (Ord a, Ord b) => Word a b -> CodecM a b X
encodeWord'Cu word = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the word and do *not* update the codec.
encodeWord'Cn :: (Ord a, Ord b) => Word a b -> CodecM a b X
encodeWord'Cn word = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu :: (Ord a, Ord b) => SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cu sent = do
    ps <- mapM (encodeWordL'Cu) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn :: (Ord a, Ord b) => SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cn sent = do
    ps <- mapM (encodeWordL'Cn) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode labels into an ascending vector of distinct label codes.
encodeLabels :: Ord b => Codec a b -> [b] -> AVec Lb
encodeLabels codec = fromList . C.evalCodec codec . mapM encodeLbN

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL :: (Ord a, Ord b) => Codec a b -> SentL a b -> (Xs, Ys)
encodeSentL codec = C.evalCodec codec . encodeSentL'Cn

-- | Encode the sentence and update the codec.
encodeSent'Cu :: (Ord a, Ord b) => Sent a b -> CodecM a b Xs
encodeSent'Cu = fmap V.fromList . mapM encodeWord'Cu

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn :: (Ord a, Ord b) => Sent a b -> CodecM a b Xs
encodeSent'Cn = fmap V.fromList . mapM encodeWord'Cn

-- | Encode the sentence using the given codec.
encodeSent :: (Ord a, Ord b) => Codec a b -> Sent a b -> Xs
encodeSent codec = C.evalCodec codec . encodeSent'Cn

-- | Create the codec on the basis of the labeled dataset, return the
-- resultant codec and the encoded dataset.
mkCodec :: (Ord a, Ord b) => [SentL a b] -> (Codec a b, [(Xs, Ys)])
mkCodec
    = swap
    . C.runCodec empty
    . mapM encodeSentL'Cu
  where
    swap (x, y) = (y, x)

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL :: (Ord a, Ord b) => Codec a b -> [SentL a b] -> [(Xs, Ys)]
encodeDataL codec = C.evalCodec codec . mapM encodeSentL'Cn

-- | Encode the dataset with the codec.
encodeData :: (Ord a, Ord b) => Codec a b -> [Sent a b] -> [Xs]
encodeData codec = map (encodeSent codec)

-- | Decode the label.
decodeLabel :: Ord b => Codec a b -> Lb -> Maybe b
decodeLabel codec x = C.evalCodec codec $ C.decode sndLens (unLb x)

-- | Decode the sequence of labels.
decodeLabels :: Ord b => Codec a b -> [Lb] -> [Maybe b]
decodeLabels codec xs = C.evalCodec codec $
    sequence [C.decode sndLens (unLb x) | x <- xs]

hasLabel :: Ord b => Codec a b -> b -> Bool
hasLabel codec x = M.member (Just x) (C.to $ snd codec)
{-# INLINE hasLabel #-}

-- | Return the label when 'Just' or one of the unknown values
-- when 'Nothing'.
unJust :: Ord b => Codec a b -> Word a b -> Maybe b -> b
unJust _ _ (Just x) = x
unJust codec word Nothing = case allUnk of
    (x:_)   -> x
    []      -> error "unJust: Nothing and all values known"
  where
    allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)

-- | Replace 'Nothing' labels with all unknown labels from
-- the set of potential interpretations.
unJusts :: Ord b => Codec a b -> Word a b -> [Maybe b] -> [b]
unJusts codec word xs =
    concatMap deJust xs
  where
    allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)
    deJust (Just x) = [x]
    deJust Nothing  = allUnk
