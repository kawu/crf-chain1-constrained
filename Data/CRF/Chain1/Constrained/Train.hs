{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.CRF.Chain1.Constrained.Train
( CRF (..)
, train
) where

import Control.Applicative ((<$>), (<*>))
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Data.Binary (Binary, put, get)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Chain1.Constrained.Dataset.Internal
import Data.CRF.Chain1.Constrained.Dataset.External (SentL, unknown, unDist)
import Data.CRF.Chain1.Constrained.Dataset.Codec
    (mkCodec, Codec, obMax, lbMax, encodeDataL, encodeLabels)
import Data.CRF.Chain1.Constrained.Feature (Feature, featuresIn)
import Data.CRF.Chain1.Constrained.Model
    (Model (..), mkModel, FeatIx (..), featToJustInt)
import Data.CRF.Chain1.Constrained.Inference (accuracy, expectedFeaturesIn)

-- | A conditional random field model with additional codec used for
-- data encoding.
data CRF a b = CRF {
    -- | The codec is used to transform data into internal representation,
    -- where each observation and each label is represented by a unique
    -- integer number.
    codec :: Codec a b,
    -- | The actual model, which is a map from 'Feature's to potentials.
    model :: Model }

instance (Ord a, Ord b, Binary a, Binary b) => Binary (CRF a b) where
    put CRF{..} = put codec >> put model
    get = CRF <$> get <*> get

-- | Train the CRF using the stochastic gradient descent method.
-- The resulting model will contain features extracted with
-- the user supplied extraction function.
-- You can use the functions provided by the "Data.CRF.Chain1.Feature.Present"
-- and "Data.CRF.Chain1.Feature.Hidden" modules for this purpose.
-- When the evaluation data 'IO' action is 'Just', the iterative
-- training process will notify the user about the current accuracy
-- on the evaluation part every full iteration over the training part.
-- TODO: Accept custom r0 construction function.
train
    :: (Ord a, Ord b)
    => SGD.SgdArgs                  -- ^ Args for SGD
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> Maybe (IO [SentL a b])       -- ^ Maybe evalation data
    -> (AVec Lb -> [(Xs, Ys)] -> [Feature])     -- ^ Feature selection
    -> IO (CRF a b)                 -- ^ Resulting model
train sgdArgs trainIO evalIO'Maybe extractFeats = do
    hSetBuffering stdout NoBuffering
    (_codec, trainData) <- mkCodec <$> trainIO
    _r0 <- encodeLabels _codec . S.toList . unkSet <$> trainIO
    evalDataM <- case evalIO'Maybe of
        Just evalIO -> Just . encodeDataL _codec <$> evalIO
        Nothing     -> return Nothing
    let feats = extractFeats _r0 trainData
        crf = (mkModel (obMax _codec) (lbMax _codec) feats) { r0 = _r0 }
    para <- SGD.sgdM sgdArgs
        (notify sgdArgs crf trainData evalDataM)
        (gradOn crf) (V.fromList trainData) (values crf)
    return $ CRF _codec (crf { values = para })

-- | Collect labels assigned to unknown words (with empty list
-- of potential interpretations).
unkSet :: Ord b => [SentL a b] -> S.Set b
unkSet =
    S.fromList . concatMap onSent
  where
    onSent = concatMap onWord
    onWord word
        | unknown (fst word)    = M.keys . unDist . snd $ word
        | otherwise             = []

gradOn :: Model -> SGD.Para -> (Xs, Ys) -> SGD.Grad
gradOn crf para (xs, ys) = SGD.fromLogList $
    [ (featToJustInt curr feat, L.fromPos val)
    | (feat, val) <- featuresIn xs ys ] ++
    [ (ix, L.fromNeg val)
    | (FeatIx ix, val) <- expectedFeaturesIn curr xs ]
  where
    curr = crf { values = para }

notify
    :: SGD.SgdArgs -> Model -> [(Xs, Ys)] -> Maybe [(Xs, Ys)]
    -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} crf trainData evalDataM para k 
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | Just dataSet <- evalDataM = do
        let x = accuracy (crf { values = para }) dataSet
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = " ++ show x)
    | otherwise =
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = #")
  where
    doneTotal :: Int -> Int
    doneTotal = floor . done
    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize
    trainSize = length trainData
