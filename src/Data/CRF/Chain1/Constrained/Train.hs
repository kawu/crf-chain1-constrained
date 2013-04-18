{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}


module Data.CRF.Chain1.Constrained.Train
(
-- * Model
  CRF (..)

-- * Training
, train

-- * R0 construction
, oovChosen
, anyChosen
, anyInterps
) where


import Control.Applicative ((<$>), (<*>))
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Data.Binary (Binary, put, get)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Chain1.Constrained.Dataset.Internal
import Data.CRF.Chain1.Constrained.Dataset.External
    (SentL, WordL, lbs, unknown, unProb)
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
--
-- The resulting model will contain features extracted with
-- the user supplied extraction function.
-- You can use the functions provided by the "Data.CRF.Chain1.Feature.Present"
-- and "Data.CRF.Chain1.Feature.Hidden" modules for this purpose.
--
-- You also have to supply R0 construction method (e.g. `oovChosen`).
train
    :: (Ord a, Ord b)
    => SGD.SgdArgs                          -- ^ Args for SGD
    -> Bool                                 -- ^ Store dataset on a disk
    -> ([SentL a b] -> S.Set b)             -- ^ R0 construction
    -> (AVec Lb -> [(Xs, Ys)] -> [Feature]) -- ^ Feature selection
    -> IO [SentL a b]                       -- ^ Training data 'IO' action
    -> IO [SentL a b]                       -- ^ Evaluation data
    -> IO (CRF a b)                         -- ^ Resulting model
train sgdArgs onDisk mkR0 featSel trainIO evalIO = do
    hSetBuffering stdout NoBuffering

    -- Create codec and encode the training dataset
    codec <- mkCodec <$> trainIO
    trainData_ <- encodeDataL codec <$> trainIO
    SGD.withData onDisk trainData_ $ \trainData -> do

    -- Encode the evaluation dataset
    evalData_ <- encodeDataL codec <$> evalIO
    SGD.withData onDisk evalData_ $ \evalData -> do

    -- A default set of labels
    r0 <- encodeLabels codec . S.toList . mkR0 <$> trainIO

    -- A set of features
    feats <- featSel r0 <$> SGD.loadData trainData

    -- Train the model
    let model = (mkModel (obMax codec) (lbMax codec) feats) { r0 = r0 }
    para <- SGD.sgd sgdArgs
        (notify sgdArgs model trainData evalData)
        (gradOn model) trainData (values model)
    return $ CRF codec (model { values = para })


gradOn :: Model -> SGD.Para -> (Xs, Ys) -> SGD.Grad
gradOn model para (xs, ys) = SGD.fromLogList $
    [ (featToJustInt curr feat, L.fromPos val)
    | (feat, val) <- featuresIn xs ys ] ++
    [ (ix, L.fromNeg val)
    | (FeatIx ix, val) <- expectedFeaturesIn curr xs ]
  where
    curr = model { values = para }


notify
    :: SGD.SgdArgs -> Model
    -> SGD.Dataset (Xs, Ys)     -- ^ Training dataset
    -> SGD.Dataset (Xs, Ys)     -- ^ Evaluation dataset
    -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} model trainData evalData para k
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | SGD.size evalData > 0 = do
        x <- accuracy (model { values = para }) <$> SGD.loadData evalData
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
    trainSize = SGD.size trainData


------------------------------------------------------
-- R0 construction
------------------------------------------------------


-- | Collect labels assigned to OOV words.
oovChosen :: Ord b => [SentL a b] -> S.Set b
oovChosen = collect onWord where
    onWord word
        | unknown (fst word)    = M.keys . unProb . snd $ word
        | otherwise             = []


-- | Collect labels assigned to words in a dataset.
anyChosen :: Ord b => [SentL a b] -> S.Set b
anyChosen = collect $ M.keys . unProb . snd


-- | Collect interpretations (also labels assigned) of words in a dataset.
anyInterps :: Ord b => [SentL a b] -> S.Set b
anyInterps = S.union
    <$> collect (S.toList . lbs . fst)
    <*> anyChosen


-- | Collect labels given function which selects labels from a word.
collect :: Ord b => (WordL a b -> [b]) -> [SentL a b] -> S.Set b
collect onWord = S.fromList . concatMap (concatMap onWord)
