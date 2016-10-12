{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}


-- | Provisional training module which works on sequential external data
-- but transforms it to DAG internal data form.


module Data.CRF.Chain1.Constrained.DAG.Train
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
import qualified Data.Vector as V
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

-- import Data.CRF.Chain1.Constrained.Dataset.Internal
-- import Data.CRF.Chain1.Constrained.Dataset.External
--     (SentL, WordL (..), lbs, unknown, unProb)
-- import Data.CRF.Chain1.Constrained.Dataset.Codec
--     (mkCodec, Codec, obMax, lbMax, encodeDataL, encodeLabels)
-- import Data.CRF.Chain1.Constrained.Feature (Feature, featuresIn)
-- import Data.CRF.Chain1.Constrained.Inference (accuracy, expectedFeaturesIn)

import           Data.CRF.Chain1.Constrained.Core (X, Y, Lb, AVec, Feature)
-- import qualified Data.CRF.Chain1.Constrained.Core as C
import qualified Data.CRF.Chain1.Constrained.Model as Md
import qualified Data.CRF.Chain1.Constrained.Dataset.Codec as Cd
import qualified Data.CRF.Chain1.Constrained.Dataset.External as E
import qualified Data.CRF.Chain1.Constrained.Dataset.Internal as Int

-- import           Data.CRF.Chain1.Constrained.Model
--     (Model (..), mkModel, FeatIx (..), featToJustInt)
-- import           Data.CRF.Chain1.Constrained.Dataset.External
--     (SentL, WordL (..), lbs, unknown, unProb)

import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (DAG)
import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
import           Data.CRF.Chain1.Constrained.DAG.Feature (featuresIn)
import qualified Data.CRF.Chain1.Constrained.DAG.Inference as I -- (accuracy, expectedFeaturesIn)


-- | A conditional random field model with additional codec used for
-- data encoding.
data CRF a b = CRF {
    -- | The codec is used to transform data into internal representation,
    -- where each observation and each label is represented by a unique
    -- integer number.
    codec :: Cd.Codec a b,
    -- | The actual model, which is a map from 'Feature's to potentials.
    model :: Md.Model }


instance (Ord a, Ord b, Binary a, Binary b) => Binary (CRF a b) where
    put CRF{..} = put codec >> put model
    get = CRF <$> get <*> get


-- | Train the CRF using the stochastic gradient descent method.
--
-- The resulting model will contain features extracted with the user supplied
-- extraction function.  You can use the functions provided by the
-- "Data.CRF.Chain1.Constrained.Feature.Present" and
-- "Data.CRF.Chain1.Constrained.Feature.Hidden"
-- modules for this purpose.
--
-- You also have to supply R0 construction method (e.g. `oovChosen`)
-- which determines the contents of the default set of labels.
train
    :: (Ord a, Ord b)
    => SGD.SgdArgs                          -- ^ Args for SGD
    -> Bool                                 -- ^ Store dataset on a disk
    -> ([E.SentL a b] -> S.Set b)           -- ^ R0 construction
    -> (AVec Lb -> [DAG () (X, Y)] -> [Feature]) -- ^ Feature selection
    -> IO [E.SentL a b]                     -- ^ Training data 'IO' action
    -> IO [E.SentL a b]                     -- ^ Evaluation data
    -> IO (CRF a b)                         -- ^ Resulting model
train sgdArgs onDisk mkR0 featSel trainIO evalIO = do
    hSetBuffering stdout NoBuffering

    -- Create codec and encode the training dataset
    codec <- Cd.mkCodec <$> trainIO
    trainData_ <- dagData . Cd.encodeDataL codec <$> trainIO

    -- TODO: finished here; need to convert internal sequential data
    -- to internal DAG-based data.

    SGD.withData onDisk trainData_ $ \trainData -> do

    -- Encode the evaluation dataset
    evalData_ <- dagData . Cd.encodeDataL codec <$> evalIO
    SGD.withData onDisk evalData_ $ \evalData -> do

    -- A default set of labels
    r0 <- Cd.encodeLabels codec . S.toList . mkR0 <$> trainIO

    -- A set of features
    feats <- featSel r0 <$> SGD.loadData trainData

    -- Train the model
    let model = (Md.mkModel (Cd.obMax codec) (Cd.lbMax codec) feats) { Md.r0 = r0 }
    para <- SGD.sgd sgdArgs
        (notify sgdArgs model trainData evalData)
        (gradOn model) trainData (Md.values model)
    return $ CRF codec (model { Md.values = para })


gradOn :: Md.Model -> SGD.Para -> DAG a (X, Y) -> SGD.Grad
gradOn model para dag = SGD.fromLogList $
    [ (Md.featToJustInt curr feat, L.fromPos val)
    | (feat, val) <- featuresIn dag ] ++
    [ (ix, L.fromNeg val)
    | (Md.FeatIx ix, val) <- I.expectedFeaturesIn curr (fmap fst dag) ]
  where
    curr = model { Md.values = para }


notify
    :: SGD.SgdArgs -> Md.Model
    -> SGD.Dataset (DAG a (X, Y))     -- ^ Training dataset
    -> SGD.Dataset (DAG a (X, Y))     -- ^ Evaluation dataset
    -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} model trainData evalData para k
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | SGD.size evalData > 0 = do
        x <- I.accuracy (model { Md.values = para }) <$> SGD.loadData evalData
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
-- Dataset conversion
------------------------------------------------------


-- | Convert the sequential representation to DAG-based one.
dagSent :: (Int.Xs, Int.Ys) -> DAG () (X, Y)
dagSent (xs, ys) = DAG.fromList (zip (V.toList xs) (V.toList ys))


-- | Convert the sequential representation to DAG-based one.
dagData :: [(Int.Xs, Int.Ys)] -> [DAG () (X, Y)]
dagData = map dagSent


------------------------------------------------------
-- R0 construction
------------------------------------------------------


-- | Collect labels assigned to OOV words.
oovChosen :: Ord b => [E.SentL a b] -> S.Set b
oovChosen =
  collect onWord
  where
    onWord x
      | E.unknown (E.word x) = M.keys . E.unProb . E.choice $ x
      | otherwise = []


-- | Collect labels assigned to words in a dataset.
anyChosen :: Ord b => [E.SentL a b] -> S.Set b
anyChosen = collect $ M.keys . E.unProb . E.choice


-- | Collect interpretations (also labels assigned) of words in a dataset.
anyInterps :: Ord b => [E.SentL a b] -> S.Set b
anyInterps = S.union
    <$> collect (S.toList . E.lbs . E.word)
    <*> anyChosen


-- | Collect labels given function which selects labels from a word.
collect :: Ord b => (E.WordL a b -> [b]) -> [E.SentL a b] -> S.Set b
collect onWord = S.fromList . concatMap (concatMap onWord)
