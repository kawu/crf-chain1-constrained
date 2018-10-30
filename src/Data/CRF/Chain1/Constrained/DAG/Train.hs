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

-- * Utils
, verifyDAG
, Error(..)
-- , dagProb
) where


import Control.Applicative ((<$>), (<*>))
-- import qualified Control.Arrow as Arr
import Control.Monad (when, guard)
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Data.Binary (Binary, put, get)
-- import           Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
-- import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Foldable as F
import qualified Numeric.SGD.Momentum as SGD
import qualified Data.Number.LogFloat as LogFloat
import qualified Numeric.SGD.LogSigned as L
import qualified Data.MemoCombinators as Memo

-- import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (DAG)
-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
import           Data.DAG (DAG)
import qualified Data.DAG as DAG

import           Data.CRF.Chain1.Constrained.Core (X, Y, Lb, AVec, Feature)
import qualified Data.CRF.Chain1.Constrained.Core as C
import qualified Data.CRF.Chain1.Constrained.Model as Md
-- import qualified Data.CRF.Chain1.Constrained.Dataset.Internal as Int

import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Codec as Cd
import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.External as E

-- import           Data.CRF.Chain1.Constrained.Model
--     (Model (..), mkModel, FeatIx (..), featToJustInt)
-- import           Data.CRF.Chain1.Constrained.Dataset.External
--     (SentL, WordL (..), lbs, unknown, unProb)

import           Data.CRF.Chain1.Constrained.DAG.Feature (featuresIn)
import qualified Data.CRF.Chain1.Constrained.DAG.Inference as I -- (accuracy, expectedFeaturesIn)
import qualified Data.CRF.Chain1.Constrained.DAG.Probs as P -- (accuracy, expectedFeaturesIn)


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
    trainData_ <- Cd.encodeDataL codec <$> trainIO
    let trainLenOld = length trainData_
        trainData0 = verifyDataset trainData_
        trainLenNew = length trainData0
    -- mapM_ print $ map dagProb trainData_
    when (trainLenNew < trainLenOld) $ do
      putStrLn $ "Discarded "
        ++ show (trainLenOld - trainLenNew) ++ "/" ++ show trainLenOld
        ++  " elements from the training dataset"
    SGD.withData onDisk trainData0 $ \trainData -> do

    -- Encode the evaluation dataset
    evalData_ <- Cd.encodeDataL codec <$> evalIO
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
  | otherwise = do
      putStrLn "" >> report para
--       report $ U.map (*50.0) para
--       report $ U.map (*10.0) para
--       report $ U.map (*2.0) para
--       report $ U.map (*0.9) para
--       report $ U.map (*0.5) para
--       report $ U.map (*0.1) para
  where
    report paraNow = do
      let crf = model {Md.values = paraNow}
      llh <- show
        . LogFloat.logFromLogFloat
        . P.parLikelihood crf
        <$> SGD.loadData trainData
      acc <-
        if SGD.size evalData > 0
        then show . I.accuracy crf <$> SGD.loadData evalData
        else return "#"
      putStrLn $ "[" ++ show (doneTotal k) ++ "] stats:"
      putStrLn $ "min(params) = " ++ show (U.minimum para)
      putStrLn $ "max(params) = " ++ show (U.maximum para)
      putStrLn $ "log(likelihood(train)) = " ++ llh
      putStrLn $ "acc(eval) = " ++ acc
    doneTotal :: Int -> Int
    doneTotal = floor . done
    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize
    trainSize = SGD.size trainData


------------------------------------------------------
-- Verification
------------------------------------------------------


-- -- | Compute the probability of the DAG, based on the probabilities assigned to
-- -- different edges and their labels.
-- TODO: This implementation is not correct!
-- dagProb :: DAG a (X, Y) -> Double
-- dagProb dag = sum
--   [ fromEdge edgeID
--   | edgeID <- DAG.dagEdges dag
--   , DAG.isInitialEdge edgeID dag ]
--   where
--     fromEdge =
--       Memo.wrap DAG.EdgeID DAG.unEdgeID Memo.integral fromEdge'
--     fromEdge' edgeID
--       = edgeProb edgeID
--       * fromNode (DAG.endsWith edgeID dag)
--     edgeProb edgeID =
--       let (_x, y) = DAG.edgeLabel edgeID dag
--       in  sum . map snd $ C.unY y
--     fromNode nodeID =
--       case DAG.outgoingEdges nodeID dag of
--         [] -> 1
--         xs -> sum (map fromEdge xs)


-- | Filter incorrect sentences.
verifyDataset :: [DAG a (X, Y)] -> [DAG a (X, Y)]
verifyDataset =
  filter verify
  where
    verify dag = verifyDAG dag == Nothing
--     verify dag =
--       let p = dagProb dag
--       in  p >= 1 - eps && p <= 1 + eps
--     eps = 1e-9


-- | Verification error.
data Error
  = Malformed
  | Cyclic
  | SeveralSources [DAG.NodeID]
  | SeveralTargets [DAG.NodeID]
  | WrongBalance [DAG.NodeID]
    -- ^ Nodes for which the total sum of the incoming probabilities does not
    -- equal the total sum of the outgoing probabilities
  deriving (Show, Eq, Ord)


-- | Check if the DAG satisfies all the desirable properties.
verifyDAG :: DAG a (X, Y) -> Maybe Error
verifyDAG dag
  | not (DAG.isOK dag) = Just Malformed
  | not (DAG.isDAG dag) = Just Cyclic
  | length sources /= 1 = Just $ SeveralSources sources
  | length targets /= 1 = Just $ SeveralTargets targets
  | length wrong > 1 = Just $ WrongBalance wrong
  | otherwise = Nothing
  where
    sources = do
      node <- DAG.dagNodes dag
      guard . null $ DAG.ingoingEdges node dag
      return node
    targets = do
      node <- DAG.dagNodes dag
      guard . null $ DAG.outgoingEdges node dag
      return node
    wrong = do
      node <- DAG.dagNodes dag
      let ing = DAG.ingoingEdges node dag
          out = DAG.outgoingEdges node dag
          ingBalance =
            if node `elem` sources
               then 1
               else sum (map edgeProb ing)
          outBalance =
            if node `elem` targets
               then 1
               else sum (map edgeProb out)
      guard . not $ equal ingBalance outBalance
      return node
    edgeProb edgeID =
      let (_x, y) = DAG.edgeLabel edgeID dag
      in  sum . map snd $ C.unY y
    equal x y =
      x - eps <= y && x + eps >= y
    eps = 1e-9


------------------------------------------------------
-- Expectation maximization
------------------------------------------------------


-- TODO:


-- ------------------------------------------------------
-- -- Dataset conversion (provisional?)
-- ------------------------------------------------------
--
--
-- -- | Convert the sequential representation to DAG-based one.
-- dagSent :: (Int.Xs, Int.Ys) -> DAG () (X, Y)
-- dagSent (xs, ys) = DAG.fromList (zip (V.toList xs) (V.toList ys))
--
--
-- -- | Convert the sequential representation to DAG-based one.
-- dagData :: [(Int.Xs, Int.Ys)] -> [DAG () (X, Y)]
-- dagData = map dagSent


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
collect onWord = S.fromList . concatMap (F.concatMap onWord)
