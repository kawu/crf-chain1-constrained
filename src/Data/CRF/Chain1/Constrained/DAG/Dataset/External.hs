module Data.CRF.Chain1.Constrained.DAG.Dataset.External
( Sent
, SentL
, module Data.CRF.Chain1.Constrained.Dataset.External
) where


import           Prelude hiding (Word)
-- import qualified Data.Set as S
-- import qualified Data.Map as M

-- import qualified Data.DAG as DAG
import           Data.DAG (DAG)
-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
-- import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (DAG)
import           Data.CRF.Chain1.Constrained.Dataset.External hiding (Sent, SentL)


-- | A sentence (DAG) of words.
type Sent a b = DAG () (Word a b)


-- | A sentence (DAG) of labeled words.
type SentL a b = DAG () (WordL a b)
