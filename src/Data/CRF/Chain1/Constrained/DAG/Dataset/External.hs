module Data.CRF.Chain1.Constrained.DAG.Dataset.External
( Sent
, SentL
) where


-- import qualified Data.Set as S
-- import qualified Data.Map as M

import qualified Data.DAG as DAG
import           Data.DAG (DAG)
-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
-- import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (DAG)
import qualified Data.CRF.Chain1.Constrained.Dataset.External as C


-- | A sentence (DAG) of words.
type Sent a b = DAG () (C.Word a b)


-- | A sentence (DAG) of labeled words.
type SentL a b = DAG () (C.WordL a b)
