module LogicWeb.PropositionalLogic.TruthTable where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))

type TruthTable =
  { variables :: Array String
  , output :: String
  , table :: (String -> Maybe Boolean) -> Maybe Boolean
  }

emptyTruthTable :: TruthTable
emptyTruthTable =
  { variables: []
  , output: ""
  , table: const Nothing
  }

null :: TruthTable -> Boolean
null t = A.null t.variables && t.table (const Nothing) == Nothing