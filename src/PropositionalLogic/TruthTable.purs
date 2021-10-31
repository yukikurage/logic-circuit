module LogicWeb.PropositionalLogic.TruthTable where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))

newtype TruthTable = TruthTable
  { variables :: Array String
  , output :: String
  , table :: (String -> Maybe Boolean) -> Maybe Boolean
  }

emptyTruthTable :: TruthTable
emptyTruthTable = TruthTable
  { variables: []
  , output: ""
  , table: const Nothing
  }

null :: TruthTable -> Boolean
null (TruthTable t) = A.null t.variables && t.table (const Nothing) == Nothing