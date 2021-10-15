module LogicWeb.PropositionalLogic where

import Prelude

import Data.Array (findIndex, index, length)
import Data.Set as Set
import LogicWeb.PropositionalLogic.Formula (Formula, Variable, compute, variables)
import LogicWeb.PropositionalLogic.TruthTable (TruthTable)

formulaToTruthTable :: Formula -> TruthTable
formulaToTruthTable f =
  { variablesNum: length vs
  , variableSymbols: \i -> index (map (_.symbol) vs) i
  , table: \g -> compute f (\s -> g =<< findIndex (_ == s) vs)
  }
  where
  vs = Set.toUnfoldable $ variables f :: Array Variable