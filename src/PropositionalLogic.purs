module LogicWeb.PropositionalLogic where


import LogicWeb.PropositionalLogic.Formula (Formula, compute, variables)
import LogicWeb.PropositionalLogic.TruthTable (TruthTable)

class PropositionalLogic a where
  toTruthTable :: a -> TruthTable

instance PropositionalLogic Formula where
  toTruthTable :: Formula -> TruthTable
  toTruthTable f =
    { variables: vs
    , table: compute f
    }
    where
    vs = variables f