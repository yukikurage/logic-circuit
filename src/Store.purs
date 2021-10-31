module LogicWeb.Store where

import LogicWeb.Type.RawFormula (RawFormula)

type Store = {formulas :: Array RawFormula}

initialStore :: Store
initialStore = {formulas: []}

data Action
  = SetFormulas (Array RawFormula)

reduce :: Store -> Action -> Store
reduce _ = case _ of
  SetFormulas x -> {formulas: x}