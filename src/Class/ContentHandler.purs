module LogicWeb.Class.ContentHandler where

import Prelude

import LogicWeb.Type.RawFormula (RawFormula)
import LogicWeb.Type.RawTruthTable (RawTruthTable)

class Monad m <= ContentHandler m where
  save :: m Unit
  load :: m Unit
  getFormulas :: m (Array RawFormula)
  getTruthTables :: Int -> m RawTruthTable
  getFormulaIds :: m (Array Int)
  setFormulas :: Array RawFormula -> m Unit
  setTruthTables :: Int ->RawTruthTable -> m Unit
  setFormulaIds :: Array Int -> m Unit