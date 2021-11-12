module LogicWeb.Type.Page where

import Prelude

data Page = TruthTableEditor | FormulaEditor | SymbolsEditor

derive instance Eq Page