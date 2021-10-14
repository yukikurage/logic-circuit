module LogicWeb.PropositionalLogic.TruthTable where

import Prelude

import Data.Array (all, index)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set as Set
import Data.String (joinWith)
import Data.Unfoldable (replicateA)

type TruthTable =
  { variablesNum :: Int
  , variableSymbols :: Int -> String
  , table :: (Int -> Maybe Boolean) -> Maybe Boolean
  }

showTruthTable :: TruthTable -> String
showTruthTable t = joinWith "\t" $ map showLine $ replicateA t.variablesNum [true, false]
  where
  showBoolean x = if x then "1" else "0"
  showMaybeBoolean = maybe "*" showBoolean
  showLine :: Array Boolean -> String
  showLine xs = joinWith " | " (map showBoolean xs) <> " || " <> showMaybeBoolean (t.table $ index xs)

isValid :: TruthTable -> Boolean
isValid t = all (isJust <<< t.table <<< index) $ replicateA t.variablesNum [true, false]