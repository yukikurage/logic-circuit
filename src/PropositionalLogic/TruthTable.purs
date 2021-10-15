module LogicWeb.PropositionalLogic.TruthTable where

import Prelude

import Data.Array (all, index, range, replicate)
import Data.Maybe (Maybe, fromMaybe, isJust, maybe)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Data.Unfoldable (replicateA)

type TruthTable =
  { variablesNum :: Int
  , variableSymbols :: Int -> Maybe String
  , table :: (Int -> Maybe Boolean) -> Maybe Boolean
  }

showTruthTable :: TruthTable -> String
showTruthTable t = header <> "\n" <> line <> "\n" <> content
  where
  header = (fromMaybe "" $ joinWith " | " <$> (sequence $ map t.variableSymbols $ range 0 (t.variablesNum - 1))) <> " ||"
  line = joinWith "-|-" (replicate t.variablesNum "-") <> "-||---"
  showBoolean x = if x then "1" else "0"
  showMaybeBoolean = maybe "*" showBoolean
  showLine :: Array Boolean -> String
  showLine xs = joinWith " | " (map showBoolean xs) <> " || " <> showMaybeBoolean (t.table $ index xs)
  content = joinWith "\n" $ map showLine $ replicateA t.variablesNum [true, false]

isValid :: TruthTable -> Boolean
isValid t = all (isJust <<< t.table <<< index) $ replicateA t.variablesNum [true, false]