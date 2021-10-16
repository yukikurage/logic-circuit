module LogicWeb.PropositionalLogic.TruthTable where

import Prelude

import Data.Array (findIndex, index, length, replicate)
import Data.Maybe (Maybe)
import Data.String (codePointFromChar, joinWith)
import Data.String as String
import Data.Traversable (traverse)
import Data.Unfoldable (replicateA)

type TruthTable =
  { variables :: Array String
  , output :: String
  , table :: (String -> Maybe Boolean) -> Maybe Boolean
  }

showTruthTable :: TruthTable -> Maybe String
showTruthTable t =
  (\c -> header
  <> "\n"
  <> line
  <> "\n"
  <> c) <$> content
  where
  header = joinWith " | " t.variables <> " || " <> t.output
  line = joinWith "-|-" (replicate (length t.variables) "-") <> "-||-"
    <> String.fromCodePointArray (replicate (String.length $ t.output) $ codePointFromChar '-') <> "-"
  showBoolean true = "1"
  showBoolean false = "0"
  showContentLine xs =
    (\result -> joinWith " | " (map showBoolean xs) <> " || " <> result)
    <$> showBoolean
    <$> t.table (\v -> index xs =<< findIndex (_ == v) t.variables)
  content = joinWith "\n"
    <$> (traverse showContentLine $ replicateA (length t.variables) [false, true])