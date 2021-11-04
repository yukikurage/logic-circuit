module LogicWeb.Type.RawTruthTable where

import Prelude

import Data.Array (findIndex, index, length)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Data.Unfoldable (replicateA)
import LogicWeb.Common (readBool, showBool)
import LogicWeb.PropositionalLogic.TruthTable (TruthTable(..))

type RawTruthTable =
  { name :: String
  , variables :: Array String
  , results :: Array Boolean
  }

toString :: RawTruthTable -> String
toString raw = raw.name <> ":" <> joinWith "," raw.variables <> ":" <> joinWith "," (map showBool raw.results)

fromString :: String -> Maybe RawTruthTable
fromString str = do
  let
    spl = split (Pattern ":") str
  name <- index spl 0
  vs <- index spl 1
  rs <- index spl 2
  let
    variables = split (Pattern ",") vs
  results <- traverse readBool $ split (Pattern ",") rs
  pure {variables, results, name}

emptyRawTruthTable :: RawTruthTable
emptyRawTruthTable =
  { name: "New Table"
  , variables: ["P"]
  , results: [true, false]
  }

fromTruthTable :: TruthTable -> Maybe RawTruthTable
fromTruthTable (TruthTable {variables, table}) = (\rs ->
  { name: emptyRawTruthTable.name
  , variables: variables
  , results: rs
  })
  <$>
  (traverse (\xs -> table (\v -> index xs =<< findIndex (_ == v) variables))
    $ replicateA (length variables) [false, true]
  )