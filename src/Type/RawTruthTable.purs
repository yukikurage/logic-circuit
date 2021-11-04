module LogicWeb.Type.RawTruthTable where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import LogicWeb.Common (readBool, showBool)

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