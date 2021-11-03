module LogicWeb.Type.RawTruthTable where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import LogicWeb.Common (readBool, showBool)

type RawTruthTable =
  { variables :: Array String
  , results :: Array Boolean
  }

toString :: RawTruthTable -> String
toString raw = joinWith "," raw.variables <> ":" <> joinWith "," (map showBool raw.results)

fromString :: String -> Maybe RawTruthTable
fromString str = do
  let
    spl = split (Pattern ":") str
  vs <- index spl 0
  rs <- index spl 1
  let
    variables = split (Pattern ",") vs
  results <- traverse readBool $ split (Pattern ",") rs
  pure {variables, results}