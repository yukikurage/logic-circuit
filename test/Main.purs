module Test.Main where

import Prelude

import Data.Either (either)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class.Console (log)
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.Formula.Parser (parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)
import LogicWeb.PropositionalLogic.TruthTable (showTruthTable)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."

parseAndShowTruthTable :: String -> Effect Unit
parseAndShowTruthTable str =
  log $ either show (maybe "Nothing" identity) $ (showTruthTable <<< toTruthTable) <$> parse primEnv str