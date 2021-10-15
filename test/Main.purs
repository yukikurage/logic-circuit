module Test.Main where

import Prelude

import Data.Either (fromRight)
import Effect (Effect)
import Effect.Class.Console (log)
import LogicWeb.PropositionalLogic (formulaToTruthTable)
import LogicWeb.PropositionalLogic.Formula.Parser (parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)
import LogicWeb.PropositionalLogic.TruthTable (showTruthTable)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."

parseAndShowTruthTable str = (showTruthTable <<< formulaToTruthTable) <$> parse primEnv str