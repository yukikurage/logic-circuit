module Test.Main where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class.Console (log)
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.Formula.Parser (ParseError, parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)
import LogicWeb.PropositionalLogic.TruthTable (showTruthTable)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."

parseAndShowTruthTable :: String -> Either ParseError (Maybe String)
parseAndShowTruthTable str = (showTruthTable <<< toTruthTable) <$> parse primEnv str