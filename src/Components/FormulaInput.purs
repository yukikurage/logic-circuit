module LogicWeb.Components.FormulaInput where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import LogicWeb.Components.Common (css)
import LogicWeb.PropositionalLogic.Formula (Formula)
import LogicWeb.PropositionalLogic.Formula.Parser (ParseError, parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)

data Output = Changed (Either ParseError Formula)

component :: forall q m. Component q Int Output m
component = Hooks.component \token num -> Hooks.do
  inputValue /\ inputValueId <- Hooks.useState ""
  errorMessage /\ errorMessageId <- Hooks.useState ""

  Hooks.pure $ HH.div [css "flex flex-row w-full border-b-2 border-yukiRed relative"]
    [ HH.div [css "bg-yukiRed text-white w-5"] [HH.text $ show num]
    , HH.input
      [ css "font-math text-3xl flex-grow p-4 tracking-wider outline-none"
      , HP.value inputValue
      , HE.onValueInput \s -> do
        Hooks.put inputValueId s
        let
          result = parse primEnv s
        Hooks.raise token.outputToken $ Changed $ result
        case result of
          Right _ -> Hooks.put errorMessageId ""
          Left m -> Hooks.put errorMessageId $ show m
      ]
    , HH.div [css "absolute right-0 bottom-0 text-base bg-yukiRed text-white"]
      [ HH.text errorMessage

      ]
    ]