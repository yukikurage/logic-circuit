module LogicWeb.Components.FormulaEditor where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import LogicWeb.Components.Common (css, makeText)
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.Formula.Parser (parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)
import LogicWeb.PropositionalLogic.TruthTable (showTruthTable)

component :: forall q i o m. Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  inputValue /\ inputValueId <- Hooks.useState ""
  truthTable /\ truthTableId <- Hooks.useState ""

  Hooks.pure $ HH.div [css "m-5"] $
    [ HH.input
      [ HP.value inputValue
      , HE.onValueInput \s -> do
        Hooks.put inputValueId s
        case (showTruthTable <<< toTruthTable) <$> parse primEnv s of
          Right (Just x) -> Hooks.put truthTableId x
          _ -> pure unit
      ]
    , makeText truthTable
    ]