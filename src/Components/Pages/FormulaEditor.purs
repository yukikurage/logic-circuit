module LogicWeb.Components.Pages.FormulaEditor where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import LogicWeb.Components.Common (css, makeText)
import LogicWeb.Components.FormulaInput as FormulaInput
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.TruthTable (showTruthTable)
import Type.Proxy (Proxy(..))

formulaInput_ = Proxy :: Proxy "formulaInput"

component :: forall q i o m. Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  truthTable /\ truthTableId <- Hooks.useState ""

  let
    handleChangedFormula = case _ of
      FormulaInput.Changed o -> case o of
        Right f -> case showTruthTable (toTruthTable f) of
          Just x -> Hooks.put truthTableId x
          _ -> pure unit
        _ -> pure unit

  Hooks.pure $ HH.div [css "flex flex-row h-full"] $
    [ HH.div [css "flex flex-col w-2/3 overflow-auto"]
      [ HH.slot formulaInput_ unit FormulaInput.component 1 handleChangedFormula
      ]
    , HH.div [css "w-1/3 font-meiryo text-lg bg-yukiYellow"] $ makeText truthTable
    ]