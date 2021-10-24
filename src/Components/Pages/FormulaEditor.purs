module LogicWeb.Components.Pages.FormulaEditor where

import Prelude

import Data.Array (delete, find, mapWithIndex, notElem, range)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import LogicWeb.Components.Common (css)
import LogicWeb.Components.FormulaInput (Output(..))
import LogicWeb.Components.FormulaInput as FormulaInput
import LogicWeb.Components.HTML.RootButton (button)
import LogicWeb.Components.HTML.TruthTable (displayTruthTable)
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.Formula.Parser (BadRequestValue(..), ParseError(..))
import LogicWeb.PropositionalLogic.TruthTable (emptyTruthTable)
import Type.Proxy (Proxy(..))

formulaInput_ = Proxy :: Proxy "formulaInput"

component :: forall q i o m. Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  truthTable /\ truthTableId <- Hooks.useState emptyTruthTable
  formulas /\ formulasId <- Hooks.useState [0]

  let
    handleChangedFormula id = case _ of
      FormulaInput.Changed o -> case o of
        Right f -> Hooks.put truthTableId $ toTruthTable f
        Left (BadRequest EmptyRequest) -> Hooks.put truthTableId $ emptyTruthTable
        _ -> Hooks.put truthTableId $ emptyTruthTable
      Delete -> Hooks.put formulasId $ delete id formulas

  Hooks.pure $ HH.div [css "flex flex-row h-full relative"] $
    [ HH.div [css "flex flex-col flex-grow overflow-auto shadow-md relative z-30"] $
      [ HH.div [css "h-12 w-16 m-6"]
        [ button [HH.i [css "fas fa-plus"] []] \_ -> case (\m -> find (flip notElem formulas) $ range 0 $ m + 1) =<< maximum formulas of
            Just x -> Hooks.put formulasId $ (formulas <> [x])
            Nothing -> Hooks.put formulasId $ [0]
        ]
      ]
      <>
      flip mapWithIndex formulas \i id -> HH.slot formulaInput_ id FormulaInput.component (show i) (handleChangedFormula id)
    , HH.div [css "overflow-auto w-auto font-meiryo text-lg bg-yukiYellow flex-col flex items-center p-10"] $ [displayTruthTable truthTable]
    ]