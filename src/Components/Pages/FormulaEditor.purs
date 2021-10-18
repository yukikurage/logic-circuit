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
    [ HH.div [css "flex flex-col flex-grow overflow-auto shadow-lg relative z-30"]
      $ flip mapWithIndex formulas \i id -> HH.slot formulaInput_ id FormulaInput.component (show i) (handleChangedFormula id)
    , HH.div [css "overflow-auto w-auto font-meiryo text-lg bg-yukiYellow flex-col flex items-center p-10"] $ [displayTruthTable truthTable]
    , HH.div
      [ css "z-40 absolute left-10 bottom-10 h-14 w-14 border-2 border-yukiRed border-b-yu bg-white text-yukiRed hover:bg-yukiRed hover:text-white flex justify-center items-center text-2xl rounded-lg cursor-pointer"
      , HE.onClick \_ -> case (\m -> find (flip notElem formulas) $ range 0 $ m + 1) =<< maximum formulas of
        Just x -> Hooks.put formulasId $ (formulas <> [x])
        Nothing -> Hooks.put formulasId $ [0]
      ]
      [ HH.i [css "fas fa-plus"] []
      ]
    ]