module LogicWeb.Components.Pages.FormulaEditor where

import Prelude

import Data.Array (catMaybes, delete, find, length, mapWithIndex, notElem)
import Data.Either (Either(..))
import Data.Enum (enumFromThenTo)
import Data.Foldable (maximum)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Class (class MonadEffect)
import Halogen (Component, lift)
import Halogen.HTML as HH
import Halogen.Hooks (useLifecycleEffect)
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Store.UseSelector (useSelector)
import LogicWeb.Class.ContentHandler (class ContentHandler)
import LogicWeb.Common (upRange)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.FormulaInput (Output(..), Query(..))
import LogicWeb.Components.FormulaInput as FormulaInput
import LogicWeb.Components.HTML.RootButton (button)
import LogicWeb.Components.HTML.TruthTable (displayTruthTable)
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.Formula.Parser (BadRequestValue(..), ParseError(..))
import LogicWeb.PropositionalLogic.TruthTable (emptyTruthTable)
import LogicWeb.Store (Action(..))
import LogicWeb.Store as Store
import Type.Proxy (Proxy(..))

formulaInput_ = Proxy :: Proxy "formulaInput"

component :: forall q i o m.
  ContentHandler m
  => MonadStore Store.Action Store.Store m
  => MonadEffect m
  => Component q i o m
component = Hooks.component \token _ -> Hooks.do
  truthTable /\ truthTableId <- Hooks.useState emptyTruthTable
  formulaInputs /\ formulaInputsId <- Hooks.useState []
  formulas <- useSelector $ selectEq _.formulas

  let
    handleChangedFormula id = case _ of
      FormulaInput.Changed o -> do
        case o of
          Right f -> do Hooks.put truthTableId $ toTruthTable f
          Left (BadRequest EmptyRequest) -> Hooks.put truthTableId $ emptyTruthTable
          _ -> Hooks.put truthTableId $ emptyTruthTable
        saveData
      Delete -> do
        Hooks.put formulaInputsId $ delete id formulaInputs
        saveData
    saveData = do
      xs <- for formulaInputs \id -> Hooks.request token.slotToken formulaInput_ id GetValue
      lift $ updateStore $ SetFormulas $ catMaybes xs
    loadData = do
      xs <- _.formulas <$> getStore
      Hooks.put formulaInputsId $ upRange 0 $ length xs - 1
      forWithIndex_ xs \i str -> Hooks.tell token.slotToken formulaInput_ i $ SetValue str

  useLifecycleEffect do
    loadData
    pure Nothing

  Hooks.captures {formulas} Hooks.useTickEffect do
    loadData
    pure Nothing

  Hooks.pure $ HH.div [css "flex flex-row h-full relative"] $
    [ HH.div [css "flex flex-col flex-grow overflow-auto shadow-md relative z-30"] $
      [ HH.div [css "h-12 w-16 m-6"]
        [ button [HH.i [css "fas fa-plus"] []] \_ -> case (\m -> find (flip notElem formulaInputs) $ upRange 0 $ m + 1) =<< maximum formulaInputs of
          Just x -> do
            loadData
            Hooks.put formulaInputsId $ (formulaInputs <> [x])
          Nothing -> Hooks.put formulaInputsId $ [0]
        ]
      ]
      <>
      flip mapWithIndex (spy "a" formulaInputs) \i id -> HH.slot formulaInput_ id FormulaInput.component {name: (show i)} (handleChangedFormula id)
    , HH.div [css "overflow-auto w-auto font-meiryo text-lg bg-yukiYellow flex-col flex items-center p-10"] $ [displayTruthTable truthTable]
    ]