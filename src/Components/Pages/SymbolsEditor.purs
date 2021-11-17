module LogicWeb.Components.Pages.SymbolsEditor(component) where

import Prelude

import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.HTML.TruthTable (displayTruthTable)
import LogicWeb.Components.InputsList as InputsList
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.Formula.Parser (BadRequestValue(..), ParseError(..), parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)
import LogicWeb.PropositionalLogic.TruthTable (emptyTruthTable)
import LogicWeb.Store as Store
import Type.Proxy (Proxy(..))

inputsList_ = Proxy :: Proxy "inputsList_"

component :: forall q i o m.
  Monad m =>
  MonadStore Store.Action Store.Store m =>
  Component q i o m
component = Hooks.component \token _ -> Hooks.do
  truthTable /\ truthTableId <- Hooks.useState emptyTruthTable

  let
    operators = map (\x -> x.symbol <> "P") primEnv.monadicOperators <> map (\x -> "P" <> x.symbol <> "Q") primEnv.binaryOperators
    messageHandler _ = ""
    handleInputsOutput _ = do
      xs <- Hooks.request token.slotToken inputsList_ unit InputsList.GetValues
      i <- Hooks.request token.slotToken inputsList_ unit InputsList.Focusing
      let
        s = fromMaybe "" do
          xs' <- xs
          i' <- i
          index xs' i'
      case parse primEnv s of
        Right f -> do Hooks.put truthTableId $ toTruthTable f
        Left (BadRequest EmptyRequest) -> Hooks.put truthTableId $ emptyTruthTable
        _ -> Hooks.put truthTableId $ emptyTruthTable

  Hooks.useLifecycleEffect do
    Hooks.tell token.slotToken inputsList_ unit $ InputsList.SetValues operators
    pure Nothing

  Hooks.pure $ HH.div [css "flex flex-row h-full relative animate-fade-in-quick"] $
    [ HH.div [css "duration-75 flex-grow h-full bg-white shadow-md relative"] $
      [ HH.slot inputsList_ unit InputsList.component {messageHandler, isReadOnly : true} handleInputsOutput
      ]
    , HH.div [css "overflow-auto w-auto font-meiryo text-lg flex-col flex items-center p-3"] $ [displayTruthTable truthTable]
    ]
