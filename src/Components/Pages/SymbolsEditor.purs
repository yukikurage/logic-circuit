module LogicWeb.Components.Pages.SymbolsEditor(component) where

import Prelude

import Data.Functor (mapFlipped)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.HTML.TruthTable (displayTruthTable)
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.Formula (BinaryOperator, Formula(..), MonadicOperator)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)
import LogicWeb.Store as Store

component :: forall q i o m.
  Monad m =>
  MonadStore Store.Action Store.Store m =>
  Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  Hooks.pure $ HH.div [css "max-w-full items-start flex flex-row flex-wrap"]
    $ (mapFlipped primEnv.binaryOperators $ binaryOperatorPanel) <>
      (mapFlipped primEnv.monadicOperators $ monadicOperatorPanel)

symbolPanel :: forall w i. HH.HTML w i -> HH.HTML w i
symbolPanel x = HH.div [css "h-auto w-auto m-3"] [x]

monadicOperatorPanel :: forall w m.
  Monad m =>
  MonadStore Store.Action Store.Store m =>
  MonadicOperator -> HH.HTML w (Hooks.HookM m Unit)
monadicOperatorPanel op = symbolPanel $ HH.div [css "text-center"] [
  HH.div [css "font-math text-3xl w-full h-auto"] [HH.text $ op.symbol <> "P"],
  displayTruthTable $ toTruthTable $ MonadicOperate op $ Var "P"
]

binaryOperatorPanel :: forall w m.
  Monad m =>
  MonadStore Store.Action Store.Store m =>
  BinaryOperator -> HH.HTML w (Hooks.HookM m Unit)
binaryOperatorPanel op = symbolPanel $ HH.div [css "text-center"] [
  HH.div [css "font-math text-3xl w-full h-auto"] [HH.text $ "P" <> op.symbol <> "Q"],
  displayTruthTable $ toTruthTable $ BinaryOperate op (Var "P")$ Var "Q"
]