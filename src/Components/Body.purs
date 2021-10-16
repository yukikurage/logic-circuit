module LogicWeb.Components.Body where

import Prelude

import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import LogicWeb.Components.FormulaEditor as FormulaEditor
import Type.Proxy (Proxy(..))

formulaEditor_ = Proxy :: Proxy "formulaEditor"

component :: forall q i o m. Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  Hooks.pure $ HH.slot_ formulaEditor_ unit FormulaEditor.component unit