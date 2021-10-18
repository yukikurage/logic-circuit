module LogicWeb.Components.Body where

import Prelude

import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import LogicWeb.Components.Common (css)
import LogicWeb.Components.Pages.FormulaEditor as FormulaEditor
import Type.Proxy (Proxy(..))

formulaEditor_ = Proxy :: Proxy "formulaEditor"

component :: forall q i o m. Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  Hooks.pure $ HH.div [css "h-screen flex flex-row text-yukiBlack"]
    [ HH.div [css "h-full w-16 bg-yukiBlack"] []
    , HH.div [css "h-full flex-grow"]
      [ HH.slot_ formulaEditor_ unit FormulaEditor.component unit
      ]
    ]