module LogicWeb.Components.Body where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.Pages.FormulaEditor as FormulaEditor
import LogicWeb.Components.Pages.TruthTableEditor as TruthTableEditor
import LogicWeb.Store as Store
import LogicWeb.Type.Page (Page(..))
import Type.Proxy (Proxy(..))
import LogicWeb.Components.Pages.SymbolsEditor as SymbolsEditor

formulaEditor_ = Proxy :: Proxy "formulaEditor"
truthTableEditor_ = Proxy :: Proxy "truthTableEditor"
symbolsEditor_ = Proxy :: Proxy "symbolsEditor"

component :: forall q i o m. MonadEffect m
  => MonadStore Store.Action Store.Store m
  => Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  page /\ pageId <- Hooks.useState FormulaEditor
  let
    makeMenu iconFont p =
      HH.div
        [ css $ "hover:opacity-70 cursor-pointer text-3xl w-14 h-16 flex items-center justify-center "
            <> if p == page then "hover:opacity-100" else "opacity-50"
        , HE.onClick \_ -> Hooks.put pageId p
        ]
        [ HH.i [css iconFont][]
        ]

  Hooks.pure $ HH.div [css "h-auto flex flex-row text-yukiBlack"]
    [ HH.div [css "min-h-screen w-16 relative"] []
    , HH.div [css "w-16 bg-yukiBlack text-white flex flex-col items-center fixed left-0 top-0 bottom-0"]
      [ makeMenu "fas fa-align-left" FormulaEditor
      , makeMenu "fas fa-table" TruthTableEditor
      , makeMenu "fas fa-arrows-alt-h" SymbolsEditor
      , HH.div [css "font-math absolute bottom-11 transform -rotate-90 text-lg cursor-pointer hover:opacity-70 opacity-50"]
        [ HH.text "Yukiworks"
        , HH.a [css "absolute bottom-0 top-0 right-0 left-0", HP.href " https://yukikurage.github.io/portfolio/"] []
        ]
      ]
    , HH.div [css "min-h-screen h-auto w-full flex-col flex-grow flex"]
      [ HH.div [css "h-auto bg-yukiRed text-white p-2 font-meiryo"] [
        HH.text $ case page of
          FormulaEditor -> "論理式エディタ"
          TruthTableEditor -> "真理値表エディタ"
          SymbolsEditor -> "演算子一覧"
      ]
      , HH.div [css "h-auto flex-grow bg-yukiYellow"]
        [ case page of
            FormulaEditor -> HH.slot_ formulaEditor_ unit FormulaEditor.component unit
            TruthTableEditor -> HH.slot_ truthTableEditor_ unit TruthTableEditor.component unit
            SymbolsEditor -> HH.slot_ symbolsEditor_ unit SymbolsEditor.component unit
        ]
      ]
    ]