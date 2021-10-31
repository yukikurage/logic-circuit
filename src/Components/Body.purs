module LogicWeb.Components.Body where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore)
import LogicWeb.Class.ContentHandler (class ContentHandler)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.Pages.FormulaEditor as FormulaEditor
import LogicWeb.Components.Pages.TruthTableEditor as TruthTableEditor
import LogicWeb.PropositionalLogic.TruthTable (TruthTable(..))
import LogicWeb.Type.Page (Page(..))
import Type.Proxy (Proxy(..))
import LogicWeb.Store as Store

formulaEditor_ = Proxy :: Proxy "formulaEditor"
truthTableEditor_ = Proxy :: Proxy "truthTableEditor"

component :: forall q i o m. MonadEffect m
  => MonadStore Store.Action Store.Store m
  => ContentHandler m
  => Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  page /\ pageId <- Hooks.useState FormulaEditor

  let
    makeMenu iconFont p =
      HH.div
        [ css $ "cursor-pointer text-3xl w-14 h-16 flex items-center justify-center "
            <> if p == page then "" else "opacity-50"
        , HE.onClick \_ -> Hooks.put pageId p
        ]
        [ HH.i [css iconFont][]
        ]

  Hooks.pure $ HH.div [css "h-auto flex flex-row text-yukiBlack"]
    [ HH.div [css "min-h-screen w-16 bg-yukiBlack text-white flex flex-col items-center"]
      [ makeMenu "fas fa-align-left" FormulaEditor
      , makeMenu "fas fa-table" TruthTableEditor
      ]
    , HH.div [css "min-h-screen flex-grow"]
      [ case page of
          FormulaEditor -> HH.slot_ formulaEditor_ unit FormulaEditor.component unit
          TruthTableEditor -> HH.slot_ truthTableEditor_ unit TruthTableEditor.component $ TruthTable
            { variables: ["α"]
            , output: "output"
            , table: \f -> case f "0" of
              Just _ -> Just true
              _ -> Nothing
            }
      ]
    ]