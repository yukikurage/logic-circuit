module LogicWeb.Components.Pages.TruthTableEditor where

import Prelude

import Control.Alt ((<|>))
import Data.Array (deleteAt, index, modifyAt, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Halogen (Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Store.UseSelector (useSelector)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.HTML.RootButton (button)
import LogicWeb.Components.InputsList as InputsList
import LogicWeb.Components.TruthTableEditPanel as TruthTableEditPanel
import LogicWeb.PropositionalLogic.TruthTable (TruthTable(..), emptyTruthTable)
import LogicWeb.Store (Action(..), load, save)
import LogicWeb.Store as Store
import LogicWeb.Type.RawTruthTable (emptyRawTruthTable)
import Type.Proxy (Proxy(..))

inputsList_ = Proxy :: Proxy "inputsList"
truthTableEditPanel_ = Proxy :: Proxy "truthTableEditPanel"

divButton :: forall t2 t3. Boolean -> HTML t2 t3
divButton frag = HH.div [css "w-full h-full flex justify-center items-center cursor-pointer"]
  [ HH.div [css $ "h-5 w-5 rounded-md " <> if frag then "bg-yukiRed" else "bg-transparent"][]
  ]

component :: forall q i o m.
  MonadStore Store.Action Store.Store m
  => Component q i o m
component = Hooks.component \{slotToken} _ -> Hooks.do
  storeSelector <- useSelector $ selectEq _.truthTables
  rawTruthTables /\ rawTruthTablesId <- Hooks.useState []
  selectedTable /\ selectedTableId <- Hooks.useState 0

  Hooks.captures {storeSelector} Hooks.useTickEffect do
    res <- (_.truthTables) <$> getStore
    Hooks.put rawTruthTablesId res
    Hooks.tell slotToken inputsList_ unit $ InputsList.SetValues $ map (\{name} -> name) res
    pure $ Nothing

  Hooks.useLifecycleEffect do
    load
    res <- (_.truthTables) <$> getStore
    Hooks.put rawTruthTablesId res
    Hooks.tell slotToken inputsList_ unit $ InputsList.SetValues $ map (\{name} -> name) res
    pure $ Just save

  let
    messageHandler = const ""

    handleInputsOutput o = do
      case o of
        InputsList.Focus i -> Hooks.put selectedTableId $ i
        InputsList.Delete i -> do
          updateStore $ SetTruthTables $ fromMaybe rawTruthTables $ deleteAt i rawTruthTables
        InputsList.Add -> do
          updateStore $ SetTruthTables $ snoc rawTruthTables emptyRawTruthTable
        InputsList.Changed i n -> do
          updateStore $ SetTruthTables $ fromMaybe rawTruthTables $ (modifyAt i (\t -> t{name = n}) rawTruthTables)

      save

    handleTableEditOutput o = do
      case o of
        TruthTableEditPanel.Change t -> do
          updateStore $ SetTruthTables $ fromMaybe rawTruthTables $ (updateAt selectedTable t rawTruthTables)

      save

  Hooks.pure $ HH.div [css "h-full flex flex-row relative animate-fade-in-quick"] $
    [ HH.div [css "h-full bg-white shadow-md relative w-[480px]"] $
      [ HH.slot inputsList_ unit InputsList.component {messageHandler} handleInputsOutput
      ]
    , HH.div [css "flex-grow"] $ case flip index selectedTable rawTruthTables of
        Just t -> [ HH.slot truthTableEditPanel_ selectedTable TruthTableEditPanel.component t handleTableEditOutput]
        Nothing -> []
    ]