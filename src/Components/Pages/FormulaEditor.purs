module LogicWeb.Components.Pages.FormulaEditor where

import Prelude

import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component, lift)
import Halogen.HTML as HH
import Halogen.Hooks (useLifecycleEffect)
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Store.UseSelector (useSelector)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.HTML.TruthTable (displayTruthTable)
import LogicWeb.Components.InputsList as InputsList
import LogicWeb.PropositionalLogic (toTruthTable)
import LogicWeb.PropositionalLogic.Formula.Parser (BadRequestValue(..), ParseError(..), parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)
import LogicWeb.PropositionalLogic.TruthTable (emptyTruthTable)
import LogicWeb.Store (Action(..), load, save)
import LogicWeb.Store as Store
import Type.Proxy (Proxy(..))

inputsList_ = Proxy :: Proxy "inputsList_"

component :: forall q i o m.
  MonadStore Store.Action Store.Store m
  => MonadEffect m
  => Component q i o m
component = Hooks.component \token _ -> Hooks.do
  truthTable /\ truthTableId <- Hooks.useState emptyTruthTable
  formulas <- useSelector $ selectEq _.formulas

  let
    messageHandler s = case parse primEnv s of
      Right _ -> ""
      Left (BadRequest Inconsistencies) -> "入力に不整合があります"
      Left (InternalError str) -> "内部エラー: " <> str
      Left (BadRequest (DuplicatedToken str)) -> "入力に不整合があります: "<> str
      Left (BadRequest (NoSuchToken str)) -> "トークンが見つかりません: "<> str
      Left (BadRequest EmptyRequest) -> ""
    handleChangedInputs _ = do
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
      saveData
      save
    saveData = do
      xs <- Hooks.request token.slotToken inputsList_ unit InputsList.GetValues
      lift $ updateStore $ SetFormulas $ fromMaybe [] xs
    loadData = do
      xs <- _.formulas <$> getStore
      Hooks.tell token.slotToken inputsList_ unit $ InputsList.SetValues xs

  useLifecycleEffect do
    load
    loadData
    pure $ Just $ save

  Hooks.captures {formulas} Hooks.useTickEffect do
    loadData
    pure Nothing

  Hooks.pure $ HH.div [css "flex flex-row h-full relative animate-fade-in-quick"] $
    [ HH.div [css "duration-75 flex-grow h-full bg-white shadow-md relative"] $
      [ HH.slot inputsList_ unit InputsList.component {messageHandler, isReadOnly : false} handleChangedInputs
      ]
    , HH.div [css "overflow-auto w-auto font-meiryo text-lg flex-col flex items-center p-3"] $ [displayTruthTable truthTable]
    ]