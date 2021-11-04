module LogicWeb.Store where

import Prelude

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import LogicWeb.Type.RawFormula (RawFormula)
import LogicWeb.Type.RawTruthTable (RawTruthTable, fromString, toString)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

type Store = {formulas :: Array RawFormula, truthTables :: Array RawTruthTable}

initialStore :: Store
initialStore =
  {
  formulas: ["(¬P&P)=>Q"],
  truthTables: [{name: "Example", variables: ["P", "Q"], results: [true, false, false, true]}]
  }

data Action
  = SetFormulas (Array RawFormula)
  | SetTruthTables (Array RawTruthTable)

reduce :: Store -> Action -> Store
reduce store = case _ of
  SetFormulas x -> store {formulas = x}
  SetTruthTables x -> store {truthTables = x}

save :: forall m action. MonadEffect m => MonadStore action Store m => m Unit
save = do
  s <- liftEffect $ localStorage =<< window

  formulas <- _.formulas <$> getStore
  liftEffect $ setItem "formulas" (joinWith ";" formulas) s

  truthTables <- _.truthTables <$> getStore
  liftEffect $ setItem "truthTables" (joinWith ";" $ map toString truthTables) s

load :: forall m action. MonadEffect m => MonadStore Action action m => m Unit
load = do
  s <- liftEffect $ localStorage =<< window
  fs <- liftEffect $ getItem "formulas" s
  ts <- liftEffect $ getItem "truthTables" s
  let
    formulas = (\x -> if x == [""] then [] else x) $ fromMaybe initialStore.formulas $ split (Pattern ";") <$> fs
    truthTables = fromMaybe initialStore.truthTables $ traverse fromString =<< split (Pattern ";") <$> ts
  updateStore $ SetFormulas formulas
  updateStore $ SetTruthTables truthTables