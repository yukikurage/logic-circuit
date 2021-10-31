module LogicWeb.App where

import Prelude

import Data.Array (head, last)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, emitSelected, getStore, runStoreT, updateStore)
import LogicWeb.Class.ContentHandler (class ContentHandler)
import LogicWeb.Store as Store
import Safe.Coerce (coerce)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype App a
  = Cons (StoreT Store.Action Store.Store Aff a)

derive newtype instance Functor App
derive newtype instance Apply App
derive newtype instance Applicative App
derive newtype instance Bind App
derive newtype instance Monad App
derive newtype instance MonadEffect App
derive newtype instance MonadAff App

runApp :: forall q i o m. Monad m => Store.Store ->  H.Component q i o App -> Aff (H.Component q i o m)
runApp store = runStoreT store Store.reduce <<< coerce

instance MonadStore Store.Action Store.Store App where
  getStore = Cons getStore
  updateStore x = Cons $ updateStore x
  emitSelected x = Cons $ emitSelected x

instance ContentHandler App where
  save = pure unit
  load = pure unit
  {-
  getTruthTables id = liftEffect do
    w <- window
    s <- localStorage w
    res <- getItem ("truth_table_" <> show id) s
    let
      stringToBoolean = case _ of
        "1" -> true
        "0" -> false
        _ -> false
    case res of
      Just x -> pure $ (\uni ->
        { variables: split (Pattern ",") $ fromMaybe "" $ head $ split (Pattern "|") uni
        , results: map stringToBoolean $ split (Pattern ",") $ fromMaybe "" $ last $ split (Pattern "|") uni
        }) x
      Nothing -> do
        log "Truth Table: データが見つかりません"
        pure
          { variables: []
          , results: [false]
          }-}
  {-
  setFormulas xs = liftEffect do
    w <- window
    s <- localStorage w
    setItem "formulas" (joinWith ";" xs) s-}