module LogicWeb.App where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, emitSelected, getStore, runStoreT, updateStore)
import LogicWeb.Store as Store
import Safe.Coerce (coerce)

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