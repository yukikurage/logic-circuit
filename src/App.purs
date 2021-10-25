module LogicWeb.App where

import Prelude

import Data.Array (head, last)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen as H
import LogicWeb.Class.ContentHandler (class ContentHandler, getFormulaIds)
import Safe.Coerce (coerce)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype App a
  = Cons (Aff a)

derive newtype instance Functor App
derive newtype instance Apply App
derive newtype instance Applicative App
derive newtype instance Bind App
derive newtype instance Monad App
derive newtype instance MonadEffect App
derive newtype instance MonadAff App

runApp :: forall q i o m. Monad m => H.Component q i o App -> Aff (H.Component q i o m)
runApp = pure <<< coerce

instance ContentHandler App where
  save = pure unit
  load = pure unit
  getFormulas = liftEffect do
    w <- window
    s <- localStorage w
    res <- getItem "formulas" s
    case res of
      Just x -> pure $ split (Pattern ";") x
      Nothing -> do
        log "Formulas: データが見つかりません"
        pure []
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
          }
  getFormulaIds = liftEffect do
    w <- window
    s <- localStorage w
    res <- getItem ("formula_ids") s
    case traverse (fromString) <<< split (Pattern ",") =<< res of
      Just x -> pure x
      Nothing -> pure []
  setFormulaIds xs = liftEffect do
    w <- window
    s <- localStorage w
    setItem ("formula_ids") (joinWith "," $ map show xs) s
  setFormulas xs = liftEffect do
    w <- window
    s <- localStorage w
    setItem "formulas" (joinWith ";" xs) s
  setTruthTables id t = liftEffect do
    w <- window
    s <- localStorage w
    let
      booleanToString = case _ of
        true -> "1"
        false -> "0"
    setItem ("truth_table_ " <> show id)
      (joinWith "," t.variables <> "|" <> joinWith "," (map booleanToString t.results))
      s