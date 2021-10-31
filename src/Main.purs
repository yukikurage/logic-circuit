module Main where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import LogicWeb.Store (initialStore)
import LogicWeb.App (runApp)
import LogicWeb.Components.Body as Body
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  launchAff_ do
    HA.awaitLoad

    body <- runApp initialStore Body.component

    bodyElem <- HA.selectElement (QuerySelector "body")

    traverse_ (runUI body unit) bodyElem