module LogicWeb.Components.InputsList where

import Prelude

import Data.Array (deleteAt, find, length, mapWithIndex, notElem)
import Data.Foldable (maximum)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, sequence)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import LogicWeb.Common (upRange)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.HTML.RootButton (button)
import LogicWeb.Components.Input as Input
import Type.Proxy (Proxy(..))

input_ = Proxy :: Proxy "input"

data Output = Changed Int String | Focus Int | Delete Int | Add

data Query a = GetValues (Array String -> a) | SetValues (Array String) a

component :: forall m.
  MonadEffect m =>
  Component Query {messageHandler :: String -> String} Output m
component = Hooks.component \{outputToken, queryToken, slotToken} {messageHandler} -> Hooks.do
  inputs /\ inputsId <- Hooks.useState []

  Hooks.useQuery queryToken case _ of
    GetValues reply -> do
      res <- sequence <$> (for inputs \id -> Hooks.request slotToken input_ id Input.GetValue)
      pure $ reply <$> res
    SetValues strings a -> do
      Hooks.put inputsId $ upRange 0 (length strings - 1)
      forWithIndex_ strings \i str -> Hooks.tell slotToken input_ i $ Input.SetValue str
      pure $ Just $ a

  let
    handleChangedInput i = case _ of
      Input.Changed o -> Hooks.raise outputToken $ Changed i o
      Input.Delete -> do
        Hooks.put inputsId $ fromMaybe inputs $ deleteAt i inputs
        Hooks.raise outputToken $ Delete i
      Input.Focus -> Hooks.raise outputToken $ Focus i

  Hooks.pure $
    HH.div [css "flex flex-col overflow-auto relative"] $
      ( flip mapWithIndex inputs \i id ->
        HH.slot input_ id Input.component {name: show i, messageHandler} $ handleChangedInput i
      )
      <>
      [ HH.div [css "h-12 w-16 m-3"]
        [ button [HH.i [css "fas fa-plus"] []] \_ -> do
          Hooks.put inputsId $
            case (\m -> find (flip notElem inputs) $ upRange 0 $ m + 1) =<< maximum inputs of
              Just x -> inputs <> [x]
              Nothing -> [0]
          Hooks.raise outputToken $ Add
        ]
      ]
