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

data Output = Changed Int String | Focus (Maybe Int) | Add | Delete Int

data Query a = GetValues (Array String -> a) | SetValues (Array String) a | Focusing (Int -> a)

component :: forall m.
  MonadEffect m =>
  Component Query {messageHandler :: String -> String} Output m
component = Hooks.component \{outputToken, queryToken, slotToken} {messageHandler} -> Hooks.do
  inputs /\ inputsId <- Hooks.useState []
  focusing /\ focusingId <- Hooks.useState $ Nothing

  let
    getValues xs = fromMaybe [] <<< sequence <$> (for xs \id -> Hooks.request slotToken input_ id Input.GetValue)

    handleChangedInput i = case _ of
      Input.Changed o -> do
        Hooks.raise outputToken $ Changed i o

      Input.Delete -> do
        let
          newInputs = fromMaybe inputs $ deleteAt i inputs
        Hooks.put inputsId newInputs
        Hooks.raise outputToken $ Delete i

        Hooks.put focusingId Nothing
        Hooks.raise outputToken $ Focus Nothing

      Input.Focus -> do
        Hooks.put focusingId $ Just i
        Hooks.raise outputToken $ Focus $ Just i

    handleAddButton _ = do
      let
        newInputs = case (\m -> find (flip notElem inputs) $ upRange 0 $ m + 1) =<< maximum inputs of
          Just x -> inputs <> [x]
          Nothing -> [0]
      Hooks.put inputsId newInputs
      Hooks.raise outputToken $ Add

  Hooks.useQuery queryToken case _ of
    GetValues reply -> do
      res <- getValues inputs
      pure $ Just $ reply res
    SetValues strings a -> do
      Hooks.put inputsId $ upRange 0 (length strings - 1)
      forWithIndex_ strings \i str -> Hooks.tell slotToken input_ i $ Input.SetValue str
      pure $ Just a
    Focusing reply -> do
      pure $ reply <$> focusing

  Hooks.pure $
    HH.div [css "flex flex-col overflow-auto relative"] $
      ( flip mapWithIndex inputs \i id ->
        HH.slot input_ id Input.component {name: show i, messageHandler, focusing: focusing == Just i} $ handleChangedInput i
      )
      <>
      [ HH.div [css "h-12 w-16 m-3"]
        [ button [HH.i [css "fas fa-plus"] []] handleAddButton
        ]
      ]