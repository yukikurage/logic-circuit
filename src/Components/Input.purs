module LogicWeb.Components.Input where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import LogicWeb.Components.Common (css)

data Output = Changed String | Delete | Focus

data Query a = GetValue (String -> a) | SetValue String a

component :: forall m. MonadEffect m => Component Query
  { messageHandler :: String -> String
  , name :: String
  , focusing :: Boolean
  , isReadOnly :: Boolean
  }
  Output m
component = Hooks.component \token {name, messageHandler, focusing, isReadOnly} -> Hooks.do
  errorMessage /\ errorMessageId <- Hooks.useState ""
  input /\ inputId <- Hooks.useState ""

  Hooks.useLifecycleEffect do
    Hooks.put errorMessageId $ messageHandler input
    pure $ Nothing

  Hooks.useQuery token.queryToken case _ of
    GetValue reply -> pure $ Just $ reply input
    SetValue str a -> do
      Hooks.put inputId str
      Hooks.put errorMessageId $ messageHandler str
      pure $ Just $ a

  Hooks.pure $ HH.div [css "flex flex-row items-start w-full border-b-2 border-yukiRed relative"] $
    [ HH.div
      [ css $ "duration-75 text-lg px-2 py rounded-br-lg border-yukiRed "
        <> if focusing then "bg-yukiRed text-white" else "bg-white text-yukiBlack"
      ]
      [ HH.text $ name ]
    , HH.input
      [ css "font-math text-3xl flex-grow p-4 tracking-widest outline-none w-full"
      , HP.readOnly isReadOnly
      , HP.value input

      , HE.onValueInput \s -> do
        Hooks.put inputId s
        Hooks.raise token.outputToken $ Changed $ s
        Hooks.put errorMessageId $ messageHandler s

      , HE.onFocusIn \_ -> do
        Hooks.put errorMessageId $ messageHandler input
        Hooks.raise token.outputToken $ Changed $ input
        Hooks.raise token.outputToken $ Focus
      ]
    ]
    <> if isReadOnly then [] else
    [ HH.div
      [ css $ "absolute right-0 top-0 bg-transparent rounded-bl-lg border-yukiRed b text-yukiRed hover:bg-yukiRed hover:text-white text-xl cursor-pointer px-2"
      , HE.onClick \_ -> Hooks.raise token.outputToken Delete
      ]
      [ HH.i [css "fas fa-times"][]]
    ]
    <>
    [ HH.div [css $ "absolute right-1 bottom-1 rounded-md text-base bg-yukiBlack text-white px-2 py-1 bg-opacity-50 "
        <> if errorMessage == "" then "invisible" else "visible"]
      [ HH.text errorMessage
      ]
    ]