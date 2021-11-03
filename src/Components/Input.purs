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

data Output = Changed String | Delete

data Query a = GetValue (String -> a) | SetValue String a

component :: forall m. MonadEffect m => Component Query
  { messageHandler :: String -> String
  , name :: String
  }
  Output m
component = Hooks.component \token {name, messageHandler} -> Hooks.do
  errorMessage /\ errorMessageId <- Hooks.useState ""
  input /\ inputId <- Hooks.useState ""
  isFocus /\ isFocusId <- Hooks.useState false

  Hooks.useQuery token.queryToken case _ of
    GetValue reply -> pure $ Just $ reply input
    SetValue str a -> do
      Hooks.put inputId str
      pure $ Just $ a

  Hooks.pure $ HH.div [css "flex flex-row items-start w-full border-b-2 border-yukiRed relative"]
    [ HH.div
      [ css $ "text-lg px-2 py rounded-br-lg border-yukiRed "
        <> if isFocus then "bg-yukiRed text-white" else "bg-white text-yukiBlack"
      ]
      [ HH.text $ name ]
    , HH.input
      [ css "font-math text-3xl flex-grow p-4 tracking-widest outline-none w-full"
      , HP.value input
      , HE.onValueInput \s -> do
        Hooks.put inputId s
        Hooks.raise token.outputToken $ Changed $ s
        Hooks.put errorMessageId $ messageHandler s
      , HE.onFocusIn \_ -> do
        Hooks.put isFocusId true
        Hooks.raise token.outputToken $ Changed $ input
      , HE.onFocusOut \_ -> Hooks.put isFocusId false
      ]
    , HH.div
      [ css $ "absolute right-0 top-0 bg-transparent rounded-bl-lg border-yukiRed b text-yukiRed hover:bg-yukiRed hover:text-white text-xl cursor-pointer px-2"
      , HE.onClick \_ -> Hooks.raise token.outputToken Delete
      ]
      [ HH.i [css "fas fa-times"][]]
    , HH.div [css $ "absolute right-1 bottom-1 rounded-md text-base bg-yukiBlack text-white px-2 py-1 bg-opacity-50 "
        <> if errorMessage == "" then "invisible" else "visible"]
      [ HH.text errorMessage
      ]
    ]