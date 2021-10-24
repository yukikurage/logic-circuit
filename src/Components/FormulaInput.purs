module LogicWeb.Components.FormulaInput where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import LogicWeb.Components.Common (css)
import LogicWeb.PropositionalLogic.Formula (Formula)
import LogicWeb.PropositionalLogic.Formula.Parser (BadRequestValue(..), ParseError(..), parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)

data Output = Changed (Either ParseError Formula) | Delete

component :: forall q m. Component q String Output m
component = Hooks.component \token initName -> Hooks.do
  inputValue /\ inputValueId <- Hooks.useState ""
  errorMessage /\ errorMessageId <- Hooks.useState ""
  isFocus /\ isFocusId <- Hooks.useState false

  Hooks.pure $ HH.div [css "flex flex-row items-start w-full border-b-2 border-yukiRed relative"]
    [ HH.div
      [ css $ "text-lg px-2 py rounded-br-lg border-yukiRed "
        <> if isFocus then "bg-yukiRed text-white" else "bg-white text-yukiBlack"
      ]
      [ HH.text $ initName]
    , HH.input
      [ css "font-math text-3xl flex-grow p-4 tracking-widest outline-none w-full"
      , HP.value inputValue
      , HE.onValueInput \s -> do
        Hooks.put inputValueId s
        let
          result = parse primEnv s
        Hooks.raise token.outputToken $ Changed $ result
        Hooks.put errorMessageId $ case result of
          Right _ -> ""
          Left (BadRequest Inconsistencies) -> "入力に不整合があります"
          Left (InternalError str) -> "内部エラー: " <> str
          Left (BadRequest (DuplicatedToken str)) -> "入力に不整合があります: "<> str
          Left (BadRequest (NoSuchToken str)) -> "トークンが見つかりません: "<> str
          Left (BadRequest EmptyRequest) -> ""
      , HE.onFocusIn \_ -> do
        Hooks.put isFocusId true
        Hooks.raise token.outputToken $ Changed $ parse primEnv inputValue
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