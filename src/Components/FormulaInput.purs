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

data Output = Changed (Either ParseError Formula)

component :: forall q m. Component q Int Output m
component = Hooks.component \token num -> Hooks.do
  inputValue /\ inputValueId <- Hooks.useState ""
  errorMessage /\ errorMessageId <- Hooks.useState ""

  Hooks.pure $ HH.div [css "flex flex-row w-full border-b-2 border-yukiRed relative"]
    [ HH.div [css "bg-yukiRed text-white w-7 flex items-center justify-center text-lg"] [HH.text $ show num]
    , HH.input
      [ css "font-math text-3xl flex-grow p-4 tracking-widest outline-none"
      , HP.value inputValue
      , HE.onValueInput \s -> do
        Hooks.put inputValueId s
        let
          result = parse primEnv s
        Hooks.raise token.outputToken $ Changed $ result
        case result of
          Right _ -> Hooks.put errorMessageId ""
          Left (BadRequest Inconsistencies) -> Hooks.put errorMessageId $ "入力に不整合があります"
          Left (InternalError str) -> Hooks.put errorMessageId $ "内部エラー: " <> str
          Left (BadRequest (DuplicatedToken str)) -> Hooks.put errorMessageId $ "入力に不整合があります: "<> str
          Left (BadRequest (NoSuchToken str)) -> Hooks.put errorMessageId $ "トークンが見つかりません: "<> str
      ]
    , HH.div [css $ "absolute right-0 bottom-0 text-base bg-yukiRed text-white px-2 py-1 "
        <> if errorMessage == "" then "invisible" else "visible"]
      [ HH.text errorMessage
      ]
    ]