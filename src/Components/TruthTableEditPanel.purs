module LogicWeb.Components.TruthTableEditPanel where

import Prelude

import Data.Array (all, filter, index, length, mapWithIndex, modifyAt, notElem, nub, replicate, singleton)
import Data.Either (Either(..))
import Data.Int (pow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, sum)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Halogen (Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore)
import LogicWeb.Common (showBool)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.HTML.TruthTable (displayTruthTable)
import LogicWeb.Components.InputsList as InputsList
import LogicWeb.PropositionalLogic.Formula (Formula(..))
import LogicWeb.PropositionalLogic.Formula.Parser (parse)
import LogicWeb.PropositionalLogic.Formula.Primitive (primEnv)
import LogicWeb.PropositionalLogic.TruthTable (TruthTable(..))
import LogicWeb.Store as Store
import LogicWeb.Type.RawTruthTable (RawTruthTable)
import Type.Proxy (Proxy(..))

inputsList_ = Proxy :: Proxy "inputsList_"

data Output = Change RawTruthTable

divButton :: forall t2 t3. Boolean -> HTML t2 t3
divButton frag = HH.div [css "w-full h-full flex justify-center items-center p-1"]
  [ HH.div [css $ "h-6 w-6 border-yukiRed border-2 rounded-md " <> if frag then "bg-yukiRed" else "bg-transparent"][]
  ]

component :: forall q m.
  MonadStore Store.Action Store.Store m
  => Component q RawTruthTable Output m
component = Hooks.component \{slotToken, outputToken} inputRawTruthTable -> Hooks.do
  variables /\ variablesId <- Hooks.useState inputRawTruthTable.variables
  results /\ resultsId <- Hooks.useState inputRawTruthTable.results

  Hooks.useLifecycleEffect do
    Hooks.tell slotToken inputsList_ unit $ InputsList.SetValues variables
    pure $ Nothing

  let
    messageHandler = case _ of
      "" -> "変数名が空です"
      s | length (filter (_ == s) variables) > 1 -> "変数名が重複しています"
      s | not $ isValidVariables s -> "不正な値です"
      _ -> ""

    -- | 変数を変更したときの処理
    handleChangeInputs _ = do
      res <- fromMaybe [] <$> Hooks.request slotToken inputsList_ unit InputsList.GetValues

      let
        newResults = if length res == length variables
          then results
          else replicate (pow 2 $ length res) false

      Hooks.put variablesId $ res
      Hooks.put resultsId $ newResults
      Hooks.raise outputToken $ Change {variables: res, results: newResults, name: inputRawTruthTable.name}

    isValidVariables v = case parse primEnv v of
      Right (Var _) -> true
      _ -> false
    makeTruthTable
      | nub variables == variables
      && notElem "" variables
      && all isValidVariables variables
      = Just $ TruthTable
      { variables: variables
      , output: ""
      , table: \f -> index results
        =<< (sum <<< mapWithIndex (\i x -> x * pow 2 (length variables - i - 1))
        <$> sequence (map (\v -> (if _ then 1 else 0) <$> f v) variables))
      }
    makeTruthTable = Nothing
    makeRow i xs = HH.tr [css "font-sans h-10"]
      $ map (HH.td [css "border-t px-3 border-yukiRed"] <<< singleton <<< HH.text <<< showBool) xs
      <> [ HH.td
          [ css "border-l-4 border-t border-yukiRed h-9 w-9 cursor-pointer"
          , HE.onClick \_ -> do
            let
              newResults = fromMaybe results $ modifyAt i not results
            Hooks.put resultsId $ newResults
            Hooks.raise outputToken $ Change {variables: variables, results: newResults, name: inputRawTruthTable.name}
          ]
          [ divButton $ fromMaybe false $ index results i ]
        ]

  Hooks.pure $ HH.div [css "h-full flex flex-row relative animate-fade-in-quick"] $
    [ HH.div [css "p-3 flex flex-row items-start w-full"]
      [ HH.div [css "flex flex-row flex-grow items-start"]
        [ HH.div [css "flex flex-col"] $
          [ HH.div [css "rounded-md bg-white shadow-md text-center text-lg w-72"] $
            [
              HH.div [css "m-3"] [HH.text "変数"]
            , HH.slot inputsList_ unit InputsList.component {messageHandler} handleChangeInputs
            ]
          ]
        , HH.div [css "shadow-md bg-white relative z-30 p-3 rounded-md flex flex-col items-center ml-3"]
          [
            HH.div [css "mb-3 text-lg"] [HH.text "真理値"]
          , HH.table [css "table-fixed text-xl border-collapse whitespace-nowrap tracking-wider"] $
            [ HH.tr [css "font-math h-10"] $ map (HH.th [css "border-b-4 px-3 border-yukiRed"] <<< singleton <<< HH.text) variables
              <> [HH.th [css "border-l-4 border-b-4 px-3 border-yukiRed"] []]
            ]
            <>
            mapWithIndex makeRow (replicateA (length variables) [false, true] :: Array (Array _))
          ]
        ]
      , HH.div [css "ml-3"]
        [
          fromMaybe (HH.div_ []) $ displayTruthTable <$> makeTruthTable
        ]
      ]
    ]