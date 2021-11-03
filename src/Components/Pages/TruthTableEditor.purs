module LogicWeb.Components.Pages.TruthTableEditor where

import Prelude

import Data.Array (concat, find, findIndex, head, index, length, mapWithIndex, modifyAt, nub, singleton, updateAt, zipWith)
import Data.Int (pow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, sum)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Debug (spy)
import Halogen (Component, lift, liftEffect)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.HTML.RootButton (button)
import LogicWeb.Components.HTML.TruthTable (displayTruthTable)
import LogicWeb.PropositionalLogic.TruthTable (TruthTable(..))
import LogicWeb.Store (Action(..), load, save)
import LogicWeb.Store as Store

divButton :: forall t2 t3. Boolean -> HTML t2 t3
divButton frag = HH.div [css "w-full h-full flex justify-center items-center cursor-pointer"]
  [ HH.div [css $ "h-5 w-5 rounded-md " <> if frag then "bg-yukiRed" else "bg-transparent"][]
  ]



component :: forall q o m.
  MonadStore Store.Action Store.Store m
  => Component q TruthTable o m
component = Hooks.component \_ (TruthTable input) -> Hooks.do
  variables /\ variablesId <- Hooks.useState input.variables
  results /\ resultsId <- Hooks.useState
    $ map (\xs -> fromMaybe false
      $ input.table (\v -> index xs =<< findIndex (_ == v) input.variables))
    $ replicateA (length input.variables) [false, true] -- Hooks.useState

  Hooks.useLifecycleEffect do
    load
    res <- lift getStore
    let t = fromMaybe {variables: ["0"], results: [false]} $ head $ res.truthTables
    Hooks.put variablesId t.variables
    Hooks.put resultsId t.results
    pure $ Just save

  let
    makeTruthTable | nub variables == variables = Just $ TruthTable
      { variables: variables
      , output: ""
      , table: \f -> index results
        =<< (sum <<< mapWithIndex (\i x -> x * pow 2 (length variables - i - 1))
        <$> sequence (map (\v -> (if _ then 1 else 0) <$> f v) variables))
      }
    makeTruthTable = Nothing
    showBoolean = if _ then "1" else "0"
    makeRow i xs = HH.tr [css "font-sans h-10"] $ map (HH.td [css "border-t px-3 border-yukiRed"] <<< singleton <<< HH.text <<< showBoolean) xs
      <>[ HH.td
          [ css "border-l-4 border-t border-yukiRed h-9 w-9"
          , HE.onClick \_ -> do
            Hooks.put resultsId $ fromMaybe results $ modifyAt i not results
            updateStore $ SetTruthTables [{variables, results}]
          ]
          [ divButton $ fromMaybe false $ index results i ]
        ]

  Hooks.pure $ HH.div [css "h-full flex flex-row relative animate-fade-in-quick"] $
    [ HH.div [css "flex flex-col flex-grow overflow-auto shadow-md relative z-30 bg-white"] 
      [ HH.div [css "h-12 w-16 m-6"]
        [ button [HH.i [css "fas fa-plus"] []] \_ -> pure unit
        ]
      ]
    , HH.div [css "p-3 flex flex-row items-start"]
      [ HH.div [css "flex flex-row flex-grow"]
        [ HH.div [css "flex flex-col"] $
          [ HH.div [css "flex flex-col rounded-md bg-white shadow-md items-start"] $
            [ HH.div [css "h-12 w-auto m-3"]
              [ button [HH.text "変数追加"] \_ -> do
                let vs = variables <> [show $ length variables]
                let rs = concat $ zipWith (\x y -> [x, y]) results results
                Hooks.put variablesId $ vs
                Hooks.put resultsId $ rs
                updateStore $ SetTruthTables $ [{variables: vs, results: rs}]
              ]
            ]
            <>
            mapWithIndex (\i v -> HH.input
              [ css "text-lg w-80 p-3 border-b-2 mb-3 border-yukiRed outline-none", HP.value v
              , HE.onValueInput \s -> do
                Hooks.put variablesId $ fromMaybe variables $ (updateAt i s variables)
                updateStore $ SetTruthTables [{variables, results}]
              ]) variables
          ]
        , HH.div [css "shadow-md bg-white relative z-30 p-3 rounded-md flex flex-col items-center ml-3"]
          [ HH.table [css "table-fixed text-xl border-collapse whitespace-nowrap tracking-wider"] $
            [ HH.tr [css "font-math h-10"] $ map (HH.th [css "border-b-4 px-3 border-yukiRed"] <<< singleton <<< HH.text) variables
              <> [HH.th [css "border-l-4 border-b-4 px-3 border-yukiRed"] []]
            ]
            <>
            mapWithIndex makeRow (replicateA (length variables) [false, true] :: Array (Array _))
          ]
        ]
      , HH.div [css "ml-3"]
        [
          fromMaybe (HH.div [css "p-3 bg-white"] [HH.text "変数名が重複しています"]) $ displayTruthTable <$> makeTruthTable
        ]
      ]
    ]