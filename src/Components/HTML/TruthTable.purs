module LogicWeb.Components.HTML.TruthTable where

import Prelude

import Data.Array (findIndex, index, length, singleton)
import Data.Maybe (maybe)
import Data.Unfoldable (replicateA)
import Halogen.HTML as HH
import LogicWeb.Components.Common (css)
import LogicWeb.PropositionalLogic.TruthTable (TruthTable)
import LogicWeb.PropositionalLogic.TruthTable as T

displayTruthTable :: forall w i. TruthTable -> HH.HTML w i
displayTruthTable t | T.null t = HH.table_ []
displayTruthTable t = HH.div [css "shadow-md bg-white relative z-30 p-5 rounded-md flex flex-col items-center text-lg"]
  [ HH.text "真理値表"
  , HH.table [css "table-fixed text-xl border-collapse whitespace-nowrap mt-3 tracking-wider"] $
    [ HH.tr [css "font-math h-10"] $ map (HH.th [css "border-b-4 px-3 border-yukiRed"] <<< singleton <<< HH.text) t.variables
      <> [HH.th [css "border-l-4 border-b-4 px-3 border-yukiRed"] []]
    ]
    <>
    map makeRow (replicateA (length t.variables) [false, true] :: Array (Array _))
  ]
  where
    showBoolean = if _ then "1" else "0"
    showMaybeBoolean = maybe "*" showBoolean
    makeRow xs = HH.tr [css "font-sans h-10"] $ map (HH.td [css "border-t px-3 border-yukiRed"] <<< singleton <<< HH.text <<< showBoolean) xs
      <> [HH.td [css "border-l-4 border-t px-3 border-yukiRed"]
        [HH.text $ showMaybeBoolean $  t.table (\v -> index xs =<< findIndex (_ == v) t.variables)]]