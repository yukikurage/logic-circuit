
module LogicWeb.Components.TruthTablePanel where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (findIndex, index, length, singleton, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Unfoldable (replicateA)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import LogicWeb.Class.ContentHandler (class ContentHandler, getFormulas, setFormulas)
import LogicWeb.Components.Common (css)
import LogicWeb.Components.HTML.RootButton (button)
import LogicWeb.PropositionalLogic (cnf)
import LogicWeb.PropositionalLogic.TruthTable (TruthTable(..))
import LogicWeb.PropositionalLogic.TruthTable as T

data Output = UpdateFormulas

component :: forall q m. Monad m => ContentHandler m => Component q TruthTable Output m
component = Hooks.component \{outputToken} t -> Hooks.do
  Hooks.pure $ displayTruthTable t outputToken

displayTruthTable :: forall w m. Monad m => ContentHandler m => TruthTable -> Hooks.OutputToken Output -> HH.HTML w (HookM m Unit)
displayTruthTable t _ | T.null t = HH.table_ []
displayTruthTable (TruthTable t) outputToken = HH.div [css "shadow-md bg-white relative z-30 p-5 rounded-md flex flex-col items-center text-lg"]
  [ HH.text "真理値表"
  , button [HH.text "連言標準形を作成"] \_ -> do
    fs <- lift getFormulas
    case cnf (TruthTable t) of
      Just g -> lift $ setFormulas $ snoc fs $ show g
      Nothing -> pure unit
    Hooks.raise outputToken UpdateFormulas
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