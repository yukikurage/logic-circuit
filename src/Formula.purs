module LogicWeb.Formula where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set

-- | 命題変数
type Variable = String

-- | 命題定数
type Constant =
  { symbol :: String
  , value :: Boolean
  }

-- | 論理結合子(1引数)
type SingleConnective =
  { symbol :: String
  , value :: Boolean -> Boolean
  }

-- | 論理結合子
type Connective =
  { symbol :: String
  , value :: Boolean -> Boolean -> Boolean
  }

-- | 論理式
data Formula =
  Var Variable |
  Const Constant |
  SingleConnect SingleConnective Formula |
  Connect Connective Formula Formula

-- | 論理式から命題変数を取り出す
variables :: Formula -> Set.Set Variable
variables (Var v) = Set.singleton v
variables (Const _) = Set.empty
variables (SingleConnect _ f) = variables f
variables (Connect _ f0 f1) = variables f0 <> variables f1

-- | 論理式を論理式へ代入
substitution :: Formula -> Map.Map Variable Formula -> Formula
substitution (Var v) vs = fromMaybe (Var v) $ Map.lookup v vs
substitution (Const c) _ = Const c
substitution (SingleConnect c f) vs = SingleConnect c (substitution f vs)
substitution (Connect c f0 f1) vs = Connect c (substitution f0 vs) (substitution f1 vs)

-- | 論理式を計算
compute :: Formula -> Maybe Boolean
compute (Var _) = Nothing
compute (Const c) = Just c.value
compute (SingleConnect c f) = c.value <$> compute f
compute (Connect c f0 f1) = c.value <$> compute f0 <*> compute f1