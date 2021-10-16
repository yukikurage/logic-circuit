module LogicWeb.PropositionalLogic.Formula where

import Prelude

import Data.Array (nub)
import Data.Maybe (Maybe, fromMaybe)

-- | 命題変数
type Variable = String

data Associative = Left | Right

derive instance Eq Associative

-- | 論理結合子(1引数)
type MonadicOperator =
  { symbol :: String
  , value :: Boolean -> Boolean
  , priority :: Int
  , associative :: Maybe Associative
  }

-- | 論理結合子
type BinaryOperator =
  { symbol :: String
  , value :: Boolean -> Boolean -> Boolean
  , priority :: Int
  , associative :: Maybe Associative
  }
-- | 論理式
data Formula =
  Var Variable |
  MonadicOperate MonadicOperator Formula |
  BinaryOperate BinaryOperator Formula Formula

-- | 論理式から命題変数を取り出す
variables :: Formula -> Array Variable
variables (Var v) = [v]
variables (MonadicOperate _ f) = nub $ variables f
variables (BinaryOperate _ f0 f1) = nub $ variables f0 <> variables f1

-- | 論理式を論理式へ代入
substitution :: Formula -> (Variable -> Maybe Formula) -> Formula
substitution (Var v) vs = fromMaybe (Var v) $ vs v
substitution (MonadicOperate c f) vs = MonadicOperate c (substitution f vs)
substitution (BinaryOperate c f0 f1) vs = BinaryOperate c (substitution f0 vs) (substitution f1 vs)

-- | 論理式を計算
compute :: Formula -> (Variable -> Maybe Boolean) -> Maybe Boolean
compute (Var v) vs = vs v
compute (MonadicOperate c f) vs = c.value <$> compute f vs
compute (BinaryOperate c f0 f1) vs = c.value <$> compute f0 vs <*> compute f1 vs

instance Show Formula where
  show (Var v) = v
  show (MonadicOperate c f) = "(" <> c.symbol <> show f <> ")"
  show (BinaryOperate c f0 f1) = "(" <> show f0 <> c.symbol <> show f1 <> ")"