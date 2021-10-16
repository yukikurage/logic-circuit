module LogicWeb.PropositionalLogic.Formula.Primitive where

import Prelude

import Data.Enum (enumFromTo)
import Data.HeytingAlgebra (implies)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String as String
import LogicWeb.PropositionalLogic.Formula as F
import LogicWeb.PropositionalLogic.Formula.Parser (Environment)

-- | デフォルトの値
primNot :: F.MonadicOperator
primNot =
  { symbol: "¬"
  , value: not
  , priority: 100
  , associative: Just F.Right
  }

primAnd :: F.BinaryOperator
primAnd =
  { symbol: "∧"
  , value: (&&)
  , priority: 80
  , associative: Just F.Left
  }

primOr :: F.BinaryOperator
primOr =
  { symbol: "∨"
  , value: (||)
  , priority: 60
  , associative: Just F.Left
  }

primImplication :: F.BinaryOperator
primImplication =
  { symbol: "⇒"
  , value: implies
  , priority: 40
  , associative: Just F.Right
  }

primImplication2 :: F.BinaryOperator
primImplication2 =
  { symbol: "=>"
  , value: implies
  , priority: 40
  , associative: Just F.Right
  }

primEq :: F.BinaryOperator
primEq =
  { symbol : "⇔"
  , value: eq
  , priority: 20
  , associative: Just F.Left
  }

primEnv :: Environment
primEnv =
  { variables: map ((\x -> {symbol : x}) <<< String.singleton <<< codePointFromChar)
      $ enumFromTo 'a' 'z'
      <> enumFromTo 'A' 'Z'
      <> enumFromTo 'α' 'ω'
      <> enumFromTo '0' '9'
  , binaryOperators: [primAnd, primEq, primImplication, primImplication2, primOr]
  , monadicOperators: [primNot]
  , brackets:
    { left: "("
    , right: ")"
    }
  }
