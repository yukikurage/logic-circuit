module LogicWeb.PropositionalLogic.Formula.Primitive where

import Prelude

import Data.Array (all, elem, find)
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
  , associative: Nothing
  }

primEq :: F.BinaryOperator
primEq =
  { symbol : "⇔"
  , value: eq
  , priority: 20
  , associative: Just F.Left
  }

isVariable :: String -> Boolean
isVariable = String.toCodePointArray
  >>> all
    ( flip elem (map codePointFromChar
      $ enumFromTo 'a' 'z'
      <> enumFromTo 'A' 'Z'
      <> enumFromTo 'α' 'ω'
      <> (enumFromTo '0' '9' :: Array Char)
      )
    )

primEnv :: Environment
primEnv =
  { variables: \str -> if isVariable str && String.length str > 0 then Just {symbol : str} else Nothing
  , binaryOperators: \str -> find ((_.symbol) >>> (_ == str)) $ [primAnd, primEq, primImplication, primOr]
  , monadicOperators: \str -> find ((_.symbol) >>> (_ == str)) $ [primNot]
  , brackets: {left: "(", right: ")"}
  }
