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

primNot_ :: F.MonadicOperator
primNot_ =
  { symbol: "!"
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

primAnd_ :: F.BinaryOperator
primAnd_ =
  { symbol: "*"
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

primOr_ :: F.BinaryOperator
primOr_ =
  { symbol: "+"
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

primImplication_ :: F.BinaryOperator
primImplication_ =
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
  { variables: map (String.singleton <<< codePointFromChar)
      $ enumFromTo 'a' 'z'
      <> enumFromTo 'A' 'Z'
      <> enumFromTo 'α' 'ω'
      <> enumFromTo '0' '9'
  , binaryOperators: [primAnd, primAnd_, primEq, primImplication, primImplication_, primOr, primOr_]
  , monadicOperators: [primNot, primNot_]
  , brackets:
    { left: "("
    , right: ")"
    }
  }
