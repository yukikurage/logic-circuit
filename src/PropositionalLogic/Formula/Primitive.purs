module LogicWeb.PropositionalLogic.Formula.Primitive where

import Prelude

import Data.HeytingAlgebra (implies)
import Data.Maybe (Maybe(..))
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

primNot2 :: F.MonadicOperator
primNot2 =
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

primAnd2 :: F.BinaryOperator
primAnd2 =
  { symbol: "*"
  , value: (&&)
  , priority: 80
  , associative: Just F.Left
  }

primAnd3 :: F.BinaryOperator
primAnd3 =
  { symbol: "&"
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

primOr2 :: F.BinaryOperator
primOr2 =
  { symbol: "+"
  , value: (||)
  , priority: 60
  , associative: Just F.Left
  }

primOr3 :: F.BinaryOperator
primOr3 =
  { symbol: "|"
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

primEq2 :: F.BinaryOperator
primEq2 =
  { symbol : "<=>"
  , value: eq
  , priority: 20
  , associative: Just F.Left
  }

primEq4 :: F.BinaryOperator
primEq4 =
  { symbol : "⟺"
  , value: eq
  , priority: 20
  , associative: Just F.Left
  }

primEnv :: Environment
primEnv =
  { binaryOperators: [primAnd, primAnd2, primAnd3, primEq, primEq2, primEq4, primImplication, primImplication2, primOr, primOr2, primOr3]
  , monadicOperators: [primNot, primNot2]
  , brackets:
    { left: "("
    , right: ")"
    }
  }
