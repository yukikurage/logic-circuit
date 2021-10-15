module LogicWeb.PropositionalLogic.Formula.Parser where

import Prelude

import Data.Array (catMaybes, concat, cons, filter, find, head, length, snoc, span, tail, uncons, (..), (:))
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (sequence)
import LogicWeb.PropositionalLogic.Formula (Associative, BinaryOperator, Formula(..), MonadicOperator, Variable)
import LogicWeb.PropositionalLogic.Formula as F

type Environment =
  { variables :: Array Variable
  , monadicOperators :: Array MonadicOperator
  , binaryOperators :: Array BinaryOperator
  , brackets :: {left :: String, right :: String}
  }

data Symbols = Variable Variable | MonadicOperator MonadicOperator | BinaryOperator BinaryOperator | LeftBracket | RightBracket | NotSymbol

symbol :: Environment -> String -> Symbols
symbol env str = fromMaybe NotSymbol $ head $ catMaybes
  [ if env.brackets.left == str then Just LeftBracket else Nothing
  , if env.brackets.right == str then Just RightBracket else Nothing
  , Variable <$> find (\x -> x.symbol == str) env.variables
  , MonadicOperator <$> find (\x -> x.symbol == str) env.monadicOperators
  , BinaryOperator <$> find (\x -> x.symbol == str) env.binaryOperators]

isNotSymbol :: Symbols -> Boolean
isNotSymbol NotSymbol = true
isNotSymbol _ = false

data ParseError = NoSuchToken | EmptyArray String | Uncertain String

instance Show ParseError where
  show NoSuchToken = "Parse Error: NoSuchToken"
  show (EmptyArray str) = "Parse Error: EmptyArray: " <> str
  show (Uncertain str) = "Parse Error: EmptyArray: " <> str

type ShuntingYardState =
  { remaining :: Array String
  , output :: Array Symbols
  , stack :: Array Symbols
  }

parse :: Environment -> String -> Either ParseError Formula
parse env str = polishNotationToFormula env =<< shuntingYard env str

polishNotationToFormula :: Environment -> Array Symbols -> Either ParseError Formula
polishNotationToFormula env s = interpretation env {remaining: s, stack: []}

-- | ポーランド記法を解釈
interpretation :: Environment
  -> {remaining :: Array Symbols, stack :: Array Formula}
  -> Either ParseError (Formula)
interpretation env state = case uncons state.remaining of
  Just token -> interpretation env =<< newState
    where
    newState = case token.head of
      MonadicOperator op -> note NoSuchToken do
        stack <- uncons state.stack
        pure {remaining: token.tail, stack: (MonadicOperate op stack.head): stack.tail}
      BinaryOperator op -> note NoSuchToken do
        stack0 <- uncons state.stack
        stack1 <- uncons stack0.tail
        pure {remaining: token.tail, stack: (BinaryOperate op stack1.head stack0.head): stack1.tail}
      Variable v -> pure {remaining: token.tail, stack: (Var v): state.stack}
      _ -> Left NoSuchToken
  Nothing -> note NoSuchToken $ head state.stack

shuntingYard :: Environment -> String -> Either ParseError (Array Symbols)
shuntingYard env str = (\s -> loop s [] []) =<< split env str
  where
  loop :: Array String -> Array Symbols -> Array Symbols -> Either ParseError (Array Symbols)
  loop remaining output stack = case uncons remaining of
    Just token -> case symbol env token.head of
      s@(MonadicOperator o1) -> caseOperator s o1
      s@(BinaryOperator o1) -> caseOperator s o1
      LeftBracket -> loop token.tail output $ LeftBracket : stack
      RightBracket ->
        loop token.tail (output <> extract.init) $ fromMaybe [] $ tail extract.rest
        where
          frag = case _ of
            LeftBracket -> true
            _ -> false
          extract = span frag stack
      Variable v -> loop token.tail (output `snoc` (Variable v)) stack
      _ -> Left NoSuchToken
      where
      caseOperator :: forall r. Symbols -> {priority :: Int, associative :: Maybe Associative | r}
        -> Either ParseError (Array Symbols)
      caseOperator sym o1 = loop token.tail (output <> extract.init) $ sym : extract.rest
        where
        extract = span (notFrag >>> not) stack
        notFrag s = fromMaybe true
          $ (\p -> o1.priority > p || (o1.associative /= Just F.Left && o1.priority >= p))
          <$> priority s
    Nothing -> Right $ output <> stack

priority :: Symbols -> Maybe Int
priority = case _ of
  MonadicOperator op -> Just op.priority
  BinaryOperator op -> Just op.priority
  _ -> Nothing

split :: Environment -> String -> Either ParseError (Array String)
split env = map concat <<< sequence <<< map loop <<< String.split (String.Pattern " ")
  where
  loop "" = Right []
  loop str = cons <$> current <*> next
    where
    allTokens s = not $ isNotSymbol $ symbol env s
    uniquePrefix = findUniquePrefix allTokens str
    current = (\x -> x.prefix) <$> uniquePrefix
    next = (\x -> split env x.remaining) =<< uniquePrefix

findUniquePrefix :: (String -> Boolean) -> String -> Either ParseError {prefix :: String, remaining :: String}
findUniquePrefix f str =
  note (Uncertain "一致するトークンが複数あります")
  $ (\tmp -> {prefix : tmp.before, remaining: tmp.after}) <$> result
  where
  filtered = filter (flip String.take str >>> f) $ 0 .. String.length str
  result = if length filtered == 1 then flip String.splitAt str <$> head filtered else Nothing