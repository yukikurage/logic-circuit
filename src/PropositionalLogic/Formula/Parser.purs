module LogicWeb.PropositionalLogic.Formula.Parser where

import Prelude

import Data.Array (catMaybes, cons, filter, findLastIndex, head, length, snoc, span, tail, uncons, (..), (:))
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (codePointFromChar)
import Data.String as String
import LogicWeb.PropositionalLogic.Formula (Associative, BinaryOperator, Formula(..), MonadicOperator, Variable)
import LogicWeb.PropositionalLogic.Formula as F
import Prim.Row (class Lacks)
import Record as Record
import Type.Proxy (Proxy(..))

type Environment =
  { variables :: String -> Maybe Variable
  , monadicOperators :: String -> Maybe MonadicOperator
  , binaryOperators :: String -> Maybe BinaryOperator
  , brackets :: {left :: String, right :: String}
  }

data Types = Bracket | Variable | SingleConnective | Connective

data ParseError = NoSuchToken | EmptyArray String | Uncertain String

instance Show ParseError where
  show NoSuchToken = "Parse Error: NoSuchToken"
  show (EmptyArray str) = "Parse Error: EmptyArray: " <> str
  show (Uncertain str) = "Parse Error: EmptyArray: " <> str

type ShuntingYardState =
  { remaining :: Array String
  , output :: Array String
  , stack :: Array String
  }

type Operator =
  { symbol :: String
  , associative :: Maybe Associative
  , priority :: Int
  }

toOperator :: forall r v. Lacks "value" r => {value :: v | r} -> Record r
toOperator = Record.delete $ (Proxy :: Proxy "value")

isOperator :: Environment -> String -> Boolean
isOperator env s = isJust (env.binaryOperators s) || isJust (env.monadicOperators s)

genericOperator :: Environment -> String -> Maybe Operator
genericOperator env s = head $ catMaybes $ [toOperator <$> env.binaryOperators s, toOperator <$> env.monadicOperators s]

parse :: Environment -> String -> Either ParseError Formula
parse env str = polishNotationToFormula env =<< toPolishNotation env str

polishNotationToFormula :: Environment -> Array String -> Either ParseError Formula
polishNotationToFormula env s = interpretation env {remaining: s, stack: []}

-- | ポーランド記法を解釈
interpretation :: Environment
  -> {remaining :: Array String, stack :: Array Formula}
  -> Either ParseError (Formula)
interpretation env state = case uncons state.remaining of
  Just token -> interpretation env =<< newState
    where
    newState
      | isJust $ env.monadicOperators token.head = note NoSuchToken do
        op <- env.monadicOperators token.head
        stack <- uncons state.stack
        pure {remaining: token.tail, stack: (MonadicOperate op stack.head): stack.tail }
      | isJust (env.binaryOperators token.head) = note NoSuchToken do
        op <- env.binaryOperators token.head
        stack0 <- uncons state.stack
        stack1 <- uncons stack0.tail
        pure {remaining: token.tail, stack: (BinaryOperate op stack1.head stack0.head): stack1.tail }
      | isJust (env.variables token.head) = note NoSuchToken do
        v <- env.variables token.head
        pure $ {remaining: token.tail, stack: (Var v): state.stack}
      | otherwise = Left NoSuchToken
  Nothing -> note NoSuchToken $ head state.stack

toPolishNotation :: Environment -> String -> Either ParseError (Array String)
toPolishNotation env str = result
  where
  result = (\s -> shuntingYard env {remaining: s, output: [], stack: []}) =<< split env str

shuntingYard :: Environment -> ShuntingYardState -> Either ParseError (Array String)
shuntingYard env state = case uncons state.remaining of
  Just token -> shuntingYard env =<< newState
    where
    newState
      | isOperator env token.head --演算子のとき
        = Right $ {remaining: token.tail, output: state.output <> extract.init, stack: token.head: extract.rest}
        where
          notFrag s = not (isOperator env s) || fromMaybe false do
            o1 <- genericOperator env token.head
            o2 <- genericOperator env s
            Just $ o1.priority > o2.priority || (o1.associative /= Just F.Left && o1.priority >= o2.priority)
          extract = span (notFrag >>> not) state.stack
      | token.head == env.brackets.left --左括弧のとき
        = Right $ {remaining: token.tail, output: state.output, stack: token.head: state.stack}
      | token.head == env.brackets.right --左括弧のとき
        = Right $ {remaining: token.tail, output: state.output <> extract.init, stack: delLeftBracket}
        where
          frag s = s /= env.brackets.left
          extract = span frag state.stack
          delLeftBracket = fromMaybe [] $ tail extract.rest
      | isJust (env.variables token.head)
        = Right {remaining: token.tail, output: state.output `snoc` token.head, stack: state.stack}
      | otherwise = Left NoSuchToken
  Nothing -> Right $ state.output <> state.stack

split :: Environment -> String -> Either ParseError (Array String)
split _ "" = Right []
split env str = cons <$> current <*> next
  where
  allTokens s =
    isJust (env.variables s)
    || isJust (env.monadicOperators s)
    || isJust (env.binaryOperators s)
    || s == env.brackets.left
    || s == env.brackets.right
  uniquePrefix = findMaxPrefix allTokens $ deletePrefixSpaces str
  current = (\x -> x.prefix) <$> uniquePrefix
  next = (\x -> split env x.remaining) =<< uniquePrefix

findUniquePrefix :: (String -> Boolean) -> String -> Either ParseError {prefix :: String, remaining :: String}
findUniquePrefix f str = note (Uncertain "一致するトークンが複数あります") $ (\tmp -> {prefix : tmp.before, remaining: tmp.after}) <$> result
  where
  filtered = filter (flip String.take str >>> f) $ 0 .. String.length str
  result = if length filtered == 1 then flip String.splitAt str <$> head filtered else Nothing

findMaxPrefix :: (String -> Boolean) -> String -> Either ParseError {prefix :: String, remaining :: String}
findMaxPrefix f str = note NoSuchToken $ (\tmp -> {prefix : tmp.before, remaining: tmp.after}) <$> res
  where
  i = findLastIndex f $ map (flip String.take str) $ 0 .. String.length str
  res = flip String.splitAt str <$> i

deletePrefixSpaces :: String -> String
deletePrefixSpaces = String.dropWhile (_ == codePointFromChar ' ')