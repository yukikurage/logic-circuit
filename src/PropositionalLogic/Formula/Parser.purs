module LogicWeb.PropositionalLogic.Formula.Parser (parse, ParseError(..), Environment, BadRequestValue(..)) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (all, catMaybes, concat, cons, elem, find, head, length, null, snoc, span, tail, uncons, (..), (:))
import Data.Either (Either(..), hush, note)
import Data.Enum (enumFromTo)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, codePointFromChar)
import Data.String as String
import Data.Traversable (sequence)
import LogicWeb.PropositionalLogic.Formula (Associative, Formula(..), MonadicOperator, Variable, BinaryOperator)
import LogicWeb.PropositionalLogic.Formula as F

type Environment =
  { monadicOperators :: Array MonadicOperator
  , binaryOperators :: Array BinaryOperator
  , brackets :: {left :: String, right :: String}
  }

data Symbols =
    Variable Variable
  | MonadicOperator MonadicOperator
  | BinaryOperator BinaryOperator
  | LeftBracket
  | RightBracket

data ParseError = BadRequest BadRequestValue | InternalError String
data BadRequestValue = NoSuchToken String | DuplicatedToken String | Inconsistencies | EmptyRequest

instance Show ParseError where
  show (BadRequest v) = "Parse Error: BadRequest: " <> show v
  show (InternalError str) = "Parse Error: InternalError: " <> str

instance Show BadRequestValue where
  show (NoSuchToken str) = "NoSuchToken: " <> str
  show (DuplicatedToken str) = "DuplicatedToken: " <> str
  show (Inconsistencies) = "Inconsistencies"
  show EmptyRequest = "EmptyRequest"

parse :: Environment -> String -> Either ParseError Formula
parse env str = interpretation <<< shuntingYard =<< split env str

isAlphabet :: CodePoint -> Boolean
isAlphabet x = elem x $ map codePointFromChar
  $ enumFromTo 'a' 'z'
  <> enumFromTo 'A' 'Z'
  <> enumFromTo 'α' 'ω'
  <> enumFromTo '0' '9'

symbol :: Environment -> String -> Either ParseError Symbols
symbol env str = note (BadRequest (NoSuchToken str)) $
  if env.brackets.left == str then Just LeftBracket else Nothing
  <|> if env.brackets.right == str then Just RightBracket else Nothing
  <|> MonadicOperator <$> find (\x -> x.symbol == str) env.monadicOperators
  <|> BinaryOperator <$> find (\x -> x.symbol == str) env.binaryOperators
  <|> if all isAlphabet (String.toCodePointArray str) && String.length str >= 1 then Just (Variable str) else Nothing

interpretation :: Array Symbols -> Either ParseError Formula
interpretation s | null s = Left $ BadRequest EmptyRequest
interpretation s = loop s []
  where
  loop :: Array Symbols -> Array Formula
    -> Either ParseError Formula
  loop remaining stack = case uncons remaining of
    Just token -> case token.head of
      MonadicOperator op -> do
        st <- note (BadRequest (Inconsistencies)) $ uncons stack
        loop token.tail $ MonadicOperate op st.head : st.tail
      BinaryOperator op -> do
        stack0 <- note (BadRequest Inconsistencies) $ uncons stack
        stack1 <- note (BadRequest Inconsistencies) $ uncons stack0.tail
        loop token.tail
          $ BinaryOperate op stack1.head stack0.head : stack1.tail
      Variable v -> loop token.tail $ Var v : stack
      _ -> Left $ BadRequest Inconsistencies
    Nothing -> note (BadRequest Inconsistencies)
      $ if length stack == 1 then head stack else Nothing

shuntingYard :: Array Symbols -> Array Symbols
shuntingYard symbols = loop symbols [] []
  where
  loop remaining output stack = case uncons remaining of
    Just token -> case token.head of
      s@(MonadicOperator o1) -> caseOperator s o1
      s@(BinaryOperator o1) -> caseOperator s o1
      LeftBracket -> loop token.tail output $ LeftBracket : stack
      RightBracket ->
        loop token.tail (output <> extract.init)
        $ fromMaybe [] $ tail $ extract.rest
        where
          frag = case _ of
            LeftBracket -> false
            _ -> true
          extract = span frag stack
      Variable v -> loop token.tail (output `snoc` Variable v) stack
      where
      caseOperator :: forall r. Symbols
        -> {priority :: Int, associative :: Maybe Associative | r}
        -> Array Symbols
      caseOperator sym o1 =
        loop token.tail (output <> extract.init) $ sym : extract.rest
        where
        extract = span (notFrag >>> not) stack
        notFrag = case _ of
          MonadicOperator o2 ->
            o1.priority > o2.priority
            || o1.associative /= Just F.Left && o1.priority >= o2.priority
          BinaryOperator o2 ->
            o1.priority > o2.priority
            || o1.associative /= Just F.Left && o1.priority >= o2.priority
          _ -> true
    Nothing -> output <> stack

split :: Environment -> String -> Either ParseError (Array Symbols)
split env =
  map concat <<< sequence <<< map loop <<< String.split (String.Pattern " ")
  where
  loop :: String -> Either ParseError (Array Symbols)
  loop str = case head (String.toCodePointArray str) of
    Just x | isAlphabet x -> cons current <$> loop next
      where
      res = span isAlphabet $ String.toCodePointArray str
      current = Variable $ String.fromCodePointArray res.init
      next = String.fromCodePointArray res.rest
    Just _ -> cons <$> current <*> (loop =<< next)
      where
      res = span (not isAlphabet) $ String.toCodePointArray str
      uniquePrefix = findUniquePrefix env $ String.fromCodePointArray res.init
      current = (_.prefix) <$> uniquePrefix
      next = (_ <> String.fromCodePointArray res.rest) <$> ((\x -> x.after) <$> uniquePrefix)
    _ -> Right []

findUniquePrefix :: Environment
  -> String
  -> Either ParseError {prefix :: Symbols, after :: String}
findUniquePrefix env str = result
  where
  f i = map {prefix: _, after: s.after} $ hush $ symbol env s.before
    where
    s = String.splitAt i str
  filtered = catMaybes
    $ map f
    $ 0 .. String.length str
  result = case length filtered of
    0 -> Left $ BadRequest $ NoSuchToken str
    1 -> note (InternalError "findUniquePrefix") $ head filtered
    _ -> Left $ BadRequest $ DuplicatedToken str