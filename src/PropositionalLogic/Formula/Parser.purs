module LogicWeb.PropositionalLogic.Formula.Parser (parse, ParseError, Environment) where

import Prelude

import Data.Array (catMaybes, concat, cons, find, head, length, snoc, span, tail, uncons, (..), (:))
import Data.Either (Either(..), hush, note)
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

data Symbols =
    Variable Variable
  | MonadicOperator MonadicOperator
  | BinaryOperator BinaryOperator
  | LeftBracket
  | RightBracket

data ParseError = BadRequest BadRequestValue | InternalError String
data BadRequestValue = NoSuchToken | DuplicatedToken | Inconsistencies

instance Show ParseError where
  show (BadRequest v) = "Parse Error: BadRequest: " <> show v
  show (InternalError str) = "Parse Error: InternalError: " <> str

instance Show BadRequestValue where
  show NoSuchToken = "NoSuchToken"
  show DuplicatedToken = "DuplicatedToken"
  show Inconsistencies = "Inconsistencies"

parse :: Environment -> String -> Either ParseError Formula
parse env str = interpretation <<< shuntingYard =<< split env str

symbol :: Environment -> String -> Either ParseError Symbols
symbol env str = note (BadRequest NoSuchToken) $ head $ catMaybes
  [ if env.brackets.left == str then Just LeftBracket else Nothing
  , if env.brackets.right == str then Just RightBracket else Nothing
  , Variable <$> find (_ == str) env.variables
  , MonadicOperator <$> find (\x -> x.symbol == str) env.monadicOperators
  , BinaryOperator <$> find (\x -> x.symbol == str) env.binaryOperators]

interpretation :: Array Symbols -> Either ParseError Formula
interpretation s = loop s []
  where
  loop :: Array Symbols -> Array Formula
    -> Either ParseError Formula
  loop remaining stack = case uncons remaining of
    Just token -> case token.head of
      MonadicOperator op -> do
        st <- note (BadRequest Inconsistencies) $ uncons stack
        loop token.tail $ MonadicOperate op st.head : st.tail
      BinaryOperator op -> do
        stack0 <- note (BadRequest Inconsistencies) $ uncons stack
        stack1 <- note (BadRequest Inconsistencies) $ uncons stack0.tail
        loop token.tail
          $ BinaryOperate op stack1.head stack0.head : stack1.tail
      Variable v -> loop token.tail $ Var v : stack
      _ -> Left (BadRequest Inconsistencies)
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
        $ fromMaybe [] $ tail extract.rest
        where
          frag = case _ of
            LeftBracket -> true
            _ -> false
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
        notFrag s = fromMaybe true
          $ (\p -> o1.priority > p
            || (o1.associative /= Just F.Left && o1.priority >= p)
            )
          <$> priority s
    Nothing -> output <> stack

priority :: Symbols -> Maybe Int
priority = case _ of
  MonadicOperator op -> Just op.priority
  BinaryOperator op -> Just op.priority
  _ -> Nothing

split :: Environment -> String -> Either ParseError (Array Symbols)
split env =
  map concat <<< sequence <<< map loop <<< String.split (String.Pattern " ")
  where
  loop "" = Right []
  loop str = cons <$> current <*> next
    where
    uniquePrefix = findUniquePrefix env str
    current = (\x -> x.prefix) <$> uniquePrefix
    next = (\x -> split env x.after) =<< uniquePrefix

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
    0 -> Left $ BadRequest NoSuchToken
    1 -> note (InternalError "findUniquePrefix") $ head filtered
    _ -> Left $ BadRequest DuplicatedToken