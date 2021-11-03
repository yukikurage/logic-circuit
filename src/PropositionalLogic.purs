module LogicWeb.PropositionalLogic where


import Prelude

import Data.Array (filter, findIndex, foldl, index, length, uncons)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicateA)
import LogicWeb.PropositionalLogic.Formula (Formula(..), compute, variables)
import LogicWeb.PropositionalLogic.Formula.Primitive (primAnd, primNot, primOr)
import LogicWeb.PropositionalLogic.TruthTable (TruthTable(..))

class PropositionalLogic a where
  toTruthTable :: a -> TruthTable

instance PropositionalLogic TruthTable where
  toTruthTable = identity

instance PropositionalLogic Formula where
  toTruthTable :: Formula -> TruthTable
  toTruthTable f = TruthTable
    { variables: vs
    , output: show f
    , table: compute f
    }
    where
    vs = variables f

-- | 連言標準形
cnf :: forall a. PropositionalLogic a => a -> Maybe Formula
cnf a = res xs
  where
  TruthTable t = toTruthTable a
  xs = filter
    (\vs -> t.table (\v -> index vs =<< findIndex (_ == v) t.variables) == Just false)
    $ replicateA (length t.variables) [false, true]

  literal :: Int -> Boolean -> Maybe Formula
  literal i f = if f then MonadicOperate primNot <$> v else v
    where
      v = Var <$> index t.variables i
  term :: Array Boolean -> Maybe Formula
  term xs = bind (uncons xs) \{head, tail} -> do
    foldlWithIndex (\i mf x -> BinaryOperate primOr <$> mf <*> literal (i + 1) x) (literal 0 head) tail
  res :: Array (Array Boolean) -> Maybe Formula
  res xs = bind (uncons xs) \{head, tail} -> do
    foldl (\mf x -> BinaryOperate primAnd <$> mf <*> term x) (term head) tail

-- | 宣言標準形
dnf :: forall a. PropositionalLogic a => a -> Maybe Formula
dnf a = res xs
  where
  TruthTable t = toTruthTable a
  xs = filter
    (\vs -> t.table (\v -> index vs =<< findIndex (_ == v) t.variables) == Just true)
    $ replicateA (length t.variables) [false, true]

  literal :: Int -> Boolean -> Maybe Formula
  literal i f = if f then v else MonadicOperate primNot <$> v
    where
    v = Var <$> index t.variables i
  term :: Array Boolean -> Maybe Formula
  term xs = bind (uncons xs) \{head, tail} -> do
    foldlWithIndex (\i mf x -> BinaryOperate primAnd <$> mf <*> literal (i + 1) x) (literal 0 head) tail
  res :: Array (Array Boolean) -> Maybe Formula
  res xs = bind (uncons xs) \{head, tail} -> do
    foldl (\mf x -> BinaryOperate primOr <$> mf <*> term x) (term head) tail