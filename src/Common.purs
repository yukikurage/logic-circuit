module LogicWeb.Common where

import Prelude

import Data.Array (range)

upRange :: Int -> Int -> Array Int
upRange x y = if x > y then [] else range x y