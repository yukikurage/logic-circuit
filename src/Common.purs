module LogicWeb.Common where

import Prelude

import Data.Array (init, range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)

upRange :: Int -> Int -> Array Int
upRange x y = if x > y then [] else range x y

showBool :: Boolean -> String
showBool = if _ then "1" else "0"

readBool :: String -> Maybe Boolean
readBool = case _ of
  "1" -> Just true
  "0" -> Just false
  _ -> Nothing

splitCaseEmpty :: String -> String -> Array String
splitCaseEmpty p = case _ of
  s -> fromMaybe [] $ init $ split (Pattern p) s