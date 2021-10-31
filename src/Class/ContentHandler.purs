module LogicWeb.Class.ContentHandler where

import Prelude

import LogicWeb.Type.RawTruthTable (RawTruthTable)

class Monad m <= ContentHandler m where
  save :: m Unit
  load :: m Unit