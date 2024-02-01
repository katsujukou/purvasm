module Purvasm.MiddleEnd.Types where

import Prelude

newtype Var = Var Int

instance Show Var where
  show (Var v) = "(Var " <> show v <> ")"

