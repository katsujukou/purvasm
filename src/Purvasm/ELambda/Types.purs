module Purvasm.ELambda.Types where

import Prelude

newtype Var = Var Int

instance Show Var where
  show (Var v) = "(Var " <> show v <> ")"

derive newtype instance Eq Var
derive newtype instance Ord Var
