module Purvasm.MiddleEnd.Types where

import Prelude

newtype Var = Var Int

instance Show Var where
  show (Var v) = "(Var " <> show v <> ")"

type Arity = Int

newtype ModuleName = ModuleName String

instance Show ModuleName where
  show (ModuleName mn) = "(ModuleName " <> mn <> ")"

newtype Ident = Ident String

instance Show Ident where
  show (Ident ident) = "(Ident " <> ident <> ")"