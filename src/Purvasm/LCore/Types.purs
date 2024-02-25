module Purvasm.LCore.Types where

import Prelude

import Data.List (List)

newtype Var = Var Int

instance Show Var where
  show (Var v) = "(Var " <> show v <> ")"

derive newtype instance Eq Var
derive newtype instance Ord Var

type Occurrunce = List Int