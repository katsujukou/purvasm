module Purvasm.LCore.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)

newtype Var = Var Int

instance Show Var where
  show (Var v) = "(Var " <> show v <> ")"

derive newtype instance Eq Var
derive newtype instance Ord Var

data FieldPos = PosIndex Int | PosProp String

derive instance Generic FieldPos _
instance Show FieldPos where
  show = genericShow

type Occurrunce = List FieldPos