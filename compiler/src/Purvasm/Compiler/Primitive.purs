module Purvasm.Compiler.Primitive where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
  
data PrimOp
  = AddInt
  | SubInt
  | MulInt
  | DivInt
  | ModInt
  | AddNumber
  | SubNumber
  | MulNumber
  | DivNumber
  | EqInt
  | EqString
  | EqNumber
  | EqBool
  | LtInt
  | LtString
  | LtNumber
  | AndBool
  | OrBool
  | NotBool
  | Append
  | IndexArray
  | LengthArray
  | NewArray
  | SetArray


derive instance Eq PrimOp
derive instance Generic PrimOp _

instance Show PrimOp where
  show = genericShow