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
  -- Cross-representation scalar conversions (ADR-0041): Int->Number widening and the
  -- ECMAScript ToInt32 coercion (JS `n | 0`) for Number->Int.
  | IntToNumber
  | NumberToInt
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
  -- Dynamic record field access by a runtime label (ADR-0010 record-as-field-map; Record.Unsafe).
  | RecordGet
  | RecordSet
  | RecordHas
  | RecordDelete


derive instance Eq PrimOp
derive instance Generic PrimOp _

instance Show PrimOp where
  show = genericShow