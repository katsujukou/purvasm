module Purvasm.Primitives where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Purvasm.Types (BlockTag, GlobalName)

data Primitive
  = PDeref
  | PGetGlobal GlobalName -- load value by looking up from symbol table
  | PSetGlobal GlobalName
  | PMakeBlock BlockTag
  | PMakeArray
  | PBlockSize
  | PGetField Int
  | PSetField Int
  | PGetRecordField String
  | PLookupField String
  -- Arithmetic operations
  | P_add_i32
  | P_mul_i32
  | P_add_f64
  | P_mul_f64

derive instance Generic Primitive _
instance Show Primitive where
  show = genericShow
