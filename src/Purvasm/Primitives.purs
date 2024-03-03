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
  -- Logical operations 
  | P_lnot
  | P_land
  | P_lor
  | P_lxor
  -- Numeric comparison & arithmetic operations
  | P_add_i32
  | P_mul_i32
  | P_sub_i32
  | P_div_i32
  | P_mod_i32
  | P_neg_i32
  | P_equ_i32
  | P_neq_i32
  | P_ge_i32
  | P_gt_i32
  | P_lt_i32
  | P_le_i32
  | P_add_f64
  | P_mul_f64
  | P_equ_f64
  | P_neq_f64
  | P_ge_f64
  | P_gt_f64
  | P_lt_f64
  | P_le_f64

derive instance Generic Primitive _
instance Show Primitive where
  show = genericShow
