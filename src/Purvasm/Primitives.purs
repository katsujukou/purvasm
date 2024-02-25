module Purvasm.Primitives where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Purvasm.Types (BlockTag)
import Purvasm.Global (GlobalName)

data Primitive
  = PDeref
  | PGetGlobal GlobalName -- load value by looking up from symbol table
  | PSetGlobal GlobalName
  | PMakeBlock BlockTag
  | PMakeArray
  | PGetField Int
  | PSetField Int
  | PGetRecordField String
  | PLookupField String
  -- Arithmetic operations
  | P_add_i32

derive instance Generic Primitive _
instance Show Primitive where
  show = genericShow
