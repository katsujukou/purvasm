module Purvasm.Primitives where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Purvasm.Types (BlockTag, GlobalName)

data Primitive
  = PGetGlobal GlobalName
  | PSetGlobal GlobalName
  | PMakeBlock BlockTag
  | PGetField Int
  | PSetField Int
  | PLookupField String
  -- Arithmetic operations
  | P_add_i32

derive instance Generic Primitive _
instance Show Primitive where
  show = genericShow
