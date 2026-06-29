module Purvasm.Compiler.Binder where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Purvasm.Compiler.Literal (Literal)

-- | A pattern, matched structurally against an already-evaluated value (ADR-0011,
-- | ADR-0012). Covers all of CoreFn's binders: wildcard, variable, scalar literal,
-- | (nested) constructor, array, record, and as-pattern. Guards live on the
-- | alternative, not the binder, and are a separate axis (ADR-0013).
data Binder
  = BNull
  | BVar String
  | BLit Literal
  | BCtor String (Array Binder)
  | BArray (Array Binder)
  | BRecord (Array { prop :: String, binder :: Binder })
  | BNamed String Binder

derive instance Eq Binder
derive instance Generic Binder _
instance Show Binder where
  show it = genericShow it

