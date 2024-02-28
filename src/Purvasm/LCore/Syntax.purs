module Purvasm.LCore.Syntax where

-- This module defines the syntax of the *LCore* intermediate representation (IR).

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Purvasm.Global (GlobalName)
import Purvasm.LCore.Env (VariableDesc)
import Purvasm.LCore.Types (Var)
import Purvasm.Primitives (Primitive)
import Purvasm.Types (Arity, AtomicConstant, ConstructorTag, Ident, ModuleName, StructuredConstant)

newtype Program = Program
  { name :: ModuleName
  , decls :: Array Declaration
  , foreigns :: Array Ident
  }

derive instance Newtype Program _
instance Show Program where
  show (Program m) = "(Program " <> show m <> ")"

type Declaration =
  { name :: Ident
  , lambda :: LCore
  }

data LCore
  = LCNil
  | LCConst StructuredConstant
  | LCStatic (Array StaticValue)
  | LCVar VariableDesc Var
  | LCFunction Arity LCore
  | LCApply LCore (Array LCore)
  | LCPrim Primitive (Array LCore)
  | LClet (Array LCore) LCore
  | LCletrec (Array LCore) LCore
  | LCSwitch (LCore) (Array (Tuple ConstructorTag LCore))
  | LCConditional (LCore) (Array (Tuple AtomicConstant LCore))
  | LCifthenelse LCore LCore LCore
  | LCStaticFail
  | LCStaticHandle LCore LCore
  | LCNone

derive instance Generic LCore _
instance Show LCore where
  show elambda = genericShow elambda

data StaticValue
  = StaticConst StructuredConstant
  | StaticRef GlobalName

derive instance Generic StaticValue _
instance Show StaticValue where
  show it = genericShow it
