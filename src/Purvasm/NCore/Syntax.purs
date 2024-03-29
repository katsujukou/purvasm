module Purvasm.NCore.Syntax where

-- This module defines the syntax of the *NCore* intermediate representation (IR).

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Purvasm.NCore.Env (VariableDesc)
import Purvasm.NCore.Types (Var)
import Purvasm.Primitives (Primitive)
import Purvasm.Types (Arity, AtomicConstant, ConstructorTag, Ident, ModuleName, StructuredConstant)

newtype Module = Module
  { name :: ModuleName
  , decls :: Array ToplevelDecl
  , foreigns :: Array Ident
  }

derive instance Newtype Module _
instance Show Module where
  show (Module m) = "(Module " <> show m <> ")"

data ToplevelDecl
  = DeclNonRec Declaration
  | DeclRec (Array Declaration)

derive instance Generic ToplevelDecl _
instance Show ToplevelDecl where
  show = genericShow

type Declaration =
  { name :: Ident
  , lambda :: NCore
  }

data NCore
  = NCNil
  | NCConst StructuredConstant
  | NCVar VariableDesc Var
  | NCFunction Arity NCore
  | NCApply NCore (Array NCore)
  | NCPrim Primitive (Array NCore)
  | NClet (Array NCore) NCore
  | NCletrec (Array NCore) NCore
  | NCSwitch (NCore) (Array (Tuple ConstructorTag NCore))
  | NCConditional (NCore) (Array (Tuple AtomicConstant NCore))
  | NCifthenelse NCore NCore NCore
  | NCStaticFail
  | NCStaticHandle NCore NCore
  | NCNone

derive instance Generic NCore _
instance Show NCore where
  show elambda = genericShow elambda
