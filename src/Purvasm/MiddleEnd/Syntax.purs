module Purvasm.MiddleEnd.Syntax where

-- This module defines the syntax of *ELambda*, the first
-- intermediate representation (IR).

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Purvasm.MiddleEnd.Types (Arity, Ident, ModuleName, Var)
import Purvasm.Types (StructuredConstant, Tag)

newtype Module = Module
  { name :: ModuleName
  , decls :: Array Declaration
  }

derive instance Newtype Module _
instance Show Module where
  show (Module m) = "(Module " <> show m <> ")"

type Declaration =
  { name :: Ident
  , lambda :: ELambda
  }

data ELambda
  = ELConst StructuredConstant
  | ELVar Var
  | ELFunction Arity ELambda
  | ELApply ELambda (Array ELambda)
  | ELPrim Primitive (Array ELambda)
  | ELlet (Array ELambda) ELambda
  | ELletrec (Array ELambda) ELambda
  | ELifthenelse ELambda ELambda ELambda

derive instance Generic ELambda _
instance Show ELambda where
  show elambda = genericShow elambda

data Primitive
  = PGetGlobal String String
  | PSetGlobal String String
  | PMakeBlock Tag
  | PGetField Int
  | PSetField Int
  | PLookupField String
  | PCall String
  -- Arithmetic operations
  | P_add_i32

derive instance Generic Primitive _
instance Show Primitive where
  show = genericShow
