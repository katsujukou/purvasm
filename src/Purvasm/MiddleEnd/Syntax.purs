module Purvasm.MiddleEnd.Syntax where

-- This module defines the syntax of *ELambda*, the first
-- intermediate representation (IR).

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Purvasm.MiddleEnd.Types (Var)
import Purvasm.Primitives (Primitive)
import Purvasm.Types (Arity, Ident, ModuleName, StructuredConstant)

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
