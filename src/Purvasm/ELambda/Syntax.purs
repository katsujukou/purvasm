module Purvasm.ELambda.Syntax where

-- This module defines the syntax of *ELambda*, the first
-- intermediate representation (IR).

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Purvasm.ELambda.Types (Var)
import Purvasm.Primitives (Primitive)
import Purvasm.Types (Arity, Ident, ModuleName, StructuredConstant)

newtype Program = Program
  { name :: ModuleName
  , decls :: Array Declaration
  }

derive instance Newtype Program _
instance Show Program where
  show (Program m) = "(Program " <> show m <> ")"

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
