module Purvasm.MiddleEnd.ECF.Syntax where

import Prelude

import Data.Tuple (Tuple)
import Purvasm.Types (Arity, GlobalName, Ident(..), ModuleName(..), ConstructorTag)

newtype Module a = Module
  { name :: ModuleName
  , decls :: Array (Binding a)
  }

data Bind a
  = NonRec (Binding a)
  | Rec (Array (Binding a))

data Binding a = Binding Ident (Expr a)

data Expr a
  = ExprLit a (Literal a)
  | ExprVar a Ident
  | ExprGlobal a GlobalName
  | ExprAbs a (Array Ident) (Expr a)
  | ExprApp a (Expr a) (Array (Expr a))
  | ExprAccess a (Expr a) String
  | ExprUpdate a (Expr a) (Array (Tuple String (Expr a)))
  | ExprLookup a (Expr a) String
  | ExprGetField (Expr a) Int
  | ExprIf a (Expr a) (Expr a) (Expr a)
  | ExprCase a (Array (Expr a)) (Array (CaseAlternative a))
  | ExprStaticFail a
  | ExprstaticHandle a (Expr a) (Expr a)

newtype CaseAlternative a = CaseAlternative
  { patterns :: Array Pattern
  , action :: Expr a
  }

data Pattern
  = PatWildcard
  | PatVar Ident
  | PatLiteral AtomicLiteral
  | PatArray (Array Pattern)
  | PatRecord (Array (Prop Pattern))
  | PatConstruct Constructor (Array Pattern)

data Literal a
  = LitAtomic AtomicLiteral
  | LitStruct (StructuredLiteral a)

data AtomicLiteral
  = LitInt Int
  | LitBoolean Boolean
  | LitChar Char
  | LitString String
  | LitUnit

data StructuredLiteral a
  = LitArray (Array a)
  | LitRecord (Array (Prop a))
  | LitConstructor a Constructor (Array (Literal a))

newtype Constructor = Constructor
  { name :: GlobalName
  , arity :: Arity
  , tag :: ConstructorTag
  }

data Prop a = Prop String a
