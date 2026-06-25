-- The lower IR (ADR-0025): A-normal form　i.e. every argument is an *atom* and every compound
-- subexpression is [let]-named, so evaluation order is explicit; functions and
-- calls are *uncurried* (eval/apply: a call carries all its arguments at once) —
-- the substrate the optimiser and a future stack-machine codegen need.

module Purvasm.Compiler.MiddleEnd.ANF where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Purvasm.Compiler.Binder (Binder)
import Purvasm.Compiler.Literal (Literal)
import Purvasm.Compiler.Primitive (PrimOp)

data Atom 
  = AtomVar String
  | AtomLit Literal
  | AtomForeign String

derive instance Eq Atom
derive instance Generic Atom _
instance Show Atom where
  show = genericShow

-- | A computation - a single step that produces a value
-- | Its operants are atoms; its sub-*expressions* (`CIf`/`CCase` branches, `CLam` body) are full `Expr`s.
data CExpr 
  = CAtom Atom 
  | CLam (Array String) Expr 
  | CApp Atom (Array Atom)
  | CPrim PrimOp (Array Atom)
  | CCtor String Int (Array Atom) -- name of constructor, arity, args 
  | CArray (Array Atom)
  | CRecord (Array { prop :: String, val :: Atom })
  | CAccessor Atom String
  | CUpdate Atom (Array { prop :: String, val :: Atom})
  | CIf Atom Expr Expr
  | CCase (Array Atom) (Array Alt)

-- | A let-sequence ending in a tail computation.
-- | `Let` binds a (non-recursive) computation;
-- | `LetRec` a recursive group (each rhs a full `Expr`,
-- | since its internal bindings may reference the group and cannot bt hoisted
data Expr 
  = Ret CExpr
  | Let String CExpr Expr 
  | LetRec (Array { var :: String, rhs :: Expr }) Expr 
  
type Alt =
  { binders :: Array Binder
  , result :: Rhs
  } 

data Rhs
  = Uncond Expr
  | Guarded (Array { guard :: Expr, rhs :: Expr })

derive instance Eq CExpr
derive instance Generic CExpr _
instance Show CExpr where
  show c = genericShow c

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  show e = genericShow e

derive instance Eq Rhs
derive instance Generic Rhs _
instance Show Rhs where
  show r = genericShow r
