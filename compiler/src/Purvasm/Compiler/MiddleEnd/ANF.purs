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
  | CUpdate Atom (Array { prop :: String, val :: Atom })
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

-- | Rewrite every `Atom` occurrence in an expression (operands, scrutinees, ctor/record/array fields),
-- | recursing through nested `Expr`s (`CLam`/`CIf`/`CCase` bodies, the `Let`/`LetRec` spine, guard/rhs
-- | clauses). A structure-preserving map: the shape is untouched, only atoms are transformed. Tree
-- | recursion, bounded by control-flow/binding nesting (like the middle-end passes).
mapAtoms :: (Atom -> Atom) -> Expr -> Expr
mapAtoms f = goE
  where
  goE = case _ of
    Ret c -> Ret (goC c)
    Let x c e -> Let x (goC c) (goE e)
    LetRec bs e -> LetRec (map (\b -> b { rhs = goE b.rhs }) bs) (goE e)

  goC = case _ of
    CAtom a -> CAtom (f a)
    CLam ps e -> CLam ps (goE e)
    CApp a as -> CApp (f a) (map f as)
    CPrim op as -> CPrim op (map f as)
    CCtor n ar as -> CCtor n ar (map f as)
    CArray as -> CArray (map f as)
    CRecord fs -> CRecord (map (\r -> r { val = f r.val }) fs)
    CAccessor a l -> CAccessor (f a) l
    CUpdate a ups -> CUpdate (f a) (map (\r -> r { val = f r.val }) ups)
    CIf a t e -> CIf (f a) (goE t) (goE e)
    CCase as alts -> CCase (map f as) (map goAlt alts)

  goAlt alt = alt { result = goRhs alt.result }

  goRhs = case _ of
    Uncond e -> Uncond (goE e)
    Guarded gs -> Guarded (map (\g -> { guard: goE g.guard, rhs: goE g.rhs }) gs)
