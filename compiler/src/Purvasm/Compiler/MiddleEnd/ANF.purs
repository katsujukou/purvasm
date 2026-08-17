-- The lower IR (ADR-0025): A-normal form　i.e. every argument is an *atom* and every compound
-- subexpression is [let]-named, so evaluation order is explicit; functions and
-- calls are *uncurried* (eval/apply: a call carries all its arguments at once) —
-- the substrate the optimiser and a future stack-machine codegen need.

module Purvasm.Compiler.MiddleEnd.ANF where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
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
  -- | Run an `Effect`/`ST` thunk (GER, ADR-0099): `CPerform t ≃ CApp t [unit]`, but kept
  -- | **distinct** on the optimiser seam as an explicit run marker so the head-based purity
  -- | analysis never loses which thunk gets performed. Backends lower it to the unit application.
  | CPerform Atom

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

-- | One item of [`foldAtoms`]' explicit work stack — the three node kinds its walk descends into.
data AtomWork
  = WkE Expr
  | WkC CExpr
  | WkR Rhs

-- | Fold over every `Atom` occurrence, in source order (operands, scrutinees, ctor/record/array
-- | fields, and through every nested `Expr`).
-- |
-- | **Stack-safe by construction**: an explicit work stack under `tailRec`, not spine recursion. This
-- | walk is load-bearing — ADR-0109 §2.2-amended derives the entry object's hoisted-cell set from it,
-- | so an overflow on a deep generated `Let` spine would take out a whole build, and a MISSED atom
-- | would leave live module code reading a cell the entry never defined (a link error at best).
-- |
-- | It is an INDEPENDENT case tree from [`mapAtoms`], not a shared traversal: the map rebuilds and
-- | this one accumulates, and PureScript gives no way to write one over the other that stays
-- | stack-safe. The two are held in agreement by the per-node fidelity matrix in the unit tests
-- | (every constructor, a distinct atom in every atom position, an exact expected list) — a field
-- | dropped from one tree and not the other is what that matrix exists to catch.
foldAtoms :: forall a. (a -> Atom -> a) -> a -> Expr -> a
foldAtoms f z0 e0 = tailRec step { acc: z0, work: WkE e0 : Nil }
  where
  -- children are pushed onto the FRONT in source order, so the walk stays left-to-right
  -- depth-first — the order in which the lowering would meet the same atoms.
  -- Stack-safe in WIDTH as well as depth: `Data.Foldable.foldl` over an `Array` is the FFI loop,
  -- so a 100k-binding `LetRec`, a 100k-arm `CCase` or a 100k-clause guard pushes without touching
  -- the host stack. (Reversed first, so the items still land in source order at the front.)
  push :: Array AtomWork -> List AtomWork -> List AtomWork
  push items rest = foldl (flip Cons) rest (Array.reverse items)

  step st = case st.work of
    Nil -> Done st.acc
    Cons w rest -> case w of
      WkE (Ret c) -> Loop { acc: st.acc, work: WkC c : rest }
      WkE (Let _ c b) -> Loop { acc: st.acc, work: WkC c : WkE b : rest }
      WkE (LetRec bs b) -> Loop { acc: st.acc, work: push (map (\bd -> WkE bd.rhs) bs) (WkE b : rest) }
      WkC c -> case c of
        CAtom a -> Loop { acc: f st.acc a, work: rest }
        CLam _ e -> Loop { acc: st.acc, work: WkE e : rest }
        CApp a as -> Loop { acc: foldl f (f st.acc a) as, work: rest }
        CPrim _ as -> Loop { acc: foldl f st.acc as, work: rest }
        CCtor _ _ as -> Loop { acc: foldl f st.acc as, work: rest }
        CArray as -> Loop { acc: foldl f st.acc as, work: rest }
        CRecord fs -> Loop { acc: foldl (\a r -> f a r.val) st.acc fs, work: rest }
        CAccessor a _ -> Loop { acc: f st.acc a, work: rest }
        CUpdate a ups -> Loop { acc: foldl (\a' r -> f a' r.val) (f st.acc a) ups, work: rest }
        CIf a t e -> Loop { acc: f st.acc a, work: WkE t : WkE e : rest }
        CCase as alts -> Loop { acc: foldl f st.acc as, work: push (map (WkR <<< _.result) alts) rest }
        CPerform a -> Loop { acc: f st.acc a, work: rest }
      WkR (Uncond e) -> Loop { acc: st.acc, work: WkE e : rest }
      WkR (Guarded gs) -> Loop { acc: st.acc, work: push (gs >>= \g -> [ WkE g.guard, WkE g.rhs ]) rest }

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
    CPerform a -> CPerform (f a)

  goAlt alt = alt { result = goRhs alt.result }

  goRhs = case _ of
    Uncond e -> Uncond (goE e)
    Guarded gs -> Guarded (map (\g -> { guard: goE g.guard, rhs: goE g.rhs }) gs)
