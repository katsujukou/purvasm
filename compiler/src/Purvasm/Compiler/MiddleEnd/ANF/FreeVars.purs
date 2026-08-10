-- | Free-variable and referenced-foreign-key analyses over the backend-neutral ANF. Pure middle-end
-- | analysis (no backend vocabulary), shared by every backend: lambda-lifting and native-foreign
-- | resolution on the LLVM side (ADR-0072 §4 / ADR-0073 §3), and per-decl `deps`/foreign accumulation on
-- | the bytecode side (ADR-0088 §2 — the reachability edges are the free **global** refs `fvExpr` returns;
-- | the required foreigns are `cfExpr`). A faithful transcription of boot's `binder_vars`/`fv_*`/`cf_*`.
-- |
-- | Stack-safety: the array folds are `foldl` (safe), and the `Let`/`LetRec` SPINE is walked with a
-- | pure `tailRec` loop and folded back with a stack-safe `foldl`, so recursion depth is control-flow
-- | nesting only, never spine length. (This was a deferred hardening — "until real large modules
-- | exercise it" — and ADR-0107 slice 1 exercised it: `activationFacts` calls `fvExpr` on every
-- | activation body, and the self-host corpus carries `Let` spines thousands of bindings long, which
-- | overflowed the host stack immediately. Set union is commutative and idempotent, so the rewritten
-- | traversal order returns the same sets.)
module Purvasm.Compiler.MiddleEnd.ANF.FreeVars
  ( binderVars
  , fvAtom
  , fvAtoms
  , fvExpr
  , fvCexpr
  , fvAlt
  , cfExpr
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Set (Set)
import Data.Set as Set
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))

-- | The variables a binder introduces (ADR-0011/0012).
binderVars :: Binder -> Set String
binderVars = case _ of
  BNull -> Set.empty
  BLit _ -> Set.empty
  BVar x -> Set.singleton x
  BNamed x i -> Set.insert x (binderVars i)
  BCtor _ subs -> foldl (\a b -> Set.union a (binderVars b)) Set.empty subs
  BArray subs -> foldl (\a b -> Set.union a (binderVars b)) Set.empty subs
  BRecord fs -> foldl (\a f -> Set.union a (binderVars f.binder)) Set.empty fs

-- | An atom's free variables, minus those already `bound`.
fvAtom :: Set String -> Atom -> Set String
fvAtom bound = case _ of
  AtomVar x -> if Set.member x bound then Set.empty else Set.singleton x
  AtomLit _ -> Set.empty
  AtomForeign _ -> Set.empty

-- | The union of a list of atoms' free variables.
fvAtoms :: Set String -> Array Atom -> Set String
fvAtoms bound = foldl (\a x -> Set.union a (fvAtom bound x)) Set.empty

-- | One collected spine step, with the `bound` set in force AT that step (it grows as the walk
-- | descends, so each step's contribution must be computed under its own).
data SpineStep
  = SLet (Set String) CExpr
  | SLetRec (Set String) (Array { var :: String, rhs :: Expr })

-- | An expression's free variables (a `Let`/`LetRec`/`CLam`/binder adds to `bound`). The spine is
-- | iterative (see the module note); branch/lambda nesting recurses as before.
fvExpr :: Set String -> Expr -> Set String
fvExpr bound0 e0 =
  let
    spine = tailRec
      ( \st -> case st.e of
          Let x c rest ->
            Loop { e: rest, bound: Set.insert x st.bound, acc: SLet st.bound c : st.acc }
          LetRec binds rest ->
            let
              bound' = foldl (\s b -> Set.insert b.var s) st.bound binds
            in
              Loop { e: rest, bound: bound', acc: SLetRec bound' binds : st.acc }
          Ret c -> Done { steps: st.acc, tail: fvCexpr st.bound c }
      )
      { e: e0, bound: bound0, acc: Nil }
  in
    foldl step spine.tail spine.steps
  where
  step acc = case _ of
    SLet b c -> Set.union acc (fvCexpr b c)
    SLetRec b binds -> foldl (\a r -> Set.union a (fvExpr b r.rhs)) acc binds

-- | A computation's free variables.
fvCexpr :: Set String -> CExpr -> Set String
fvCexpr bound = case _ of
  CAtom a -> fvAtom bound a
  CLam ps b -> fvExpr (foldl (\s p -> Set.insert p s) bound ps) b
  CApp f args -> Set.union (fvAtom bound f) (fvAtoms bound args)
  CPrim _ args -> fvAtoms bound args
  CArray args -> fvAtoms bound args
  CCtor _ _ args -> fvAtoms bound args
  CRecord fs -> fvAtoms bound (map _.val fs)
  CAccessor a _ -> fvAtom bound a
  CUpdate a fs -> Set.union (fvAtom bound a) (fvAtoms bound (map _.val fs))
  CIf a t e -> Set.union (fvAtom bound a) (Set.union (fvExpr bound t) (fvExpr bound e))
  CPerform a -> fvAtom bound a
  CCase scruts alts ->
    foldl (\acc alt -> Set.union acc (fvAlt bound alt)) (fvAtoms bound scruts) alts

-- | An alternative's free variables (its binders bind within its result/guards).
fvAlt :: Set String -> Alt -> Set String
fvAlt bound alt =
  let
    bvs = foldl (\a b -> Set.union a (binderVars b)) Set.empty alt.binders
    bound' = Set.union bound bvs
  in
    case alt.result of
      Uncond e -> fvExpr bound' e
      Guarded gs ->
        foldl
          (\acc g -> Set.union acc (Set.union (fvExpr bound' g.guard) (fvExpr bound' g.rhs)))
          Set.empty
          gs

-- | Every foreign key an expression references (a superset over dead bindings is harmless — the
-- | dead-strip link drops an unreferenced leaf, ADR-0073 §3).
-- | Spine-iterative for the same reason as `fvExpr` (the module note): a self-host `Let` spine is
-- | thousands of bindings long.
cfExpr :: Expr -> Set String
cfExpr e0 =
  let
    spine = tailRec
      ( \st -> case st.e of
          Let _ c rest -> Loop { e: rest, acc: Left c : st.acc }
          LetRec binds rest -> Loop { e: rest, acc: Right binds : st.acc }
          Ret c -> Done { steps: st.acc, tail: cfCexpr c }
      )
      { e: e0, acc: Nil }
  in
    foldl step spine.tail spine.steps
  where
  step acc = case _ of
    Left c -> Set.union acc (cfCexpr c)
    Right binds -> foldl (\a r -> Set.union a (cfExpr r.rhs)) acc binds

cfCexpr :: CExpr -> Set String
cfCexpr = case _ of
  CAtom a -> cfAtom a
  CLam _ b -> cfExpr b
  CApp f args -> foldl (\s a -> Set.union s (cfAtom a)) (cfAtom f) args
  CPrim _ args -> cfAtoms args
  CArray args -> cfAtoms args
  CCtor _ _ args -> cfAtoms args
  CRecord fs -> foldl (\s f -> Set.union s (cfAtom f.val)) Set.empty fs
  CUpdate a0 fs -> foldl (\s f -> Set.union s (cfAtom f.val)) (cfAtom a0) fs
  CAccessor a _ -> cfAtom a
  CIf a t e -> Set.union (cfAtom a) (Set.union (cfExpr t) (cfExpr e))
  CPerform a -> cfAtom a
  CCase scruts alts -> foldl (\s alt -> Set.union s (cfAlt alt)) (cfAtoms scruts) alts

cfAtom :: Atom -> Set String
cfAtom = case _ of
  AtomForeign k -> Set.singleton k
  AtomVar _ -> Set.empty
  AtomLit _ -> Set.empty

cfAtoms :: Array Atom -> Set String
cfAtoms = foldl (\s a -> Set.union s (cfAtom a)) Set.empty

cfAlt :: Alt -> Set String
cfAlt alt = case alt.result of
  Uncond e -> cfExpr e
  Guarded gs ->
    foldl (\s g -> Set.union s (Set.union (cfExpr g.guard) (cfExpr g.rhs))) Set.empty gs
