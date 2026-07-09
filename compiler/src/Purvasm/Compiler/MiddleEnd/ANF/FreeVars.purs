-- | Free-variable and referenced-foreign-key analyses over the backend-neutral ANF. Pure middle-end
-- | analysis (no backend vocabulary), shared by every backend: lambda-lifting and native-foreign
-- | resolution on the LLVM side (ADR-0072 §4 / ADR-0073 §3), and per-decl `deps`/foreign accumulation on
-- | the bytecode side (ADR-0088 §2 — the reachability edges are the free **global** refs `fvExpr` returns;
-- | the required foreigns are `cfExpr`). A faithful transcription of boot's `binder_vars`/`fv_*`/`cf_*`.
-- |
-- | Stack-safety note: the array folds are `foldl` (safe), but the `Expr` tree recursion is ordinary
-- | structural recursion, so a very deep ANF `Let` spine recurses to its length. The values are pure
-- | `Set`s (no bearing on emitted-artifact bytes), so an iterative-spine rewrite is a result-preserving
-- | hardening deferred until real large modules exercise it.
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

import Data.Foldable (foldl)
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

-- | An expression's free variables (a `Let`/`LetRec`/`CLam`/binder adds to `bound`).
fvExpr :: Set String -> Expr -> Set String
fvExpr bound = case _ of
  Ret c -> fvCexpr bound c
  Let x c body -> Set.union (fvCexpr bound c) (fvExpr (Set.insert x bound) body)
  LetRec binds body ->
    let
      bound' = foldl (\s b -> Set.insert b.var s) bound binds
      rhs = foldl (\a b -> Set.union a (fvExpr bound' b.rhs)) Set.empty binds
    in
      Set.union rhs (fvExpr bound' body)

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
cfExpr :: Expr -> Set String
cfExpr = case _ of
  Ret c -> cfCexpr c
  Let _ c body -> Set.union (cfCexpr c) (cfExpr body)
  LetRec binds body -> foldl (\a b -> Set.union a (cfExpr b.rhs)) (cfExpr body) binds

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
