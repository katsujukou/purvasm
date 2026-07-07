-- | Copy-propagation and small-callee inlining (ADR-0028) — the first `--opt` pass, ported
-- | from boot's `Simplify` (`middle_end/passes/simplify.ml`) to the Level-2 ANF. Two
-- | scope-respecting, dependency-directed rewrites, applied with an environment threaded down
-- | the term to a bounded fixpoint:
-- |
-- |   * copy-propagation — a `let x = <atom> in …` is dropped and `x` resolved to the atom (no
-- |     work duplicated, no capture); this clears trivial aliases (e.g. DictElim's `$x = intAdd`).
-- |   * saturated inlining — a call `f a₁…aₙ` whose binding is `\p₁…pₙ -> Ret c` with `c` *flat*
-- |     (no nested lambda/if/case) and *closed but for its parameters*, exactly saturated,
-- |     becomes `c` with each `pᵢ` replaced by `aᵢ`.
-- |
-- | The decision is the callee's own shape, never who calls it, so the pass stays module-local
-- | ([[optimizer-modular-not-whole-program]]). Semantics-preserving: gated by
-- | `--opt ≡ --no-opt ≡ oracle` (ADR-0082 §2), not byte-identity with boot.
module Purvasm.Compiler.MiddleEnd.Optimizer.Simplify (run) where

import Prelude

import Data.Array (length, zip) as Array
import Data.Foldable (all, elem, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), Alt, CExpr(..), Expr(..), Rhs(..))

-- | What a name is known to denote in the current scope: a copy-propagation alias to an atom,
-- | or a flat, inlinable function body with its parameter names.
data Known
  = KAlias Atom
  | KFun (Array String) CExpr

-- | The variables a binder introduces (its shadowing set).
binderVars :: Binder -> Array String
binderVars = case _ of
  BNull -> []
  BLit _ -> []
  BVar x -> [ x ]
  BNamed x b -> [ x ] <> binderVars b
  BCtor _ bs -> bs >>= binderVars
  BArray bs -> bs >>= binderVars
  BRecord fs -> fs >>= \f -> binderVars f.binder

-- | Drop `names` from the environment: a name re-bound in an inner scope must not resolve to a
-- | now-shadowed outer alias/fun (a scope bug that only surfaces once name reuse appears).
removeAll :: forall a. Array String -> Map String a -> Map String a
removeAll names env = foldr Map.delete env names

-- | A flat computation: small, with no nested scope. Only a flat body is ever inlined.
isFlat :: CExpr -> Boolean
isFlat = case _ of
  CLam _ _ -> false
  CIf _ _ _ -> false
  CCase _ _ -> false
  _ -> true

atomVar :: Atom -> Array String
atomVar = case _ of
  AtomVar x -> [ x ]
  _ -> []

-- | The variables a flat computation refers to (atom positions only).
avars :: CExpr -> Array String
avars = case _ of
  CAtom a -> atomVar a
  CApp h args -> atomVar h <> (args >>= atomVar)
  CPrim _ args -> args >>= atomVar
  CArray args -> args >>= atomVar
  CCtor _ _ args -> args >>= atomVar
  CRecord fs -> fs >>= \f -> atomVar f.val
  CAccessor a _ -> atomVar a
  CUpdate a ups -> atomVar a <> (ups >>= \u -> atomVar u.val)
  CLam _ _ -> []
  CIf _ _ _ -> []
  CCase _ _ -> []

-- | Inlinable iff flat and closed but for its parameters, so substituting it at any call site
-- | cannot be captured by a shadowing binder there. Conservative (a free top-level reference
-- | would also be safe), but it covers the eta-expanded primops, the v1 target.
inlinable :: Array String -> CExpr -> Boolean
inlinable params c = isFlat c && all (\v -> elem v params) (avars c)

-- | Follow copy-propagation aliases to the underlying atom.
resolveAtom :: Map String Known -> Atom -> Atom
resolveAtom env a = case a of
  AtomVar x -> case Map.lookup x env of
    Just (KAlias b) -> resolveAtom env b
    _ -> a
  _ -> a

-- | Substitute parameter atoms into a flat inlined body (atom positions only).
subst :: Map String Atom -> CExpr -> CExpr
subst m = go
  where
  s a = case a of
    AtomVar x -> fromMaybe a (Map.lookup x m)
    _ -> a
  go = case _ of
    CAtom a -> CAtom (s a)
    CApp h args -> CApp (s h) (map s args)
    CPrim op args -> CPrim op (map s args)
    CCtor t n args -> CCtor t n (map s args)
    CArray args -> CArray (map s args)
    CRecord fs -> CRecord (map (\f -> f { val = s f.val }) fs)
    CAccessor a l -> CAccessor (s a) l
    CUpdate a ups -> CUpdate (s a) (map (\u -> u { val = s u.val }) ups)
    -- not flat: never an inlined body, kept verbatim.
    c@(CLam _ _) -> c
    c@(CIf _ _ _) -> c
    c@(CCase _ _) -> c

rwExpr :: Map String Known -> Expr -> Expr
rwExpr env = case _ of
  Ret c -> Ret (rwCExpr env c)
  Let x c body ->
    let
      c' = rwCExpr env c
    in
      case c' of
        -- copy-propagation: drop the binding, resolve x to the atom.
        CAtom a -> rwExpr (Map.insert x (KAlias (resolveAtom env a)) env) body
        -- a flat, saturable callee: keep its binding, register it for inlining at call sites.
        CLam params (Ret cf)
          | inlinable params cf -> Let x c' (rwExpr (Map.insert x (KFun params cf) env) body)
        -- x is re-bound to a non-inlinable computation: it must shadow any outer entry.
        _ -> Let x c' (rwExpr (Map.delete x env) body)
  LetRec binds body ->
    -- The bound names shadow any outer entries in both the members and the body; recursive
    -- members are conservatively never registered as inlinable.
    let
      env' = removeAll (map _.var binds) env
    in
      LetRec (map (\b -> b { rhs = rwExpr env' b.rhs }) binds) (rwExpr env' body)

rwCExpr :: Map String Known -> CExpr -> CExpr
rwCExpr env = case _ of
  CAtom a -> CAtom (resolveAtom env a)
  CApp h args ->
    let
      args' = map (resolveAtom env) args
    in
      case resolveAtom env h of
        AtomVar g -> case Map.lookup g env of
          Just (KFun params cf) | Array.length params == Array.length args' ->
            subst (Map.fromFoldable (Array.zip params args')) cf
          _ -> CApp (AtomVar g) args'
        h' -> CApp h' args'
  CPrim op args -> CPrim op (map (resolveAtom env) args)
  CCtor t n args -> CCtor t n (map (resolveAtom env) args)
  CArray args -> CArray (map (resolveAtom env) args)
  CRecord fs -> CRecord (map (\f -> f { val = resolveAtom env f.val }) fs)
  CAccessor a l -> CAccessor (resolveAtom env a) l
  CUpdate a ups -> CUpdate (resolveAtom env a) (map (\u -> u { val = resolveAtom env u.val }) ups)
  CLam ps body -> CLam ps (rwExpr (removeAll ps env) body)
  CIf a t e -> CIf (resolveAtom env a) (rwExpr env t) (rwExpr env e)
  CCase ats alts -> CCase (map (resolveAtom env) ats) (map (rwAlt env) alts)

rwAlt :: Map String Known -> Alt -> Alt
rwAlt env a =
  -- The alternative's binders shadow any outer entries in the result/guards.
  let
    env' = removeAll (a.binders >>= binderVars) env
  in
    a { result = rwRhs env' a.result }

rwRhs :: Map String Known -> Rhs -> Rhs
rwRhs env = case _ of
  Uncond e -> Uncond (rwExpr env e)
  Guarded gs -> Guarded (map (\g -> { guard: rwExpr env g.guard, rhs: rwExpr env g.rhs }) gs)

-- | Run the two rewrites to a bounded fixpoint: a flat inlined body may expose a further
-- | inline, but in practice one or two passes suffice (eta-primops are non-recursive); the cap
-- | bounds any pathological chain. Convergence is detected structurally (a fired rewrite always
-- | changes the term).
run :: Expr -> Expr
run = loop 5
  where
  loop n e
    | n <= 0 = e
    | otherwise =
        let
          e' = rwExpr Map.empty e
        in
          if e' == e then e' else loop (n - 1) e'
