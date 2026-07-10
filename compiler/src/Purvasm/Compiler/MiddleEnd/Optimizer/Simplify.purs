-- | Copy-propagation and small-callee inlining (ADR-0028) — the first `--opt` pass, ported
-- | from boot's `Simplify` (`middle_end/passes/simplify.ml`) to the Level-2 ANF. Three
-- | scope-respecting, dependency-directed rewrites, applied with an environment threaded down
-- | the term to a bounded fixpoint:
-- |
-- |   * copy-propagation — a `let x = <atom> in …` is dropped and `x` resolved to the atom (no
-- |     work duplicated, no capture); this clears trivial aliases (e.g. DictElim's `$x = intAdd`).
-- |   * saturated inlining — a call `f a₁…aₙ` whose binding is `\p₁…pₙ -> Ret c` with `c` *flat*
-- |     (no nested lambda/if/case) and *closed but for its parameters*, exactly saturated,
-- |     becomes `c` with each `pᵢ` replaced by `aᵢ`.
-- |   * intrinsic-foreign saturation — a call `f a₁…aₙ` whose head resolves to a *foreign* key
-- |     the intrinsic rung eta-expands to an arity-`n` primop becomes `Prim(op, a₁…aₙ)`. Boot's
-- |     whole-program (B1) `Simplify` got this for free — the linker had already resolved the
-- |     foreign into an eta-primop binding it then inlined; under B2 separate compilation the
-- |     eta body is a link-time binding this module-local pass never sees, so the compiler-global
-- |     intrinsic table (`Ffi.intrinsicPrim`, injected as `IntrinsicLookup`) restores exactly
-- |     that rewrite. Under-/over-applied intrinsic calls are left for the link-time closure.
-- |
-- | The decision is the callee's own shape (or the global intrinsic table), never who calls it,
-- | so the pass stays module-local ([[optimizer-modular-not-whole-program]]). Semantics-preserving:
-- | gated by `--opt ≡ --no-opt ≡ oracle` (ADR-0082 §2), not byte-identity with boot.
-- |
-- | The pass is **module-scoped, not body-scoped**: `purs` floats a method's dictionary application
-- | to its own top-level binding (`add1 = add semiringInt`), so the alias/inlinable facts a call
-- | site needs live in *sibling* bindings (ADR-0086 §Context: an optimiser must see sibling
-- | bindings). `moduleKnown` computes those facts from a module's (already `DictElim`'d) top-level
-- | members; `run` takes them as its outer environment. They are known-body facts — recomputed
-- | from the current module state each `optimizeModule` iteration, never carried in
-- | `LocalFacts`/`BuildEnv` (ADR-0086 §3).
module Purvasm.Compiler.MiddleEnd.Optimizer.Simplify
  ( IntrinsicLookup
  , Known
  , moduleKnown
  , run
  ) where

import Prelude

import Data.Array (length, mapMaybe, zip) as Array
import Data.Foldable (all, elem, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), Alt, CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.Primitive (PrimOp)

-- | The compile-time view of the intrinsic foreign rung (`Ffi.intrinsicPrim`, injected so this
-- | pass stays table-agnostic and directly testable): the primop an eta-expanded foreign key
-- | denotes, with its arity.
type IntrinsicLookup = String -> Maybe { op :: PrimOp, arity :: Int }

-- | What a name is known to denote in the current scope: a copy-propagation alias to an atom,
-- | or a flat, inlinable function body with its parameter names. Abstract outside this module —
-- | facts enter via `moduleKnown` (top-level) or are collected by the walk itself (locals).
data Known
  = KAlias Atom
  | KFun (Array String) CExpr

-- | The module-level facts (the sibling-binding channel): what each **non-recursive** top-level
-- | member denotes, by the same shapes the local walk registers — an atom body (`add1 = intAdd`,
-- | the floated dictionary application after `DictElim`) becomes an alias; a flat,
-- | parameter-closed lambda becomes an inlinable callee. Everything a top-level body mentions is
-- | itself top-level (globally in scope), so lifting these facts to any call site cannot capture.
-- | Recursive-group members are conservatively excluded (their force-cell/by-need semantics must
-- | not be short-circuited by an alias).
moduleKnown :: Array { recursive :: Boolean, members :: Array (String /\ Expr) } -> Map String Known
moduleKnown decls = Map.fromFoldable (Array.mapMaybe known (nonrecMembers decls))
  where
  nonrecMembers ds = ds >>= \d -> if d.recursive then [] else d.members

  known (Tuple k e) = case e of
    Ret (CAtom a) -> Just (Tuple k (KAlias a))
    Ret (CLam params (Ret cf)) | inlinable params cf -> Just (Tuple k (KFun params cf))
    _ -> Nothing

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

rwExpr :: IntrinsicLookup -> Map String Known -> Expr -> Expr
rwExpr intr env = case _ of
  Ret c -> Ret (rwCExpr intr env c)
  Let x c body ->
    let
      c' = rwCExpr intr env c
    in
      case c' of
        -- copy-propagation: drop the binding, resolve x to the atom.
        CAtom a -> rwExpr intr (Map.insert x (KAlias (resolveAtom env a)) env) body
        -- a flat, saturable callee: keep its binding, register it for inlining at call sites.
        CLam params (Ret cf)
          | inlinable params cf -> Let x c' (rwExpr intr (Map.insert x (KFun params cf) env) body)
        -- x is re-bound to a non-inlinable computation: it must shadow any outer entry.
        _ -> Let x c' (rwExpr intr (Map.delete x env) body)
  LetRec binds body ->
    -- The bound names shadow any outer entries in both the members and the body; recursive
    -- members are conservatively never registered as inlinable.
    let
      env' = removeAll (map _.var binds) env
    in
      LetRec (map (\b -> b { rhs = rwExpr intr env' b.rhs }) binds) (rwExpr intr env' body)

rwCExpr :: IntrinsicLookup -> Map String Known -> CExpr -> CExpr
rwCExpr intr env = case _ of
  CAtom a -> CAtom (resolveAtom env a)
  CApp h args ->
    let
      args' = map (resolveAtom env) args
    in
      case resolveAtom env h of
        AtomVar g -> case Map.lookup g env of
          Just (KFun params cf) | Array.length params == Array.length args' ->
            subst (Map.fromFoldable (Array.zip params args')) cf
          -- intrinsic-foreign saturation, the AtomVar spelling: a cross-module reference to a
          -- foreign (`Purvasm.Int.add` from an overlaid instance) rides a plain qualified key.
          -- No shadow risk: intrinsic keys are dotted qualified names, which no local binder can
          -- be — and a tracked local (`Just`) took the branch above.
          Nothing
            | Just { op, arity } <- intr g
            , arity == Array.length args' -> CPrim op args'
          _ -> CApp (AtomVar g) args'
        -- intrinsic-foreign saturation: the eta-primop the linker would bind, collapsed here.
        AtomForeign k
          | Just { op, arity } <- intr k
          , arity == Array.length args' -> CPrim op args'
        h' -> CApp h' args'
  CPrim op args -> CPrim op (map (resolveAtom env) args)
  CCtor t n args -> CCtor t n (map (resolveAtom env) args)
  CArray args -> CArray (map (resolveAtom env) args)
  CRecord fs -> CRecord (map (\f -> f { val = resolveAtom env f.val }) fs)
  CAccessor a l -> CAccessor (resolveAtom env a) l
  CUpdate a ups -> CUpdate (resolveAtom env a) (map (\u -> u { val = resolveAtom env u.val }) ups)
  CLam ps body -> CLam ps (rwExpr intr (removeAll ps env) body)
  CIf a t e -> CIf (resolveAtom env a) (rwExpr intr env t) (rwExpr intr env e)
  CCase ats alts -> CCase (map (resolveAtom env) ats) (map (rwAlt intr env) alts)

rwAlt :: IntrinsicLookup -> Map String Known -> Alt -> Alt
rwAlt intr env a =
  -- The alternative's binders shadow any outer entries in the result/guards.
  let
    env' = removeAll (a.binders >>= binderVars) env
  in
    a { result = rwRhs intr env' a.result }

rwRhs :: IntrinsicLookup -> Map String Known -> Rhs -> Rhs
rwRhs intr env = case _ of
  Uncond e -> Uncond (rwExpr intr env e)
  Guarded gs -> Guarded (map (\g -> { guard: rwExpr intr env g.guard, rhs: rwExpr intr env g.rhs }) gs)

-- | Run the three rewrites over one binding body to a bounded fixpoint, under the module-level
-- | facts (`moduleKnown`; `Map.empty` for a body-only run): a flat inlined body may expose a
-- | further inline, but in practice one or two passes suffice (eta-primops are non-recursive);
-- | the cap bounds any pathological chain. Convergence is detected structurally (a fired rewrite
-- | always changes the term).
run :: IntrinsicLookup -> Map String Known -> Expr -> Expr
run intr known = loop 5
  where
  loop n e
    | n <= 0 = e
    | otherwise =
        let
          e' = rwExpr intr known e
        in
          if e' == e then e' else loop (n - 1) e'
