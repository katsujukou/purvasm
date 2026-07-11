-- | The NbE general inliner's seam entry (ADR-0089 §1): `nbeBinding` runs the per-binding
-- | `quote ∘ eval` inner fixpoint — evaluate under the previous round's gate-B marks, quote,
-- | re-analyse, repeat until the term and the marks are stable — bounded by a rewrite fuel whose
-- | exhaustion is a **loud crash**, never silent truncation. `candidatesOf` is the slice-2 publish
-- | predicate; `nbeEnvOf` assembles the gate-site-A extern entries from the dependency candidates
-- | (`BuildEnv.inlines`) plus the module's own siblings.
-- |
-- | Publication is deliberately **shallow**: published bodies evaluate a bounded number of levels
-- | deep (the layered `nbeEnvOf` construction — dependencies see each other one link, locals see
-- | dependencies), so unfolding cannot chase arbitrary chains within a single round — the driver's
-- | outer `optimizeModule` fixpoint (ADR-0087) resolves local chains one link per round, deeper
-- | cross-module chains degrade to neutral calls, and the construction stays acyclic (no knot to
-- | tie, and a multi-reference body is still evaluated at most once per round via its `Lazy`).
-- | Recursive decls are never published (a recursive reference stays a call — ADR-0089 §5), and a
-- | non-recursive binding cannot reference itself, so no self-stop is needed.
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe
  ( candidatesOf
  , nbeBinding
  , nbeEnvOf
  ) where

import Prelude

import Data.Array as Array
import Data.Lazy (defer)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Ffi (resolver)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Analysis (inlineMarks, sizeExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Distribute (distributeCases)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Eval (evalExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Quote (quote)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (EvalEnv, ExternEntry, InlineCandidate, NbeEnv)
import Purvasm.Compiler.Primitive (PrimOp)

-- | The inner rewrite-fuel cap (the reference's `rewriteLimit` analogue, sidenote 0012 §1).
-- | Rounds scale with alias/inline chain depth, not term size; a pathological module fails loudly.
rewriteFuel :: Int
rewriteFuel = 1000

-- | Normalise one binding body to its (gate-bounded) normal form. `key` names the binding in the
-- | fuel-exhaustion crash.
-- |
-- | Convergence is **term stability and mark stability together** (`e' == e && marks' == marks`).
-- | Term stability alone under-runs: an input already in `$q`-normal form (a previous driver
-- | round's output, possibly with fresh redexes a later `DictElim` exposed) quotes to itself on
-- | round one, before its marks were ever applied. Mark stability alone over-runs: a shared
-- | constructor scrutinising a *guarded* case is marked, inlined, found undecidable, and re-shared
-- | to the identical term every round — its mark is permanently re-discovered, and the combined
-- | check stops exactly when the re-discovered marks are the ones just applied.
-- |
-- | The final term's **outer kind/arity is preserved for the binding surface**: a top-level body
-- | that was not a syntactic lambda must not become one (NbE can reduce a CAF like
-- | `f = let g = \x -> x in g` to its lambda), or the `.pmi` `ExportKind`/hash would differ
-- | between `--opt` and `--no-opt` — ADR-0084 pins the `.pmi` core as mode-stable — and native
-- | consumers' ADR-0077 call facts (derived pre-optimisation) would mismatch the emitted object.
-- | The lambda normal form is re-shared under the reserved binder `$q0` (the quote supply starts
-- | at `$q1`, so the wrap never collides and round-trips stably).
nbeBinding :: NbeEnv -> String -> Expr -> Expr
nbeBinding nbe key e0 = preserveOuterShape e0 (loop rewriteFuel Set.empty e0)
  where
  loop n marks e
    | n <= 0 = unsafeCrashWith ("Nbe.nbeBinding: rewrite fuel exhausted at " <> key)
    | otherwise =
        let
          -- fold-guaranteed case-of-case distribution (slice 3) runs on the quoted term (unique
          -- binders); the placed leaf cases fold in the next evaluation round.
          e' = distributeCases (quote (evalExpr { locals: Map.empty, marks, nbe } e))
          marks' = inlineMarks e'
        in
          if e' == e && marks' == marks then e
          else loop (n - 1) marks' e'

-- | The binding-surface guard: a non-lambda body that normalised to a bare lambda is re-shared as
-- | a CAF (`let $q0 = \… in $q0`), keeping `classifyDecl`/`ExportKind` mode-stable. A lambda body
-- | stays a lambda of the same arity by construction (quote preserves the parameter count and no
-- | pass uncurries), so only this one transition needs the guard.
preserveOuterShape :: Expr -> Expr -> Expr
preserveOuterShape input output = case input, output of
  Ret (CLam _ _), _ -> output
  _, Ret lam@(CLam _ _) -> Let "$q0" lam (Ret (CAtom (AtomVar "$q0")))
  _, _ -> output

-- | The spine-independent, conservative **publish predicate** (ADR-0089 §8 slice 2): which of a
-- | module's bindings become `InlineCandidate`s for its dependents (and, reused locally, for its
-- | own siblings). Deliberately a size-bounded superset — the consumer's gate site (A) makes the
-- | final, spine-dependent call. Published shapes:
-- |
-- |   * a lambda body `Ret (CLam …)` (arity = parameter count);
-- |   * the slice-1 binding-surface wrap `let $q0 = \… in $q0` (the inner lambda — constructing a
-- |     closure is pure, so inlining it never re-executes the CAF's init);
-- |   * a value body (alias / data CAF — `arity: Nothing`, forced on demand);
-- |   * a **strictly under-applied** pure partial application `Ret (CApp t args)` where `t`'s
-- |     arity is known (a dependency candidate, an intrinsic, or a local syntactic lambda) and
-- |     `length args < arity` — the residual arity is published. A **saturated** (or
-- |     unknown-arity) CAF application is a computation executed once at init and is **never**
-- |     published: inlining it would re-execute it per use site.
-- |
-- | Recursive decls are never published (a recursive reference stays a call, ADR-0089 §5); the
-- | bound (`< 64`, the gate's largest size threshold) keeps the in-memory summary small.
candidatesOf
  :: (String -> Maybe { op :: PrimOp, arity :: Int })
  -> Set String
  -> Map String InlineCandidate
  -> Array { recursive :: Boolean, members :: Array (String /\ Expr) }
  -> Map String InlineCandidate
candidatesOf intrinsic gkeys deps decls =
  Map.fromFoldable (Array.mapMaybe candidateOf nonrecMembers)
  where
  nonrecMembers = decls >>= \d -> if d.recursive then [] else d.members

  publishBound = 64

  -- The gate-A closedness classifier (ADR-0089 Addendum extension, Accepted 2026-07-11): a free
  -- name is relocation-safe iff it is a known top-level key (own module ∪ dependency closure,
  -- the same set `dictElimExpr` classifies with) or in the compiler-global intrinsic/structural
  -- domain. Anything else — a local escape, an unresolved key — keeps the body un-closed; never
  -- a naming-convention judgement.
  isGlobal n = Set.member n gkeys || isJust (intrinsic n) || isJust (resolver n)

  -- a partial-application target's arity: dependency candidate, intrinsic, or local syntactic
  -- lambda (local partial-CAF chains are conservatively not chased).
  localArity = Map.fromFoldable
    ( Array.mapMaybe
        ( \(Tuple k e) -> case e of
            Ret (CLam ps _) -> Just (Tuple k (Array.length ps))
            _ -> Nothing
        )
        nonrecMembers
    )

  targetArity t = case Map.lookup t deps of
    Just c -> c.arity
    Nothing -> case intrinsic t of
      Just ia -> Just ia.arity
      Nothing -> Map.lookup t localArity

  candidateOf (Tuple k e) = Tuple k <$> (bounded =<< shapeOf e)
    where
    bounded cand = if cand.size < publishBound then Just cand else Nothing

  -- Unwrap the body's pure-value `let` chain (every rhs a value construction — re-constructing
  -- them per use site is pure, so a marked-inline evaluation of the chain cannot re-execute
  -- anything), collecting the binders' shapes for arity chasing; then judge the tail.
  shapeOf e0 = go Map.empty e0
    where
    go chain = case _ of
      Let x c rest | pureValueRhs c -> go (Map.insert x c chain) rest
      Ret c -> tailShape chain c
      _ -> Nothing

    tailShape chain c = case c of
      CLam ps body -> Just
        { arity: Just (Array.length ps)
        , size: sizeExpr e0
        , cxLeqDeref: false
        -- capture-safety only: free *globals* do not un-close (`AtomForeign`s are link-time
        -- globals by construction, so the old `cfExpr` conjunct is gone); a chain binder used by
        -- the body surfaces here as a free non-global, so the chain needs no separate conjunct.
        , closed: Set.isEmpty (Set.filter (not <<< isGlobal) (fvExpr (Set.fromFoldable ps) body))
        , body: e0
        }
      -- an alias tail: chase it through the chain for the published arity.
      CAtom (AtomVar x)
        | Just c' <- Map.lookup x chain -> tailShape (Map.delete x chain) c'
      CApp h args
        | Just residual <- partialResidual h args -> Just
            { arity: Just residual, size: sizeExpr e0, cxLeqDeref: false, closed: false, body: e0 }
      _ | valueBody c -> Just
        { arity: Nothing
        , size: sizeExpr e0
        , cxLeqDeref: isAtomBody c && isRet e0
        , closed: false
        , body: e0
        }
      _ -> Nothing

  partialResidual h args = do
    a <- case h of
      AtomVar t -> targetArity t
      AtomForeign t -> targetArity t
      AtomLit _ -> Nothing
    let n = Array.length args
    if n < a then Just (a - n) else Nothing

  isRet = case _ of
    Ret _ -> true
    _ -> false

  valueBody = case _ of
    CAtom _ -> true
    CCtor _ _ _ -> true
    CArray _ -> true
    CRecord _ -> true
    _ -> false

  isAtomBody = case _ of
    CAtom _ -> true
    _ -> false

-- | The pure-value `let`-rhs classes a publishable chain may contain: value constructions and
-- | aliases only (never a call, a branch, or a pinned primop — those are init-once computations).
pureValueRhs :: CExpr -> Boolean
pureValueRhs = case _ of
  CAtom _ -> true
  CLam _ _ -> true
  CCtor _ _ _ -> true
  CArray _ -> true
  CRecord _ -> true
  _ -> false

-- | Build the evaluation facts for one module's `nbeBinding` runs: the dependency candidates (from
-- | `BuildEnv`, ADR-0084/0089 slice 2) plus the module's own siblings (via the same publish
-- | predicate). The construction is layered to stay knot-free (no recursive value groups — a JS
-- | TDZ hazard in PS codegen):
-- |
-- |   * level 0 — dependency bodies evaluate under an intrinsic-only environment (they are
-- |     post-optimisation normal forms already, so their internal reductions happened in their own
-- |     module);
-- |   * level 1 — dependency bodies re-evaluate seeing level-0 siblings (so a dependency's pure
-- |     partial application resolves its target one link deep);
-- |   * level 2 — local sibling bodies evaluate seeing the level-1 dependencies.
-- |
-- | Deeper cross-module chains degrade gracefully to neutral calls; local chains resolve one link
-- | per driver round (ADR-0087's outer fixpoint).
nbeEnvOf
  :: (String -> Maybe { op :: PrimOp, arity :: Int })
  -> Set String
  -> Map String InlineCandidate
  -> Array { recursive :: Boolean, members :: Array (String /\ Expr) }
  -> NbeEnv
nbeEnvOf intrinsic gkeys deps decls =
  { externs: Map.union localExterns depExterns
  , intrinsic
  , structural
  }
  where
  base = { locals: Map.empty, marks: Set.empty, nbe: { externs: Map.empty, intrinsic, structural } }

  depExterns0 = map (entryFromCandidate base) deps
  envD = base { nbe = { externs: depExterns0, intrinsic, structural } }
  depExterns = map (entryFromCandidate envD) deps

  envL = base { nbe = { externs: depExterns, intrinsic, structural } }
  localExterns = map (entryFromCandidate envL) (candidatesOf intrinsic gkeys deps decls)

  -- The compiler-global **structural rung** as on-demand extern entries (ADR-0089 §1's
  -- compiler-global table, extended from the eta-primop view to the resolver's guest terms): a
  -- structural foreign's term is what the VM linker would substitute anyway, so unfolding it at
  -- compile time (size-gated by the consumer, like any extern) is the same program earlier.
  -- Intrinsic keys are excluded (they ride the `TIntrinsic` saturation path), and a structural
  -- body evaluates with the structural lookup *disabled* — depth one, no re-entry, no knot.
  structuralBase =
    { locals: Map.empty
    , marks: Set.empty
    , nbe: { externs: Map.empty, intrinsic, structural: \_ -> Nothing }
    }

  structural k =
    if isJust (intrinsic k) then Nothing
    else resolver k <#> \term ->
      let
        body = normalize term
      in
        case body of
          Ret (CLam ps b) ->
            { arity: Just (Array.length ps)
            , size: sizeExpr body
            , cxLeqDeref: false
            , closed: Set.isEmpty (fvExpr (Set.fromFoldable ps) b) && Set.isEmpty (cfExpr b)
            , value: defer \_ -> evalExpr structuralBase body
            }
          _ ->
            { arity: Nothing
            , size: sizeExpr body
            , cxLeqDeref: false
            , closed: false
            , value: defer \_ -> evalExpr structuralBase body
            }

-- | Rebuild a consumer-side extern entry from a published candidate. The body's (validated
-- | pure-value) chain binders are **marked**, so forcing the entry yields the bare tail value —
-- | a projection/match peeks straight through, and the chain's constructions are re-materialised
-- | at use sites (pure by the publish predicate).
entryFromCandidate :: EvalEnv -> InlineCandidate -> ExternEntry
entryFromCandidate env c =
  { arity: c.arity
  , size: c.size
  , cxLeqDeref: c.cxLeqDeref
  , closed: c.closed
  , value: defer \_ -> evalExpr (env { marks = chainBinders c.body }) c.body
  }
  where
  chainBinders = go Set.empty
    where
    go acc = case _ of
      Let x _ rest -> go (Set.insert x acc) rest
      _ -> acc
