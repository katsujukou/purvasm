-- | The NbE general inliner's seam entry (ADR-0089 §1): `nbeBinding` runs the per-binding
-- | `quote ∘ eval` inner fixpoint — evaluate under the previous round's gate-B marks, quote,
-- | re-analyse, repeat until the term and the marks are stable — bounded by a rewrite fuel whose
-- | exhaustion is a **loud crash**, never silent truncation. `nbeEnvOf` publishes a module's
-- | sibling bindings for gate-site-A unfolding.
-- |
-- | Sibling publication is deliberately **one binding deep**: a published body is evaluated under
-- | an intrinsic-only environment (its own sibling references stay opaque), so unfolding cannot
-- | chase chains within a single round — the driver's outer `optimizeModule` fixpoint (ADR-0087)
-- | resolves chains one link per round, and the construction stays acyclic (no knot to tie, and a
-- | multi-reference body is still evaluated at most once per round via its `Lazy`). Recursive
-- | decls are never published (a recursive reference stays a call — ADR-0089 §5), and a
-- | non-recursive binding cannot reference itself, so no self-stop is needed.
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe
  ( nbeBinding
  , nbeEnvOf
  ) where

import Prelude

import Data.Array as Array
import Data.Lazy (defer)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Analysis (inlineMarks, sizeExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Eval (evalC, evalExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Quote (quote)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (NbeEnv)
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
          e' = quote (evalExpr { locals: Map.empty, marks, nbe } e)
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

-- | Publish a module's non-recursive sibling bindings as gate-site-A extern entries (ADR-0089 §4):
-- | a `Ret (CLam …)` body publishes with its arity (unfolded at saturation), a value body
-- | (alias / data CAF) with `arity: Nothing` (forced on demand, size-gated). Computation bodies
-- | (unresolved CAF applications) are not publishable — inlining one would duplicate the
-- | computation.
nbeEnvOf
  :: (String -> Maybe { op :: PrimOp, arity :: Int })
  -> Array { recursive :: Boolean, members :: Array (String /\ Expr) }
  -> NbeEnv
nbeEnvOf intrinsic decls =
  { externs: Map.fromFoldable (Array.mapMaybe entryOf nonrecMembers)
  , intrinsic
  }
  where
  nonrecMembers = decls >>= \d -> if d.recursive then [] else d.members

  -- published bodies evaluate one level deep: sibling references inside them stay opaque.
  baseEnv = { locals: Map.empty, marks: Set.empty, nbe: { externs: Map.empty, intrinsic } }

  entryOf (Tuple k e) = case e of
    Ret c@(CLam ps body) -> Just $ Tuple k
      { arity: Just (Array.length ps)
      , size: sizeExpr e
      , cxLeqDeref: false
      , closed: Set.isEmpty (fvExpr (Set.fromFoldable ps) body) && Set.isEmpty (cfExpr body)
      , value: defer \_ -> evalC baseEnv c
      }
    Ret c | valueBody c -> Just $ Tuple k
      { arity: Nothing
      , size: sizeExpr e
      , cxLeqDeref: isAtomBody c
      , closed: false
      , value: defer \_ -> evalC baseEnv c
      }
    _ -> Nothing

  valueBody = case _ of
    CAtom _ -> true
    CCtor _ _ _ -> true
    CArray _ -> true
    CRecord _ -> true
    _ -> false

  isAtomBody = case _ of
    CAtom _ -> true
    _ -> false
