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
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Ffi (resolver)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Analysis (inlineMarks, sizeExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Distribute (distributeCases)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Eval (evalC, evalExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Quote (quote)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (ArgUse, EvalEnv, ExternEntry, InlineCandidate, NbeEnv, RefTarget(..), Sem(..))
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
nbeBinding nbe key e0 = preserveOuterShape e0 (loop rewriteFuel Set.empty Set.empty e0)
  where
  loop n marks defers e
    | n <= 0 = unsafeCrashWith ("Nbe.nbeBinding: rewrite fuel exhausted at " <> key)
    | otherwise =
        let
          -- fold-guaranteed case-of-case distribution (slice 3) runs on the quoted term (unique
          -- binders); the placed leaf cases fold in the next evaluation round.
          e' = distributeCases (quote (evalExpr { locals: Map.empty, marks, defers, nbe } e))
          marks' = inlineMarks e'
          defers' = deferMarks nbe e'
        in
          if e' == e && marks' == marks && defers' == defers then e
          else loop (n - 1) marks' defers' e'

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
  Map.union
    (Map.fromFoldable (Array.mapMaybe candidateOf nonrecMembers))
    groupedBuilders
  where
  nonrecMembers = decls >>= \d -> if d.recursive then [] else d.members

  publishBound = 64

  -- Rec-group dictionary builders (ADR-0089 parameterized-instance extension, Accepted
  -- 2026-07-11): a Rec-group member is publishable iff it is dictionary-shaped — a lambda whose
  -- body tail is a record construction — carrying its whole group's key set. The consumer's
  -- entry evaluates group-stopped and folds only through the deferred-ref projection trigger.
  groupedBuilders = Map.fromFoldable
    ( decls >>= \d ->
        if not d.recursive then []
        else
          let
            group = Set.fromFoldable (map (\(Tuple k _) -> k) d.members)
          in
            d.members # Array.mapMaybe \(Tuple k e) -> case e of
              Ret (CLam ps body)
                | recordTail body, sizeExpr e < publishBound -> Just $ Tuple k
                    { arity: Just (Array.length ps)
                    , size: sizeExpr e
                    , cxLeqDeref: false
                    , closed: strictClosed ps body
                    , argUses: argUsesOf ps body
                    , group
                    , body: e
                    }
              _ -> Nothing
    )

  recordTail = case _ of
    Let _ _ rest -> recordTail rest
    Ret (CRecord _) -> true
    _ -> false

  -- A saturated application headed by a grouped builder: the group of the head, if so.
  groupOfHead t = case Map.lookup t deps of
    Just c | not (Set.isEmpty c.group) -> Just { arity: c.arity, group: c.group }
    _ -> case Map.lookup t groupedBuilders of
      Just c | not (Set.isEmpty c.group) -> Just { arity: c.arity, group: c.group }
      _ -> Nothing

  -- The 64-tier `closed` is the reference's strict form again (ADR-0089 self-compile extension):
  -- **any** free name — local or global — un-closes the body. The S7 relaxation (free globals
  -- allowed) opened the sibling-web channel the reference bars with `not s.externs`, and the
  -- compiler's own parser modules blew up ×14.5 through it; the dictionary shapes it was aimed
  -- at ride the scrutinised-known-arg tier and the grouped trigger instead. (`gkeys` stays a
  -- parameter for the driver's classifier plumbing, unused by this predicate.)
  strictClosed ps body =
    Set.isEmpty (fvExpr (Set.fromFoldable ps) body) && Set.isEmpty (cfExpr body)

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
        , closed: Map.isEmpty chain && strictClosed ps body
        , argUses: argUsesOf ps body
        , group: Set.empty
        , body: e0
        }
      -- an alias tail: chase it through the chain for the published arity.
      CAtom (AtomVar x)
        | Just c' <- Map.lookup x chain -> tailShape (Map.delete x chain) c'
      -- the CAF-split spelling of a grouped-builder application (`M.bind = bindStateT(dict)`):
      -- publishable as a value candidate — the ADR-pinned exception to the saturated-CAF-app
      -- rule (dictionary construction is pure, so a consumer re-materialising it re-executes
      -- nothing observable). Only the whole-body spelling (empty chain) is published.
      CApp h args
        | Map.isEmpty chain
        , AtomVar t <- h
        , Just g <- groupOfHead t
        , g.arity == Just (Array.length args) -> Just
            { arity: Nothing
            , size: sizeExpr e0
            , cxLeqDeref: false
            , closed: false
            , argUses: []
            , group: g.group
            , body: e0
            }
      CApp h args
        | Just residual <- partialResidual h args -> Just
            { arity: Just residual, size: sizeExpr e0, cxLeqDeref: false, closed: false, argUses: [], group: Set.empty, body: e0 }
      _ | valueBody c -> Just
        { arity: Nothing
        , size: sizeExpr e0
        , cxLeqDeref: isAtomBody c && isRet e0
        , closed: false
        , argUses: []
        , group: Set.empty
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

-- | Per-parameter consumption facts for the scrutinised-known-arg 64-tier (ADR-0089 self-compile
-- | extension): count every occurrence (`total`), projection/match-scrutinee positions
-- | (`projected`), and call-head positions (`appliedHead`) of each lambda parameter across the
-- | whole body. Shadowing cannot occur on the shapes we publish (post-quote `$q`-unique binders;
-- | the pre-optimisation first round is conservative at worst).
argUsesOf :: Array String -> Expr -> Array ArgUse
argUsesOf ps body = ps <#> \p -> goE p { total: 0, projected: 0, appliedHead: 0 } body
  where
  hit p u a = case a of
    AtomVar y | y == p -> u { total = u.total + 1 }
    _ -> u

  hits p = Array.foldl (hit p)

  proj p u a = case a of
    AtomVar y | y == p -> u { total = u.total + 1, projected = u.projected + 1 }
    _ -> u

  hd p u a = case a of
    AtomVar y | y == p -> u { total = u.total + 1, appliedHead = u.appliedHead + 1 }
    _ -> u

  goE p u = case _ of
    Ret c -> goC p u c
    Let _ c rest -> goE p (goC p u c) rest
    LetRec bs rest -> goE p (Array.foldl (\a b -> goE p a b.rhs) u bs) rest

  goC p u = case _ of
    CAtom a -> hit p u a
    CLam _ b -> goE p u b
    CApp h as -> hits p (hd p u h) as
    CPrim _ as -> hits p u as
    CCtor _ _ as -> hits p u as
    CArray as -> hits p u as
    CRecord fs -> hits p u (map _.val fs)
    CAccessor a _ -> proj p u a
    CUpdate a us -> hits p (hit p u a) (map _.val us)
    CIf a t e -> goE p (goE p (hit p u a) t) e
    CCase ss alts -> Array.foldl (goAlt p) (Array.foldl (proj p) u ss) alts

  goAlt p u alt = case alt.result of
    Uncond e -> goE p u e
    Guarded gs -> Array.foldl (\a g -> goE p (goE p a g.guard) g.rhs) u gs

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
  base = { locals: Map.empty, marks: Set.empty, defers: Set.empty, nbe: { externs: Map.empty, intrinsic, structural } }

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
    , defers: Set.empty
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
            , argUses: argUsesOf ps b
            , group: Set.empty
            , value: defer \_ -> evalExpr structuralBase body
            }
          _ ->
            { arity: Nothing
            , size: sizeExpr body
            , cxLeqDeref: false
            , closed: false
            , argUses: []
            , group: Set.empty
            , value: defer \_ -> evalExpr structuralBase body
            }

-- | Rebuild a consumer-side extern entry from a published candidate. The body's (validated
-- | pure-value) chain binders are **marked**, so forcing the entry yields the bare tail value —
-- | a projection/match peeks straight through, and the chain's constructions are re-materialised
-- | at use sites (pure by the publish predicate).
-- |
-- | A **grouped** candidate (ADR-0089 parameterized-instance extension) is special twice: its
-- | body evaluates with the whole group removed from the externs (the `InlineNever` self-stop —
-- | sibling and self references degrade to neutral global calls, so re-entry is structurally
-- | impossible), and a grouped *alias* (a published saturated builder application) forces
-- | directly to the deferred saturated ref, so a consumer's projection can judge the fold.
entryFromCandidate :: EvalEnv -> InlineCandidate -> ExternEntry
entryFromCandidate env c =
  { arity: c.arity
  , size: c.size
  , cxLeqDeref: c.cxLeqDeref
  , closed: c.closed
  , argUses: c.argUses
  , group: c.group
  , value: defer \_ -> case aliasRef of
      Just ref -> ref
      Nothing -> evalExpr (stopped { marks = chainBinders c.body }) c.body
  }
  where
  stopped =
    if Set.isEmpty c.group then env
    else env { nbe = env.nbe { externs = Map.filterKeys (\k -> not (Set.member k c.group)) env.nbe.externs } }

  aliasRef = case c.body of
    Ret (CApp h@(AtomVar t) args)
      | not (Set.isEmpty c.group), isNothing c.arity ->
          case Map.lookup t env.nbe.externs of
            Just hEntry | not (Set.isEmpty hEntry.group) -> Just
              ( SRef
                  { atom: h
                  , target: TExtern hEntry
                  , spine: map (\a -> evalC env (CAtom a)) args
                  }
              )
            _ -> Nothing
    _ -> Nothing

  chainBinders = go Set.empty
    where
    go acc = case _ of
      Let x _ rest -> go (Set.insert x acc) rest
      _ -> acc

-- | Discovery half of the grouped deferral (ADR-0089 parameterized-instance extension), run on
-- | the quoted output like `inlineMarks` (binders are `$q`-unique, so plain occurrence counting
-- | is shadow-free): mark a binder whose rhs is a **saturated grouped application** and whose
-- | use count is exactly one, that use being a projection scrutinee. Single-use is the
-- | sharing-safety condition: applied next round, the deferral can never re-materialise the
-- | application at more than the one site it already occupies.
deferMarks :: NbeEnv -> Expr -> Set String
deferMarks nbe = goE Set.empty
  where
  goE acc = case _ of
    Ret c -> goC acc c
    Let x (CApp h args) rest
      | groupedSaturated h args
      , singleProjectionUse x rest -> goE (Set.insert x acc) rest
    Let _ c rest -> goE (goC acc c) rest
    LetRec bs rest -> goE (Array.foldl (\a b -> goE a b.rhs) acc bs) rest

  goC acc = case _ of
    CLam _ b -> goE acc b
    CIf _ t e -> goE (goE acc t) e
    CCase _ alts -> Array.foldl goAlt acc alts
    _ -> acc

  goAlt acc alt = case alt.result of
    Uncond e -> goE acc e
    Guarded gs -> Array.foldl (\a g -> goE (goE a g.guard) g.rhs) acc gs

  groupedSaturated h args = case h of
    AtomVar t -> case Map.lookup t nbe.externs of
      Just e -> not (Set.isEmpty e.group) && e.arity == Just (Array.length args)
      Nothing -> false
    _ -> false

  -- The qualifying use must sit on the binder's own `let`-spine level: a projection *inside* a
  -- lambda or a branch (`deep`) never qualifies — deferring there would, on refusal, reify the
  -- application at the projection site, moving the pinned construction across a lambda/branch
  -- boundary (the ADR pins refusal as term-identity). Deep occurrences still count toward
  -- `total`, so any such use also disqualifies the single-use condition.
  singleProjectionUse x rest =
    let
      u = usesIn rest
    in
      u.total == 1 && u.proj == 1
    where
    add u a = case a of
      AtomVar y | y == x -> u { total = u.total + 1 }
      _ -> u

    adds u = Array.foldl add u

    usesIn = goUE false { total: 0, proj: 0 }
      where
      goUE deep u = case _ of
        Ret c -> goUC deep u c
        Let _ c rest' -> goUE deep (goUC deep u c) rest'
        LetRec bs rest' -> goUE deep (Array.foldl (\a b -> goUE true a b.rhs) u bs) rest'

      goUC deep u = case _ of
        CAtom a -> add u a
        CLam _ b -> goUE true u b
        CApp h as -> adds (add u h) as
        CPrim _ as -> adds u as
        CCtor _ _ as -> adds u as
        CArray as -> adds u as
        CRecord fs -> adds u (map _.val fs)
        CAccessor a _ -> case a of
          AtomVar y
            | y == x ->
                if deep then u { total = u.total + 1 }
                else u { total = u.total + 1, proj = u.proj + 1 }
          _ -> u
        CUpdate a us -> adds (add u a) (map _.val us)
        CIf a t e -> goUE true (goUE true (add u a) t) e
        CCase ss alts -> Array.foldl (goUAlt) (adds u ss) alts

      goUAlt u alt = case alt.result of
        Uncond e -> goUE true u e
        Guarded gs -> Array.foldl (\a g -> goUE true (goUE true a g.guard) g.rhs) u gs
