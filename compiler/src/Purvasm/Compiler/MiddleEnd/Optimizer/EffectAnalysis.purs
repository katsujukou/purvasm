-- | Structural effect analysis on the optimiser seam (ADR-0095, the ADR-0034 model): classify
-- | values by whether forcing them performs an observable effect, from term structure and foreign
-- | leaf bits only — no type information.
-- |
-- | The force/saturation model (ADR-0034 I1: construction ≠ execution) needs two properties per
-- | value, not one — *building* an `Effect` is pure; the effect fires only when the built thunk is
-- | itself saturated. Throughout, "saturate a value" means "apply that value's own `arity` of
-- | arguments to it". Because `Effect a` is the nullary thunk `Unit -> a`, an effectful call
-- | stacks *two* saturations on *two different values*, and the summary's two booleans name them:
-- |
-- |   * `vsat` — does saturating *this* value perform?
-- |   * `retVsat` — is the value *produced* by that saturation itself effectful, i.e. does
-- |     saturating *it* in turn perform?
-- |
-- | The summary extends `ForeignShape` (ADR-0095 §1 / ADR-0096 §1) with the two-level
-- | mutable-store bit: `mtouch` ("may saturating this value touch the mutable store" — the motion
-- | hazard `vsat` cannot express) and `retMtouch` (the same question one saturation later).
-- | Foreigns are the leaf case, read from the ADR-0090 `foreignSigs` channel through the
-- | `EffectGlobals` oracle and **lifted dirty** (`liftShape`): a type cannot prove
-- | memory-cleanness (ADR-0096 §1 — `unsafeSetByte` is the pure-typed hidden-write
-- | counterexample), so only structurally computed facts ever carry a clean `mtouch`.
-- |
-- | `eperfC` ("does evaluating this computation perform") is the dead-drop predicate (ADR-0095
-- | §3): construction — including a *partial* application — is pure; an exact saturation takes
-- | the callee's `vsat`; an over-application is conservatively may-perform (a two-level summary
-- | cannot see effects buried deeper); the mutation primops (`NewArray`/`SetArray`) perform.
-- |
-- | The licensing boundary (ADR-0095 §4, completed by ADR-0096): `vsat = false` **alone** never
-- | licenses motion — a callee wrapping `IndexArray` is perform-clean yet must not move across a
-- | `SetArray`. Motion is licensed only by the conjunction the `mtouch` pair supplies: an
-- | exact-saturated call proving `vsat = false ∧ mtouch = false` (`sinkableCall`) may **sink** to
-- | its sole use site (single use, capture at most a branch — the mark walk's half of the
-- | clause); elimination when dead still needs only the perform bit.
-- |
-- | Local-name uniqueness invariant (boot's `effect_analysis.ml` carries the same note): CoreFn
-- | renames shadowed locals and the quote supply is `$q`-fresh, so a name missing from the local
-- | scope is genuinely free and the conservative `unknown` default is correct. Lambda parameters
-- | are still explicitly shadowed to `unknown` as a defence.
module Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis
  ( EffectFact
  , EffectEnv
  , EffectGlobals
  , pureValue
  , unknownValue
  , liftShape
  , emptyEffectEnv
  , bindFact
  , bindUnknown
  , moduleEffects
  , moduleEffectsLazy
  , vsumC
  , vsumExpr
  , eperfC
  , eperfExpr
  , mtouchC
  , mtouchExpr
  , sinkableCall
  , fixGroup
  , extendGroupVars
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Lazy (Lazy, defer, force)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.ForeignSig (ForeignShape)
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Module (Decl)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (pinnedPrim)
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | The analysis' summary (ADR-0096 §1): `ForeignShape`'s triple plus the two-level
-- | mutable-store bit. In-memory only — `ForeignShape` itself (FSR output, `ulib.json`, codec)
-- | is never extended; it lifts at the boundary via `liftShape`.
type EffectFact =
  { arity :: Int
  , vsat :: Boolean
  , retVsat :: Boolean
  , mtouch :: Boolean
  , retMtouch :: Boolean
  }

-- | The non-local fact oracle: dependencies' exported summaries, foreign shapes (own and
-- | dependencies', ADR-0090, lifted dirty), and — inside `optimizeModule` — the module's own
-- | pass-local top-level summaries. `Nothing` is the conservative miss.
type EffectGlobals = String -> Maybe EffectFact

-- | The lexically-scoped analysis environment (ADR-0095 §1): the global oracle plus the local
-- | facts of the binding body being walked — per-binding lifetime, never a module-flat local map.
-- |
-- | Local facts are **demand-driven** (`Lazy`): the mark walk (`Nbe/Analysis`) binds a fact at
-- | every `let` it passes, but consults one only at the call branch's decisions — a dead `CApp`'s
-- | drop (ADR-0095 §3), a live single-use `CApp`'s `sinkableCall` (ADR-0096 §2) — or through a
-- | callee's fact chain. A lambda's summary walks its whole body (`eperfExpr` + `vsumExpr` +
-- | `mtouchExpr`), so computing it eagerly at every binder made the per-iteration analysis pay
-- | the full effect walk even when nothing consumed it (measured as an opt-compile time×
-- | regression past the ADR-0089 §7 gate on the benchmark corpus).
type EffectEnv =
  { globals :: EffectGlobals
  , locals :: Map String (Lazy EffectFact)
  }

-- | A plain, fully-evaluated data value: nothing to force, nothing to perform, nothing touched.
pureValue :: EffectFact
pureValue = { arity: 0, vsat: false, retVsat: false, mtouch: false, retMtouch: false }

-- | The conservative default for anything opaque (a free variable, a parameter, a projected
-- | field): applying it may perform and may touch the store, and so may its result.
unknownValue :: EffectFact
unknownValue = { arity: 0, vsat: true, retVsat: true, mtouch: true, retMtouch: true }

-- | Lift a foreign's reconstructed shape to a fact — **dirty** (ADR-0096 §1): the declared type
-- | proves the perform bits (under the §3 FFI contract) but can never prove memory-cleanness,
-- | so every foreign leaf conservatively may touch the store at every level.
liftShape :: ForeignShape -> EffectFact
liftShape s = { arity: s.arity, vsat: s.vsat, retVsat: s.retVsat, mtouch: true, retMtouch: true }

emptyEffectEnv :: EffectGlobals -> EffectEnv
emptyEffectEnv globals = { globals, locals: Map.empty }

-- | Bind a deferred fact — forced only if some consumer actually consults the name.
bindFact :: String -> Lazy EffectFact -> EffectEnv -> EffectEnv
bindFact x s env = env { locals = Map.insert x s env.locals }

lazyUnknown :: Lazy EffectFact
lazyUnknown = defer \_ -> unknownValue

-- | Shadow names (lambda parameters, case binders) to the conservative default.
bindUnknown :: Array String -> EffectEnv -> EffectEnv
bindUnknown xs env =
  env { locals = foldl (\m x -> Map.insert x lazyUnknown m) env.locals xs }

-- | Branch-join: fewer args to saturate and either side's hazard bits — conservative both ways.
joinSum :: EffectFact -> EffectFact -> EffectFact
joinSum a b =
  { arity: min a.arity b.arity
  , vsat: a.vsat || b.vsat
  , retVsat: a.retVsat || b.retVsat
  , mtouch: a.mtouch || b.mtouch
  , retMtouch: a.retMtouch || b.retMtouch
  }

atomSum :: EffectEnv -> Atom -> EffectFact
atomSum env = case _ of
  AtomVar x -> case Map.lookup x env.locals of
    Just s -> force s
    Nothing -> fromMaybe unknownValue (env.globals x)
  -- a body-less foreign leaf: its reconstructed shape (ADR-0080/0090), or may-perform.
  AtomForeign k -> fromMaybe unknownValue (env.globals k)
  AtomLit _ -> pureValue

-- | The summary of the value an expression evaluates to.
vsumExpr :: EffectEnv -> Expr -> EffectFact
vsumExpr env = case _ of
  Ret c -> vsumC env c
  Let x c rest -> vsumExpr (bindFact x (defer \_ -> vsumC env c) env) rest
  LetRec binds rest -> vsumExpr (extendGroupVars env binds) rest

-- | The summary of the value a computation produces (the ADR-0096 §1 equations; each `mtouch`
-- | line mirrors its `vsat` analogue one to one).
vsumC :: EffectEnv -> CExpr -> EffectFact
vsumC env = case _ of
  CAtom a -> atomSum env a
  CLam ps body ->
    let
      env' = bindUnknown ps env
      bodySum = vsumExpr env' body
    in
      { arity: Array.length ps
      , vsat: eperfExpr env' body -- saturating the closure runs its body
      , retVsat: bodySum.vsat -- its result, when forced, performs?
      , mtouch: mtouchExpr env' body -- saturating the closure may touch the store
      , retMtouch: bodySum.mtouch -- its result, when forced, may touch?
      }
  CApp f args ->
    let
      sf = atomSum env f
      n = Array.length args
    in
      -- PAP: constructing it is clean, but its summary carries the callee's bits **verbatim**
      -- (ADR-0096 review P2 — the residual saturation must not lose the hazard).
      if n < sf.arity then
        { arity: sf.arity - n, vsat: sf.vsat, retVsat: sf.retVsat, mtouch: sf.mtouch, retMtouch: sf.retMtouch }
      -- saturated result: one level of `ret` info shifts down; `arity: 0` forces any later
      -- application of it onto the over-application path (conservatively safe).
      else if n == sf.arity then
        { arity: 0, vsat: sf.retVsat, retVsat: sf.retVsat, mtouch: sf.retMtouch, retMtouch: sf.retMtouch }
      else unknownValue -- over-application result is an opaque value (see `eperfC`)
  CPrim _ _ -> pureValue
  CCtor _ _ _ -> pureValue
  CArray _ -> pureValue
  CRecord _ -> pureValue
  CUpdate _ _ -> pureValue
  CAccessor _ _ -> unknownValue -- a field may be any value, incl. an effectful function
  CPerform _ -> unknownValue -- GER run point (ADR-0099): the performed result is opaque
  CIf _ t e -> joinSum (vsumExpr env t) (vsumExpr env e)
  CCase _ alts -> foldl (\acc a -> joinSum acc (altVsum env a)) pureValue alts

altVsum :: EffectEnv -> Alt -> EffectFact
altVsum env alt = case alt.result of
  Uncond e -> vsumExpr env e
  Guarded gs -> foldl (\acc g -> joinSum acc (vsumExpr env g.rhs)) pureValue gs

-- | Does evaluating this expression perform?
eperfExpr :: EffectEnv -> Expr -> Boolean
eperfExpr env = case _ of
  Ret c -> eperfC env c
  Let x c rest -> eperfC env c || eperfExpr (bindFact x (defer \_ -> vsumC env c) env) rest
  LetRec binds rest -> eperfExpr (extendGroupVars env binds) rest -- building closures is pure

-- | Does evaluating this computation perform? (The ADR-0095 §3 dead-drop predicate.)
eperfC :: EffectEnv -> CExpr -> Boolean
eperfC env = case _ of
  -- construction / projection is pure (I1)
  CAtom _ -> false
  CLam _ _ -> false
  CCtor _ _ _ -> false
  CArray _ -> false
  CRecord _ -> false
  CAccessor _ _ -> false
  CUpdate _ _ -> false
  CPrim op _ -> case op of
    NewArray -> true
    SetArray -> true
    _ -> false
  CApp f args ->
    let
      sf = atomSum env f
      n = Array.length args
    in
      if n < sf.arity then false -- partial application: builds a PAP, body not run
      -- exact saturation: runs only this callee; deeper levels of a returned closure fire at
      -- their own application sites.
      else if n == sf.arity then sf.vsat
      -- over-application (e.g. `(log s) u` in one node): the extra arguments force the produced
      -- value, and a summary tracks only one level below — conservatively may-perform.
      else true
  CPerform _ -> true -- GER run point (ADR-0099): performing a thunk always may-perform
  CIf _ t e -> eperfExpr env t || eperfExpr env e
  CCase _ alts -> Array.any (altEperf env) alts

altEperf :: EffectEnv -> Alt -> Boolean
altEperf env alt = case alt.result of
  Uncond e -> eperfExpr env e
  Guarded gs -> Array.any (\g -> eperfExpr env g.guard || eperfExpr env g.rhs) gs

-- | May evaluating this expression touch the mutable store? (`eperfExpr`'s `mtouch` mirror.)
mtouchExpr :: EffectEnv -> Expr -> Boolean
mtouchExpr env = case _ of
  Ret c -> mtouchC env c
  Let x c rest -> mtouchC env c || mtouchExpr (bindFact x (defer \_ -> vsumC env c) env) rest
  LetRec binds rest -> mtouchExpr (extendGroupVars env binds) rest -- building closures is clean

-- | May evaluating this computation touch the mutable store? (The ADR-0096 §1 evaluation
-- | predicate.) Covers all three `pinnedPrim`s for honesty, but only `IndexArray` adds
-- | information over `eperfC` — `NewArray`/`SetArray` are already `may-perform`, and must stay
-- | so (dead-drop consults `eperfC` alone; observable writes never enter a relaxed class).
mtouchC :: EffectEnv -> CExpr -> Boolean
mtouchC env = case _ of
  CAtom _ -> false
  CLam _ _ -> false
  CCtor _ _ _ -> false
  CArray _ -> false
  CRecord _ -> false
  CAccessor _ _ -> false
  CUpdate _ _ -> false
  CPrim op _ -> pinnedPrim op
  CApp f args ->
    let
      sf = atomSum env f
      n = Array.length args
    in
      if n < sf.arity then false -- constructing a PAP is clean
      else if n == sf.arity then sf.mtouch
      else true -- over-application: a two-level summary cannot see deeper
  CPerform _ -> true -- GER run point (ADR-0099): performing a thunk may touch the store
  CIf _ t e -> mtouchExpr env t || mtouchExpr env e
  CCase _ alts -> Array.any (altMtouch env) alts

altMtouch :: EffectEnv -> Alt -> Boolean
altMtouch env alt = case alt.result of
  Uncond e -> mtouchExpr env e
  Guarded gs -> Array.any (\g -> mtouchExpr env g.guard || mtouchExpr env g.rhs) gs

-- | The ADR-0096 §2 sink condition on a `let`'s right-hand side: an **exact-saturated** call
-- | whose saturation neither performs nor touches the store. (Usage/capture gating — single
-- | use, at most branch capture — is the mark walk's half of the clause.)
sinkableCall :: EffectEnv -> CExpr -> Boolean
sinkableCall env = case _ of
  CApp f args ->
    let
      sf = atomSum env f
    in
      Array.length args == sf.arity && not sf.vsat && not sf.mtouch
  _ -> false

-- | The arity to *seed* a recursive-group member with before the fixpoint: its parameter count
-- | when it is a (possibly let-wrapped) lambda. Seeding low is safe: an under-approximate arity
-- | only makes applications look like over-applications (conservatively may-perform).
rhsArity :: Expr -> Int
rhsArity = case _ of
  Ret (CLam ps _) -> Array.length ps
  Ret _ -> 0
  Let _ _ rest -> rhsArity rest
  LetRec _ rest -> rhsArity rest

-- | Least fixpoint over a recursive group (ADR-0095 §1, mirroring boot's `fix_group`): start each
-- | member optimistically pure (with its seeded arity) and re-evaluate under the group's own
-- | bindings until nothing changes. Monotone (`false → true`; arity settles from the lambda a
-- | point-free member bottoms out at), so it converges within the bounded loop.
fixGroup :: EffectEnv -> Array { name :: String, rhs :: Expr } -> Map String EffectFact
fixGroup env binds = loop (Array.length binds + 2) init
  where
  init = Map.fromFoldable
    ( binds <#> \b ->
        Tuple b.name (pureValue { arity = rhsArity b.rhs })
    )

  step facts =
    let
      envG = env { locals = Map.union (map (\s -> defer \_ -> s) facts) env.locals }
    in
      Map.fromFoldable (binds <#> \b -> Tuple b.name (vsumExpr envG b.rhs))

  changed a b = Array.any
    ( \bind -> case Map.lookup bind.name a, Map.lookup bind.name b of
        Just x, Just y -> x /= y
        _, _ -> true
    )
    binds

  loop n facts
    | n <= 0 = facts
    | otherwise =
        let
          facts' = step facts
        in
          if changed facts facts' then loop (n - 1) facts' else facts'

-- | Extend the local scope with a recursive group's fixpoint facts (the ANF `LetRec` spelling).
-- | The whole fixpoint is deferred behind one shared `Lazy`: consulting *any* member forces the
-- | group's solution once; passing the group unconsulted costs nothing.
extendGroupVars :: EffectEnv -> Array { var :: String, rhs :: Expr } -> EffectEnv
extendGroupVars env binds =
  let
    solved = defer \_ -> fixGroup env (binds <#> \b -> { name: b.var, rhs: b.rhs })
  in
    env
      { locals = foldl
          ( \m b -> Map.insert b.var
              (defer \_ -> fromMaybe unknownValue (Map.lookup b.var (force solved)))
              m
          )
          env.locals
          binds
      }

-- | Per-top-level-binding summaries for one module (ADR-0095 §1): fold the decls in source order,
-- | each member's summary computed under the earlier siblings' facts (dependency-directed within
-- | the module), recursive groups by `fixGroup`. Pass-local inside `optimizeModule` (ADR-0095 §2:
-- | recomputed each round from the current term — never stored in `LocalFacts`, never read back
-- | from the module's own `BuildSummary`).
moduleEffects :: EffectGlobals -> Array Decl -> Map String EffectFact
moduleEffects globals decls = map force (moduleEffectsLazy globals decls)

-- | The demand-driven spelling: entries force (and memoise) on first consultation, so an oracle
-- | that touches only the callees the mark walk actually meets never pays the full module walk.
moduleEffectsLazy :: EffectGlobals -> Array Decl -> Map String (Lazy EffectFact)
moduleEffectsLazy globals decls = foldl step Map.empty decls
  where
  step acc d =
    if d.recursive then
      let
        solved = defer \_ ->
          fixGroup { globals, locals: acc } (d.members <#> \(Tuple k e) -> { name: k, rhs: e })
      in
        foldl
          ( \m (Tuple k _) -> Map.insert k
              (defer \_ -> fromMaybe unknownValue (Map.lookup k (force solved)))
              m
          )
          acc
          d.members
    else
      foldl
        (\m (Tuple k e) -> Map.insert k (defer \_ -> vsumExpr { globals, locals: m } e) m)
        acc
        d.members
