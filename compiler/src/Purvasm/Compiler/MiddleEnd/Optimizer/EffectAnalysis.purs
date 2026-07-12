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
-- | The summary type is `ForeignShape` (ADR-0095 §1): a binding's effect summary has the same
-- | shape a foreign declares — foreigns are simply the leaf case of the analysis, read from the
-- | ADR-0090 `foreignSigs` channel through the `EffectGlobals` oracle.
-- |
-- | `eperfC` ("does evaluating this computation perform") is the dead-drop predicate (ADR-0095
-- | §3): construction — including a *partial* application — is pure; an exact saturation takes
-- | the callee's `vsat`; an over-application is conservatively may-perform (a two-level summary
-- | cannot see effects buried deeper); the mutation primops (`NewArray`/`SetArray`) perform.
-- |
-- | These facts license **elimination when dead, never motion** (ADR-0095 §4): the summary does
-- | not track mutable-memory reads (a callee wrapping `IndexArray` is `vsat = false` yet must not
-- | move across a `SetArray`), so no consumer may reorder, sink, or duplicate a call on the
-- | strength of `vsat = false` alone.
-- |
-- | Local-name uniqueness invariant (boot's `effect_analysis.ml` carries the same note): CoreFn
-- | renames shadowed locals and the quote supply is `$q`-fresh, so a name missing from the local
-- | scope is genuinely free and the conservative `unknown` default is correct. Lambda parameters
-- | are still explicitly shadowed to `unknown` as a defence.
module Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis
  ( EffectEnv
  , EffectGlobals
  , pureValue
  , unknownValue
  , emptyEffectEnv
  , bindFact
  , bindUnknown
  , moduleEffects
  , moduleEffectsLazy
  , vsumC
  , vsumExpr
  , eperfC
  , eperfExpr
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
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | The non-local fact oracle: dependencies' exported summaries, foreign shapes (own and
-- | dependencies', ADR-0090), and — inside `optimizeModule` — the module's own pass-local
-- | top-level summaries. `Nothing` is the conservative miss.
type EffectGlobals = String -> Maybe ForeignShape

-- | The lexically-scoped analysis environment (ADR-0095 §1): the global oracle plus the local
-- | facts of the binding body being walked — per-binding lifetime, never a module-flat local map.
-- |
-- | Local facts are **demand-driven** (`Lazy`): the mark walk (`Nbe/Analysis`) binds a fact at
-- | every `let` it passes, but consults one only at a dead `CApp` (the ADR-0095 §3 branch) or
-- | through a callee's fact chain — a lambda's summary walks its whole body (`eperfExpr` +
-- | `vsumExpr`), so computing it eagerly at every binder made the per-iteration analysis pay the
-- | full effect walk even when nothing consumed it (measured as an opt-compile time× regression
-- | past the ADR-0089 §7 gate on the benchmark corpus).
type EffectEnv =
  { globals :: EffectGlobals
  , locals :: Map String (Lazy ForeignShape)
  }

-- | A plain, fully-evaluated data value: nothing to force, nothing to perform.
pureValue :: ForeignShape
pureValue = { arity: 0, vsat: false, retVsat: false }

-- | The conservative default for anything opaque (a free variable, a parameter, a projected
-- | field): applying it may perform, and its result may itself be effectful.
unknownValue :: ForeignShape
unknownValue = { arity: 0, vsat: true, retVsat: true }

emptyEffectEnv :: EffectGlobals -> EffectEnv
emptyEffectEnv globals = { globals, locals: Map.empty }

-- | Bind a deferred fact — forced only if some consumer actually consults the name.
bindFact :: String -> Lazy ForeignShape -> EffectEnv -> EffectEnv
bindFact x s env = env { locals = Map.insert x s env.locals }

lazyUnknown :: Lazy ForeignShape
lazyUnknown = defer \_ -> unknownValue

-- | Shadow names (lambda parameters, case binders) to the conservative default.
bindUnknown :: Array String -> EffectEnv -> EffectEnv
bindUnknown xs env =
  env { locals = foldl (\m x -> Map.insert x lazyUnknown m) env.locals xs }

-- | Branch-join: fewer args to saturate and either side's perform bit — conservative both ways.
joinSum :: ForeignShape -> ForeignShape -> ForeignShape
joinSum a b =
  { arity: min a.arity b.arity
  , vsat: a.vsat || b.vsat
  , retVsat: a.retVsat || b.retVsat
  }

atomSum :: EffectEnv -> Atom -> ForeignShape
atomSum env = case _ of
  AtomVar x -> case Map.lookup x env.locals of
    Just s -> force s
    Nothing -> fromMaybe unknownValue (env.globals x)
  -- a body-less foreign leaf: its reconstructed shape (ADR-0080/0090), or may-perform.
  AtomForeign k -> fromMaybe unknownValue (env.globals k)
  AtomLit _ -> pureValue

-- | The summary of the value an expression evaluates to.
vsumExpr :: EffectEnv -> Expr -> ForeignShape
vsumExpr env = case _ of
  Ret c -> vsumC env c
  Let x c rest -> vsumExpr (bindFact x (defer \_ -> vsumC env c) env) rest
  LetRec binds rest -> vsumExpr (extendGroupVars env binds) rest

-- | The summary of the value a computation produces.
vsumC :: EffectEnv -> CExpr -> ForeignShape
vsumC env = case _ of
  CAtom a -> atomSum env a
  CLam ps body ->
    let
      env' = bindUnknown ps env
    in
      { arity: Array.length ps
      , vsat: eperfExpr env' body -- saturating the closure runs its body
      , retVsat: (vsumExpr env' body).vsat -- its result, when forced, performs?
      }
  CApp f args ->
    let
      sf = atomSum env f
      n = Array.length args
    in
      if n < sf.arity then { arity: sf.arity - n, vsat: sf.vsat, retVsat: sf.retVsat } -- PAP
      -- saturated result: one level of `ret` info; `arity: 0` forces any later application of
      -- it onto the over-application path (conservatively safe).
      else if n == sf.arity then { arity: 0, vsat: sf.retVsat, retVsat: sf.retVsat }
      else unknownValue -- over-application result is an opaque value (see `eperfC`)
  CPrim _ _ -> pureValue
  CCtor _ _ _ -> pureValue
  CArray _ -> pureValue
  CRecord _ -> pureValue
  CUpdate _ _ -> pureValue
  CAccessor _ _ -> unknownValue -- a field may be any value, incl. an effectful function
  CIf _ t e -> joinSum (vsumExpr env t) (vsumExpr env e)
  CCase _ alts -> foldl (\acc a -> joinSum acc (altVsum env a)) pureValue alts

altVsum :: EffectEnv -> Alt -> ForeignShape
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
  CIf _ t e -> eperfExpr env t || eperfExpr env e
  CCase _ alts -> Array.any (altEperf env) alts

altEperf :: EffectEnv -> Alt -> Boolean
altEperf env alt = case alt.result of
  Uncond e -> eperfExpr env e
  Guarded gs -> Array.any (\g -> eperfExpr env g.guard || eperfExpr env g.rhs) gs

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
fixGroup :: EffectEnv -> Array { name :: String, rhs :: Expr } -> Map String ForeignShape
fixGroup env binds = loop (Array.length binds + 2) init
  where
  init = Map.fromFoldable
    ( binds <#> \b ->
        Tuple b.name { arity: rhsArity b.rhs, vsat: false, retVsat: false }
    )

  step facts =
    let
      envG = env { locals = Map.union (map (\s -> defer \_ -> s) facts) env.locals }
    in
      Map.fromFoldable (binds <#> \b -> Tuple b.name (vsumExpr envG b.rhs))

  changed a b = Array.any
    ( \bind -> case Map.lookup bind.name a, Map.lookup bind.name b of
        Just x, Just y -> x.arity /= y.arity || x.vsat /= y.vsat || x.retVsat /= y.retVsat
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
moduleEffects :: EffectGlobals -> Array Decl -> Map String ForeignShape
moduleEffects globals decls = map force (moduleEffectsLazy globals decls)

-- | The demand-driven spelling: entries force (and memoise) on first consultation, so an oracle
-- | that touches only the callees the mark walk actually meets never pays the full module walk.
moduleEffectsLazy :: EffectGlobals -> Array Decl -> Map String (Lazy ForeignShape)
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
