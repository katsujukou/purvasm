-- | The post-`quote` analysis and gate site B (ADR-0089 §3/§4): per-binder usage (occurrence count
-- | + capture context, `None < Branch < Closure`), node sizes, and the per-`let` inline verdicts —
-- | the reference's `shouldInlineLet` clauses with its thresholds pinned as starting values
-- | (sidenote 0012 §5B). A verdict marks the binder for the **next** evaluation round (the
-- | discovery/application split): `Eval` then binds the value in its environment instead of
-- | sharing it.
-- |
-- | Only *gate-eligible* right-hand sides are ever marked through the ordinary `verdict`: value
-- | forms (`CLam`/`CCtor`/`CArray`/`CRecord`), pure dereference forms (`CAccessor`, non-pinned
-- | `CPrim`, `CUpdate`). Pinned computations (calls, pinned prims, branches) never enter it.
-- |
-- | The ADR-0095/0096 **call branch** is the one exception, and it is deliberately *not* verdict
-- | eligibility (the verdict's multi-use clauses would duplicate calls). A `CApp` is marked in
-- | exactly two cases:
-- |
-- |   * **drop** (ADR-0095): dead (`usage = Nothing`) and `eperfC` false — eliminated in place
-- |     (the bound value is never reified);
-- |   * **sink** (ADR-0096): live at exactly one use, captured at most by a branch, and
-- |     `sinkableCall` — an exact saturation that neither performs nor touches the mutable
-- |     store (`vsat = false ∧ mtouch = false`). The mark re-materialises the call at its sole
-- |     use site (conditional execution when that site sits under a branch — the same
-- |     partial-correctness class as dead-drop).
-- |
-- | Multi-use stays pinned (duplication); `CapClosure` stays pinned (multi-execution); a merely
-- | *pure* call without the clean-store fact stays pinned (ADR-0095 §3/§4's boundary, licensed
-- | away only by the strictly stronger `mtouch = false`).
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Analysis
  ( inlineMarks
  , sizeExpr
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Lazy (defer)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis (EffectEnv, EffectGlobals, bindFact, bindUnknown, emptyEffectEnv, eperfC, extendGroupVars, sinkableCall, vsumC)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (binderVarsOrdered, pinnedPrim)

-- --- usage ---------------------------------------------------------------------------------------

data Capture = CapNone | CapBranch | CapClosure

derive instance Eq Capture
derive instance Ord Capture

type Usage = { total :: Int, capture :: Capture }

-- | Every marked binder's usage (occurrence **count and capture**), for the ADR-0097 audit.
-- | Recorded for **verdict** marks *and* the ADR-0095/0096 call marks: an alias — or any value that
-- | embeds the call — must be auditable, or it launders the call's single-use / motion constraint
-- | into multi-use fan-out or into a closure (P1). The count drives the duplication axis; the
-- | capture (already raised to `CapClosure` inside a lambda by `capAt`) drives the motion axis.
type Info = { usages :: Map String Usage, size :: Int, marks :: Set String, markUsages :: Map String Usage }

mergeU :: Map String Usage -> Map String Usage -> Map String Usage
mergeU = Map.unionWith \a b -> { total: a.total + b.total, capture: max a.capture b.capture }

emptyI :: Info
emptyI = { usages: Map.empty, size: 0, marks: Set.empty, markUsages: Map.empty }

mergeI :: Info -> Info -> Info
mergeI a b =
  { usages: mergeU a.usages b.usages
  , size: a.size + b.size
  , marks: Set.union a.marks b.marks
  , markUsages: Map.union a.markUsages b.markUsages
  }

-- | Raise every usage in an inner info to at least the given capture context.
capAt :: Capture -> Info -> Info
capAt c i = i { usages = map (\u -> u { capture = max c u.capture }) i.usages }

dropVars :: Array String -> Info -> Info
dropVars xs i = i { usages = foldl (flip Map.delete) i.usages xs }

useAtom :: Atom -> Info
useAtom = case _ of
  AtomVar x -> emptyI { usages = Map.singleton x { total: 1, capture: CapNone }, size = 1 }
  _ -> emptyI { size = 1 }

useAtoms :: Array Atom -> Info
useAtoms = foldl (\i a -> mergeI i (useAtom a)) emptyI

-- --- gate site B ---------------------------------------------------------------------------------

data Cx = Trivial | Deref | KnownSize | NonTrivial

derive instance Eq Cx
derive instance Ord Cx

type RhsFacts = { cx :: Cx, isAbs :: Boolean, closedParams :: Boolean }

-- | The gate-eligible right-hand-side classes; `Nothing` = pinned/ineligible (never marked
-- | through `verdict` — a pure dead `CApp` rides the ADR-0095 dead-only branch instead).
classifyRhs :: CExpr -> Maybe RhsFacts
classifyRhs = case _ of
  CAtom _ -> Just { cx: Trivial, isAbs: false, closedParams: false }
  CLam ps b -> Just
    { cx: KnownSize
    , isAbs: true
    -- closed-but-for-params: **no free references at all** — no `$q` locals, no top-level globals,
    -- no foreigns. This is the reference's `Map.isEmpty usages && not externs` (both halves): a
    -- large multi-use lambda that calls globals must not ride the unconditional closed-inline
    -- clause, or the size blow-up guard is hollowed out for "closed over globals" wrappers.
    , closedParams:
        Set.isEmpty (fvExpr (Set.fromFoldable ps) b) && Set.isEmpty (cfExpr b)
    }
  CCtor _ _ _ -> Just { cx: KnownSize, isAbs: false, closedParams: false }
  CArray _ -> Just { cx: KnownSize, isAbs: false, closedParams: false }
  CRecord _ -> Just { cx: KnownSize, isAbs: false, closedParams: false }
  CUpdate _ _ -> Just { cx: KnownSize, isAbs: false, closedParams: false }
  CAccessor _ _ -> Just { cx: Deref, isAbs: false, closedParams: false }
  CPrim op _ | not (pinnedPrim op) -> Just { cx: Deref, isAbs: false, closedParams: false }
  _ -> Nothing

-- | The size ceilings the two multi-use re-materialising clauses promise (small-deref, small-lambda).
-- | Single-sourced so `verdict`'s syntactic test and the ADR-0097 audit's transitive enforcement
-- | of that promise cannot drift apart.
derefSizeBound :: Int
derefSizeBound = 5

lambdaSizeBound :: Int
lambdaSizeBound = 16

-- | `shouldInlineLet` (sidenote 0012 §5B), the reference clauses over the eligible classes:
-- | dead → drop; linear-and-uncaptured → always; small deref under at most a branch; a known-size
-- | construction used once; a lambda that is single-use, parameter-closed, or small.
verdict :: RhsFacts -> Int -> Maybe Usage -> Boolean
verdict f rhsSize = case _ of
  Nothing -> true
  Just u ->
    (f.cx == Trivial)
      || (u.capture == CapNone && u.total == 1)
      || (u.capture <= CapBranch && f.cx <= Deref && rhsSize < derefSizeBound)
      || (f.cx == KnownSize && u.total == 1)
      || (f.isAbs && (u.total == 1 || f.closedParams || rhsSize < lambdaSizeBound))

-- --- the walk ------------------------------------------------------------------------------------

-- | The gate-B marks for one quoted binding body: the `let` binders the next round should inline
-- | (or, for the ADR-0095 dead-only branch, drop). `globals` is the effect-fact oracle
-- | (dependencies' summaries + foreign shapes + the module's own pass-local summaries); the local
-- | fact environment is threaded lexically along this walk — per-binding lifetime (ADR-0095 §1).
inlineMarks :: EffectGlobals -> Expr -> Set String
inlineMarks globals e =
  let
    i = infoExpr (emptyEffectEnv globals) e
  in
    auditStrip i.markUsages e i.marks

-- | A binding body's node-count size (also the extern-entry size for gate A). Size and usage are
-- | fact-independent, so the degenerate oracle is fine here.
sizeExpr :: Expr -> Int
sizeExpr e = (infoExpr (emptyEffectEnv (const Nothing)) e).size

-- | How costly it is to re-materialise a marked binder at each use site — the property an **alias
-- | inherits** so a single-use exemption cannot be laundered into multi-use fan-out (ADR-0097 P1).
data DupPolicy
  = ShareOnly -- a fixed structure (`CRecord`/`CCtor`/…) or an effectful call: never re-materialise at >1 site
  | Bounded Int -- cheap: re-materialise while transitive materialised size stays under the bound
  | Unbounded -- explicitly duplication-safe: a bare var/literal, or a closed lambda (the accepted trade)

-- | The ADR-0097 **materialised-size audit**: one top-down walk over the quoted term that, for each
-- | marked binder, computes the size `Eval` would actually re-materialise at a use site and **strips
-- | any multi-use mark whose duplication policy forbids it** — restoring bounded per-round growth on
-- | chains the syntactic rhs-size test alone lets compound `2^depth`.
-- |
-- | The materialised size is computed by *recursing into the term* (not from a pre-tabulated free-var
-- | multiset): a marked binder's size expands its still-marked references **and its still-marked
-- | inner `let` binders inside lambda/branch bodies** (ADR-0097 P2 — those inner marks fan out at
-- | `Eval` time, so a small lambda whose body re-materialises marked locals is not "small"). A binder
-- | absent from the environment (unmarked, or already stripped) is a shared 1-node variable.
-- |
-- | Each mark carries a `DupPolicy` (the **duplication** axis) and a `motionCap` (the **motion**
-- | axis). The two propagate differently, because the recursive `M` and `motionCap` already cover
-- | what a `DupPolicy` would need to inherit through a non-alias embedding:
-- |
-- |   * **Duplication** — `DupPolicy` from the rhs, inherited only **through aliases** (`w = v`): a
-- |     `CRecord`/`CApp` marked *single-use* is `ShareOnly` (strip at multi-use, no numeric bound);
-- |     `small-deref`/`small-lambda` are `Bounded` (strip at multi-use once `M` breaches the clause
-- |     bound); a closed lambda / bare-var alias is `Unbounded`. It need **not** compose through
-- |     other embeddings: a big structure embedded in a `Bounded` binder is caught by `M` (its size
-- |     inflates the embedder past the bound), a small one is harmless to copy, and the one embedding
-- |     that *is* dangerous — a call — is caught by the motion axis instead.
-- |   * **Motion** — an ADR-0096 call sink is licensed only up to `CapBranch` (never into a closure)
-- |     and only single-use. A call's `motionCap` is `CapBranch`; every other rhs owns `CapClosure`
-- |     but **inherits the `min`** of the caps of the marked binders it embeds (so `r = {v: x}` gets
-- |     `x`'s `CapBranch`); a lambda is `CapClosure` (building a closure is pure). A binder with
-- |     `motionCap < CapClosure` — it is, or embeds, a call — is stripped when **used more than once**
-- |     (duplicating the call) **or used above its cap** (moving it into a closure). Unlike the
-- |     duplication policy, this composes through *every* embedding, which is what closes the
-- |     non-alias laundering (`r = {v: x}; l = \_ -> r`).
-- |
-- | The materialised size, `stripped` set, policy, and `motionCap` all fall out of one top-down walk
-- | that recurses into lambda/branch bodies (P2 — inner marks fan out too) and threads a
-- | `kept-mark → { m, policy, motionCap }` environment. Outer-first order makes each referenced
-- | binder final before the binder that references it is judged; stripping only ever removes marks.
auditStrip :: Map String Usage -> Expr -> Set String -> Set String
auditStrip markUsages term marks = Set.difference marks (auditE Map.empty term).stripped
  where
  auditE
    :: Map String { m :: Int, policy :: DupPolicy, motionCap :: Capture }
    -> Expr
    -> { size :: Int, stripped :: Set String, motionCap :: Capture }
  auditE env = case _ of
    Ret c -> auditC env c
    Let x c rest ->
      let
        cr = auditC env c
      in
        case Map.lookup x markUsages of
          Just u ->
            let
              pol = policyOf env c
              -- motion axis: a call (or a value embedding one — `motionCap < CapClosure`) may be
              -- re-materialised only if it stays **single-use** *and* within the capture it
              -- tolerates. Multi-use would multiply its executions; a use past `motionCap` (i.e.
              -- inside a closure) would move it there. Either forces sharing (ADR-0096 §2's boundary).
              motionStrip = cr.motionCap < CapClosure && (u.total > 1 || u.capture > cr.motionCap)
            in
              if motionStrip || strip u.total cr.size pol then
                -- kept as a shared `let`: 1 (the let) + rhs once + rest (uses stay 1-node vars).
                let
                  r = auditE env rest
                in
                  { size: 1 + cr.size + r.size
                  , stripped: Set.insert x (Set.union cr.stripped r.stripped)
                  , motionCap: min cr.motionCap r.motionCap
                  }
              else
                -- inlined: the `let` vanishes; each use in `rest` re-materialises `c` via env.
                let
                  r = auditE (Map.insert x { m: cr.size, policy: pol, motionCap: cr.motionCap } env) rest
                in
                  { size: r.size, stripped: Set.union cr.stripped r.stripped, motionCap: r.motionCap }
          Nothing ->
            let
              r = auditE env rest
            in
              { size: 1 + cr.size + r.size
              , stripped: Set.union cr.stripped r.stripped
              , motionCap: min cr.motionCap r.motionCap
              }
    LetRec bs rest ->
      let
        rbs = foldl (\acc b -> plus acc (auditE env b.rhs)) zero' bs
        r = auditE env rest
      in
        { size: 1 + rbs.size + r.size, stripped: Set.union rbs.stripped r.stripped, motionCap: min rbs.motionCap r.motionCap }

  auditC env = case _ of
    CAtom a -> atom env a
    -- a call owns `CapBranch`; everything else owns `CapClosure` and inherits its operands' caps.
    CApp h as -> atomsFrom env CapBranch 1 (Array.cons h as)
    -- a run point performs — treated like a call for motion (`CapBranch`).
    CPerform a -> atomsFrom env CapBranch 1 [ a ]
    CPrim _ as -> atomsFrom env CapClosure 1 as
    CCtor _ _ as -> atomsFrom env CapClosure 1 as
    CArray as -> atomsFrom env CapClosure 1 as
    CRecord fs -> atomsFrom env CapClosure 1 (map _.val fs)
    CAccessor a _ -> atomsFrom env CapClosure 1 [ a ]
    CUpdate a ups -> atomsFrom env CapClosure 1 (Array.cons a (map _.val ups))
    -- a closure is pure to build, hence re-materialisable anywhere (`CapClosure`); its body's own
    -- motion-constrained refs are stripped by their use-capture before they reach here.
    CLam _ b -> let r = auditE env b in { size: 1 + r.size, stripped: r.stripped, motionCap: CapClosure }
    CIf a t e ->
      let
        ai = atom env a
        rt = auditE env t
        re = auditE env e
      in
        { size: 1 + ai.size + rt.size + re.size
        , stripped: Set.union rt.stripped re.stripped
        , motionCap: min ai.motionCap (min rt.motionCap re.motionCap)
        }
    CCase ss alts ->
      foldl (\acc alt -> plus acc (auditAlt env alt))
        (atomsFrom env CapClosure 1 ss)
        alts

  auditAlt env alt = case alt.result of
    Uncond e -> auditE env e
    Guarded gs -> foldl (\acc g -> plus acc (plus (auditE env g.guard) (auditE env g.rhs))) zero' gs

  -- a single atom: a kept-marked var carries its materialised size and motion cap; anything else is
  -- a 1-node, unconstrained value.
  atom env = case _ of
    AtomVar y
      | Just e <- Map.lookup y env -> { size: e.m, stripped: Set.empty, motionCap: e.motionCap }
    _ -> { size: 1, stripped: Set.empty, motionCap: CapClosure }

  -- fold a base node count plus a list of atoms into a result, mining each atom's motion cap under
  -- the constructor's own cap (`CapBranch` for a call, `CapClosure` otherwise).
  atomsFrom env ownCap base as =
    foldl
      ( \acc a ->
          let
            ai = atom env a
          in
            acc { size = acc.size + ai.size, motionCap = min acc.motionCap ai.motionCap }
      )
      { size: base, stripped: Set.empty, motionCap: ownCap }
      as

  zero' = { size: 0, stripped: Set.empty, motionCap: CapClosure }
  plus a b = { size: a.size + b.size, stripped: Set.union a.stripped b.stripped, motionCap: min a.motionCap b.motionCap }

  -- The rhs's re-materialisation policy; an alias inherits its target's (a bare var / stripped
  -- target defaults to `Unbounded` — duplicating a shared variable costs one node and cannot chain).
  policyOf env = case _ of
    CAtom (AtomVar t) -> maybe Unbounded _.policy (Map.lookup t env)
    CAtom _ -> Unbounded
    CAccessor _ _ -> Bounded derefSizeBound
    CPrim _ _ -> Bounded derefSizeBound
    CLam ps body -> if closed ps body then Unbounded else Bounded lambdaSizeBound
    -- `CRecord`/`CCtor`/`CArray`/`CUpdate` (KnownSize) and `CApp` (a call): share-only.
    _ -> ShareOnly
    where
    closed ps body = Set.isEmpty (fvExpr (Set.fromFoldable ps) body) && Set.isEmpty (cfExpr body)

  strip total m = case _ of
    ShareOnly -> total > 1
    Bounded n -> total > 1 && m >= n
    Unbounded -> false

infoExpr :: EffectEnv -> Expr -> Info
infoExpr env = case _ of
  Ret c -> infoC env c
  Let x c rest ->
    let
      rc = infoC env c
      rr = infoExpr (bindFact x (defer \_ -> vsumC env c) env) rest
      usage = Map.lookup x rr.usages
      noMark = { mark: Set.empty, rec: Map.empty }
      -- Every mark records its usage (count + capture); the audit derives policy, materialised size,
      -- and the motion constraint from the term. Call marks record too (P1): an alias — or any value
      -- embedding the call — must be auditable, and its use capture drives the motion axis.
      marked = { mark: Set.singleton x, rec: Map.singleton x (fromMaybe { total: 0, capture: CapNone } usage) }
      { mark, rec } = case classifyRhs c of
        Just f | verdict f rc.size usage -> marked
        Just _ -> noMark
        -- The call branch: usage first. Dead pure → drop (ADR-0095 §3); live single-use,
        -- at most branch-captured, exact-saturated and store-clean → sink (ADR-0096 §2).
        Nothing -> case c, usage of
          CApp _ _, Nothing | not (eperfC env c) -> marked
          CApp _ _, Just u
            | u.total == 1
            , u.capture <= CapBranch
            , sinkableCall env c -> marked
          _, _ -> noMark
      merged = mergeI rc (dropVars [ x ] rr)
    in
      merged
        { size = merged.size + 1
        , marks = Set.union merged.marks mark
        , markUsages = Map.union merged.markUsages rec
        }
  LetRec binds body ->
    let
      names = map _.var binds
      env' = extendGroupVars env binds
      ri = foldl (\i b -> mergeI i (infoExpr env' b.rhs)) emptyI binds
      rb = infoExpr env' body
      merged = dropVars names (mergeI ri rb)
    in
      merged { size = merged.size + 1 }

infoC :: EffectEnv -> CExpr -> Info
infoC env = case _ of
  CAtom a -> useAtom a
  CLam ps body -> bump (dropVars ps (capAt CapClosure (infoExpr (bindUnknown ps env) body)))
  CApp h args -> bump (mergeI (useAtom h) (useAtoms args))
  CPrim _ args -> bump (useAtoms args)
  CCtor _ _ args -> bump (useAtoms args)
  CArray args -> bump (useAtoms args)
  CRecord fs -> bump (useAtoms (map _.val fs))
  CAccessor a _ -> bump (useAtom a)
  CUpdate a ups -> bump (mergeI (useAtom a) (useAtoms (map _.val ups)))
  CIf a t e -> bump (mergeI (useAtom a) (capAt CapBranch (mergeI (infoExpr env t) (infoExpr env e))))
  CCase scruts alts -> bump (mergeI (useAtoms scruts) (capAt CapBranch (foldl (\i alt -> mergeI i (infoAlt env alt)) emptyI alts)))
  CPerform a -> bump (useAtom a)
  where
  bump i = i { size = i.size + 1 }

infoAlt :: EffectEnv -> Alt -> Info
infoAlt env alt =
  let
    vars = Array.concatMap binderVarsOrdered alt.binders
    env' = bindUnknown vars env
  in
    dropVars vars case alt.result of
      Uncond e -> infoExpr env' e
      Guarded gs -> foldl (\i g -> mergeI i (mergeI (infoExpr env' g.guard) (infoExpr env' g.rhs))) emptyI gs
