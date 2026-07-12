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
import Data.Maybe (Maybe(..))
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

type Info = { usages :: Map String Usage, size :: Int, marks :: Set String }

mergeU :: Map String Usage -> Map String Usage -> Map String Usage
mergeU = Map.unionWith \a b -> { total: a.total + b.total, capture: max a.capture b.capture }

emptyI :: Info
emptyI = { usages: Map.empty, size: 0, marks: Set.empty }

mergeI :: Info -> Info -> Info
mergeI a b = { usages: mergeU a.usages b.usages, size: a.size + b.size, marks: Set.union a.marks b.marks }

-- | Raise every usage in an inner info to at least the given capture context.
capAt :: Capture -> Info -> Info
capAt c i = i { usages = map (\u -> u { capture = max c u.capture }) i.usages }

dropVars :: Array String -> Info -> Info
dropVars xs i = i { usages = foldl (flip Map.delete) i.usages xs }

useAtom :: Atom -> Info
useAtom = case _ of
  AtomVar x -> { usages: Map.singleton x { total: 1, capture: CapNone }, size: 1, marks: Set.empty }
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

-- | `shouldInlineLet` (sidenote 0012 §5B), the reference clauses over the eligible classes:
-- | dead → drop; linear-and-uncaptured → always; small deref under at most a branch; a known-size
-- | construction used once; a lambda that is single-use, parameter-closed, or small.
verdict :: RhsFacts -> Int -> Maybe Usage -> Boolean
verdict f rhsSize = case _ of
  Nothing -> true
  Just u ->
    (f.cx == Trivial)
      || (u.capture == CapNone && u.total == 1)
      || (u.capture <= CapBranch && f.cx <= Deref && rhsSize < 5)
      || (f.cx == KnownSize && u.total == 1)
      || (f.isAbs && (u.total == 1 || f.closedParams || rhsSize < 16))

-- --- the walk ------------------------------------------------------------------------------------

-- | The gate-B marks for one quoted binding body: the `let` binders the next round should inline
-- | (or, for the ADR-0095 dead-only branch, drop). `globals` is the effect-fact oracle
-- | (dependencies' summaries + foreign shapes + the module's own pass-local summaries); the local
-- | fact environment is threaded lexically along this walk — per-binding lifetime (ADR-0095 §1).
inlineMarks :: EffectGlobals -> Expr -> Set String
inlineMarks globals e = (infoExpr (emptyEffectEnv globals) e).marks

-- | A binding body's node-count size (also the extern-entry size for gate A). Size and usage are
-- | fact-independent, so the degenerate oracle is fine here.
sizeExpr :: Expr -> Int
sizeExpr e = (infoExpr (emptyEffectEnv (const Nothing)) e).size

infoExpr :: EffectEnv -> Expr -> Info
infoExpr env = case _ of
  Ret c -> infoC env c
  Let x c rest ->
    let
      rc = infoC env c
      rr = infoExpr (bindFact x (defer \_ -> vsumC env c) env) rest
      usage = Map.lookup x rr.usages
      marked = case classifyRhs c of
        Just f | verdict f rc.size usage -> Set.singleton x
        Just _ -> Set.empty
        -- The call branch: usage first. Dead pure → drop (ADR-0095 §3); live single-use,
        -- at most branch-captured, exact-saturated and store-clean → sink (ADR-0096 §2).
        Nothing -> case c, usage of
          CApp _ _, Nothing | not (eperfC env c) -> Set.singleton x
          CApp _ _, Just u
            | u.total == 1
            , u.capture <= CapBranch
            , sinkableCall env c -> Set.singleton x
          _, _ -> Set.empty
      merged = mergeI rc (dropVars [ x ] rr)
    in
      merged { size = merged.size + 1, marks = Set.union merged.marks marked }
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
