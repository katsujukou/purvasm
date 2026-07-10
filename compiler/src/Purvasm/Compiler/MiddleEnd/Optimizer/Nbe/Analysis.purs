-- | The post-`quote` analysis and gate site B (ADR-0089 §3/§4): per-binder usage (occurrence count
-- | + capture context, `None < Branch < Closure`), node sizes, and the per-`let` inline verdicts —
-- | the reference's `shouldInlineLet` clauses with its thresholds pinned as starting values
-- | (sidenote 0012 §5B). A verdict marks the binder for the **next** evaluation round (the
-- | discovery/application split): `Eval` then binds the value in its environment instead of
-- | sharing it.
-- |
-- | Only *gate-eligible* right-hand sides are ever marked: value forms (`CLam`/`CCtor`/`CArray`/
-- | `CRecord`), pure dereference forms (`CAccessor`, non-pinned `CPrim`, `CUpdate`). Pinned
-- | computations (calls, pinned prims, branches) are never marked — not even dead — preserving
-- | ADR-0089 §5 (dead-drop of pinned bindings waits for `EffectAnalysis` purity facts).
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Analysis
  ( inlineMarks
  , sizeExpr
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
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

-- | The gate-eligible right-hand-side classes; `Nothing` = pinned/ineligible (never marked).
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

-- | The gate-B marks for one quoted binding body: the `let` binders the next round should inline.
inlineMarks :: Expr -> Set String
inlineMarks e = (infoExpr e).marks

-- | A binding body's node-count size (also the extern-entry size for gate A).
sizeExpr :: Expr -> Int
sizeExpr e = (infoExpr e).size

infoExpr :: Expr -> Info
infoExpr = case _ of
  Ret c -> infoC c
  Let x c rest ->
    let
      rc = infoC c
      rr = infoExpr rest
      marked = case classifyRhs c of
        Just f | verdict f rc.size (Map.lookup x rr.usages) -> Set.singleton x
        _ -> Set.empty
      merged = mergeI rc (dropVars [ x ] rr)
    in
      merged { size = merged.size + 1, marks = Set.union merged.marks marked }
  LetRec binds body ->
    let
      names = map _.var binds
      ri = foldl (\i b -> mergeI i (infoExpr b.rhs)) emptyI binds
      rb = infoExpr body
      merged = dropVars names (mergeI ri rb)
    in
      merged { size = merged.size + 1 }

infoC :: CExpr -> Info
infoC = case _ of
  CAtom a -> useAtom a
  CLam ps body -> bump (dropVars ps (capAt CapClosure (infoExpr body)))
  CApp h args -> bump (mergeI (useAtom h) (useAtoms args))
  CPrim _ args -> bump (useAtoms args)
  CCtor _ _ args -> bump (useAtoms args)
  CArray args -> bump (useAtoms args)
  CRecord fs -> bump (useAtoms (map _.val fs))
  CAccessor a _ -> bump (useAtom a)
  CUpdate a ups -> bump (mergeI (useAtom a) (useAtoms (map _.val ups)))
  CIf a t e -> bump (mergeI (useAtom a) (capAt CapBranch (mergeI (infoExpr t) (infoExpr e))))
  CCase scruts alts -> bump (mergeI (useAtoms scruts) (capAt CapBranch (foldl (\i alt -> mergeI i (infoAlt alt)) emptyI alts)))
  where
  bump i = i { size = i.size + 1 }

infoAlt :: Alt -> Info
infoAlt alt = dropVars (Array.concatMap binderVarsOrdered alt.binders) case alt.result of
  Uncond e -> infoExpr e
  Guarded gs -> foldl (\i g -> mergeI i (mergeI (infoExpr g.guard) (infoExpr g.rhs))) emptyI gs
