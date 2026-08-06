-- | The ADR-0107 by-need fact lattice — slice 0: the fact computation and its census,
-- | analysis-only (no emission consumer yet; slice 1 wires `ForceDecision`s into the plan
-- | and the emitter through the shared-classifier seam).
-- |
-- | Identity is a binder OCCURRENCE, never a source name (ADR-0107 §2): the walk resolves
-- | each demand site against an innermost-first scope IN PLACE — the assoc list grows a new
-- | entry per binder occurrence, so an inner binder of the same name shadows correctly and
-- | no name survives into the aggregated counts (the resolved-decision form of the §2 pin).
-- |
-- | The lattice is the pinned two-value form: unknown FALLS to `May`, always — every
-- | positive `Never` arm below names its ADR-0107 §1 producer; anything unmatched is `May`
-- | by the catch-all, mechanically.
module Purvasm.Compiler.Backend.LLVM.ByNeed
  ( ByNeedFact(..)
  , SiteClass(..)
  , CensusCounts
  , emptyCensus
  , censusGdef
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Backend.LLVM.Types (Gdef(..))
import Purvasm.Compiler.Binder (Binder(..))

import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.Primitive (PrimOp(..))

data ByNeedFact = NeverByNeed | MayBeByNeed

derive instance eqByNeedFact :: Eq ByNeedFact

-- | The §1 meet: `Never` only when both sides are.
meet :: ByNeedFact -> ByNeedFact -> ByNeedFact
meet NeverByNeed NeverByNeed = NeverByNeed
meet _ _ = MayBeByNeed

-- | The in-scope demand recipes (ADR-0107 "demand recipes", pinned): forced-VARIABLE
-- | operands per recipe, plus guard results. `SForceCell`/entry forces are out of scope.
data SiteClass
  = SPrimOperand
  | SIfCond
  | SGuardResult
  | SCaseScrutinee
  | SAccessorBase
  | SUpdateBase

siteIx :: SiteClass -> Int
siteIx = case _ of
  SPrimOperand -> 0
  SIfCond -> 1
  SGuardResult -> 2
  SCaseScrutinee -> 3
  SAccessorBase -> 4
  SUpdateBase -> 5

-- | Census: per site class, `[never, may]` counts (flat array, index = `siteIx * 2 + fact`).
type CensusCounts = Array Int

emptyCensus :: CensusCounts
emptyCensus = Array.replicate 12 0

bump :: SiteClass -> ByNeedFact -> CensusCounts -> CensusCounts
bump cls fact c =
  let
    i = siteIx cls * 2 + (if fact == NeverByNeed then 0 else 1)
  in
    case Array.modifyAt i (_ + 1) c of
      Just c' -> c'
      Nothing -> c

-- | Innermost-first occurrence scope (one entry per binder occurrence).
type Scope = List (Tuple String ByNeedFact)

lookupFact :: Scope -> String -> ByNeedFact
lookupFact sc x = case List.find (\(Tuple k _) -> k == x) sc of
  Just (Tuple _ f) -> f
  -- an unbound name is a global/foreign — `May` by the §1 pin.
  Nothing -> MayBeByNeed

binderVarsOf :: Binder -> Array String
binderVarsOf = case _ of
  BNull -> []
  BVar v -> [ v ]
  BLit _ -> []
  BNamed v inner -> [ v ] <> binderVarsOf inner
  BCtor _ subs -> subs >>= binderVarsOf
  BArray subs -> subs >>= binderVarsOf
  BRecord fields -> fields >>= (_.binder >>> binderVarsOf)

atomFact :: Scope -> Atom -> ByNeedFact
atomFact sc = case _ of
  AtomVar x -> lookupFact sc x
  -- literals are values (boxed ones allocate, but never a cell).
  AtomLit _ -> NeverByNeed
  -- foreign leaf references build a closure, but the §1 pin keeps foreign = `May`.
  AtomForeign _ -> MayBeByNeed

-- | The §1 scalar-primitive `Never` set: arithmetic/comparison/logical results. The
-- | projections (`RecordGet`/`IndexArray`/`ReadField`-class) read container slots that may
-- | hold cells; everything else falls to `May` by the catch-all.
primNever :: PrimOp -> Boolean
primNever = case _ of
  AddInt -> true
  SubInt -> true
  MulInt -> true
  DivInt -> true
  ModInt -> true
  AndInt -> true
  OrInt -> true
  XorInt -> true
  ShlInt -> true
  ShrInt -> true
  ZshrInt -> true
  ComplementInt -> true
  AddNumber -> true
  SubNumber -> true
  MulNumber -> true
  DivNumber -> true
  IntToNumber -> true
  NumberToInt -> true
  EqInt -> true
  EqString -> true
  EqNumber -> true
  EqBool -> true
  LtInt -> true
  LtString -> true
  LtNumber -> true
  AndBool -> true
  OrBool -> true
  NotBool -> true
  LengthArray -> true
  _ -> false

-- | A computation's result fact (the §1 producers; catch-all = `May`).
cexprFact :: Scope -> CExpr -> ByNeedFact
cexprFact sc = case _ of
  CAtom a -> atomFact sc a
  CPrim op _ -> if primNever op then NeverByNeed else MayBeByNeed
  CCtor _ _ _ -> NeverByNeed
  CArray _ -> NeverByNeed
  CRecord _ -> NeverByNeed
  CLam _ _ -> NeverByNeed
  CIf _ t e -> meet (exprFact sc t) (exprFact sc e)
  -- the §1 branch meet over EVERY arm result (binders occur as May; guards' rhs included).
  -- an EMPTY result set (no arms, or an arm with no guarded clauses) is vacuous — it
  -- falls to May (round 3: folding from Never would "prove" the degenerate case).
  CCase _ alts ->
    let
      armFacts = alts >>= \alt ->
        let
          sc' = foldl (\s v -> Tuple v MayBeByNeed : s) sc (alt.binders >>= binderVarsOf)
        in
          case alt.result of
            Uncond b -> [ exprFact sc' b ]
            Guarded gs -> map (\g -> exprFact sc' g.rhs) gs
    in
      if Array.null armFacts then MayBeByNeed
      else foldl meet NeverByNeed armFacts
  _ -> MayBeByNeed

-- | An expression's RESULT fact, walking `Let` spines with occurrence scope.
exprFact :: Scope -> Expr -> ByNeedFact
exprFact sc = case _ of
  Ret c -> cexprFact sc c
  Let x c body -> exprFact (Tuple x (cexprFact sc c) : sc) body
  LetRec binds body ->
    exprFact (foldl (\s r -> Tuple r.var MayBeByNeed : s) sc binds) body

-- | Walk one expression, counting every in-scope demand site's fact; binders extend the
-- | occurrence scope exactly where the recipes bind them.
census :: Scope -> Expr -> CensusCounts -> CensusCounts
census sc e0 acc0 = case e0 of
  Ret c -> goC sc c acc0
  Let x c body -> census (Tuple x (cexprFact sc c) : sc) body (goC sc c acc0)
  LetRec binds body ->
    let
      sc' = foldl (\s r -> Tuple r.var MayBeByNeed : s) sc binds
    in
      census sc' body (foldl (\a r -> census sc' r.rhs a) acc0 binds)
  where
  -- only VARIABLE operands emit a force chain (`forceAtom`); guard results always force.
  forcedVar cls a acc = case a of
    AtomVar x -> bump cls (lookupFact sc x) acc
    _ -> acc

  goC scope c acc = case c of
    CPrim _ args -> foldl (flip (forcedVar SPrimOperand)) acc args
    CIf a t e -> census scope e (census scope t (forcedVar SIfCond a acc))
    CAccessor a _ -> forcedVar SAccessorBase a acc
    -- ONLY the base forces (update values are plain `atom` reads — no chain).
    CUpdate a _ -> forcedVar SUpdateBase a acc
    CCase scruts alts ->
      let
        acc1 = foldl (flip (forcedVar SCaseScrutinee)) acc scruts
        -- arm binders are occurrences over the (May) extracted sub-values; guards force.
        goAlt ac alt =
          let
            scope' = foldl (\s v -> Tuple v MayBeByNeed : s) scope (alt.binders >>= binderVars)
          in
            case alt.result of
              Uncond b -> census scope' b ac
              Guarded gs -> foldl
                ( \a g -> census scope' g.rhs
                    (bump SGuardResult (exprFact scope' g.guard) (census scope' g.guard a))
                )
                ac
                gs
      in
        foldl goAlt acc1 alts
    CLam ps body ->
      -- a nested lambda is its OWN activation (review round 2): captures are May, so the
      -- body starts from a FRESH scope holding only its params (free vars = lookup miss
      -- = May) — keeping the outer scope would launder an outer Never into the lambda.
      census (foldl (\s p -> Tuple p MayBeByNeed : s) Nil ps) body acc
    CApp _ _ -> acc
    CPerform _ -> acc
    CAtom _ -> acc
    CCtor _ _ _ -> acc
    CArray _ -> acc
    CRecord _ -> acc

  binderVars = binderVarsOf

-- | Census one `Gdef`: `Gfun` params are `May` occurrences; `Gcaf` bodies start empty;
-- | `Grec` members are `May` (cells).
censusGdef :: CensusCounts -> Gdef -> CensusCounts
censusGdef acc = case _ of
  Gfun _ ps body -> census (foldl (\s p -> Tuple p MayBeByNeed : s) Nil ps) body acc
  Gcaf _ body -> census Nil body acc
  Grec binds ->
    let
      sc = foldl (\s (Tuple m _) -> Tuple m MayBeByNeed : s) Nil binds
    in
      foldl (\a (Tuple _ e) -> census sc e a) acc binds
