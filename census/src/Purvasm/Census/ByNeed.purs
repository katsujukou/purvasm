-- | The ADR-0107 by-need demand-site census — **measurement only**, and tool-owned: the census
-- | exists so the numbers an ADR closes on stay re-runnable. It answered the slice-0 gate
-- | (`--no-opt` 0.14 % vs `--opt` 16.79 %, the latter carrying the 2026-08-07 decision that slice 1
-- | proceeds), and it stays the instrument that re-measures the `Never` population whenever the
-- | optimiser's inlining policy — which is what manufactures provable sites — moves.
-- |
-- | **It does not own a lattice.** Every fact and every decision comes from the compiler's own
-- | `Backend.LLVM.ByNeed`, so a census measures what the compiler decides, not what the instrument
-- | believes. What lives here is the OCCURRENCE walk: which demand sites the emitter reaches, and
-- | how often. That is a different question from the fact — one proof site is emitted once per
-- | decision-tree leaf it survives into (ADR-0107 §2's proof-site / emission-occurrence pin) — and
-- | it is why `CCase` is walked through the SHARED `MatchCompile` tree rather than the raw
-- | alternatives.
-- |
-- | The walk crosses activation boundaries exactly where the emitter does: a `CLam` body and a
-- | `LetRec` member right-hand side are their own activations, so each gets its OWN fact set from
-- | `activationFacts`, as `emitFunction` gets its own plan.
module Purvasm.Census.ByNeed
  ( SiteClass(..)
  , Census
  , emptyCensus
  , mergeCensus
  , censusOf
  , censusGdefs
  , censusEntry
  , siteClasses
  , siteCount
  , totalSites
  , elidedSites
  , emittedSites
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Backend.LLVM.ByNeed (FactMap, activationFacts, elidesForce, elidesForcedValue, noFacts)
import Purvasm.Compiler.Backend.LLVM.Types (Gdef(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.MatchCompile (DTree(..))
import Purvasm.Compiler.MiddleEnd.MatchCompile (compile) as MatchCompile

-- | The demand recipes that emit a force chain, one constructor per `forceValue` call site in the
-- | emitter. `SEntryValue` is the entry stub's single force of a `--value` program's result — out
-- | of ADR-0107's elision scope, but counted so the accounting against emitted chains is total.
data SiteClass
  = SPrimOperand
  | SIfCond
  | SGuardResult
  | SCaseScrutinee
  | SAccessorBase
  | SUpdateBase
  | SEntryValue

derive instance eqSiteClass :: Eq SiteClass
derive instance ordSiteClass :: Ord SiteClass

instance showSiteClass :: Show SiteClass where
  show = case _ of
    SPrimOperand -> "prim-operand"
    SIfCond -> "if-cond"
    SGuardResult -> "guard-result"
    SCaseScrutinee -> "case-scrutinee"
    SAccessorBase -> "accessor-base"
    SUpdateBase -> "update-base"
    SEntryValue -> "entry-value"

-- | Every site class, in report order.
siteClasses :: Array SiteClass
siteClasses = [ SPrimOperand, SIfCond, SGuardResult, SCaseScrutinee, SAccessorBase, SUpdateBase, SEntryValue ]

-- | Per-class emission-occurrence counts: `elided` are the occurrences the compiler proved
-- | `NeverByNeed` and emits no chain for; `emitted` are the rest. `elided + emitted` is the
-- | occurrence total — the chains that WOULD be emitted with the lattice switched off.
type Census = Map SiteClass { elided :: Int, emitted :: Int }

emptyCensus :: Census
emptyCensus = Map.empty

mergeCensus :: Census -> Census -> Census
mergeCensus = Map.unionWith \a b -> { elided: a.elided + b.elided, emitted: a.emitted + b.emitted }

siteCount :: Census -> SiteClass -> { elided :: Int, emitted :: Int }
siteCount c cls = fromMaybe { elided: 0, emitted: 0 } (Map.lookup cls c)

-- | Every demand-site occurrence, elided or not.
totalSites :: Census -> Int
totalSites = foldl (\n r -> n + r.elided + r.emitted) 0

-- | The occurrences whose force the compiler elided — the count the deleted chains must equal.
elidedSites :: Census -> Int
elidedSites = foldl (\n r -> n + r.elided) 0

-- | The occurrences that still emit a chain — the count the emitted `.ll` must contain.
emittedSites :: Census -> Int
emittedSites = foldl (\n r -> n + r.emitted) 0

bump :: SiteClass -> Boolean -> Census -> Census
bump cls elided = Map.alter (Just <<< add <<< fromMaybe { elided: 0, emitted: 0 }) cls
  where
  add r = if elided then r { elided = r.elided + 1 } else r { emitted = r.emitted + 1 }

-- | A pending unit of walking: an expression or a decision-tree node, each under the fact set of
-- | the activation it belongs to. Both live on ONE heap worklist so neither `Let` spines nor long
-- | guard fall-through chains recurse on the host stack.
data Work
  = WExpr FactMap Expr
  | WTree FactMap DTree

-- | Walk one activation body under its OWN fact set, counting every demand-site occurrence and
-- | whether the compiler elides it.
censusOf :: FactMap -> Expr -> Census -> Census
censusOf facts0 e0 acc0 = tailRec go { work: WExpr facts0 e0 : Nil, acc: acc0 }
  where
  go st = case st.work of
    Nil -> Done st.acc
    Cons w rest -> case w of
      WExpr facts e -> Loop (goExpr facts e rest st.acc)
      WTree facts t -> Loop (goTree facts t rest st.acc)

  goExpr facts e rest acc = case e of
    Ret c -> goC facts c rest acc
    Let _ c body -> let st = goC facts c rest acc in { work: WExpr facts body : st.work, acc: st.acc }
    -- a `LetRec` member's right-hand side is built as a by-need cell over the shared env: its own
    -- activation, so its facts are computed afresh (its free names then miss and read `May`).
    LetRec binds body ->
      { work: foldl (\w b -> WExpr (activationFacts [] b.rhs) b.rhs : w) (WExpr facts body : rest) binds
      , acc: acc
      }

  -- Only a VARIABLE operand emits a chain: `Emit.forceAtom` passes a literal / foreign through.
  forced facts cls a acc = case a of
    AtomVar _ -> bump cls (elidesForce facts a) acc
    _ -> acc

  goC facts c rest acc = case c of
    CPrim _ args -> { work: rest, acc: foldl (\a arg -> forced facts SPrimOperand arg a) acc args }
    CIf a t e -> { work: WExpr facts t : WExpr facts e : rest, acc: forced facts SIfCond a acc }
    CAccessor a _ -> { work: rest, acc: forced facts SAccessorBase a acc }
    -- ONLY the base forces: the update values are plain `atom` reads (`Emit.cexpr` `CUpdate`).
    CUpdate a _ -> { work: rest, acc: forced facts SUpdateBase a acc }
    -- The emitter lowers a `case` through the SHARED decision tree, so the census walks the tree
    -- too — a row surviving into several specialised submatrices is emitted once per leaf.
    CCase scruts alts ->
      let
        { scrutBinds, tree } = MatchCompile.compile scruts alts
        acc' = foldl (\a (_ /\ scrut) -> forced facts SCaseScrutinee scrut a) acc scrutBinds
      in
        { work: WTree facts tree : rest, acc: acc' }
    -- a nested lambda is lambda-lifted: its own activation, its own facts.
    CLam ps body -> { work: WExpr (activationFacts ps body) body : rest, acc: acc }
    CApp _ _ -> { work: rest, acc: acc }
    CPerform _ -> { work: rest, acc: acc }
    CAtom _ -> { work: rest, acc: acc }
    CCtor _ _ _ -> { work: rest, acc: acc }
    CArray _ -> { work: rest, acc: acc }
    CRecord _ -> { work: rest, acc: acc }

  goTree facts t rest acc = case t of
    Dfail _ -> { work: rest, acc: acc }
    Dleaf _ body -> { work: WExpr facts body : rest, acc: acc }
    -- A guard result is forced whatever its shape (`Emit` forces the guard body's VALUE, not an
    -- atom), so every clause contributes an occurrence.
    Dguard _ clauses ft ->
      let
        step st clause =
          { work: WExpr facts clause.guard : WExpr facts clause.rhs : st.work
          , acc: bump SGuardResult (elidesForcedValue facts clause.guard) st.acc
          }
      in
        foldl step { work: WTree facts ft : rest, acc: acc } clauses
    DswitchCtor _ arms def ->
      { work: foldl (\w (_ /\ arm) -> WTree facts arm.sub : w) (WTree facts def : rest) arms, acc: acc }
    DswitchLit _ arms def ->
      { work: foldl (\w (_ /\ sub) -> WTree facts sub : w) (WTree facts def : rest) arms, acc: acc }
    DswitchLen _ arms def ->
      { work: foldl (\w (_ /\ arm) -> WTree facts arm.sub : w) (WTree facts def : rest) arms, acc: acc }
    DexpandRecord _ _ sub -> { work: WTree facts sub : rest, acc: acc }

-- | Census one module object's gdefs, entering each as its own activation exactly as the emitter
-- | plans it: a `Gfun`'s parameters, a `Gcaf`'s bare body, a `Grec`'s member right-hand sides.
censusGdefs :: Array Gdef -> Census
censusGdefs = foldl one emptyCensus
  where
  one acc = case _ of
    Gfun _ ps body -> censusOf (activationFacts ps body) body acc
    Gcaf _ body -> censusOf (activationFacts [] body) body acc
    Grec binds -> foldl (\a (Tuple _ e) -> censusOf (activationFacts [] e) e a) acc binds

-- | Census the entry object: only the entry stub's own body is emitted there (`pv_init_all` merely
-- | calls each reachable `$init`), plus the single force of a `--value` program's result. The entry
-- | stub has no plan, so its facts are empty — every force there is emitted, and the census says so.
censusEntry :: Boolean -> Expr -> Census
censusEntry isEffect entry =
  let
    acc = censusOf noFacts entry emptyCensus
  in
    if isEffect then acc else bump SEntryValue false acc
