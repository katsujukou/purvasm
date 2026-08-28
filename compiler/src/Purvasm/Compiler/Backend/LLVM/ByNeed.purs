-- | The ADR-0107 by-need fact lattice: which of an activation's values can PROVABLY never be an
-- | unforced `ByNeed` cell, so their demand sites need no force chain (and no safepoint).
-- |
-- | The lattice is two-valued and pinned conservative (§1): every positive `NeverByNeed` arm names
-- | its producer, and anything unmatched falls to `MayBeByNeed` through a catch-all — a new ANF node
-- | is `May` until someone proves otherwise. A wrong `Never` is a SEMANTIC bug (a cell read as a
-- | value), and neither the ADR-0105 token/epoch net nor the type system catches it, so the only
-- | defences are this totality, the single shared decision set below, and the behavioural fixture.
-- |
-- | **The decision set (§2, amended 2026-08-07).** One `FactMap` per activation is computed ONCE
-- | (by `Liveness.activationPlan`, which carries it in the plan) and is the ONLY thing either
-- | consumer reads: the liveness pass asks it whether a forced operand still contributes a
-- | safepoint, and the emitter asks it whether to emit the chain at all. Neither derives facts of
-- | its own, so they cannot disagree.
-- |
-- | **Why a NAME is a sound key here.** ADR-0107 §2 pins binding identity to the binder OCCURRENCE,
-- | because the ANF has no no-shadowing contract and a flat name map could otherwise let an outer
-- | `Never` leak into an inner `May` binding. This module keys by name and preserves the property by
-- | POISONING: a name bound at more than one occurrence in the activation is `MayBeByNeed`
-- | everywhere, whatever its right-hand sides. Every lookup is then correct-by-construction for the
-- | site that performs it — the fact is either the unique binding's, or the safe value — and both
-- | consumers can key into the SAME map with nothing but the operand's name. The cost is the
-- | provable-but-shadowed bindings, which the census reports.
-- |
-- | **Activation-local by construction.** `CLam` bodies and `LetRec` member right-hand sides are
-- | separate activations (lambda-lifted / built as cells), so this walk stops at them: their values
-- | arrive through `%env` or a cell, and each is planned — and gets its own `FactMap` — when it is
-- | emitted. Descending would launder an outer `Never` into a capture.
module Purvasm.Compiler.Backend.LLVM.ByNeed
  ( ByNeedFact(..)
  , FactMap
  , meet
  , noFacts
  , activationFacts
  , factOfAtom
  , factOfExpr
  , elidesForce
  , elidesForcedValue
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr, CExprF(..), Expr, ExprF(..), Rhs, RhsF(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (fvExpr)
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | `NeverByNeed` is a PROOF that the value is not an unforced cell; `MayBeByNeed` is the safe
-- | default and the meet's absorbing element.
data ByNeedFact = NeverByNeed | MayBeByNeed

derive instance eqByNeedFact :: Eq ByNeedFact
derive instance ordByNeedFact :: Ord ByNeedFact

instance showByNeedFact :: Show ByNeedFact where
  show = case _ of
    NeverByNeed -> "NeverByNeed"
    MayBeByNeed -> "MayBeByNeed"

-- | The §1 meet: `Never` only when both sides are, so a branch is provable only if every arm is.
meet :: ByNeedFact -> ByNeedFact -> ByNeedFact
meet NeverByNeed NeverByNeed = NeverByNeed
meet _ _ = MayBeByNeed

-- | One activation's decision set: binder name → fact, with shadowed names poisoned to `May`.
-- |
-- | `enabled` is part of the SET, not of either consumer: the measurement counterfactual
-- | (`PURVASM_BYNEED_OFF=1`) switches the whole decision off in one place, so the plan and the
-- | emitter cannot end up disagreeing about whether the lattice is live. Gating at the consumers
-- | would have left the fact-independent producers (a scalar-primitive result is `Never` whatever
-- | the map says) still eliding with the switch off — which is exactly the bug the unit accounting
-- | matrix caught.
newtype FactMap = FactMap { enabled :: Boolean, byName :: Map String ByNeedFact }

-- | The empty decision set — nothing is elided and every lookup is `May`. This is the conservative
-- | activation state (`beginFn`, the `LClosure` wrapper arm which has no plan, the entry stub) and
-- | the measurement counterfactual's whole-program state.
noFacts :: FactMap
noFacts = FactMap { enabled: false, byName: Map.empty }

-- | The fact of a name: an unbound name is a global / native leaf, `May` by the §1 pin (a `Gcaf`
-- | can alias a `Grec` member's cell, and cross-module provenance is out of ADR-0107's scope).
factOfName :: FactMap -> String -> ByNeedFact
factOfName (FactMap f) x = fromMaybe MayBeByNeed (Map.lookup x f.byName)

factOfAtom :: FactMap -> Atom -> ByNeedFact
factOfAtom facts = case _ of
  AtomVar x -> factOfName facts x
  -- literals are values (a boxed one allocates, but never a cell)
  AtomLit _ -> NeverByNeed
  -- a foreign leaf reference builds a closure, but the §1 pin keeps foreign = `May`
  AtomForeign _ -> MayBeByNeed

-- | **The decision.** Whether this operand's force chain can be elided at a demand site. Only a
-- | variable ever emits a chain (`Emit.forceAtom` passes literals and foreigns through unforced),
-- | so a non-variable answers `false`: there is nothing to elide.
elidesForce :: FactMap -> Atom -> Boolean
elidesForce facts@(FactMap f) = case _ of
  AtomVar x -> f.enabled && factOfName facts x == NeverByNeed
  _ -> false

-- | The same decision for a forced VALUE rather than an operand — the guard-result site, where the
-- | emitter forces a computed value. Applying the shared classifier to the shared `FactMap` and the
-- | same term is not a second derivation: both consumers that ask get the same answer by
-- | referential transparency.
elidesForcedValue :: FactMap -> Expr -> Boolean
elidesForcedValue facts@(FactMap f) e = f.enabled && factOfExpr facts e == NeverByNeed

-- | The §1 scalar-primitive `Never` set: arithmetic / comparison / logical results, whose runtime
-- | helpers return an immediate or a fresh box. The projections (`RecordGet`/`IndexArray`/…) read
-- | container slots that may hold cells — by-need dictionary members — so they stay `May` through
-- | the catch-all.
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

-- | A computation's result fact (the §1 producers; everything else `May` through the catch-all).
factOfCExpr :: FactMap -> CExpr -> ByNeedFact
factOfCExpr facts = case _ of
  CAtom a -> factOfAtom facts a
  CPrim op _ -> if primNever op then NeverByNeed else MayBeByNeed
  CCtor _ _ _ -> NeverByNeed
  CArray _ -> NeverByNeed
  CRecord _ -> NeverByNeed
  CLam _ _ _ -> NeverByNeed
  CIf _ t e -> meet (factOfExpr facts t) (factOfExpr facts e)
  -- the §1 branch meet over every arm result. An EMPTY result set — no alternatives, or an
  -- alternative with no guarded clauses — is vacuous and falls to `May`: folding from `Never`
  -- would "prove" the degenerate case.
  CCase _ alts ->
    let
      armFacts = alts >>= \alt -> case alt.result of
        Uncond b -> [ factOfExpr facts b ]
        Guarded gs -> map (\g -> factOfExpr facts g.rhs) gs
    in
      if Array.null armFacts then MayBeByNeed
      else foldl meet NeverByNeed armFacts
  _ -> MayBeByNeed

-- | An expression's RESULT fact. The `Let`/`LetRec` spine is an explicit `tailRec` loop, not
-- | recursion: this function is mutually recursive with `factOfCExpr`, which disables the
-- | PureScript backend's self-tail-call optimisation, and self-host bodies carry spines thousands
-- | of bindings long. Arm binders need no rebinding here — the activation's map already carries
-- | every binder occurrence (poisoned where shadowed).
factOfExpr :: FactMap -> Expr -> ByNeedFact
factOfExpr facts e0 = tailRec go e0
  where
  go = case _ of
    Ret c -> Done (factOfCExpr facts c)
    Let _ _ body -> Loop body
    LetRec _ body -> Loop body

-- --- building the activation's decision set -------------------------------------------------------

-- | The variable names a binder introduces at its occurrence.
binderVarsOf :: Binder -> Array String
binderVarsOf = case _ of
  BNull -> []
  BVar v -> [ v ]
  BLit _ -> []
  BNamed v inner -> [ v ] <> binderVarsOf inner
  BCtor _ subs -> subs >>= binderVarsOf
  BArray subs -> subs >>= binderVarsOf
  BRecord fields -> fields >>= (_.binder >>> binderVarsOf)

-- | A binding occurrence found while collecting: the name, and the `CExpr` whose fact it takes
-- | (`Nothing` for the occurrences that are `May` by pin — pattern binders, `LetRec` members).
data Occurrence = Occurrence String (Maybe CExpr)

-- | Compute one activation's decision set from its PARAMETERS and its BODY — and nothing else.
-- |
-- | Two passes over the same collected occurrence list: the first counts names (a name occurring at
-- | more than one occurrence is poisoned to `May`), the second resolves each surviving `Let`
-- | binding's fact. The collection order is post-order, so a binding nested inside a right-hand
-- | side's branch is resolved BEFORE the binding whose fact reads it.
-- |
-- | **Captures are not an input, deliberately.** Every capture is by definition a FREE name of the
-- | body, and free names are collected here directly, so the decision set does not depend on the
-- | lifting decision that produced the capture list. That matters twice over: a free name that is
-- | ALSO bound inside the body must be poisoned (a reference before the inner binding resolves to
-- | the free one, and a name-keyed map cannot tell them apart — the soundness case), and any
-- | out-of-tree walk over the same body (the ADR-0107 census) reproduces these facts EXACTLY
-- | without re-deriving the emitter's captures, which is where an instrument would otherwise drift
-- | from the compiler.
activationFacts :: Array String -> Expr -> FactMap
activationFacts params body =
  let
    -- Parameters arrive through the ABI, free names through `%env` / a global handle / an
    -- enclosing scope: `May` by pin. They also OCCUPY their names, so a `Let` shadowing any of
    -- them is poisoned like any other rebinding.
    entry = map (\p -> Occurrence p Nothing) params
      <> map (\x -> Occurrence x Nothing) (Set.toUnfoldable (fvExpr (Set.fromFoldable params) body))
    occs = entry <> collect body

    counts = foldl (\m (Occurrence x _) -> Map.insertWith (+) x 1 m) Map.empty occs

    step byName (Occurrence x mc) =
      if fromMaybe 0 (Map.lookup x counts) > 1 then Map.insert x MayBeByNeed byName
      else case mc of
        Nothing -> Map.insert x MayBeByNeed byName
        Just c -> Map.insert x (factOfCExpr (FactMap { enabled: true, byName }) c) byName
  in
    FactMap { enabled: true, byName: foldl step Map.empty occs }

-- | Every binding occurrence of this activation, in POST-ORDER (a right-hand side's own nested
-- | bindings precede the binding it defines). Stops at activation boundaries (`CLam` bodies,
-- | `LetRec` member right-hand sides) — those are separate activations with their own maps.
collect :: Expr -> Array Occurrence
collect e0 = Array.reverse (Array.fromFoldable (tailRec go { work: IExpr e0 : Nil, acc: Nil }))
  where
  go st = case st.work of
    Nil -> Done st.acc
    Cons item rest -> case item of
      IOcc o -> Loop { work: rest, acc: o : st.acc }
      IExpr e -> case e of
        Ret c -> Loop { work: expand c rest, acc: st.acc }
        Let x c body ->
          Loop { work: expand c (IOcc (Occurrence x (Just c)) : IExpr body : rest), acc: st.acc }
        LetRec binds body ->
          Loop
            { work: foldl (\w b -> IOcc (Occurrence b.var Nothing) : w) (IExpr body : rest) binds
            , acc: st.acc
            }

  -- a node's own contribution: its arm binders (`May` by pin) and the sub-expressions belonging to
  -- THIS activation (branch arms, case arm bodies and guards). `CLam` bodies are deliberately
  -- absent. Pushed in FRONT of the node's own occurrence, which is what makes the order post-order.
  expand c rest = case c of
    CIf _ t e -> IExpr t : IExpr e : rest
    CCase _ alts -> foldl altItems rest alts
    _ -> rest

  altItems rest alt =
    let
      binders = foldl (\w v -> IOcc (Occurrence v Nothing) : w) rest (alt.binders >>= binderVarsOf)
    in
      case alt.result of
        Uncond b -> IExpr b : binders
        Guarded gs -> foldl (\w g -> IExpr g.guard : IExpr g.rhs : w) binders gs

-- | A pending collection item: an expression to walk, or an occurrence to record.
data Item
  = IExpr Expr
  | IOcc Occurrence
