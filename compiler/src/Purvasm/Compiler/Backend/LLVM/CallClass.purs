-- | ADR-0108 §1: how a call site was lowered, and — when it could not be made direct — WHY.
-- |
-- | `Emit.directTarget` decides directness; until ADR-0108 it returned `Maybe FnInfo` and threw the
-- | reason away, so "why is this call site generic" (the question the apply-count track exists to
-- | answer) was unanswerable without re-deriving the decision outside the emitter. The reason now
-- | comes back with the answer, and every lowering arm records what it actually emitted.
-- |
-- | **The event is a SUM, not a form/outcome pair (ADR-0108 §1).** A pair would make
-- | `DirectNonTail` + a `MissReason`, or `GenericApply` + an `FnInfo`, constructible — states that
-- | mean nothing. As a sum a direct form carries only a target and a generic form only a reason,
-- | so the invalid combinations are unrepresentable and the six constructors stand one-to-one with
-- | the six accounting columns the census reconciles against the emitted `.ll`.
-- |
-- | **The form is not the classifier's to decide.** Whether a target becomes a `musttail` or a
-- | plain direct call, and whether a miss becomes a `pv_apply` or a `pv_tailcall`, is settled AFTER
-- | `directTarget` by the tail/`inDirect` branch — and two classes never reach the classifier at
-- | all. So each event is written by the arm that emits the call, not assembled from two halves.
module Purvasm.Compiler.Backend.LLVM.CallClass
  ( MissReason(..)
  , CallEvent(..)
  , CallClass(..)
  , missReasonName
  , callClassName
  , callEventClass
  , callClasses
  , profileSlotNames
  , profileSlot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Purvasm.Compiler.Backend.LLVM.Types (FnInfo)

-- | Why a call site could not be lowered to a direct known-arity call. The constructors mirror the
-- | LEAVES of `directTarget`'s decision tree (ADR-0108 §1) — they are not a priority list: a
-- | self-call shortcut that does not apply FALLS THROUGH and can still resolve directly, so
-- | "the shortcut missed" is not an outcome and has no constructor.
data MissReason
  -- | the callee atom is a literal / foreign / computed value, not a variable
  = MissCalleeNotVar
  -- | a local binding with no `knownFn` fact (a parameter, a capture, a `let`-bound value)
  | MissLocalUnknownFn
  -- | a local `knownFn` whose arity ≠ this call's argument count
  | MissArityLocal
  -- | neither a local binding nor a known global key. A DIAGNOSTIC class: `readVar` crashes on such
  -- | a callee, so a successfully emitted object cannot contain one — a non-zero count is a
  -- | compiler bug report, not an optimisation lever (ADR-0108 §1).
  | MissUnknownKey
  -- | this object's own `gfns` fact says a different arity (never falls through to the surface)
  | MissArityOwnModule
  -- | a key THIS OBJECT defines for which it holds no statically known direct-function shape (no
  -- | `gfns` fact) — a `Gcaf`, or a `Grec` member whose right-hand side is not a lambda. Such a
  -- | binding may still evaluate to a closure at run time; what the classifier knows is only that
  -- | there is no direct entry to call. Unreachable in the entry object, which defines nothing.
  | MissOwnObjectNotFn
  -- | a key from outside this object with no published direct-call fact — an unpublished function
  -- | or a dependency's `Gcaf`; separating those needs the `.pmi` `ExportKind` (owed, ADR-0108 §1)
  | MissDepNoDirectFact
  -- | a published cross-module fact whose arity ≠ this call's argument count
  | MissArityCrossModule

derive instance eqMissReason :: Eq MissReason
derive instance ordMissReason :: Ord MissReason

instance showMissReason :: Show MissReason where
  show = missReasonName

-- | The report name (also the census's TSV token).
missReasonName :: MissReason -> String
missReasonName = case _ of
  MissCalleeNotVar -> "callee-not-var"
  MissLocalUnknownFn -> "local-unknown-fn"
  MissArityLocal -> "arity-local"
  MissUnknownKey -> "unknown-key"
  MissArityOwnModule -> "arity-own-module"
  MissOwnObjectNotFn -> "own-object-not-fn"
  MissDepNoDirectFact -> "dep-no-direct-fact"
  MissArityCrossModule -> "arity-cross-module"

-- | One emitted GUEST-call occurrence, as the arm that emitted it saw it. Scope note: this is not
-- | every LLVM `call` a lowering emits — the runtime machinery beside it (`pv_root`, `pv_new_str`,
-- | the force chain) is ADR-0105's classified seam, not dispatch, and has no event here.
data CallEvent
  -- | `guestDirect` at a call site: a known target, called and settled
  = DirectNonTail FnInfo
  -- | `musttailWith`: a known target in tail position inside a direct entry
  | DirectMusttail FnInfo
  -- | `rtCall RtApply` at a non-tail `CApp`: generic dispatch, with the reason it stayed generic
  | GenericApply MissReason
  -- | `tailcallWith`: the generic TAIL form — a `pv_tailcall` trampoline store, NOT a `pv_apply`
  | GenericTail MissReason
  -- | the unsaturated-`CCtor` builder application: generic, and never classified (it applies a
  -- | closure the emitter just synthesised, so `directTarget` never sees it)
  | StructuralApply
  -- | a lifted function's own generic entry (`@<name>` unpack-and-call) — emitted per FUNCTION,
  -- | not per call site, so it is counted apart from both call columns
  | WrapperEntry

-- | An accounting column (ADR-0108 §2). A TYPE, not a string: the event sum's whole point is that
-- | invalid combinations are unrepresentable, and reporting through bare strings would give that
-- | back at the boundary — a typo'd column would type-check and quietly split a count in two.
-- | Rendering happens once, at the report edge (`callClassName`).
data CallClass
  = CDirectNonTail
  | CDirectMusttail
  | CGenericApply
  | CGenericTail
  | CStructuralApply
  | CWrapperEntry

derive instance eqCallClass :: Eq CallClass
derive instance ordCallClass :: Ord CallClass

instance showCallClass :: Show CallClass where
  show = callClassName

-- | The accounting column an event belongs to. Targets collapse to their form because the column is
-- | about HOW the call was emitted; reasons are reported on their own axis alongside. Total by
-- | construction — a new `CallEvent` constructor fails to compile here.
callEventClass :: CallEvent -> CallClass
callEventClass = case _ of
  DirectNonTail _ -> CDirectNonTail
  DirectMusttail _ -> CDirectMusttail
  GenericApply _ -> CGenericApply
  GenericTail _ -> CGenericTail
  StructuralApply -> CStructuralApply
  WrapperEntry -> CWrapperEntry

-- | The report name (the census's TSV token) — the ONLY place a class becomes a string.
callClassName :: CallClass -> String
callClassName = case _ of
  CDirectNonTail -> "direct-nontail"
  CDirectMusttail -> "direct-musttail"
  CGenericApply -> "generic-apply"
  CGenericTail -> "generic-tail"
  CStructuralApply -> "structural-apply"
  CWrapperEntry -> "wrapper-entry"

-- | Every accounting column, in report order — the census emits a row per column even at zero, so a
-- | column that stops occurring is visible as a zero rather than as a missing line. A unit test
-- | pins that this enumeration is complete (the compiler catches a missing `callClassName` arm, but
-- | not a class missing from THIS list).
callClasses :: Array CallClass
callClasses = [ CDirectNonTail, CDirectMusttail, CGenericApply, CGenericTail, CStructuralApply, CWrapperEntry ]

-- --- ADR-0108 §3: the dynamic profile's slot space -------------------------------------------------

-- | The reasons a generic dispatch can EXECUTE with. `MissUnknownKey` is deliberately absent: it
-- | cannot reach a dispatch in a valid build (`readVar` crashes first, §1), so instrumenting it
-- | would reserve a counter that must always read zero.
profiledReasons :: Array MissReason
profiledReasons =
  [ MissCalleeNotVar
  , MissLocalUnknownFn
  , MissArityLocal
  , MissArityOwnModule
  , MissOwnObjectNotFn
  , MissDepNoDirectFact
  , MissArityCrossModule
  ]

-- | The generic FORMS a dispatch can take. A reason means a different thing in each — different
-- | emitted operation, different lever — so the counters are the product, never the sum.
profiledForms :: Array CallClass
profiledForms = [ CGenericApply, CGenericTail ]

-- | The profile's slot names, IN SLOT ORDER — `(form × reason)` then the structural apply, which
-- | has no reason to key on.
-- |
-- | This array is the ONE definition of the profile's layout. An instrumented program hands it to
-- | the runtime at start-up (`pv_applyprofile_register`), so the runtime labels its counters from
-- | the compiler's own names instead of a mirrored copy that could drift — adding a `MissReason`
-- | here changes the emitted blob and the printed schema together.
profileSlotNames :: Array String
profileSlotNames =
  (profiledForms >>= \f -> map (\r -> callClassName f <> "/" <> missReasonName r) profiledReasons)
    <> [ callClassName CStructuralApply ]

-- | The slot an executing dispatch bumps, or `Nothing` for the events that are not instrumented:
-- | the direct forms (they are not dispatches), the wrapper entry (per function, not per call), and
-- | `MissUnknownKey` (unreachable, per `profiledReasons`).
profileSlot :: CallEvent -> Maybe Int
profileSlot = case _ of
  GenericApply r -> slotOf CGenericApply r
  GenericTail r -> slotOf CGenericTail r
  StructuralApply -> Just (Array.length profiledForms * Array.length profiledReasons)
  DirectNonTail _ -> Nothing
  DirectMusttail _ -> Nothing
  WrapperEntry -> Nothing
  where
  slotOf form r = do
    fi <- Array.elemIndex form profiledForms
    ri <- Array.elemIndex r profiledReasons
    pure (fi * Array.length profiledReasons + ri)
