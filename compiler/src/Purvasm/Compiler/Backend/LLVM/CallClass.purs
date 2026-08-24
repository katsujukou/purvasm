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
-- | so the invalid combinations are unrepresentable and each constructor stands one-to-one with an
-- | accounting column the census reconciles against the emitted `.ll`. ADR-0109 added the four
-- | foreign classes and ADR-0113 the two `local-deferred` ones; the count is not fixed, but the
-- | one-to-one correspondence is.
-- |
-- | **The form is not the classifier's to decide.** Whether a target becomes a `musttail` or a
-- | plain direct call, and whether a miss becomes a `pv_apply` or a `pv_tailcall`, is settled AFTER
-- | `directTarget` by the tail/`inDirect` branch — and two classes never reach the classifier at
-- | all. So each event is written by the arm that emits the call, not assembled from two halves.
module Purvasm.Compiler.Backend.LLVM.CallClass
  ( MissReason(..)
  , CallTarget(..)
  , LocalCandidate
  , Form(..)
  , callForm
  , EmissionDecision(..)
  , decide
  , CallEvent(..)
  , eventOf
  , CallClass(..)
  , missReasonName
  , callClassName
  , callEventClass
  , callClasses
  , allMissReasons
  , profiledReasons
  , profileSlotNames
  , profileSlot
  , AllocSite(..)
  , allocSiteName
  , allocSites
  , allocSiteSlot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.ForeignRef (ForeignCallMode(..), ForeignRef, refArity, refKey)
import Purvasm.Compiler.Backend.LLVM.Types (BindOrigin, CandidateKind, CapturableFact, FnInfo, bindOriginName, bindOrigins, candidateKindName, candidateKinds, unFact)

-- | Why a call site could not be lowered to a direct known-arity call. The constructors mirror the
-- | LEAVES of `directTarget`'s decision tree (ADR-0108 §1) — they are not a priority list: a
-- | self-call shortcut that does not apply FALLS THROUGH and can still resolve directly, so
-- | "the shortcut missed" is not an outcome and has no constructor.
data MissReason
  -- | the callee atom is a FOREIGN symbol. The emitter holds an arity fact for many of these
  -- | (`Ctx.foreignArity`) and does not consult it here — whether that is a lever is exactly what
  -- | ADR-0108 §4 measures, so this class must not be merged with the one below.
  = MissCalleeForeign
  -- | the callee atom is a LITERAL. A DIAGNOSTIC class, like `MissUnknownKey`: applying a literal
  -- | is not something a well-typed program does, so a non-zero count is a compiler-bug candidate
  -- | rather than an optimisation lever (ADR-0108 §4 slice 1) — it is measured, never assumed zero.
  | MissCalleeLiteral
  -- | a local binding for which NO fact was derivable at its bind site, at any depth — ADR-0113's
  -- | OPAQUE population. It carries WHERE the binding was made; a recoverability bit beside it would
  -- | build a `BindOrigin × Recoverability` product with unreachable cells (`OLetLambda/opaque`,
  -- | `OParam/recoverable`), and the recoverable population has its own class instead
  -- | ([`CLocalDeferredApply`]/[`CLocalDeferredTail`]).
  | MissLocalUnknownFn BindOrigin
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
  MissCalleeForeign -> "callee-foreign"
  MissCalleeLiteral -> "callee-literal"
  MissLocalUnknownFn o -> "local-unknown-fn/" <> bindOriginName o
  MissArityLocal -> "arity-local"
  MissUnknownKey -> "unknown-key"
  MissArityOwnModule -> "arity-own-module"
  MissOwnObjectNotFn -> "own-object-not-fn"
  MissDepNoDirectFact -> "dep-no-direct-fact"
  MissArityCrossModule -> "arity-cross-module"

-- | What the CLASSIFIER knows about a call site: whether a direct target exists, and of which
-- | kind (ADR-0109 §1.2). ELIGIBILITY only — what is actually emitted is [`EmissionDecision`], which
-- | is a function of this, the call form, and the §5.2 feature knob.
-- |
-- | Keeping the two apart is what stops a slot changing meaning between the legs of an A/B: an
-- | eligible call lowered generically because the knob is off is NOT the same fact as a call the
-- | classifier declined, and folding them into one `MissReason` would make the residue counter mean
-- | "arity mismatch" in one leg and "arity mismatch + everything the knob turned off" in the other.
data CallTarget
  -- | a direct `tailcc` guest entry
  = GuestTarget FnInfo
  -- | a native leaf at its known, MATCHING arity (ADR-0109 §2)
  | ForeignTarget ForeignRef
  -- | a local binding carrying a CANDIDATE fact (ADR-0113 §2): a fact the emitter derived at the
  -- | bind site and does not use while the knob is off. ARITY IS DELIBERATELY NOT PART OF THIS —
  -- | "a candidate exists here" is all the classifier says, so the off-leg population is one
  -- | countable class, which is what makes ADR-0113 §4's transfer a closed sum rather than a
  -- | partition whose halves are counted by different mechanisms. Slices 1-2 lower every candidate
  -- | as DEFERRED; the knob that would lower some of them directly is slice 3's, and so are its
  -- | direct/arity counters.
  | LocalCandidateTarget LocalCandidate
  -- | no direct target, with the reason the classifier declined
  | GenericTarget MissReason

-- | What the classifier hands the decision about a candidate: the fact, and the derivation that
-- | produced it. The kind travels here rather than being recovered from the binding's
-- | `BindOrigin`, which cannot distinguish `AliasLocal` from `AliasGlobal` (both `OLetValue`).
type LocalCandidate =
  { fact :: CapturableFact
  , kind :: CandidateKind
  }

derive instance eqCallTarget :: Eq CallTarget

-- `Show` for the unit assertions: a classification that fails a test must print WHICH outcome it
-- was, not just that two opaque values differed.
instance showCallTarget :: Show CallTarget where
  show = case _ of
    GuestTarget info -> "GuestTarget " <> info.dsym <> "/" <> show info.arity
    ForeignTarget ref -> "ForeignTarget " <> refKey ref <> "/" <> show (refArity ref)
    LocalCandidateTarget c ->
      "LocalCandidateTarget " <> candidateKindName c.kind <> " " <> (unFact c.fact).dsym
        <> "/"
        <> show (unFact c.fact).arity
    GenericTarget r -> "GenericTarget " <> missReasonName r

-- | The two forms a call site can take. Not the classifier's to decide (ADR-0108 §1): it is settled
-- | by the `tail`/`inDirect` branch in the lowering, after the classification.
data Form
  = FApply
  | FTail

derive instance eqForm :: Eq Form
derive instance ordForm :: Ord Form

instance showForm :: Show Form where
  show = case _ of
    FApply -> "apply"
    FTail -> "tail"

-- | The call FORM at a site, from the tail flag, the activation kind, and WHICH target it is.
-- |
-- | Target-aware on purpose. Only the guest `musttail` shape additionally needs `inDirect`: there is
-- | no `%env` word to hand over outside a `tailcc` direct entry, so a guest call in tail position in
-- | any other activation is lowered as a call-then-`ret`. A foreign leaf call (ADR-0109 §3) and a
-- | generic dispatch have no such constraint and are tail iff the site is.
-- |
-- | It is a FUNCTION, and exported, because deriving it inline once for all three targets is exactly
-- | the defect review round 5 found: the guest form was handed to `decide` for every target, so a
-- | `tail && not inDirect` site would have emitted a tail recipe while recording an Apply event. That
-- | state is currently unreachable in emission — every tail context is a lifted body, which sets
-- | `inDirect` — so it cannot be pinned by a fixture; it is pinned HERE instead, over the whole
-- | input space, which is the honest place for an invariant no fixture can reach.
callForm :: { tail :: Boolean, inDirect :: Boolean } -> CallTarget -> Form
callForm st = case _ of
  GuestTarget _ -> if st.tail && st.inDirect then FTail else FApply
  ForeignTarget _ -> if st.tail then FTail else FApply
  -- a candidate is lowered generically while the knob is off, so it takes the generic shape here;
  -- when slice 3 introduces the knob, a candidate that becomes a guest call must have its form
  -- RECOMPUTED against `GuestTarget` (this function answers differently for it) — which is why the
  -- form must not be decided before the knob (ADR-0113 §2).
  LocalCandidateTarget _ -> if st.tail then FTail else FApply
  GenericTarget _ -> if st.tail then FTail else FApply

-- | What the lowering actually EMITS: a closed function of (target × form × feature knob).
data EmissionDecision
  = EmitGuestDirect FnInfo Form
  -- | eligible AND the slice is enabled
  | EmitForeignDirect ForeignRef Form
  -- | eligible, slice DISABLED — the §5.2 counterfactual leg only
  | EmitForeignDeferred ForeignRef Form
  -- | a CANDIDATE the knob left alone (ADR-0113 §2/§4). Emits today's generic dispatch
  -- | byte-for-byte; it differs from [`EmitGeneric`] only in which counter it bumps, which is what
  -- | lets slices 1–2 name the off-leg population without changing emission.
  | EmitLocalDeferred CandidateKind Form
  | EmitGeneric MissReason Form

-- | The decision, from the classifier's answer, the form, and whether foreign direct lowering is
-- | enabled. Total and closed: there is no arm in which an eligible foreign call becomes a
-- | `MissReason`, which is exactly the confusion §1.2 forbids.
-- |
-- | ADR-0113 slices 1–2 add one arm and no knob: a candidate is always DEFERRED.
decide :: ForeignCallMode -> Form -> CallTarget -> EmissionDecision
decide mode form = case _ of
  GuestTarget info -> EmitGuestDirect info form
  -- ADR-0113 slices 1–2: a candidate is ALWAYS deferred. The direct/arity arms, the three-stage
  -- knob and their counters are slice 3's, and slice 3 is a separate re-approval — so there is no
  -- code path, from any caller, that lowers a recovered fact today.
  LocalCandidateTarget c -> EmitLocalDeferred c.kind form
  ForeignTarget ref -> case mode, form of
    ViaApply, _ -> EmitForeignDeferred ref form
    -- slice B: the apply form only. The tail form stays DEFERRED here, which is what keeps
    -- `pv_tailcall_writes` invariant across slice B's pair and its two slices separable.
    DirectApplyOnly, FApply -> EmitForeignDirect ref FApply
    DirectApplyOnly, FTail -> EmitForeignDeferred ref FTail
    DirectApplyAndTail, _ -> EmitForeignDirect ref form
  GenericTarget reason -> EmitGeneric reason form

-- | The event a decision records — one per accounted guest-call occurrence, in one-to-one
-- | correspondence with the decision, so a decision and its event cannot disagree.
eventOf :: EmissionDecision -> CallEvent
eventOf = case _ of
  EmitGuestDirect info FApply -> DirectNonTail info
  EmitGuestDirect info FTail -> DirectMusttail info
  EmitLocalDeferred k FApply -> LocalDeferredApply k
  EmitLocalDeferred k FTail -> LocalDeferredTail k
  EmitForeignDirect ref FApply -> ForeignDirectApply ref
  EmitForeignDirect ref FTail -> ForeignDirectTail ref
  EmitForeignDeferred ref FApply -> ForeignDeferredApply ref
  EmitForeignDeferred ref FTail -> ForeignDeferredTail ref
  EmitGeneric reason FApply -> GenericApply reason
  EmitGeneric reason FTail -> GenericTail reason

-- | One emitted GUEST-call occurrence, as the arm that emitted it saw it. Scope note: this is not
-- | every LLVM `call` a lowering emits — the runtime machinery beside it (`pv_root`, `pv_new_str`,
-- | the force chain) is ADR-0105's classified seam, not dispatch, and has no event here.
data CallEvent
  -- | `guestDirect` at a call site: a known target, called and settled
  = DirectNonTail FnInfo
  -- | `musttailWith`: a known target in tail position inside a direct entry
  | DirectMusttail FnInfo
  -- | a candidate the knob left generic (ADR-0113). These ARE dispatches — they belong on the
  -- | dispatch side of the §3 identities, not beside the direct forms.
  | LocalDeferredApply CandidateKind
  | LocalDeferredTail CandidateKind
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
  -- | ADR-0109 §2: a saturated native leaf called DIRECTLY through its `@pvf_` entry, non-tail
  | ForeignDirectApply ForeignRef
  -- | …and in tail position (§3: a direct call, then the frame pop and `ret` — no trampoline)
  | ForeignDirectTail ForeignRef
  -- | ADR-0109 §1.2: ELIGIBLE, but lowered generically because the §5.2 knob is off. Its own class,
  -- | so the residue counter (`callee-foreign`) means the same thing in both legs of the A/B.
  -- | Retired with the knob; until then the census fail-closes on it being non-zero in a knob-on run.
  | ForeignDeferredApply ForeignRef
  | ForeignDeferredTail ForeignRef

-- | An accounting column (ADR-0108 §2). A TYPE, not a string: the event sum's whole point is that
-- | invalid combinations are unrepresentable, and reporting through bare strings would give that
-- | back at the boundary — a typo'd column would type-check and quietly split a count in two.
-- | Rendering happens once, at the report edge (`callClassName`).
data CallClass
  = CDirectNonTail
  | CDirectMusttail
  | CLocalDeferredApply
  | CLocalDeferredTail
  | CGenericApply
  | CGenericTail
  | CStructuralApply
  | CWrapperEntry
  | CForeignDirectApply
  | CForeignDirectTail
  | CForeignDeferredApply
  | CForeignDeferredTail

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
  LocalDeferredApply _ -> CLocalDeferredApply
  LocalDeferredTail _ -> CLocalDeferredTail
  GenericApply _ -> CGenericApply
  GenericTail _ -> CGenericTail
  StructuralApply -> CStructuralApply
  WrapperEntry -> CWrapperEntry
  ForeignDirectApply _ -> CForeignDirectApply
  ForeignDirectTail _ -> CForeignDirectTail
  ForeignDeferredApply _ -> CForeignDeferredApply
  ForeignDeferredTail _ -> CForeignDeferredTail

-- | The report name (the census's TSV token) — the ONLY place a class becomes a string.
callClassName :: CallClass -> String
callClassName = case _ of
  CDirectNonTail -> "direct-nontail"
  CDirectMusttail -> "direct-musttail"
  CLocalDeferredApply -> "local-deferred-apply"
  CLocalDeferredTail -> "local-deferred-tail"
  CGenericApply -> "generic-apply"
  CGenericTail -> "generic-tail"
  CStructuralApply -> "structural-apply"
  CWrapperEntry -> "wrapper-entry"
  CForeignDirectApply -> "foreign-direct-apply"
  CForeignDirectTail -> "foreign-direct-tail"
  CForeignDeferredApply -> "foreign-deferred-apply"
  CForeignDeferredTail -> "foreign-deferred-tail"

-- | Every accounting column, in report order — the census emits a row per column even at zero, so a
-- | column that stops occurring is visible as a zero rather than as a missing line. A unit test
-- | pins that this enumeration is complete (the compiler catches a missing `callClassName` arm, but
-- | not a class missing from THIS list).
callClasses :: Array CallClass
callClasses =
  [ CDirectNonTail
  , CDirectMusttail
  , CLocalDeferredApply
  , CLocalDeferredTail
  , CGenericApply
  , CGenericTail
  , CStructuralApply
  , CWrapperEntry
  , CForeignDirectApply
  , CForeignDirectTail
  , CForeignDeferredApply
  , CForeignDeferredTail
  ]

-- --- ADR-0108 §3: the dynamic profile's slot space -------------------------------------------------

-- | EVERY `MissReason`, in report order — the STATIC enumeration.
-- |
-- | Distinct from [`profiledReasons`], and the difference is load-bearing. A dynamic counter for
-- | `MissUnknownKey` would have to read zero by construction (`readVar` crashes on such a callee
-- | first), so the profile does not reserve one. The static census has no such argument available:
-- | it counts what the classifier RETURNED, and if the classifier ever returns `MissUnknownKey` the
-- | census must have a row to put it in — otherwise the ADR-0113 §3 identity (i) is short by a count
-- | that exists, and the shortfall reads as a broken sum rather than as the compiler bug it is.
allMissReasons :: Array MissReason
allMissReasons =
  [ MissCalleeForeign
  , MissCalleeLiteral
  ]
    <> map MissLocalUnknownFn bindOrigins
    <>
      [ MissArityLocal
      , MissUnknownKey
      , MissArityOwnModule
      , MissOwnObjectNotFn
      , MissDepNoDirectFact
      , MissArityCrossModule
      ]

-- | The reasons a generic dispatch can EXECUTE with. `MissUnknownKey` is deliberately absent: it
-- | cannot reach a dispatch in a valid build (`readVar` crashes first, §1), so instrumenting it
-- | would reserve a counter that must always read zero.
profiledReasons :: Array MissReason
profiledReasons =
  [ MissCalleeForeign
  , MissCalleeLiteral
  ]
    <> map MissLocalUnknownFn bindOrigins
    <>
      [ MissArityLocal
      , MissArityOwnModule
      , MissOwnObjectNotFn
      , MissDepNoDirectFact
      , MissArityCrossModule
      ]

-- | The generic FORMS a dispatch can take. A reason means a different thing in each — different
-- | emitted operation, different lever — so the counters are the product, never the sum.
profiledForms :: Array CallClass
profiledForms = [ CGenericApply, CGenericTail ]

-- | The DISPATCH slots, in slot order — `(form × reason)` then the structural apply, which has no
-- | reason to key on. Kept apart from the allocation-site slots below so the two families cannot be
-- | confused: only these participate in the ADR-0108 §3 dispatch identities.
dispatchSlotNames :: Array String
dispatchSlotNames =
  (profiledForms >>= \f -> map (\r -> callClassName f <> "/" <> missReasonName r) profiledReasons)
    <> localDeferredSlotNames
    <> [ callClassName CStructuralApply ]

-- | The ADR-0113 candidate slots, `(form × kind)`. Keyed by [`CandidateKind`] and NOT by
-- | `BindOrigin`: the two alias kinds share an origin, and the whole point of the split is that they
-- | are separable in the dynamic table as well as the static one.
localDeferredSlotNames :: Array String
localDeferredSlotNames =
  [ CLocalDeferredApply, CLocalDeferredTail ] >>= \f ->
    map (\k -> callClassName f <> "/" <> candidateKindName k) candidateKinds

-- | An instrumented ALLOCATION SITE (ADR-0108 §5 / ADR-0109 §5.1) — a place the emitter allocates,
-- | counted so an allocation-removing change can be checked against the runtime's per-`Kind` census
-- | rather than argued for. A site is NOT a dispatch: these slots are excluded from every §3
-- | identity, which is why their names carry their own prefix.
data AllocSite
  -- | `Emit.atom` materialising an `AtomForeign` — one per execution of a foreign reference, in
  -- | call AND value position. It is the LEFT side of ADR-0109 §5.1's three-way identity, and it
  -- | stays at the same logical site across that ADR's slice A (which changes what the
  -- | materialisation emits, not how many times it runs), so the two legs count the same
  -- | occurrences and their `Kind::Closure` delta is attributable.
  = SiteForeignMaterialise
  -- | `Root.emitForeignCloInit` building one hoisted leaf closure — the RIGHT side of the same
  -- | identity (ADR-0109 §5.1). Its total is also statically predicted (the entry object's
  -- | reachable-leaf set, each built once), so two derivations must land on one integer.
  | SiteForeignCloInit

derive instance eqAllocSite :: Eq AllocSite
derive instance ordAllocSite :: Ord AllocSite

instance showAllocSite :: Show AllocSite where
  show = allocSiteName

-- | The slot label — prefixed, so a dispatch sum that matches on `generic-apply/` &c. can never
-- | pick one of these up (and so the printed schema says which family a row belongs to).
allocSiteName :: AllocSite -> String
allocSiteName = case _ of
  SiteForeignMaterialise -> "alloc/site/foreign-materialise"
  SiteForeignCloInit -> "alloc/site/foreign-clo-init"

-- | Every instrumented allocation site, in slot order.
allocSites :: Array AllocSite
allocSites = [ SiteForeignMaterialise, SiteForeignCloInit ]

-- | The profile's slot names, IN SLOT ORDER — the dispatch slots first, then the allocation sites.
-- |
-- | This array is the ONE definition of the profile's layout. An instrumented program hands it to
-- | the runtime at start-up (`pv_applyprofile_register`), so the runtime labels its counters from
-- | the compiler's own names instead of a mirrored copy that could drift — adding a `MissReason`
-- | (or a site) here changes the emitted blob and the printed schema together.
-- |
-- | Dispatch slots keep the LOW indices, so adding an allocation site cannot renumber a dispatch
-- | slot (the harness reads slots by name, but a renumbering would still invalidate a profile taken
-- | with a different build of the same compiler).
-- | The classes that EXECUTE a dispatch-or-direct-call and are not keyed by a reason: the foreign
-- | direct and deferred forms (ADR-0109). They get slots for the same reason `structural-apply`
-- | does — they execute, and the A/B's endpoints are stated over them.
foreignSlotNames :: Array String
foreignSlotNames =
  [ callClassName CForeignDirectApply
  , callClassName CForeignDirectTail
  , callClassName CForeignDeferredApply
  , callClassName CForeignDeferredTail
  ]

profileSlotNames :: Array String
profileSlotNames = dispatchSlotNames <> foreignSlotNames <> map allocSiteName allocSites

-- | The slot an executing allocation site bumps. Total (every site has one) — unlike `profileSlot`,
-- | there is no un-instrumented case here: a site exists in this enumeration precisely because it
-- | is counted.
allocSiteSlot :: AllocSite -> Int
allocSiteSlot s = case Array.elemIndex s allocSites of
  -- the allocation sites sit after BOTH dispatch families in the layout; adding a family without
  -- this offset would point every site at another slot, which the layout test pins by NAME.
  Just i -> Array.length dispatchSlotNames + Array.length foreignSlotNames + i
  -- A constructor missing from `allocSites` would otherwise silently count into another site's
  -- slot — the one failure this layout cannot detect downstream, so it crashes here instead.
  Nothing -> unsafeCrashWith ("CallClass.allocSiteSlot: " <> allocSiteName s <> " is missing from allocSites")

-- | The slot an executing dispatch bumps, or `Nothing` for the events that are not instrumented:
-- | the direct forms (they are not dispatches), the wrapper entry (per function, not per call), and
-- | `MissUnknownKey` (unreachable, per `profiledReasons`).
profileSlot :: CallEvent -> Maybe Int
profileSlot = case _ of
  GenericApply r -> slotOf CGenericApply r
  GenericTail r -> slotOf CGenericTail r
  StructuralApply ->
    Just (Array.length profiledForms * Array.length profiledReasons + Array.length localDeferredSlotNames)
  LocalDeferredApply k -> localSlot CLocalDeferredApply k
  LocalDeferredTail k -> localSlot CLocalDeferredTail k
  ForeignDirectApply _ -> foreignSlot CForeignDirectApply
  ForeignDirectTail _ -> foreignSlot CForeignDirectTail
  ForeignDeferredApply _ -> foreignSlot CForeignDeferredApply
  ForeignDeferredTail _ -> foreignSlot CForeignDeferredTail
  DirectNonTail _ -> Nothing
  DirectMusttail _ -> Nothing
  WrapperEntry -> Nothing
  where
  slotOf form r = do
    fi <- Array.elemIndex form profiledForms
    ri <- Array.elemIndex r profiledReasons
    pure (fi * Array.length profiledReasons + ri)

  foreignSlot cls = do
    i <- Array.elemIndex (callClassName cls) foreignSlotNames
    pure (Array.length dispatchSlotNames + i)

  localSlot cls k = do
    i <- Array.elemIndex (callClassName cls <> "/" <> candidateKindName k) localDeferredSlotNames
    pure (Array.length profiledForms * Array.length profiledReasons + i)
