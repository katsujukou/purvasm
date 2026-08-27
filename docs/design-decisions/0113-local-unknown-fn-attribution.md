# 0113. `local-unknown-fn` attribution: bind-time provenance, then the facts already in hand

- Status: ~~Proposed~~ **Accepted** _(2026-08-19: explicit maintainer Accept after 4 review
  rounds — the totality of `BindOrigin`, the transitive candidate channel carrying its own
  `CandidateKind`, the knob- and form-blind classifier with the form derived from the chosen target,
  the three-stage knob with two independently verdictable transfers, `EmitLocalArity` keeping the
  unsaturated share attributable, and the three independent accounting identities. **Slices 1–2
  only**; slice 3 remains an explicit re-approval after the numbers)_
- Outcome: ~~Slices 1–2 accepted, slice 3 pending~~ **Slices 1–2 COMPLETE (2026-08-20 / 2026-08-24);
  slice 3 NOT AUTHORISED and intentionally stopped** — the measurement it was conditional on says
  the recoverable population is 5.16 % of dispatches (an upper bound), its second knob stage has
  nothing to move, and the dominant lever in the class is the out-of-scope `OParam`. No emitter
  change was made; the measurement substrate is kept. See the 2026-08-24 progress note.
- Date: 2026-08-17
- Deciders: maintainer
- Technical story: ADR-0108's second-ranked class — 195,562,878 generic dispatches (26.1 % of
  executions) at 9,229 call sites (48.3 % of sites) in one self-host build, all of them a local
  binding about which the emitter holds no function fact

> **Numbering note.** `0110`–`0112` were taken on the owned-VM track, then in flight on
> `feat/vm-native-ffi`; this record took `0113` so the two lines could merge without a collision.
> They merged at `a873da0` (2026-08-25) and the numbering held.

## Context

ADR-0108 ranked the reasons a call site stays generic by EXECUTIONS, not by sites, and the two
rankings disagreed. `callee-foreign` was first and has since been closed by ADR-0109. What is left
at the top is the class that dominates the *code* and comes second in the *run*:

| reason | sites | share | executions | share | `exec/site` |
| --- | ---: | ---: | ---: | ---: | ---: |
| `local-unknown-fn` | 9,229 | 48.3 % | 195,562,878 | 26.1 % | 0.54× |

with a form split unlike anything else measured so far: **119,094,012 `apply` + 76,468,866 `tail`
— 39.1 % of the class is the trampoline form**, against 2.1 % for `callee-foreign`. Whatever this
class is, it is far more often a tail call than the class ADR-0109 closed, and the tail form is a
`pv_tailcall` store rather than a `pv_apply` dispatch. That is a difference in the emitted
operation, not only in the count.

Both numbers are from ONE snapshot — the compiler built `--opt`, compiling `Purvasm.CLI.Native` in
`--no-opt`, sites and executions taken from the same CoreFn and the same pinned classifier
(`tools/apply-profile.sh --selfhost --build-mode opt --work-mode no-opt`). A ranking names its
workload; this one is that workload's.

### What the class is, mechanically

`Emit.directTarget` reaches `MissLocalUnknownFn` on exactly one path: the callee atom is an
`AtomVar`, the name resolves in the LOCAL scope, and its `EnvEntry.knownFn` is `Nothing`. So the
class is not "an unknown function" — it is **"a local binding whose bind site did not stamp a
function fact."** Which bind sites those are is a closed question: `Types.purs` exposes four binders,
and the emitter calls them at **six places**, which §1 names as **seven origins** (the `Let` and
`Grec` places each split on whether the RHS is a lambda):

| emitter place | binder used | fact stamped? | §1 origin(s) |
| --- | --- | --- | --- |
| function parameters (`emitFunction`) | `bindVar` / `bindDirectVar` | **never** | `OParam` |
| captures read from `%env` (`emitFunction`) | `bindFnVar` iff the name is in `Lifted.captureFns` | **only for recursive-group siblings** | `OCapture` |
| `Let x (CLam …)` | `bindFnVar` / `bindDirectFnVar` | always | `OLetLambda` |
| `Let x <anything else>` | `bindVar` / `bindDirectVar` | **never** | `OLetValue` |
| `LetRec`/`Grec` members (`buildGrec`) | `bindFnVar` iff the member's RHS is a lambda | non-lambda members: **never** | `OGrecLambda`, `OGrecValue` |
| match-leaf occurrences (`case` binders) | `bindVar` | **never** | `OMatchBinder` |

Two of those rows are worth stating on their own, because they are not "we do not know" — they are
"we knew and dropped it":

1. **`Emit.lift` writes `captureFns: []` unconditionally.** Only `buildGrec` ever populates that
   field, and only with the sibling infos of one recursive group. So when an ordinary lambda
   captures a `let`-bound lambda — a binding the enclosing activation had a complete `FnInfo` for —
   the lifted body binds that capture with `knownFn = Nothing` and every saturated call to it in
   that body is a generic dispatch. The fact was in `env` at the point `lift` was called.
2. **A `let` that merely re-binds a name loses the fact.** `Let x (CAtom (AtomVar y))` takes the
   `_ ->` arm and stamps nothing, even when `y`'s entry carries an `FnInfo`.

Neither observation is a measurement. Both are readings of the emitter, and the standing evidence
against acting on a reading is this track's own: ADR-0107's close-out (a fact set measured in the
wrong corpus said nothing about the shipped path) and ADR-0108 step 3 (the site ranking would have
aimed the work at the second-largest consumer). **The aggregate above names no lever.** 9,229 sites
running at 0.54× their share of the code could be a thousand hot parameter calls or a hundred
thousand cold ones, and the two ask for completely different work.

### What is NOT known, and must be measured before anything is designed

- how the 195.6 M executions and the 9,229 sites divide across the seven origins above;
- how much of the capture and alias population is *recoverable* — counted TRANSITIVELY (§2), because
  a lowering that restores a fact at one binder makes the next one recoverable too, and a
  one-level-deep count would under-size the very lever it exists to size;
- whether the recoverable population is saturated at its call sites (a recovered fact whose arity
  disagrees moves the dispatch to `arity-local`, it does not remove it);
- whether the 39.1 % tail share is spread evenly across origins or concentrated in one.

## Decision

Four slices, in this order, with a maintainer checkpoint between measurement and lowering. Slices 1
and 2 measure and change no emission. Slice 3 is a lowering that is **not authorised by this
record**: it is described so the measurement is designed to answer its question, and it proceeds
only if the numbers from slices 1–2 say it is worth doing. Slice 4 is named and excluded.

### §1 Two orthogonal channels on the environment entry: WHERE it was bound, and WHAT was known

The first draft of this section put both questions in one enumeration and got both wrong: the
taxonomy excluded the binders that DO stamp a fact (so it could not be a required argument of every
binder without leaving them nothing to pass), and a report tag cannot carry the `FnInfo` that slice 3
would use. The two are separated:

**Channel 1 — `BindOrigin`, total over every bind site, fact-blind.** A required argument of every
binder in `Types.purs`, not a defaulted field, so a bind site added later fails to compile until it
names its origin. `Types.purs` exposes four binders; the emitter calls them at six places, and those
six produce these seven origins (the `Let` and `Grec` places each split on whether the RHS is a
lambda; the capture place does not split, because whether a fact came with it is channel 2's job):

| `BindOrigin` | emitter place |
| --- | --- |
| `OParam` | `emitFunction`'s parameter prologue |
| `OCapture` | `emitFunction`'s positional `%env` reads |
| `OLetLambda` | `expr`'s `Let x (CLam …)` arm |
| `OLetValue` | `expr`'s other `Let` arm |
| `OGrecLambda` | `buildGrec`, member whose RHS is a lambda |
| `OGrecValue` | `buildGrec`, member whose RHS is not |
| `OMatchBinder` | the decision-tree leaf binder |

**Channel 2 — `LocalFact`, a closed sum that carries the `FnInfo` itself**, so the thing measured and
the thing later used are one value rather than a tag and a re-derivation:

```text
data LocalFact
  = FActive CapturableFact   -- directTarget may use it TODAY
  | FCandidate CandidateFact -- derivable at bind time; NOT used unless PURVASM_LOCAL_FACTS says so
  | FNone                    -- nothing was derivable here

-- a candidate carries the DERIVATION that produced it, not just the fact
type CandidateFact = { fact :: CapturableFact, kind :: CandidateKind }
```

**The kind travels inside the candidate, not beside it.** `directTarget` builds a
`LocalCandidate` from the entry it found, and the counters are keyed by `CandidateKind` (§2) — but
`AliasLocal` and `AliasGlobal` are BOTH `BindOrigin = OLetValue`, so the kind is not recoverable
from the origin at classification time. Storing it in a second field beside `fact` would reintroduce
exactly the two-channel mismatch §2 removes from captures. It goes in the candidate.

Propagation and classification then read different parts of it, which is what makes a multi-level
chain classify by its OWN provenance rather than the origin's:

- `candidateOf` returns only the inner `CapturableFact`, so a later bind site inherits the FACT;
- that later site stamps its own `kind` when it re-publishes. A capture of a local alias is
  therefore `Capture` (what the emitter would have to fix THERE), not `AliasLocal` inherited from
  two links back.

`EnvEntry` carries `{ bind, key, origin :: BindOrigin, fact :: LocalFact }`, and `knownFn` becomes
the derived projection `activeFn e = case e.fact of FActive f -> Just (unFact f); _ -> Nothing`.
Every existing `bindFnVar`/`bindDirectFnVar` call site passes `FActive`; every existing
`bindVar`/`bindDirectVar` site passes `FNone` or, where §2 derives one, `FCandidate`.

`MissLocalUnknownFn` then carries `BindOrigin` **and nothing else**. A recoverability bit next to it
would build a `BindOrigin × Recoverability` product with unreachable cells in it —
`OLetLambda/opaque` and `OParam/recoverable` cannot occur — and a slot that means nothing must not
be constructible (ADR-0108 §1's rule). Instead the split falls out of §2's classifier: a binding with
a candidate is `LocalCandidateTarget`, counted as `local-deferred-<form>/<kind>`, and
`MissLocalUnknownFn origin` is left meaning exactly **opaque** — no candidate, at any depth.

Two cells of the remaining enumeration are still unreachable rather than merely empty:
`MissLocalUnknownFn OLetLambda` and `MissLocalUnknownFn OGrecLambda`, since those binders always
stamp `FActive`. They are kept as **diagnostic zero rows** and the per-object gate **fails closed on
a non-zero count**, exactly as ADR-0108 treats `callee-literal` and `unknown-key`: a row that can
only be produced by a compiler bug is more useful printed and pinned at zero than deleted.

`profiledReasons` derives its slot space from the `BindOrigin` enumeration, so adding an origin
updates the reason names, the census columns and the profile slots in one place. The classifier's
decision tree is otherwise untouched; every leaf simply names its outcome more precisely, exactly as
ADR-0108 §4 slice 1 split `MissCalleeNotVar`.

### §2 One fact channel, derived transitively; the classifier stays knob-INDEPENDENT

**The undercount this fixes.** A derivation written as `lookupEnv … >>= _.knownFn` reads only the
active channel, so it stops at one level: in `x = y; z = x` the second alias is opaque, and a capture
of a capture is opaque. But slice 3, by restoring the fact at the earlier binder, would make the
later one recoverable too — so the lowering would move a LARGER population than the measurement
sized. A measurement that under-counts the lever it is sizing is worse than no measurement.

So the derivation reads BOTH channels and republishes as a candidate:

```text
-- returns the FACT only: the kind is re-stamped by whichever bind site republishes it
candidateOf :: EnvEntry -> Maybe CapturableFact
candidateOf e = case e.fact of
  FActive f    -> Just f
  FCandidate c -> Just c.fact
  FNone        -> Nothing
```

- **`OCapture`**: `lift` holds the enclosing `env` — the same `env` `makeClosure` reads each capture's
  VALUE from. The fact is `lookupEnv c env >>= candidateOf`, evaluated at the same place, over the
  same binding, as the value the capture will hold.

  **The capture's identity and its fact become ONE element**, because two parallel arrays cannot
  express the invariant that matters. Replacing `captureFns` while leaving `captures :: Array String`
  beside a `captureFacts :: Array (Tuple String LocalFact)` still permits a missing fact, a duplicate
  one, or a fact attached to the wrong name — so "exactly one fact per capture" would be a claim, not
  a type. `Lifted` therefore carries:

  ```text
  type Capture = { name :: String, fact :: LocalFact }
  -- Lifted.captures :: Array Capture      (replacing BOTH captures and captureFns)
  ```

  `makeClosure` reads each element's `name` to fetch the VALUE, and the capture prologue reads the
  same element's `fact` to stamp the binding — one array, one traversal order, so the positional
  correspondence between `%env[i]`, its value and its fact is structural rather than maintained.
  `FActive` carries what `captureFns` carried (a `Grec` sibling), `FCandidate` a recovered one,
  `FNone` otherwise.
- **`OLetValue`**: the alias — RHS is `CAtom (AtomVar y)`. The lookup mirrors `directTarget`'s OWN
  order — local scope first, then this module's `gfns`, then the published `xfns` — so a local
  rebinding never masquerades as the global, which is an invariant `directTarget` already depends
  on. Which table answered decides the candidate's `CandidateKind`: `AliasLocal` for the local
  scope, `AliasGlobal` for `gfns`/`xfns`.

  The global case is **measured, not excluded**: at the bind site a fact does exist, so excluding it
  would repeat exactly the under-count this section opens by fixing. Because `CandidateKind` keys the
  counters (§2's classifier block), `let x = <a top-level Gfun>` is separable in the STATIC and the
  DYNAMIC table alike, rather than being visible in one and folded away in the other — and its size
  is what the §4 checkpoint needs in order to decide whether slice 3 should touch it at all. Whether
  it is then LOWERED is a slice-3 question this record does not prejudge.

Because both derivations consult `candidateOf` rather than `activeFn`, they are **transitive at
measurement time**: chains and nested captures of any depth are counted in slice 1, with the knob off
and no emission change. The remaining origins derive nothing and are stamped `FNone`.

**The classifier does NOT read the knob.** Letting `directTarget` read `activeFn` when the knob is
off and `activeFn <|> candidateFn` when it is on would make the same site's `CallTarget` differ
between the two legs of an A/B — the exact confusion ADR-0109 §1.2 separated eligibility from
emission to prevent — and it would leave candidate eligibility INVISIBLE to the classifier in the off
leg, so §4's `CLocalDeferred*` could not be emitted at all without a second, parallel classification.

So `directTarget` stays a pure function of the environment, knob-blind, and grows one eligibility
outcome beside the two it already has:

```text
data CallTarget
  = GuestTarget FnInfo                    -- an ACTIVE fact, arity matching (unchanged)
  | ForeignTarget ForeignRef              -- ADR-0109 (unchanged)
  | LocalCandidateTarget LocalCandidate   -- NEW: a CANDIDATE fact exists here
  | GenericTarget MissReason              -- nothing to work with

type LocalCandidate = { fact :: CapturableFact, kind :: CandidateKind }

-- the key the deferred/direct counters are keyed by, shared by target, event and slot
data CandidateKind = Capture | AliasLocal | AliasGlobal
```

`CandidateKind`, not `BindOrigin`, is what the candidate classes carry. `BindOrigin` keys the OPAQUE
class (`MissLocalUnknownFn`) and is the totality device for the binders; the candidate population is
generated by exactly three derivations (§2), and those three are what the knob moves in stages. Using
one key for both would lose the local/global alias split in the dynamic table — a `BindOrigin`-keyed counter
cannot tell a local alias from a global one, and §2 promised that split is visible.

**Arity is deliberately NOT part of this eligibility.** `LocalCandidateTarget` means "a candidate
fact exists for this callee", nothing more; whether it is saturated at THIS site is a question the
emission decision answers. That keeps the off-leg population — every site the knob could touch — a
single countable class, which is what makes §4's transfer a closed sum rather than a partition whose
two halves are counted by different mechanisms.

**The FORM cannot be decided before the knob, and this is not a detail.** `callForm` is
target-aware: a `GuestTarget` is `FTail` only when `tail && inDirect` (there is no `%env` word to
hand over outside a `tailcc` direct entry, so any other tail context is lowered call-then-`ret`),
whereas a `GenericTarget` is `FTail` iff the site is in tail position. A candidate at
`tail = true, inDirect = false` is therefore `FTail` while the knob is off and `FApply` once it is
on. A single `Form` computed before the knob is read is wrong for one of the two legs — which is
review round 5's defect in ADR-0109, in a new place.

So the classifier stays knob-blind and form-blind, and the decision takes the raw site state and
derives the form from the target it ACTUALLY chose:

```text
type Site = { tail :: Boolean, inDirect :: Boolean, nargs :: Int }

decideLocal :: LocalFactsMode -> Site -> LocalCandidate -> EmissionDecision
decideLocal mode st c
  | not (moves mode c.kind) = EmitLocalDeferred c.kind (callForm st (GenericTarget …))
  | arityOf c.fact == st.nargs = EmitLocalDirect c.kind (callForm st (GuestTarget (unFact c.fact)))
  | otherwise = EmitLocalArity c.kind (callForm st (GenericTarget …))

-- the three knob stages, and which kinds each one moves
moves :: LocalFactsMode -> CandidateKind -> Boolean
moves Off                _           = false
moves Captures           Capture     = true
moves Captures           AliasLocal  = false
moves Captures           AliasGlobal = false
moves CapturesAndAliases _           = true
```

**An unsaturated candidate keeps its kind too.** Dropping it into the existing
`EmitGeneric MissArityLocal` would lose which derivation produced it, and that breaks the §4
accounting in a way that is easy to miss: the per-kind transfer would have to add the SAME
all-kinds `arity-local` delta to each kind's equation, so the step that moves `AliasLocal` and
`AliasGlobal` together would count that delta twice. It also throws away a headline the checkpoint
needs — WHICH candidate population was saturated at its call sites is most of what decides whether
slice 3 is worth doing.

So `EmitLocalArity CandidateKind Form` is its own decision, with its own class, event and slot
(`local-arity-<form>/<kind>`), and the legacy `MissArityLocal` stays an INVARIANT axis of the
paired run rather than a moving one. The per-kind transfer is then exact to the unit and mentions
only kinded rows:

```text
before local-deferred-<form>/k  ==  after local-direct-<form>/k + after local-arity-<form>/k
```


`moves` is the whole content of the three-stage knob, in one total function: `S0 → S1` moves
`Capture` and leaves both alias kinds deferred; `S1 → S2` moves the two alias kinds and leaves the
capture axis INVARIANT. Two independent transfers, separately verdictable — ADR-0109 §5.2's lesson
that a two-state knob makes one endpoint unobservable, applied to a population with three sources
rather than one.

- **knob OFF** (slices 1–2, and today's shipped path): every candidate becomes `EmitLocalDeferred` —
  which emits the generic dispatch **byte-for-byte as today**, and differs only in which counter it
  bumps. Emission identity is therefore checkable (§3), and the population the knob would move has a
  name in the off leg.
- **knob at a MOVING stage** (slice 3, if approved): for the kinds the current stage moves,
  saturated candidates become direct and unsaturated ones become `local-arity`; kinds the stage
  does not move stay deferred, which is what keeps the two steps separately verdictable.

The classification matrix is therefore over `mode × kind × tail × inDirect × saturated` — every
combination, including `tail && not inDirect`, which emission cannot currently reach (every tail
context is a lifted body, which sets `inDirect`) and which is pinned HERE, on the decision, over its
whole input space, exactly where ADR-0109 pinned `callForm`'s unreachable arm.

The population measured in slice 1 is thus exactly the population slice 3 would move, by
construction: both read the same transitive derivation, through the same knob-blind classifier. A
disagreement between them is a bug in one of the two, not a lever that failed to convert.

**Why a recovered capture fact is sound**, stated now because it is what slice 3 would rest on:

- the value identity holds — `makeClosure` reads the capture through `readVar env`, so the word in
  `%env[i]` IS the enclosing binding's value, not a copy of something like it;
- each `EnvSrc` that may appear stays valid when the callee is read from `%env`: `SSentinel` needs no
  env word, `SClosureEnv` reads field 2 of the callee value itself, and `SForceCell` forces the
  callee's cell — which is precisely what `buildGrec` already relies on when it propagates sibling
  infos through `captureFns` today;
- **`SSelf` is the one `EnvSrc` for which this does NOT hold, and its exclusion is STRUCTURAL, not
  observational.** Arguing "the four current call sites happen not to stamp it" pins nothing a
  refactor cannot silently break, and breaking it makes capture propagation unsound. So `Types.purs`
  gains an opaque `CapturableFact` whose ONLY constructor is fail-closed:

  ```text
  newtype CapturableFact = CapturableFact FnInfo   -- constructor NOT exported
  capturableFact :: FnInfo -> Maybe CapturableFact -- Nothing iff src == SSelf
  unFact :: CapturableFact -> FnInfo
  ```

  `LocalFact` holds `CapturableFact`, never a bare `FnInfo`, so an `SSelf` fact cannot reach an
  environment entry at all — the illegal state is unconstructible rather than merely unreached. A
  NEGATIVE unit row (`capturableFact` on an `SSelf` `FnInfo` is `Nothing`) plus one positive row per
  other `EnvSrc` pins the constructor over its whole input space, which is where ADR-0109 pinned
  `callForm`'s unreachable arm.

### §3 The measurement (slices 1 and 2)

**Slice 1 — static.** The claim that the existing reason gate "covers the new rows unchanged" is
FALSE and was the first draft's error: a candidate no longer produces `GenericTarget MissReason` at
all, it produces `LocalCandidateTarget`, and its event is `CLocalDeferred*` rather than
`CGeneric*`. So candidate rows are not inside `Σ generic-apply/<reason>`, and one gate stated over
one enumeration can no longer carry both populations. The census gains **three independent
identities**, each over its own enumeration, each fail-closed:

```text
(i)   opaque side, per object and per form:
        CGeneric<Form>            == Σ_<reason>  generic-<form>/<reason>
      with the ADR-0108 diagnostic classes still pinned at zero (unknown-key, callee-literal) and
      the §1 diagnostic-zero origins added to them (OLetLambda, OGrecLambda).

(ii)  candidate side, per object and per form:
        CLocalDeferred<Form>      == Σ_<kind>    local-deferred-<form>/<kind>
      over CandidateKind = Capture | AliasLocal | AliasGlobal.

(iii) the LLVM dispatch accounting, per object and per form — what the emitter actually emitted a
      generic dispatch for:
        dispatches<form>          == Σ generic-<form>/<reason>
                                   + Σ local-deferred-<form>/<kind>
                                   + Σ foreign-deferred-<form>            (ADR-0109)
                                   + structural                           (FApply only)
      and, once slice 3 exists, + Σ local-arity-<form>/<kind> on the same side: an unsaturated
      candidate is still a generic dispatch, so it stays inside this identity rather than
      leaving it.
```

Identity (iii) is the one that would have silently absorbed a mis-slotted candidate: (i) and (ii)
each balance within their own family, so only a statement spanning all families catches a dispatch
that left one and never arrived in the other.

On the ADR-0108 pinned corpus, `Σ (i's local-unknown-fn rows) + Σ (ii)` must equal that corpus's
published `local-unknown-fn` count to the unit — the class is being SPLIT across two families, so
the check has to span both, and checking either alone would report a shortfall as a pass.

**The self-measurement property applies again and is stated in advance**: the corpus IS the
compiler, so adding the split changes the program being measured. The exactness condition is
therefore the one ADR-0108 §4 had to correct itself into — (a) exact on the older pinned corpus,
(b) exact cross-mechanism on the new one, (c) the delta ACCOUNTED for by running one classifier over
both snapshots — never "equals the previously published integer".

**Slice 2 — dynamic.** The `(form × reason)` slots extend with the split, and the
`local-deferred-<form>/<kind>` classes join them (§2: they are the knob-blind classifier's off-leg
name for a candidate, and they emit today's generic dispatch unchanged — so they belong on the
DISPATCH side of the identities, not beside the direct forms). Both whole-program identities must
therefore hold exactly on the `--selfhost` run, with the deferred classes inside the sums
```text
Σ generic-apply/<reason> + Σ local-deferred-apply/<kind> + Σ foreign-deferred-apply + structural
  == pv_apply_entries
Σ generic-tail/<reason>  + Σ local-deferred-tail/<kind>  + Σ foreign-deferred-tail
  == pv_tailcall_writes
```

The ADR-0109 foreign classes belong in these sums because they too are dispatches the emitter chose
not to lower, and the existing harness already counts them on that side. They are zero under the
shipped default — which is a fact to ASSERT, not a reason to omit the term. Slice 3 adds
`Σ local-arity-<form>/<kind>` to the same side, since an unsaturated candidate is still a dispatch.

The four-leg harness supplies sites and executions from ONE snapshot and
one pinned classifier, as it does today. `tools/apply-profile.sh --self-test` gains injections for
the new rows — a gate that can be satisfied by the absence of its own input is not a gate.

**Neither slice may perturb emission.** The uninstrumented `.ll` for a pinned CoreFn snapshot must
be byte-identical before and after, checked against `_build/adr108-corefn` and NOT against
`output/` (which is the compiler's own CoreFn and therefore changes when the compiler does). This is
what the §2 knob-off separation buys: candidates are derived and counted, and `directTarget` does not
read them.

#### §3.1 Verification conditions, fixed here so the measurement and any later lowering agree

Fixtures, each of which the measurement must classify and — if slice 3 happens — the lowering must
move the same way:

1. **an alias chain of depth ≥ 2** (`x = y; z = x`, and one level deeper): every link after the first
   is `OLetValue`/recoverable, not opaque;
2. **a nested capture of depth ≥ 2** (a lambda capturing a binding that is itself a capture of a
   `let`-bound lambda): recoverable at every level;
3. **shadowing** — an inner binding with the SAME NAME as an outer known one, bound with no fact.
   The inner entry must be opaque and must NOT inherit the outer fact; `lookupEnv` finds the inner
   one first, and a derivation that walked past it would recover a fact for the wrong binding;
4. **totality over bind origins, INCLUDING the known-lambda ones** — one row per `BindOrigin`, the
   `OLetLambda`/`OGrecLambda` rows asserting `FActive`, so the enumeration is exercised rather than
   merely declared;
5. **a missing profile row is a FAILURE, not a zero** — `tools/apply-profile.sh --self-test` gains
   injections in which a recoverable or an opaque row is absent, and the gate must fail. A gate that
   is satisfied by the absence of its own input is not a gate (ADR-0109's lesson, and the reason a
   PASSING case belongs in a fault-injection suite too);
6. **`capturableFact` over its whole input space** — the negative `SSelf` row plus one positive row
   per other `EnvSrc` (§2);
7. **a `let` aliasing a top-level `Gfun`** — classified `OLetValue` with
   `CandidateKind = AliasGlobal`, so the global-alias population is a column of its own rather
   than being folded into the local-alias one, in the static and the dynamic table alike (§2);
8. **the two diagnostic zero rows** — `MissLocalUnknownFn OLetLambda` and `… OGrecLambda` — asserted
   zero, with an injection that makes one non-zero and must FAIL the per-object gate (§1);
9. **`directTarget` is knob-blind** — the classification matrix is run with the knob in every stage
   and must produce the IDENTICAL `CallTarget` for every row. This is the fixture that fails if the
   knob ever leaks back into the classifier (§2);
10. **the `mode × kind × tail × inDirect × saturated` decision matrix** — every combination of
    `decideLocal`'s inputs, so that the form is asserted against the target the decision CHOSE and
    not against the site alone. The `tail && not inDirect` rows are the ones emission cannot reach
    and are the reason the matrix exists (§2);
11. **the two knob steps in isolation** — under `captures`, every `AliasLocal`/`AliasGlobal` row is
    still deferred and the alias counters are INVARIANT; under `captures-and-aliases`, the capture
    counters are INVARIANT against the previous stage. A step that moved both would make one
    endpoint unobservable (§4);
12. **the form-crossing gate runs FIRST** — `sites(tail && not inDirect && candidate) == 0` in
    both legs of every paired run, evaluated BEFORE the per-form transfers, which are void (not
    adjusted) if it fails. A crossing population is an outflow from one form and an inflow to the
    other, so no single term can express it (§4);
13. **one `Capture` array, two consumers** — a fixture in which `makeClosure` and the capture
    prologue disagree must not be constructible; the positive row asserts both read the same element
    for the same `%env[i]` (§2).
14. **the kind survives a multi-level chain** — a capture of a local alias classifies as
    `Capture`, not the inherited `AliasLocal`: `candidateOf` hands on the fact and the new bind
    site stamps its own kind (§1). The fixture asserts the kind at the LAST link, which is the
    one a lowering would have to fix;
15. **an unsaturated candidate keeps its kind** — it becomes `local-arity-<form>/<kind>`, and the
    legacy all-kinds `arity-local` counter is asserted INVARIANT across every paired leg. This is
    the row that fails if the two alias kinds ever share one arity term (§2/§4).

### §4 The checkpoint, and the lowering that is not yet authorised (slice 3)

Slices 1–2 produce two tables of sites and executions from one snapshot: the OPAQUE population
keyed by `BindOrigin`, and the CANDIDATE population keyed by `CandidateKind` (§1/§2 — the two are
different enumerations because they answer different questions, and `AliasLocal`/`AliasGlobal`
share an origin). **The maintainer decides from those tables whether slice 3 happens at all**,
exactly as
ADR-0108 §4 handed the foreign result to ADR-0109 rather than acting on it.

If it does, its shape follows ADR-0109's, because the same discipline applies:

- propagation is behind a closed-type knob parsed once at the CLI, fail-closed, scrubbed by every
  `tools/*.sh` that owns emission knobs: `PURVASM_LOCAL_FACTS=off|captures|captures-and-aliases`.
  Three stages, not two, so the capture and alias populations stay separable — ADR-0109 §5.2's
  lesson that a 2-state knob makes one endpoint unobservable;
- **the transfer is a SUM, not a bijection**: a candidate whose arity matches becomes a direct call,
  and one whose arity does not becomes `arity-local`. Because §2's classifier is knob-blind, the
  off-leg population has its own name (`local-deferred-<form>/<kind>`) rather than being inferred;

- **the DYNAMIC counters do not exist today, and adding them is part of slice 3's approval rather
  than an implementation liberty.** `CallClass.profileSlot` returns `Nothing` for `DirectNonTail`
  and `DirectMusttail` — the direct forms are not dispatches, so ADR-0108 §3 deliberately gave them
  no counter — and `CallEvent`'s direct constructors carry an `FnInfo`, not a `BindOrigin`. What is
  needed, in ADR-0109's exact shape (that ADR had to introduce `ForeignDeferred*` for the same
  reason): `CLocalDeferredApply`/`CLocalDeferredTail`, `CLocalDirectApply`/`CLocalDirectTail` and
  `CLocalArityApply`/`CLocalArityTail`, each keyed by `CandidateKind`, with `profileSlot` TOTAL
  over them. Slices 1–2 introduce the **deferred** pair (they are what the off leg counts, and
  they change no emission); slice 3 introduces the **direct** AND the **arity** pairs — the arity
  pair is not optional, because §2 gave `EmitLocalArity` its own decision precisely so the
  unsaturated share stays attributable to its kind;

- **the completion conditions are stated per stage, per kind and per form, in absolute counts, with
  no signed rearrangement.** Writing them as a `Δ = on − off` identity is what made the first draft
  wrong: `local-deferred` moves DOWN while `direct` and `arity` move UP, so the two sides carried
  opposite signs. There are TWO transfers, one per knob step, and each moves its own kinds while the
  other kinds are pinned INVARIANT — `K(S0→S1) = {Capture}`, `K(S1→S2) = {AliasLocal, AliasGlobal}`:

  ```text
  for each stage step, for each kind k ∈ K(step), for each form ∈ {apply, tail}:

    before:  local-deferred-<form>/k   >  0     -- this step has something to move
             local-direct-<form>/k     == 0
             local-arity-<form>/k      == 0
    after:   local-deferred-<form>/k   == 0     -- every candidate of kind k was decided

    transfer:
             before local-deferred-<form>/k
               == after local-direct-<form>/k + after local-arity-<form>/k

    kinds not in K(step): every local-deferred / local-direct / local-arity row  INVARIANT
    legacy arity-local (the ACTIVE-fact class):                                  INVARIANT
  ```

  Every term is keyed by the same `k`, so the two alias kinds moving in one step cannot share — and
  therefore cannot double-count — a term. That is what `EmitLocalArity` (§2) buys: the unsaturated
  share is attributable, and the all-kinds `arity-local` counter stays an invariant rather than
  becoming a moving quantity that each kind's equation would have to claim a piece of;

- **the form-crossing population is a PRECONDITION, not a term.** By §2, a candidate at
  `tail = true, inDirect = false` is `local-deferred-TAIL` before the step and
  `local-direct-APPLY` after it. A term for it cannot be written uniformly: it is an OUTFLOW from
  the tail equation and an INFLOW to the apply one, so a single `+ Xover` is wrong for one of the
  two. Since that site shape is unreachable in emission today (every tail context is a lifted body,
  which sets `inDirect`), the harness asserts

  ```text
  sites(tail && not inDirect && candidate) == 0     -- both legs, before anything else is evaluated
  ```

  and the per-form transfers above are **evaluated only after that gate passes**. If it ever fails,
  the per-form identities are void — not adjusted — and must be replaced by their sum over forms, or
  by the general `Xout(form) − Xin(form)` pair. Recording it this way is the difference between an
  identity that holds and an identity that happens to hold;

  `apply` and `tail` are otherwise never summed together, every other axis is pinned INVARIANT, and
  the workload's emission is byte-identical across legs (`tools/apply-profile.sh --paired localfacts`,
  one snapshot, one toolchain, the knob a BUILD-mode axis — ADR-0109's correction);

- **the classifier's own bookkeeping is checked SEPARATELY, as a static census identity**, not folded
  into the transfer above. Mixing them would have the same population recorded twice by two
  mechanisms inside one equation, which is how an off-by-anything hides:

  ```text
  sites: Σ_<kind> local-deferred-<form>/<kind>  ==  sites the classifier returned
                                                        LocalCandidateTarget for, in <form>
  ```

  This is the cross-mechanism check ADR-0108 §4 used for the foreign drill — two independently
  derived numbers landing on one integer — and it holds in slices 1–2, before any lowering exists;
- correctness gates: the behavioural oracle, `native-run-diff`, `ffi-e2e`, the self-host fixpoint,
  and a `directTarget` classification matrix with one row per `BindOrigin`. Emission changes, so the
  L2-owned goldens are re-baselined as an intentional change (ADR-0104 §4);
- **no run-time claim** without ADR-0109 §5.2's protocol on a quiet dedicated Linux box: paired,
  order-alternated, pinned heap, ≥ 20 pairs, median of paired ratios, INCONCLUSIVE recorded as
  such. Dispatch removal is not time removal — ADR-0107's close-out is the standing evidence.

### §5 Out of scope, named so it is not silently folded in

- **`OParam` — the genuine higher-order call.** No local fact exists to recover; capturing it means
  caller-homed specialisation, which has its own prior-art study and its own blow-up failure mode
  (a size/use metric cannot tell a reducing clone from a non-reducing one). That is a separate ADR
  on the optimiser seam, not an emitter change, and this record does not prejudge it.
- **Dictionary method projections.** A `let` whose RHS is a `RecordGet` off a dictionary is
  `OLetValue`/opaque here; making it direct is the ADR-0093 specialisation track's business.
- **The 39.1 % tail share.** If it concentrates in one origin, the lever may be about the
  trampoline rather than about facts. Slices 1–2 will say; deciding is not in this record.

#### Progress (2026-08-20): Slice 1 CLOSED — the split is lossless, and the delta is attributed

**`Slice 1 CLOSED — 2026-08-20`.** The old `local-unknown-fn` class is decomposed into the OPAQUE
family (`local-unknown-fn/<origin>`, keyed by `BindOrigin`) and the CANDIDATE family
(`local-deferred-<form>/<kind>`, keyed by `CandidateKind`) with no site lost, gained or moved
between families. Emission is unchanged: no `.ll` byte differs.

Everything below is stated in terms of COMMITS, modes, object counts and toolchains rather than
local `_build` paths, so it can be re-derived on another machine.

##### What was measured, and with what

| | corpus | how obtained | mode | objects |
| --- | --- | --- | ---: | ---: |
| **S3** | the compiler's own CoreFn at `bdae1bb` (2026-08-13, ADR-0108 step 3) | `git archive bdae1bb` into a scratch tree, then a FULL workspace `spago build` | `--opt` | 304 |
| **S12** | the compiler's own CoreFn at ADR-0108 step 1+2 (`be8d1f8`, 2026-08-11), kept as a pinned snapshot | pinned on disk since it was taken | `--opt` | 303 |

Both were censused by **one classifier: the post-split CLI/census of this working tree**, via
`tools/apply-census.sh --opt --entry Purvasm.CLI.Native`. Neither run rebuilt or re-snapshotted the
other's inputs.

**Reproducing S3 needs the FULL workspace build**, not `spago build -p compiler -p cli`: that pair
yields 598 modules and the census then dies on `readVar: unbound variable Purvasm.Stdio.writeErrLine`
— the ulib-side modules are part of the closure the entry reaches. The complete build gives 687.

##### §3(a) — exact on one fixed corpus, pre-split vs post-split

The strongest form of (a) available: the SAME corpus and the SAME tree, differing only in whether
the classifier splits the class. The pre-split leg is this tree with the slice-1 changes stashed.

```text
pre-split   local-unknown-fn (undivided)   9,169   (apply 6,057, tail 3,112)
post-split  opaque 8,484 + candidate 685 = 9,169   (apply 6,057, tail 3,112)
```

Equal in total AND per form. Per (object × form) the two agree in **606 / 606 cells** — a full outer
join with absent treated as zero, so a cell present on one side only is compared, not skipped; no
non-zero cell exists on the post side that is absent from the pre side.

##### §3(b) — exact cross-mechanism on the new corpus

On S12, all **303 / 303 objects**: `recorded events == emitted call forms`, with the `(iii)`
reconciliation now summing `generic + local-deferred + foreign-deferred + structural` on the apply
side and `generic + local-deferred + foreign-deferred` on the tail side. Identities `(i)` (each
generic class == Σ its reason rows) and `(ii)` (each local-deferred class == Σ its kind rows) hold
per object, and the four diagnostic rows (`unknown-key`, `callee-literal`,
`local-unknown-fn/let-lambda`, `local-unknown-fn/grec-lambda`) are zero everywhere. The gate runs on
the census's PRODUCTION path and exits non-zero on any violation.

##### §3(c) — ONE classifier over TWO snapshots

| snapshot | taken at | objects | opaque | candidate | total |
| --- | --- | ---: | ---: | ---: | ---: |
| **S12** (earlier) | `be8d1f8`, 2026-08-11 | 303 | 8,484 | 685 | **9,169** |
| **S3** (later) | `bdae1bb`, 2026-08-13 | 304 | 8,542 | 687 | **9,229** |

**S3 reproduces ADR-0108 §4's published 9,229 exactly**, and its object count (304) matches too. That
is the point of (c): one classifier reports 9,169 on the EARLIER S12 corpus and 9,229 on the LATER
S3 corpus, so the difference is a property of the CORPUS and not of the classification.

**The +60, attributed in full** (stated in corpus order, earliest first, so the sign is the direction
the compiler actually moved):

| population | S12 (08-11) | S3 (08-13) | Δ |
| --- | ---: | ---: | ---: |
| opaque `let-value` | 2,581 | 2,631 | +50 |
| opaque `capture` | 4,670 | 4,675 | +5 |
| opaque `match-binder` | 583 | 586 | +3 |
| candidate `capture` | 685 | 687 | +2 |
| **net** | | | **+60** |

This is the same self-measurement property ADR-0108 §4 recorded of its own drill
(`callee-foreign` 3,655 → 3,659): the corpus IS the compiler, so work landed between step 1+2 and
step 3 — the step-3 instrumentation among it — enlarges the program being measured. It is corpus
growth, not classification drift.

##### The decomposition itself

On S3, the corpus whose aggregate the ADR published:

| opaque `local-unknown-fn/<origin>` | sites | | candidate `local-deferred/<kind>` | sites |
| --- | ---: | --- | --- | ---: |
| `capture` | 4,675 | | `capture` | 687 |
| `let-value` | 2,631 | | `alias-local` | 0 |
| `param` | 650 | | `alias-global` | 0 |
| `match-binder` | 586 | | | |
| `let-lambda` / `grec-lambda` | 0 (diagnostic) | | | |
| `grec-value` | 0 | | | |
| **total** | **8,542** | | **total** | **687** |

by form: opaque 5,595 apply / 2,947 tail; candidate 513 apply / 174 tail.

**`687 / 9,229 = 7.4 % of the class is recoverable, and every recoverable site is a `Capture`.**
`AliasLocal` and `AliasGlobal` are ZERO on this corpus — the `let`-alias population §2 was written
for does not exist here, which is a result about the corpus and not a gap in the derivation (the
alias rows are exercised by fixtures, and the global-alias row by `CandidateKind = AliasGlobal`).

A first, wrong baseline is recorded so it is not tried again: an `apply-census.tsv` left in
`_build/` from an earlier run reports 9,176, but it spells the non-variable class `callee-not-var`
and therefore predates BOTH ADR-0108 §4 slice 1 and ADR-0109. It is not a comparison point for
9,229, and neither is `sites-by-reason.tsv`'s 9,236 from the step-3 profile leg.

##### What this does NOT close

Slice 2 (the dynamic profile) has not run: there is no execution-weighted breakdown of these
populations yet, and 7.4 % OF SITES says nothing about the share of DISPATCHES — ADR-0108's whole
finding was that those two rankings disagree. Slice 3 remains what §4 makes it: a separate,
explicit re-approval after slice 2's numbers, and the vocabulary that would express it
(`LocalFactsMode`, `decideLocal`, `EmitLocalDirect`, `EmitLocalArity`, and the direct/arity events)
is pinned at zero occurrences in `compiler/src` by `tools/seam-audit.sh`, whose self-test injects
each identifier separately from the audit's own token list.


#### Progress (2026-08-24): Slice 2 CLOSED, and Slice 3 is NOT authorised — stopped on its own numbers

**`Slices 1–2 COMPLETE. Slice 3 NOT AUTHORISED — intentionally stopped.`** The measurement this
record was written to obtain is done, and it says the lowering it was sizing is not worth doing on
this corpus. That is a result, not a failure: §4 made slice 3 conditional on exactly this table.

##### The measurement

`tools/apply-profile.sh --selfhost --build-mode opt --work-mode no-opt` — the ADR-0108 conditions,
one snapshot, one classifier, sites and executions from the same run. Both whole-program identities
hold EXACTLY, with the ADR-0113 classes inside them:

```text
Σ generic-apply/<reason> + Σ local-deferred-apply/<kind> + foreign-deferred-apply + structural-apply
  == 210,234,708 == pv_apply_entries
Σ generic-tail/<reason>  + Σ local-deferred-tail/<kind>  + foreign-deferred-tail
  == 116,711,504 == pv_tailcall_writes
```

Output unperturbed; the static census leg's gates green on the same snapshot.

| population | sites | share | executions | share | exec/site |
| --- | ---: | ---: | ---: | ---: | ---: |
| **`local-unknown-fn/param`** | 654 | 4.16 % | **81,252,445** | **24.85 %** | **5.97×** |
| `dep-no-direct-fact` | 2,202 | 14.02 % | 60,381,120 | 18.47 % | 1.32× |
| `local-unknown-fn/capture` | 4,699 | 29.91 % | 58,392,877 | 17.86 % | 0.60× |
| `own-object-not-fn` | 2,806 | 17.86 % | 56,651,728 | 17.33 % | 0.97× |
| `local-unknown-fn/let-value` | 2,669 | 16.99 % | 25,436,176 | 7.78 % | 0.46× |
| `local-unknown-fn/match-binder` | 590 | 3.76 % | 18,879,557 | 5.77 % | 1.54× |
| **`candidate/capture`** | 696 | 4.43 % | **16,869,596** | **5.16 %** | 1.16× |
| `arity-cross-module` | 872 | 5.55 % | 4,349,620 | 1.33 % | 0.24× |
| `structural-apply` | 98 | 0.62 % | 2,314,702 | 0.71 % | 1.13× |
| `arity-local` | 119 | 0.76 % | 1,889,419 | 0.58 % | 0.76× |
| `arity-own-module` | 299 | 1.90 % | 528,972 | 0.16 % | 0.09× |
| `callee-foreign` | 6 | 0.04 % | 0 | 0 % | — |
| **total** | **15,710** | | **326,946,212** | | |

`candidate/alias-local`, `candidate/alias-global` and the diagnostic origins are zero on both axes.
By form: opaque 108.0 M apply / 75.9 M tail; candidate 14.6 M apply / 2.2 M tail.

**The executions column counts DISPATCHES only**, so its total is `pv_apply_entries +
pv_tailcall_writes`. The 442,522,201 `foreign-direct` calls ADR-0109 created are a different
operation and sit outside the table — counting them in the denominator (which this harness did once)
makes every dispatch share read about 2.3× smaller than it is.

##### What it says

- **The class is now the LARGEST dispatch class: 200,830,651 executions, 61.43 % of all dispatches**
  — up from ADR-0108's 26.1 %, not because it grew but because ADR-0109 removed the 57.8 % foreign
  population that used to sit above it.
- **The recoverable part is 16,869,596 — 8.40 % of the class, 5.16 % of all dispatches.**
- **The static and dynamic rankings invert INSIDE the class**, as they did one level up:
  `capture` holds 29.91 % of the sites and 17.86 % of the executions (0.60×), while `param` holds
  4.16 % of the sites and 24.85 % of the executions (5.97×).
- **The hot population is `OParam`** — 81.25 M, 24.85 % of all dispatches — and §5 puts it out of
  scope by construction: no local fact exists to recover, so it is caller-homed specialisation, a
  different mechanism with its own prior-art study and its own blow-up failure mode.

##### Why slice 3 stops here

Six reasons, each of which alone would be worth stating:

1. **16.9 M is an UPPER BOUND, not a removal.** It is the candidate population; saturated and
   unsaturated are not separated, and an unsaturated candidate becomes `local-arity`, not a direct
   call. The achievable figure is ≤ this and unmeasured.
2. **Even the bound is 5.16 % of dispatches** — about **1/25** of ADR-0109 slice B's 430.2 M.
3. **Against ADR-0109's own run-time evidence, the expected effect is ~0.1–0.3 %**, which is at or
   under the noise floor that harness measures (slice B resolved a ~3 % effect only once the floor
   was ±2 %).
4. **The tail candidates are 2.25 M.** ADR-0109 slice C removed 9.42 M — four times more — and came
   out INCONCLUSIVE on a quiet box.
5. **The second knob stage has nothing to move.** `AliasLocal` and `AliasGlobal` are zero on both
   axes, so §4's `before local-deferred-<form>/k > 0` completion condition is unsatisfiable for
   `K(S1→S2)`. The three-stage knob would have a stage that cannot be verdicted.
6. **The dominant lever is out of scope**: `OParam`, 24.85 % of all dispatches, needs a different
   mechanism entirely.

**No emitter change is made.** The slice-3 vocabulary stays absent and stays pinned at zero by
`tools/seam-audit.sh`; `PURVASM_LOCAL_FACTS` is never introduced.

##### What is kept, and why

The slice-1 substrate stays: `BindOrigin`, `LocalFact`/`CandidateFact`, the opaque
`CapturableFact` boundary, the `local-deferred` classes and their census/profile rows. It costs no
emission (303/303 `.ll` byte-identical) and it is what makes this decision re-checkable: the numbers
above are a property of THIS corpus, and a corpus in which the alias populations were non-zero, or in
which captures ran hotter than their sites, would be measured by re-running the same two commands
rather than by rebuilding the instrument.

##### Harness defects this slice surfaced (all fixed)

- **The joined table's key derivation did not follow the three-level key.** `split($1, "/")` then
  `p[2]` truncated `local-unknown-fn/<origin>` to one bucket and dropped the candidate rows into an
  empty-named row. Both sides now derive their key by ONE shared rule, splitting on the first
  separator only, with candidates in their own namespace — "capture" is both an origin and a kind,
  and merging them would sum a population the emitter can act on with one it cannot.
- **Direct calls were inside the executions denominator** (see above).
- **`structural-apply` fell out of both sides**: it has no reason axis, so its sites come from the
  census's `class` row rather than a `reason` row — and the footnote then claimed a total the rows
  did not add up to. It is an independent row on both sides now, and two self-test injections pin it
  inside the identity.
- The self-test gained the candidate rows (3 kinds × 2 forms, plus the missing-from-runtime and
  wrong-form negatives) and the three-level-parse row. Verified by fault injection: removing the
  candidate terms from `reconcile` fails exactly those rows.

##### Next

Not in this record: **`OParam` is the standing lever, and it needs its own ADR** — and that ADR's
first slice is a MEASUREMENT, not a design. 81.25 M dispatches at 5.97× says the population is hot;
it does not say whether it is a few call sites in hot loops or a broad population, and caller-homed
specialisation is the technique whose known failure mode is exactly that a size/use metric cannot
tell a reducing clone from a non-reducing one. The drill that decision needs is by site, by
function, by caller and by arity — the ADR-0108 §4 escalation rule, applied one class over.


## Consequences

- `EnvEntry` gains two required fields and `knownFn` stops being one of them — it becomes the derived
  projection `activeFn`. Every binder call site and every `.knownFn` reader is therefore touched: a
  mechanical, reviewable diff whose whole point is that it cannot be partially applied, and whose
  compile errors enumerate exactly the readers that must decide between the active and the candidate
  channel.
- `FnInfo` stops being directly storable in an environment entry; the `CapturableFact` constructor is
  the only way in, and it is fail-closed on `SSelf`. This is a narrowing of an existing freedom, so
  any site that legitimately needs a bare `FnInfo` (the `selfCtx` shortcut does) keeps using one —
  it simply cannot put it where a capture derivation would later read it.
- `MissReason` gains a payload, which changes the reason-name function, the census's TSV tokens, the
  profile's slot layout and the printed schema together. Profiles taken with different builds of the
  compiler are already not comparable by slot INDEX (the harness reads by name); this keeps that
  property and does not add a new one.
- **Three new closed pieces of vocabulary carry the design**: `CandidateFact` (a fact plus the
  derivation that produced it — a record alias today, and an opaque newtype if a future reader
  needs the constructor closed), `CandidateKind` (the shared key of target, event and slot), and
  the `EmitLocalArity` decision. Each exists because a piece of information was otherwise lost
  between where it is known and where it is counted — the kind at classification time, and the
  kind again when a candidate turns out unsaturated.
- **The deferred/direct class pair is split across the slices** (§4). Slices 1–2 add
  `CLocalDeferredApply`/`CLocalDeferredTail` — they are what the knob-blind classifier counts in the
  off leg, and they emit today's generic dispatch byte-for-byte, so they are instrumentation and not
  a lowering. Slice 3 adds `CLocalDirectApply`/`CLocalDirectTail` AND
  `CLocalArityApply`/`CLocalArityTail`, plus the `profileSlot` entries that make them total, and
  adds the arity classes to the dispatch side of both whole-program identities; that is new
  instrumentation on the shipped path and is part of what slice 3's approval covers, not
  something to add in passing.
- **`Lifted.captureFns` AND `Lifted.captures` are replaced by one `Array Capture`** (§2), so
  `buildGrec`'s existing sibling propagation moves onto that array's `fact` field as `FActive`. The
  diff is small but it is not additive, and it touches `makeClosure`: a reviewer should confirm that
  no reader of either old field survives and that both consumers traverse the SAME array, because a
  surviving second array is exactly the representable-mismatch this design exists to forbid.
- The static census and dynamic profile both grow rows. The per-object gates and the two
  whole-program identities are stated over enumerations and so extend without being rewritten —
  which is the argument for having written them that way.
- Slice 3, if it happens, changes emitted `.ll` for the first time since ADR-0109 — with the
  attendant golden re-baseline and fixpoint re-run, and with the interaction to check rather than
  assume: a call that becomes direct is still a safepoint, so the ADR-0105 activation plan is not
  under-rooted, but a call that becomes `musttail` changes the frame-pop choreography and must be
  reviewed against `Root`'s fused terminators.
- Whatever the table says, the deliverable of slices 1–2 alone is a ranked, attributable
  decomposition of the second-largest dispatch class — the input the next decision needs, and the
  thing the aggregate cannot supply.

## Alternatives considered

- **Skip the attribution and just propagate capture facts — it is a small change.** Rejected, and
  it is the tempting one. The change is small; knowing what it bought is not. Without the split
  there is no denominator, the paired gate has no transfer identity to state (the class it moves out
  of would be a mixture), and a null result would be indistinguishable from a bug in the
  propagation. This is the same argument ADR-0108 rejected "instrument first" with.
- **Rank the sub-populations by sites and start with the largest.** Rejected on this track's own
  evidence: sites and executions disagreed at 3.02× vs 0.54× one level up, and there is no reason
  the sub-populations behave better than their parent.
- **Attack the class in the optimiser instead (inline the higher-order callee at its use).** Not
  rejected — deferred. It is the right home for `OParam`, and this record's measurement is what
  would size that work; but the recoverable populations, if they exist, are facts the emitter
  already computed and threw away, and paying an inliner to reconstruct them is the wrong price.
- **Give the runtime a faster generic apply instead.** Rejected as the response to THIS class: ADR-0102
  already measured the dispatcher, and 4.2 % slow-path share means the win is bounded by a constant
  factor on an operation the direct path does not perform at all. It also cannot address the
  `pv_tailcall` form, which is 39.1 % of this class.
- **Attribute by caller function or by call-site symbol, as ADR-0108 §4 slice 2 did for foreign
  symbols.** Rejected as the FIRST cut: the foreign drill keyed by symbol because provenance there
  was per-module knowledge that slots cannot carry, and because a 24-symbol population was expected.
  Here the taxonomy is a closed, small, program-wide enumeration, so slots carry it exactly and give
  the cross-check identity for free. A per-caller drill remains available if one origin turns
  out to dominate and the origin alone cannot decide — the same escalation rule §4 used there.
