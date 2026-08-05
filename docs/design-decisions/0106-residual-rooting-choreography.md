# 0106. Closing the residual rooting choreography: reload-reroot elision and the plan-driven `Gcaf` init tier

- Status: ~~Proposed~~ **Accepted** _(2026-08-04: explicit maintainer Accept after 3 review rounds)_
- Date: 2026-08-04
- Deciders: maintainer
- Technical story: ADR-0105 follow-up (its Results §"Remaining-root census"), levers (A)/(B)

## Context

ADR-0105 closed with a paired A/B (`.ll` −25.0 %, `clang` wall −21.6 %, peak RSS −19.4 %,
linked binary −12.9 % on the fixed CoreFn closure) and a full classification of the 53,908
residual root blocks. Two of the residual classes are not "truly live across a safepoint"
rooting at all — they are leftover conservatism with soundness stories much simpler than
the liveness analysis itself:

- **(B) reload-reroot, 3,553 blocks (6.6 % — direct 2,685 = 5.0 %, init 868 = 1.6 %):**
  an already-rooted, un-forced value (a `VRooted` token) is re-rooted into a NEW slot by
  `evalAtoms`' later-safepoint pass and the `SForceCell` argument pre-rooting. The
  existing slot already GC-tracks the value for the lifetime of the activation frame; the
  second slot is pure duplication. Upper bound ≈ 53–64 k lines ≈ 2.4 % of the final IR.

  > **Correction (2026-08-04, round 3 — measured):** the 3,553 were a MIX, not all
  > duplication: **removable ≈ 3,215** (2,685 direct + 388 init-transient + ≈ 142
  > `GlobalSlot` re-roots eliminated by the round-2 handle-copy) and **338 REQUIRED
  > `LocalSlot` lifetime promotions** (pop-crossing candidates that must snapshot pre-pop
  > and take a fresh permanent root — phase-order-mandated, censused as their own class).
  > Slice 1's completion criterion: the removable class → 0; the 338 promotions remain,
  > named.
- **(A) the init tier's `rootAll` fallback, 15,467 blocks (28.7 %):** init bodies still
  root every definition on creation (ADR-0105 slice 2 deliberately kept the conservative
  fallback there). The pre-implementation BASELINE SPLIT (2026-08-04, required before
  slice 2 so its delta is attributable): `Gfun` 2,803 inits / 2,803 roots (all
  fixed-shape permanent roots — not a target), **`Gcaf` 4,734 inits / 12,197 roots**
  (≈ 4,734 permanent + ≈ 7,463 transient body roots = the target pool, 13.8 % of all
  root blocks), `Grec` 45 inits / 467 roots (deferred). A `Gcaf` body is an ordinary
  `Expr` — the existing `activationPlan` applies almost unchanged; truly-crossing
  values and the permanent tier stay.

This ADR deliberately does NOT include the provably-not-by-need analysis. The three levers
rest on different soundness bases — (B) on existing-slot lifetime/identity reuse, (A) on
activation lifetime and the permanent-root phase order, not-by-need on value provenance
and force semantics — and separating them keeps both failure attribution and census
attribution clean. Not-by-need gets its own ADR, gated on a force-only-crossing census
(ADR-0105 Results, follow-up map).

## Decision

Two slices, each a review checkpoint with its own census delta and the full battery
(unit + seam-audit + e2e + behavioural gate incl. stress and debug-ABI + fixpoint smoke +
bench no-regression). Both are intentional emission divergences: goldens re-baseline,
same-corefn byte-identity is asserted only for unchanged inputs of refactor-only rounds.

### Slice 1 — reload-reroot elision (B)

A rooting request for a value that already HAS a live slot reuses that slot instead of
minting a new one. Pinned:

- **API shape: rooted token in, ROOTED token out — never a raw handle, never a plain
  `Val`.** The recipe-facing operation becomes an `ensureRooted`-style form: given the
  activation's `Maybe FrameToken` and an operand `Val`, it returns an OPAQUE
  `RootedVal` — a token that is rooted BY TYPE (convertible back to an operand `Val`,
  never the reverse without rooting). Idempotent rooting is thereby promoted into the
  token/API surface, not left as a call-site convention.
- **A local root carries a non-forgeable TWO-TIER owner, checked at EVERY `LocalSlot`
  consumption — not only at `ensureRooted`.** Checking reuse alone would leave a hole:
  a `LocalSlot` token leaked across an activation boundary could still reach
  `useVal`/the reload renderer directly and reload a DEAD slot without ever passing
  through re-rooting. Pinned:
  - **`ActivationId`** — minted monotonically by `beginFn`, never reused, independent
    of the SSA numbering (an SSA/mark STRING cannot serve as an owner identity: `%tN`
    names are re-minted every `beginFn`, so equal text does not mean same activation);
  - **`FrameOwner`** — minted per `openFrame`, CONTAINING its `ActivationId`;
  - **`ensureRooted`** reuses a `LocalSlot` iff its `FrameOwner` equals the passed
    `FrameToken`'s, EXACTLY; a different owner, or `Nothing` for the frame, is
    fail-closed;
  - **`useVal`/the reload renderer** verify at least
    `token.activationId == currentActivationId` before ANY `LocalSlot` consumption
    (reload-on-miss included) — a cross-activation reload crashes instead of reading a
    dead slot;
  - **`GlobalSlot` is exempt** from both checks (permanent root).
- **The branch semantics are total and pinned per token arm:** `GlobalSlot` → reuse,
  frame NOT required (permanent root); `LocalSlot` → reuse iff `FrameOwner` matches,
  else fail-closed; `Fresh`/raw → a frame is REQUIRED (root into it; `Nothing` crashes
  via the existing no-token backstop).
- **An alias inherits the same `RootSrc`** (ADR-0105 §6.2 alias inheritance) — it must
  not mint a new slot identity.
- **A force result is a DIFFERENT value from the cell** and never reuses the cell's slot.
  This holds by construction — `forceValue` yields a `VFresh` phi token, so it cannot
  present a `RootSrc` to reuse — and the ADR-0105 census already separates these
  (`forced-phi`, 8.1 %, untouched by this slice).
- **Pop-crossing values keep the `snapshotVal` discipline** (ADR-0105 §6.4): reuse never
  extends a slot's life past its frame pop.

Retargeted sites: `evalAtoms`' later-safepoint rooting, `SForceCell`'s callee/argument
pre-rooting, and any other `atom → root` sequence the implementation inventory finds.
Expected census delta: ~~the `reload-reroot` class (3,553) drops to ≈ 0~~ _(corrected
2026-08-04, round 3: the REMOVABLE subclass — ≈ 3,215 — drops to 0; the 338 lifetime
promotions remain as their own censused class)_.

### Slice 2 — plan-driven `Gcaf` init tier (A)

- **`Gcaf` only.** A `Gcaf` body is an ordinary `Expr`; apply
  `activationPlan { params: [], captures: [], selfName: Nothing }` and drive the init
  body's rooting from the plan exactly as `emitFunction` drives `LBody`.
- **`Grec` is deferred, and its census is SPLIT OUT.** `buildGrec` owns lowering-local
  roots that are load-bearing (the shared env array, the placeholder cells, the
  backpatch reads); it cannot be poured through the same plan without its own analysis.
  It keeps the `rootAll` fallback in this ADR, and the census learns to report `Grec`
  init roots as their own class so the residual is visible, not hidden inside "init".
- **`Gfun` init is already a fixed shape** (ADR-0105 §2 round 4) — not a target.
- **The permanent-root phase order is UNCHANGED:** body completes → candidates are read
  back into epoch-checked `Fresh` tokens (`snapshotVal`) while the transient frame is
  still live → pop → permanent-root stores. Plan-driven rooting changes which BODY
  definitions get transient slots; it does not touch the wrapper-owned tier.
- **`needsFrame` = activation roots ∪ lowering-local roots**, computed for the init body
  exactly as for a direct entry.
- **The public init surface is a FIXED SHAPE, not a callback: `emitGcafInit(key, Expr)`.**
  Passing `Nothing` to a body callback is NOT enough — the callback could still call
  `openFrame` itself and leak a frame, re-opening exactly the escape hatch ADR-0105 §2
  round 4 closed for `Gfun`. The dedicated emitter owns the WHOLE order internally:
  plan → optional `openFrame` → lower the `Expr` → `snapshotVal` the candidate →
  optional pop → permanent root; no caller-supplied code runs inside the sequence. If the
  implementation keeps an internal callback for code reuse, it must be module-PRIVATE
  with its `openFrame` use sites under the exact `seam-audit` counts — and the ADR
  records that this variant is an AUDIT guarantee, not a structural one (the fixed-shape
  public surface is the structural form and the default).

Expected census delta: `init/*` classes shrink by the non-crossing population of `Gcaf`
bodies; `Grec` remainder reported separately.

### Sequencing

Slice 1 first — ~~all 3,553 blocks are direct duplication~~ _(round-3 correction: the
≈ 3,215 removable blocks are; 338 are mandated lifetime promotions)_ with a
self-contained soundness story — then slice 2. Each lands only after its checkpoint
review.

## Verification

Per slice: the remaining-root census re-run (the class the slice targets must account for
the delta — reductions must be attributable, ADR-0105's accounting discipline; slice 2
reports against the `Gfun`/`Gcaf`/`Grec` baseline split above), unit tests for the new
API contracts, seam-audit pins for any new caged identifier, and the full ADR-0105
battery (behavioural gate with stress + debug-ABI legs, fixpoint smoke, bench legs).
`clang` wall / RSS spot-check after slice 2 (the A/B harness from ADR-0105's Results is
reusable as-is).

The safety contract is closed by these pinned negative/positive controls:

- `LocalSlot`: same-frame reuse succeeds; a DIFFERENT frame's token, or `Nothing`,
  fail-closed crashes;
- `GlobalSlot`: reused with and without a frame;
- `Fresh`: with a frame → a NEW root; without → the no-token crash;
- frameless `Gcaf`: the emitted init has no frame open, no transient root, no pop — and
  its permanent root still lands;
- an activation-crossing `Gcaf` and a lowering-local-roots-only `Gcaf`: both KEEP their
  frame;
- a GC-firing fixture with a LOAD-BEARING order (a mere "collection happened during
  init" can be vacuous): `kept` = a heap value built first → SUBSEQUENT allocations
  force a collection (so `kept` must survive by being rooted, not by luck of ordering)
  → `kept`'s CONTENT is observed (full text or checksum, not just a non-crash) through
  the permanent root AFTER init returns — the gc-stress leg's init-tier sibling;
- a `LocalSlot` minted in activation A, consumed DIRECTLY via `useVal` in activation B
  (after `beginFn`): crashes — the consumption-side `ActivationId` check, not merely
  the `ensureRooted` path;
- `ActivationId` counter exhaustion/overflow: fail-closed, never wraps into reuse (the
  implementation consequence of the "never reused" pin, mirroring `bumpEpoch`'s
  overflow discipline).

## Consequences

- Root-slot idempotence becomes an API-level property of the token kernel instead of a
  per-site convention — later levers (not-by-need, `CCase` precision) inherit it.
- The init tier's censused residual after slice 2 isolates `Grec`'s lowering-local roots,
  giving the later `Grec` decision its own number instead of an aggregate.
- Not-by-need's sizing gate (forced-phi direct count + force-only-crossing census) is
  unaffected by this ADR and can proceed in parallel.

#### Progress (2026-08-04): slice 1 — reload-reroot elision landed

The pinned surface landed end-to-end: `Value` gained the two-tier `FrameOwner`
(`actId`/`frameId`), the owned `LocalSlot { handle, owner }`, and the by-type-rooted
`RootedVal` kernel (`rootedVal` one-way unwrap; `rootedFromVal` pure/total classifier;
`mkRootedLocal` the only fresh constructor, audit-caged to `Root`). `Monad` mints
`actId` monotonically in `beginFn` and `FrameOwner` via the Root-caged `mintFrameOwner`
(both overflow-fail-closed), and `checkSlotActivation` runs before EVERY `LocalSlot`
consumption in `useVal`/`useValHot` (reload-on-miss included) — a cross-activation token
crashes instead of reloading a dead slot. `Root.ensureRooted` is the total branch form
(`GlobalSlot` reuse frameless / `LocalSlot` exact-`FrameOwner` reuse, else fail-closed /
`Fresh` roots into the required frame); `rootLocal` went private; `FrameToken` carries its
owner. `Types.BindingV`'s rooted arm and `bindVar`/`bindFnVar` hold `RootedVal`; every
`Emit` rooting site (`evalAtoms`, `SForceCell`, `CCtor` builder, `CUpdate`, `CCase`
occurrences/literals/extracts, `buildGrec`, params/captures/`%env`, `Let` binds) goes
through `ensureRooted`, and the old raw-handle `root` adapter and `vRootedLocal` are
deleted. **Census accounting (the attribution requirement): the DIRECT reload-reroot
class 2,685 → 0 — fully eliminated.** The init-side 868 → 480: ~~the residual 480 are the
permanent-tier `snapshotVal`→root pairs mandated by the phase order~~ _(superseded by
Round 2 below: ≈ 142 of the 480 were `GlobalSlot` re-roots VIOLATING the accepted
GlobalSlot→reuse pin — now handle-copied away; only the 338 `LocalSlot` lifetime
promotions are the mandated remainder)_; the eliminated 388 were init-body transient
duplicates. Corpus: root blocks 53,908 → 50,907
(−5.6 %), IR −3.0 % (77.6 → 75.5 MB), reloads −4.2 % (each elided block also drops its
slot's reload traffic). Verification: unit **507/507** (branch controls: same-frame reuse
zero-emission / different-frame and no-frame fail-closed / global with-and-without frame /
fresh roots; cross-activation direct-`useVal` crash + current-activation positive;
overflow fail-closed ×2), seam-audit green (`mkRootedLocal`/`rootedFromVal`/
`mintFrameOwner` caged to `Root`, 26 self-test classes), e2e 11/11, behavioural gate FULL
GREEN (stress + debug-ABI), fixpoint smoke HOLDS, bench no-regression.

**Round 2 (slice-1 review P1×2 + P2 accounting).**

- **The forge cage is now real.** `FrameOwner` was a public record alias — constructible
  anywhere, and with the public `mkRootedLocal` it could forge a "rooted by type" token
  for a slot that does not exist. It is now an OPAQUE newtype: the only constructor is
  `unsafeMkFrameOwner` (audit-caged to `Monad.mintFrameOwner`), the readable surface is
  `sameOwner`/`ownerActId`. And the audit's per-case pins left the other allowlisted
  files (Safepoint/Program/Prim/Types) UNCOVERED for the new identifiers — replaced by a
  COMMON, TOTAL allowlist table checked for EVERY file (default zero), with smuggling
  self-tests into `Program`/`Safepoint`/`Types` (29 classes).
- **The init residual is split, and the GlobalSlot half is GONE.** The 480 residual
  load→roots divided into (a) LocalSlot lifetime promotion — a transient-slot candidate
  MUST snapshot pre-pop and take a fresh permanent root (phase-order-mandated; the census
  now names this class) — and (b) ~142 GlobalSlot candidates, which the permanent tier
  was re-rooting against the accepted GlobalSlot→reuse pin. The permanent tier now
  HANDLE-COPIES a GlobalSlot candidate: `load` the source's `$root` index, `store` it
  into the new `$root` — the two globals alias one permanent slot. ABI soundness: readers
  load their `$root` index then dereference the slot, so a shared index dereferences
  identically; init-region slots are never popped and CAF values are never re-stored
  after init. No snapshot, no root block, no reload (golden-pinned). Residual after
  round 2: **338, all init-tier lifetime promotion; direct-tier 0**.
- **The accounting gap is closed.** On paired unchanged-corefn modules (274): root blocks
  48,827 → 46,135 = **−2,692, EXACTLY the eliminated reload-reroot count (gap 0)** — the
  earlier 72-block discrepancy was entirely self-host input drift from the compiler's own
  changed modules, as suspected. Exact paired figures: bytes **−2.78 %**, lines
  **−3.03 %**, reloads **−4.20 %**.
- Stale `rootLocal`-era comments in `Emit`/`Liveness` synced. Verification re-run: unit
  **508/508** (+ the handle-copy golden), audit green, e2e 11/11, behavioural gate full
  green, fixpoint smoke holds, bench no-regression.

**Round 3 (slice-1 close).** `vRootedGlobal` (load-bearing since the handle-copy trusts a
`GlobalSlot` token) and `rootedSrc` moved into the common `allow0106` table
(Value/Monad/Root/Emit columns, default zero over every recursively-walked backend file —
Safepoint/Program/Prim/Types are now covered), with smuggle self-tests into the named
allowlisted files (`vRootedGlobal` → `Program`, `rootedSrc` → `Prim`). Normative text
synced to the measured split (the strike+dated corrections above; removable ≈ 3,215 /
required promotions 338), the round-1 misreading marked superseded, and the stale
`rootLocal` comment in `Emit` synced. **Self-test class count, defined:** the audit
self-injects **29 in-directory violation classes** (one `inject` per class) plus **3
wide-scan classes** (identifiers smuggled outside the backend directory) = 32 self-test
cases in total; earlier notes' "26/29/31 classes" counted inconsistently — the two-part
definition here is the one the script implements. **Slice 1 CLOSED by maintainer review
2026-08-04.**

#### Progress (2026-08-05): slice 2 — the plan-driven `Gcaf` init tier landed

The fixed-shape public surface is `Emit.emitGcafInit(key, Expr)` — callers (the `Program`
`Gcaf` arm) supply data only. It computes
`activationPlan { params: [], captures: [], selfName: Nothing }` and drives the body
exactly as `emitFunction` drives an `LBody` (`rootAll = false`, the plan's crossing set),
over a new single-candidate `Root.emitGcafInitEngine` that owns the WHOLE phase order for
both frame shapes: framed = open → body → snapshot (a `GlobalSlot` candidate is kept
as-is for the handle-copy) → pop → permanent root; frameless = body with `Nothing` (a
body root structurally crashes via `ensureRooted`'s fresh arm) → permanent root. Per the
ADR's audit-guarantee clause the engine callback's use sites are pinned by the
`allow0106` table (Root 3 / Emit 2 / all else 0, smuggle self-test into `Program`).
`Gfun` is untouched; `emitInitFnFramed` remains for `Grec` ONLY. **Census, against the
baseline split (attribution clean): `Gcaf` init roots 11,666 → 6,377 (−5,289, −45 % of
the pool; the remainder = the non-global permanent tier + genuinely crossing body
values); `Gfun`/`Grec` classes unchanged; 990 of 4,736 `Gcaf` inits fully frameless.**
Corpus: root blocks 50,788 → 45,527 (−10.4 %), bytes −5.15 %, lines −5.68 %, reloads
−7.79 %. Cumulative over ADR-0106 (both slices, vs the 0105-close corpus): root blocks
53,908 → 45,527 (**−15.5 %**), reloads −11.8 %. Verification: unit **511/511** (the three
pinned `Gcaf` controls: frameless golden with no open/pop and the permanent root landing;
crossing body keeps its frame; `GlobalSlot`-aliasing `Gcaf` handle-copies to a two-line
frameless init), the **new `Gate.InitGc` fixture** (the pinned load-bearing order: `kept`
built first → 2,000-element churn forces collections — gc17 on the plain small-heap legs,
gc49,675/55,349 under stress — → `kept`'s full content read back through the permanent
root after init), behavioural gate FULL GREEN (now 6 fixtures × both modes × stress +
debug-ABI), audit green, e2e 11/11, fixpoint smoke HOLDS 603/603, bench no-regression
(fib 0.97 / 12.5×, quicksort 1.04 / 19.4×).

**Slice 2 — and with it ADR-0106 — CLOSED by maintainer review 2026-08-05.** Cumulative
results over both slices (vs the ADR-0105-close corpus): root blocks 53,908 → 45,527
(**−15.5 %**), reloads −11.8 %, IR bytes ≈ −8 %; the removable reload-reroot class is 0,
the `Gcaf` init pool is down 45 % with 990 inits fully frameless, and every reduction is
census-attributed to its targeted class. Deferred with their censused residuals: `Grec`
(467 roots, lowering-local), the 338 `LocalSlot` lifetime promotions (phase-order-
mandated), and the non-global `Gcaf` permanent tier + true crossings (6,377). The
follow-up order of record stays ADR-0105 Results' map: force-only-crossing census →
not-by-need ADR → `pv_apply` re-profile.

**Post-close A/B (2026-08-05, no worktree needed — the surviving 0105 A/B corpora over the
same fixed CoreFn closure, restricted to the 273 modules paired-unchanged across all three
compilers):**

| | `.ll` | `clang -c -O2` wall | peak RSS | `.o` total |
|---|---|---|---|---|
| pre-0105 | 92 MB | 91 s | 145 MB | 23,864 KB |
| 0105-final | 69 MB | 76 s | 133 MB | 22,152 KB |
| 0106-final | 63 MB | ~77 s (alternating re-runs: 0106 ≈ 77 s vs 0105 ≈ 79 s — flat within ±3 s noise) | **117 MB** | 21,596 KB |

Reading: ADR-0106's build-side win lands in **peak clang RSS (−12 %)** and `.ll` (−8.7 %);
**clang WALL is now flat** — 0105 already removed the parse-bound mass, and the remaining
wall is `-O2` work per surviving function (Amdahl, again). Cumulative pre-0105 → 0106:
`.ll` −31.5 %, clang wall −15 %, peak RSS −19 %, `.o` −9.5 %. Consequence for the follow-up
map: further rooting reduction (`CCase` precision, `Grec`) should NOT be justified on clang
wall — memory/`.ll` only — which strengthens the case for turning to not-by-need (frame
elision + run-time) and apply-count next.

## Alternatives considered

- **Fold (A)/(B) into the not-by-need ADR.** Rejected: three different soundness bases in
  one change makes failure attribution and census attribution ambiguous (maintainer
  review, 2026-08-04).
- **Plan-drive ALL init shapes at once (incl. `Grec`).** Rejected: `buildGrec`'s
  lowering-local roots need their own treatment; "init全部" in one step trades a bounded
  win for an unbounded review surface.
- **Treat reload-reroot at the census level only (accept the duplication).** Rejected:
  3,553 blocks ≈ 2.4 % of final IR _(round-3 correction: ≈ 3,215 of them removable)_ for
  a small, closed change; and the idempotence API pays forward.
