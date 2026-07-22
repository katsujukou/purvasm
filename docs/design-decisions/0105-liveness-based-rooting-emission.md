# 0105. Liveness-based rooting emission for the native LLVM backend

- Status: Accepted
- Date: 2026-07-20
- Relates to: [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4 (the
  safepoint/rooting contract), [0072](0072-anf-to-llvm-lowering.md) §6 (`eval_atoms`
  need-driven slice — this record is its declared endgame),
  [0079](0079-ctx-header-abi-inline-rooting-fast-paths.md) (inline ABI — who executes the ops;
  this record changes *when*), [0083](0083-match-compilation-to-anf-middle-end.md)
  (dtree sub-occurrence rooting), [0104](0104-retire-boot-byte-identity-gate.md) (§2 gates =
  this record's verification net; §4 golden classes = the re-baseline licence; §5 named this
  record as the second intentional emission divergence),
  [sidenote 0011](sidenotes/0011-v1-gap-anatomy-post-0079.md) lever 1 (the measured case).

## Context

The native backend roots **on creation**: every function activation opens a root frame and
unconditionally roots all params and captures (`Emit.emitFunction`); every `Let`-bound heap
word, case scrutinee, extracted occurrence, and hoisted boxed literal is rooted at definition;
and every `AtomVar` use reloads through its root slot (`abiGet`), whether or not a safepoint
intervened. ADR-0079 made each of these operations cheap (inline ctx-header IR instead of an
extern call) but deliberately did not change *when* they happen — its §4 pins "root-on-create,
reload-after-safepoint, pop-before-`musttail`" as unchanged, and sidenote 0011 already names
the follow-up: *root only a value live across a safepoint* (lever 1, "biggest expected win").

What root-on-create costs, measured on the current corpus (2026-07-20, `--no-opt`
`--emit-llvm` over the self-host closure, 298 objects, 29,102 functions):

| quantity | value |
|---|---|
| total emitted IR | 3,027,521 lines / 101.4 MB |
| root fast-path blocks (`rchk`/`rfast`/`rslow`/`rdone`, ≈ 15–18 lines each) | 87,634 |
| slot reloads (`abiGet`, 3 lines each) | 210,203 |
| settle blocks | 7,856 |
| choreography share of all IR lines | ≈ ⅔–70 % |

That text volume is now the dominant *build-time* cost on the native path: `clang` over these
objects dominates the ADR-0104 fixpoint smoke profile and the L3 build, and it was the
motivation for 0104 §5 naming liveness rooting the second intentional divergence. It is also a
*run-time* cost: a root store is an observable side effect (the GC really reads the slot), so
`-O2` cannot delete it — only the codegen, which knows where the safepoints are, can (sidenote
0011). In `fib`-class code almost no value is live across a safepoint (a `sub` result consumed
as the very next call's argument needs no root), yet today every one is rooted and reloaded.

Why this is safe to do *now*: ADR-0104 retired the boot byte-identity gate and §5-2/§5-4
landed its replacement net — the CI-wired forced-GC behavioural gate and the stage-fixpoint
script — and §4 defines the golden re-baseline licence. The runtime's **production heap/ABI
semantics are untouched** by this record: the five rooting entries, the ctx header, the scan
and relocation contract, and the trampoline are all unchanged; the only runtime change is
test-only instrumentation (the §5 stress knob). Everything else is emission-side.

## Decision

Emit rooting choreography from a per-function **liveness analysis over the ANF body**, instead
of on creation. The ADR-0064 §4 contract is restated and preserved in the new terms: *every
heap reference live across a safepoint is slot-backed at that safepoint and its next use
reloads from the slot* — the change is that references **not** live across any safepoint now
get **no slot, no store, and no reload**.

### 1. Safepoint classification (pinned), and the seam it lives behind

A **safepoint** is an emitted operation that may allocate or run guest code (ADR-0064 §4:
allocation sites and calls that may allocate; no back-edge polls). Individually, per emitted
operation (no catch-all class rows — each symbol classified against its runtime source):

| emitted op | safepoint? | why |
|---|---|---|
| `pv_apply`, direct `call tailcc @…$d`, `pv_run_effect` | yes | runs guest code |
| `pv_tailcall` | no | stashes the pending tail into runtime-owned storage (a host `Vec` copy) — no guest allocation, no guest code; the calling-convention invariant is that NO guest allocation occurs between the stash and its take, and the actual safepoint is the later `pv_settle`/`pv_apply` that resolves it |
| native leaf call (`@pvf_…`), any unknown/unclassified call | yes | may allocate via ctx (conservative default) |
| `pv_settle` | yes | slow path resolves a pending tail (runs guest code) |
| `pv_force_if_byneed` | yes | slow path runs a thunk |
| `pv_make_closure`, `pv_new_array`, `pv_new_adt`, `pv_new_record`, `pv_new_str`, `pv_new_number`, `pv_new_byneed_placeholder` | yes | allocates |
| `pv_prim_append`, `pv_prim_new_array` | yes | allocates |
| boxed-`Number` arithmetic (`pv_prim_add_number`/`sub`/`mul`/`div`, `pv_prim_int_to_number`) | yes | boxes the result |
| `pv_record_set`, `pv_prim_record_set`, `pv_prim_record_delete`, `pv_prim_record_union` | yes | functional update allocates |
| `pv_prim_set_array` | no | in-place store (verified against the current runtime; the ADR-0052 linear array-builder / in-place store contract) |
| `pv_empty_array` | no | returns the shared immediate-empty representation, no allocation |
| scalar `Int`/`Boolean` inline primops; `pv_prim_eq_int`/`lt_int`/`eq_bool`-class comparisons | no | register-only |
| `pv_prim_eq_number`/`lt_number`, `pv_prim_eq_string`/`lt_string`, `pv_prim_number_to_int` | no | reads + immediate result |
| `pv_prim_index_array`, `pv_prim_length_array`, `pv_read_field`, `pv_read_raw`, `pv_record_get`, `pv_prim_record_get`, `pv_prim_record_has` | no | read-only |
| root/frame/get/pop inline fast paths | no | `rslow` grows the shadow stack (`roots_base` realloc), never the heap |

Each "no" row must be verified against the runtime source when the table is implemented
(slice 1); a row that cannot be verified stays "yes". Misclassification is asymmetric —
"yes"-when-actually-"no" costs a redundant root; the reverse is a missing root — so the
default and every doubt resolve to "yes".

**The seam (pinned):** the emitter must not be able to bypass the classification. All
runtime/direct/foreign call emission goes through a single classified emission API (or an
exhaustive call-descriptor ADT the classifier is total over) — raw `call`-text emission for
anything classifiable is a structural error, not a convention. This is what makes the table a
source of truth rather than documentation.

**Connecting the ANF analysis to emitted-op classification (pinned):** the backward pass runs
over ANF, but safepoints are emitted operations, and one ANF node lowers to an operation
*sequence* (a `CApp` may emit `forceValue` + a direct call + `abiSettle`; a `CCase` dtree
contains conditional calls; `evalAtoms` interleaves operand evaluation with rooting). The
bridge is a per-node **`maySafepoint` transfer function**: for every ANF node/atom class, a
pinned summary of whether its lowering can emit at least one safepoint operation, derived
from (and tested against) the lowering recipes themselves — including each recipe's
**lowering-local operation sequence** (the summary is over everything the recipe emits, not
just its "main" call). The liveness question "does this definition cross a safepoint" is
answered entirely in ANF terms via these summaries.

**The use-at-call boundary (pinned):** a value consumed *as an argument to* a safepoint
operation, with no use reachable after it, is **not** live across that safepoint — the callee
protects its own view of the arguments (callee self-rooting, per its own analysis); crossing
requires a use *after* the operation returns. A value that is BOTH an argument and used after
the call crosses as usual.

### 2. The liveness analysis (ANF, per activation) and the two-tier `RootPlan`

A backward pass over the ANF body computes, for every ANF-level rootable definition —
parameter, capture, `Let` binder, case-arm binder (a NAME the ANF tree binds), and the
self-recursion `%env` word — whether its **live range crosses at least one safepoint** (per
the §1 transfer functions and use-at-call boundary). The analysis is
syntactic on the ANF tree (`fvExpr`'s traversal is the substrate, not the implementation —
see the §2a engineering pins); closures are opaque — a capture's crossing is judged in the
*capturing* activation only up to the `pv_make_closure` that consumes it (the callee roots
its own view per its own analysis), and a value's escape into a closure ends the caller's
obligation for that use.

Immediates need no root for GC correctness (the scan is tag-directed), but the analysis does
not attempt type refinement: **liveness is the only criterion**, and rooting a live-across
immediate is accepted (correct, marginally redundant). No ANF is reshaped by this record —
the analysis reads the tree the middle end already produces.

**The `RootPlan` is two-tier (pinned), and every root site is owned by exactly ONE tier** —
the ANF analysis does NOT see every root the emitter issues, and no site may claim both
homes:

1. **Activation roots** — the ANF *names* above (params, captures, `Let`/case-arm binders,
   self `%env`), selected by the liveness analysis.
2. **Lowering-local roots** — the physical temporaries the emitter/match-compiler creates,
   which are not ANF names: the forced scrutinee, dtree physical occurrences (ADR-0083),
   hoisted boxed literals, `evalAtoms` intermediates, `SForceCell` callee/argument
   temporaries, the `CUpdate` accumulator, the under-applied constructor builder,
   `buildGrec`'s shared env and placeholder cells, and any construct on the §3
   conservative-fallback list. Each recipe declares (statically, as part of its descriptor)
   whether it **may root**; these declarations are part of the plan, not discovered during
   emission. While a construct sits on the conservative-fallback list (dtree occurrences in
   the initial slice), it is *entirely* lowering-tier: the activation analysis does not
   reason about it.

**Frame decision (pinned):** `needsFrame = activation roots ≠ ∅  ∨  any reachable lowering
recipe may root`. A frame is elided only when NEITHER tier can produce a root site — it must
be structurally impossible for a *transient* root to be emitted inside an activation that
opened no frame (otherwise the root lands in the caller's frame or the never-popped init
region: a shadow-stack ownership violation, not a precision loss).

**The rooting API is split by root lifetime (pinned)** — one blanket "no rooting without a
frame" rule would outlaw `storeRootGlobal`, which *deliberately* roots after popping the
transient frame to create a permanent init-region handle; and one blanket exception would
give transient roots an escape hatch. Instead the emission monad exposes two capabilities and
closes the raw op:

- `rootLocal` — requires the open-frame token; the ONLY way any activation/lowering root is
  emitted; unavailable when `needsFrame = false` (the token does not exist).
- `rootPermanent` — requires the init capability, held only by the **`Gfun`/`Gcaf`/`Grec`
  `$init`** emission paths (every init stores a permanent root-handle global — a `Gfun` init
  builds its closure and roots it permanently too); used after the transient frame is popped
  *if the init opened one* (a `Gfun` init has none). The entry stub does NOT hold it — it is
  RootPlan-scoped like any activation but creates no permanent roots. This is the only way
  `storeRootGlobal`'s permanent handles are created.
- raw `abiRoot` is private to these two — direct emission of it anywhere else is a
  structural error.

This guards both directions: a local root cannot leak into the caller's frame or the init
region (no token → no `rootLocal`), and a lowering recipe cannot "borrow" `rootPermanent` to
dodge the frame discipline (no init capability outside init emission).

**Scope (pinned):** the plan applies to every activation-shaped emission unit, not only
`emitFunction`: lifted functions, `Gfun`/`Gcaf`/`Grec` init bodies, the entry
expression/`@main` stub, and any wrapper the backend synthesises. Permanent init-region
handles (`rootPermanent`) are outside the liveness plan — deliberately never-popped and
unchanged by this record.

#### 2a. Engineering pins (stack safety and complexity)

The pass re-enters the territory of the 2026-07-16 stack-unsafety class, so its shape is
pinned up front: a **single** backward pass computing all definitions' crossings at once
(never per-definition `fvExpr` re-walks — that is quadratic in `Let`-spine length); the
`Let`/`LetRec` spine walked with an explicit worklist or `tailRec` (never native recursion —
`fvExpr` itself is documented as plain-recursive on deep spines and is a substrate to learn
from, not to call per definition); width-sized traversals (operand lists, case arms, record
fields) on the existing stack-safe combinators (`forA`/`foldA`, ADR class-wide sweep).
Verification includes default-stack fixtures at the established scale: a 50k-binding `Let`
spine and a wide-operand/wide-case body through the full analysis+emission path.

### 3. Root placement and the reload discipline

- **Prologue**: root exactly the params/captures whose ranges cross a safepoint (in that
  order, matching today's slot discipline); an activation with `needsFrame = false` (§2 — BOTH
  tiers empty) opens **no frame** and its returns emit **no pop**.
- **`Let`/case-arm binder definitions** (activation tier): rooted immediately after
  definition iff crossing.
- **Reload**: within a straight-line ANF segment the emitter tracks, per rooted definition,
  whether a safepoint has occurred since its definition or last reload; a use before any
  intervening safepoint reads the SSA value directly, a use after reloads once via `abiGet`
  and re-caches until the next safepoint. At a branch join (`CIf`/`CCase` arms rejoining, dtree
  merge points) the cache is conservatively invalidated iff any incoming path contains a
  safepoint. This subsumes the existing `evalAtoms` suffix-scan elision (ADR-0072 §6 slice 1),
  which is retired into the general discipline.
- **`musttail`**: pop-before-tail unchanged (ADR-0064 §4) — now "pop iff a frame was opened"
  (`needsFrame`, never the emission-time question "did anything happen to root").
- **Trampoline/settle**: unchanged; settle placement is not rooting and keeps its current
  sites.
- **Per-construct conservative fallbacks are permitted** where the precise range is subtle
  (e.g. dtree sub-occurrences under guard fallthrough, ADR-0083) — a fallback means
  root-on-create *for that construct*, never a missing root; each fallback in the landed
  implementation must be listed in this record's Progress note (they are the residual for a
  later refinement, not silent behaviour).

### 4. ABI-profile independence (the 0079 pairing survives), and its own gate

The **`RootPlan` is identical in release and `--debug`**; the ABI profiles differ only in
operation form (inline fast path vs guarded entry call), exactly as today. This keeps the
debug generation net meaningful (it checks the same slots the release build uses) and leaves
the `pv_ctx_abi_v1` link-stamp pairing untouched.

Two distinct mode axes exist and the verification treats them separately (they have been
conflated before — the behavioural gate's "both modes" is the **optimiser** axis
`--opt`/`--no-opt`, not the ABI profile): (a) optimiser mode, already covered by the gate;
(b) **ABI profile** (release/`--debug`), which today has NO Level-2 execution coverage — the
Level-2 CLI builds with `debug: false` fixed. This record adds the profile axis' gate:

- a direct unit test asserting the **logical `RootPlan` computed for the same ANF is
  equal** under `inlineAbi = true` and `false` (the plan must be computed before, and
  independently of, the operation-form switch);
- a native execution fixture built **debug-profile end-to-end** — debug entry-call IR linked
  against the debug runtime staticlib — and run under the §5 stress knob, so the
  generation-checked entries exercise the liveness-planned slots for real. Note the pairing
  here is **harness-selected, not link-stamp-enforced**: debug objects carry NO stamp
  (`abiStamp false = ""`) — the stamp only rejects the *inline-object-vs-debug-runtime*
  mixture, not the reverse — so the leg itself must assert its pairing: the debug IR contains
  no `pv_ctx_abi_v1` reference, and an `nm` audit of the linked binary/staticlib shows the
  debug symbol (`pv_ctx_abi_v1_debug`) present and the release stamp absent. The Level-2 CLI
  surface need not grow a `--debug` flag for this: the compiler-level harness (the
  `Test…LLVM.Driver` `buildIR` path, which already parameterises `debug`) plus a script leg
  that explicitly links `runtime/target/debug` is sufficient, and the vehicle is named in the
  slice-0 deliverable.

### 5. The GC-stress verification knob (runtime addition, lands FIRST)

The failure mode of a liveness bug is silent: a missing root only corrupts memory if a
collection lands inside the un-rooted range. The forced-GC behavioural gate makes collections
*frequent*; this record adds the knob that makes them *certain to land in every window*:
`PURVASM_GC_STRESS=1` — the runtime collects at **every allocation entry** (the safepoint
superset), so every missing-root window deterministically contains a collection. What that
guarantees, precisely: the *window is exercised* on every run — not that every missing root
becomes an observable failure (a moved-but-unread value, or one whose stale copy happens to
survive in place, can still pass; observability comes from the fixtures reading and
checksumming the values whose windows are stressed). Implementation: a flag read once at
`pv_runtime_new` (like `PURVASM_HEAP_WORDS`, ADR-0102 §4), checked in `alloc` before the
capacity test; debug and release runtimes both honour it. Production heap/ABI semantics are
unchanged — this is test-only instrumentation on the unfrozen runtime.

The stress leg's stats assertion is also stronger than the standing gate's: in addition to
the schema check and `gc_collections >= 1`, it asserts **`gc_copied_words > 0`** (already in
the `purvasm-stats:v1` schema) — a stress run whose collections copied nothing proves no live
object was ever relocated, i.e. the leg exercised nothing; it must fail as vacuous rather
than pass.

## Verification (the 0104 §2 net, plus the stress rung)

Sequenced so the strengthened net exists BEFORE the divergence — the same discipline 0104 §5
used for bridge removal:

1. **Slice 0 (net first):** `PURVASM_GC_STRESS` in the runtime + a rooting-stress fixture in
   `test-fixtures/l2-behavioural` (long live ranges crossing many safepoints interleaved with
   dead-before-safepoint values and churn — every stressed value read back into a printed
   checksum, per the §5 observability caveat, so hoisting cannot vacate it and a corrupted
   value cannot pass silently). The behavioural gate grows a stress leg (`GATE_GC_STRESS=1`
   runs the fixture set under the knob, asserting `gc_collections >= 1` AND
   `gc_copied_words > 0`), plus the **debug-ABI-profile leg** (§4: debug-emission fixture
   linked against the debug runtime, run under stress). All green on the CURRENT emission
   before any rooting change lands.
2. **Slice 1:** the liveness analysis + `RootPlan` as its own module (`Backend.LLVM.Liveness`
   or ANF-neutral if it has no LLVM specifics), including the §1 classification table +
   per-node transfer summaries and the §2a stack-safety shape. Unit tests over: the
   crossing/branch/closure-escape edge classes (consumed-before-safepoint, use-at-call vs
   live-after-call, crossing, branch-join invalidation, capture escape, self-recursive
   `%env`, dtree occurrence fallback); the **release/debug `RootPlan` equality** property
   (§4); lowering-recipe may-root declarations vs the recipes' actual emissions (the two-tier
   consistency, §2); and the §2a default-stack fixtures (50k `Let` spine, wide operands/arms).
3. **Slice 2:** emission integration (the classified emission seam of §1 lands here — call
   emission routed through the descriptor API). Gates, all in the same change: behavioural
   gate green on BOTH optimiser modes including the stress leg AND the debug-profile leg;
   examples sweep; `Test…LLVM.Driver`/`Abi` goldens re-baselined (emission-shape class, §4
   licence = the behavioural green; `Mangle` and all format-class goldens untouched); the
   stage-fixpoint smoke profile re-run green (the analysis is deterministic, so stage-3 ≡
   stage-4 must still hold); bench regression check (steps/allocs + native wall) per the
   standing discipline.
4. **Measurements recorded in the Progress note:** the census table above re-derived
   post-change (.ll lines/bytes, root-block and reload counts), `clang` wall time on the
   fixpoint smoke profile, binary sizes, and the bench deltas; plus the §3 list of
   per-construct conservative fallbacks actually landed. The build-time claim (this record's
   primary motivation) is judged on the fixpoint-smoke `clang` leg; the run-time claim on the
   benches.

## Consequences

- The ≈ ⅔ of emitted IR that is rooting choreography shrinks toward the true
  live-across-safepoint population; `clang` time — the dominant leg of the fixpoint smoke
  profile and the L3 build — falls with it. This is the lever 0104 was run to unlock, and it
  shortens the very gate (fixpoint) that guards it.
- Run-time: fewer root stores and reloads on every hot path (sidenote 0011's `fib`-class
  argument: almost no crossing values), on top of 0079's per-op cheapening. Both modes'
  binaries shrink; `--debug` loses entry-call count too.
- The emitter takes on real analysis complexity (today's rooting is decision-free). The risk
  class — a missed root = relocation-time corruption — is what slice 0's net targets: the
  stress knob deterministically *exercises* every missing-root window, and the fixtures'
  readback/checksums make the corruption observable (§5's precise split of those two
  guarantees).
- `--no-opt` remains the optimiser-free reference lowering (ADR-0104 §3): liveness rooting is
  **required emission correctness machinery, not an optimisation pass** — it runs identically
  in both modes (mode-split emission was rejected in 0104's alternatives, and the root set is
  mode-independent by §4 above).
- The ADR-0102 apply-count class (the milestone-waiver blocker) is NOT addressed here — that
  debt lives in the optimiser/runtime apply path, and the waiver's bounded re-attempt
  discipline continues unchanged. (Less IR to emit shortens the stalled leg's *emission*
  phase, but mod_282's stall is in optimisation, not emission.)

## Alternatives considered

- **Keep root-on-create and lean on LLVM `-O2`.** Rejected: a root store is observable (the
  GC reads the slot), so LLVM must preserve it; CSE only merges redundant header loads
  (sidenote 0011). The census shows what that leaves on the table.
- **LLVM `gc.statepoint`/stack maps.** Deferred in ADR-0064 and still deferred: it replaces
  the shadow stack with LLVM-managed maps — a large, LLVM-version-coupled machinery change
  that also obsoletes the 0079 inline ABI. Worth revisiting only at the allocation-inline
  lever (sidenote 0011 lever 3), where the safepoint discipline moves into codegen anyway.
- **Lazy rooting (root at first crossing safepoint rather than at definition).** A refinement
  of this record, not an alternative — it changes slot order and complicates the frame
  discipline for a second-order win; noted as possible follow-up once the analysis exists.
- **Type/kind refinement (never root provable immediates).** Needs representation typing the
  backend does not track; the win is bounded (immediates crossing safepoints are rare) and
  the risk profile is the same class as liveness itself. Out of scope.

#### Progress (2026-07-22): slice 0 — the stress net landed, green on the CURRENT emission

All four slice-0 deliverables are in, verified against today's root-on-create emission (the
point of net-first: the net is proven on known-good output before any rooting change exists):

- **`PURVASM_GC_STRESS`** in the runtime: a `gc_stress` flag on `Heap` (absent-or-`"1"` env
  contract like `PURVASM_STATS`, read only on the `new_native` path; `enable_gc_stress_for_test`
  for in-process tests), checked at `alloc` entry before the overflow test. Unit-tested
  (per-alloc collection count, `gc_copied_words > 0`, and survivor integrity through 9+
  consecutive relocations — an ADT chain + string read back intact); 150/150 cargo tests, the
  new test Miri-clean. Production semantics untouched when off (one flag check).
- **`Gate.RootStress`** fixture: early-built long-range values (an index-dependent string
  array + a closure capture) crossing the churn loop's hundreds of safepoints, interleaved
  with per-iteration transients that die before crossing; every stressed value read back into
  printed checksums. Expected trace generated from the JS backend per the 0104 §2 discipline.
- **The gate's stress legs**: both built binaries re-run under `PURVASM_GC_STRESS=1`,
  asserting the full oracle plus `gc_copied_words > 0` (the vacuity guard; the knob's
  accept-absent-or-1 contract means the variable is set only on the stress path). Observed on
  current emission: 28k–126k collections per leg, all 10 stress legs behaviour-identical.
- **The debug-ABI-profile leg** (the §4 gate; vehicle chosen per the slice-0 delegation):
  `PURVASM_EMIT_DEBUG_ABI=1` — a test-only env knob in the CLI build path (three lines; the
  debug profile is a verification pairing, not a user-facing build mode, so no CLI flag) —
  emits the entry-call IR, linked against the DEBUG runtime staticlib
  (`PURVASM_RT_A_DEBUG`), run under stress. The leg audits its own pairing (harness-selected,
  not stamp-enforced): no `pv_ctx_abi_v1` in the emitted IR + `nm` shows
  `pv_ctx_abi_v1_debug` in the linked staticlib. Green: `OK gc28693`.

CI: `l2-behavioural-ci.yaml` now builds both runtime profiles. Full extended gate wall time
≈ 5.5–6.5 min locally (the stress legs dominate). The §4 release/debug `RootPlan`-equality
unit test is a slice-1 deliverable (it needs the `RootPlan` to exist) and is NOT yet in.

Review round (2026-07-22), all folded and the full gate re-run green: **[P1] the harness owns
its mode axes** — not setting a variable does not make it absent, so an ambient
`PURVASM_GC_STRESS=1` would have turned every leg into a stress leg, and an ambient
`PURVASM_EMIT_DEBUG_ABI=1` would have made even the "release" legs emit debug entry-call IR
(which runs fine on the release runtime — all green with inline-ABI coverage silently gone);
the script now `unset`s both up front and each owned leg passes `=1` to its own child only.
**[P1] the fixture's observability claims are now real, and its two rooting paths are
independent** — the closure captures a heap array (not an immediate) riding a `Ref` so the
capture cannot be inlined away; the FULL concatenation of every stressed string is printed
(no prefix/length shadowing); and (second round) the direct-local path and the capture path
hold two SEPARATE allocations with distinct contents, each printed in full — aliasing one
array through both paths would let a correctly-rooted path keep it alive and mask the other
path's missing root, and a length-only fold could not see same-length corruption.
**[P2]** the debug audit covers every emitted object (not just `entry.ll`) and `nm`-asserts
both directions exactly (debug stamp present, release stamp absent; `nm` added to the
prerequisites); `PURVASM_EMIT_DEBUG_ABI` is fail-closed (present-but-not-"1" is a build
error, verified exit 1 — an ABI-profile selector must not let a typo mean "release");
`PURVASM_GC_STRESS` documented in `new_native`'s Panics contract and pinned by
STATS-mirrored subprocess tests (malformed value aborts creation; `"1"` collects with no
overflow). Runtime tests 152/152.
