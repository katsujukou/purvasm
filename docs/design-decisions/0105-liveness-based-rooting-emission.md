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
(In `fib`-class code almost no value stays live beyond a safepoint's *return*; eliding the
root of an operand that dies at the handover itself is the slice-2b read-order refinement —
see §1 — the slice-2a rule keeps such operands rooted.)

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
0011). In `fib`-class code few values are live across a safepoint, yet today every one is
rooted and reloaded.

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

**The crossing rule (pinned; amended 2026-07-24, slice 2a):** at every node whose lowering
may safepoint, the node's own operand *names* join the crossing set together with everything
live after the node — a name stays direct (raw SSA, no slot) only when its **entire live
range touches no safepoint node at all**. This still elides the pure-leaf identity class
(`CAtom`-tail bodies, all-immediate prims) but deliberately roots operands whose read
provably precedes the consuming node's first internal safepoint (e.g. the single-operand
accessor force).

> **Superseded original (the "use-at-call boundary", in the record as Accepted 2026-07-21;
> kept because its failure is the design lesson):** *a value consumed as an argument to a
> safepoint operation, with no use reachable after it, is not live across that safepoint —
> the callee protects its own view of the arguments; crossing requires a use after the
> operation returns.* This is UNSOUND as a crossing rule. The callee does protect its own
> view, but the operand must first *reach* the callee: it is read at its own position inside
> the consuming node's lowering **sequence**, and any internal safepoint of the SAME node
> ordered before that read — a sibling operand's boxed-literal materialisation
> (`pv_new_str`/`pv_new_number`), a sibling's `forceValue` slow path, a foreign-atom closure
> build, the recipe's pre-read machinery — invalidates a direct SSA operand before the
> handover. The slice-2 behavioural gate caught the class deterministically (stress legs;
> for `Data.Show`'s literal-plus-value `append` argv, even the normal legs). The piecemeal
> intra-node exceptions folded during slice-1 review (`SSelf`'s post-argument `%env` read,
> `CUpdate`'s post-`record_set` value reads, `CCase` scrutinees, `LetRec` captures) were
> instances of this one general hazard.

**Slice 2b (owed refinement of this record, NOT a slice-2 completion condition):** recover
the read-order precision soundly — a per-recipe **read-order model** (a prefix-scan sibling
of `operandsMayRoot`'s suffix scan: which operand reads provably precede the recipe's first
internal safepoint), declared beside each recipe on the emission seam and held to the
emitter by the recipe-consistency tests — together with the §3 between-safepoints reload
cache, which consumes the same per-recipe safepoint-position declarations. Both are
refinements behind the same seam; neither opens a new soundness surface.

### 2. The liveness analysis (ANF, per activation) and the two-tier `RootPlan`

A backward pass over the ANF body computes, for every ANF-level rootable definition —
parameter, capture, `Let` binder, case-arm binder (a NAME the ANF tree binds), and the
self-recursion `%env` word — whether its **live range crosses at least one safepoint** (per
the §1 transfer functions and crossing rule). The analysis is
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

**The rooting API is split by root lifetime (pinned; realization amended 2026-07-25)** — one
blanket "no rooting without a frame" rule would outlaw `storeRootGlobal`, which *deliberately*
roots after popping the transient frame to create a permanent init-region handle; and one
blanket exception would give transient roots an escape hatch. The raw rooting emission is
private to ONE module (`Backend.LLVM.Root`), reachable only through:

- `rootLocal` — consumes a `FrameToken`, an opaque witness minted only by `openFrame`:
  possession proves a frame was opened on this emission path, and an activation whose plan
  elided the frame holds none. **Honesty pin:** the token is a possession witness, NOT an
  affine ownership proof — PureScript has no affine types, and emission order is not execution
  order (one activation legitimately emits several pop sites, one per control path), so
  "no root after pop" is a per-path property the type system cannot even state. What IS
  structural: **`popFrame` is private to `Root`** — every pop is fused with what may legally
  follow it: a path terminator (`retWith`/`musttailWith`/`tailcallWith` — pop+`ret`,
  pop+`musttail`+`ret`, `pv_tailcall`+pop+`ret`), the entry teardown (`entryTeardown` —
  pop+`pv_runtime_free`+`ret i32 0`), or the framed-init epilogue below. Pop-then-anything-
  else is not expressible outside the module; `tools/seam-audit.sh` (CI) pins `popFrame` to
  `Root` at the source level too.
- the permanent tier — there is **no capability value and no rooting function in body scope**:
  the frameless `Gfun` init is a FIXED SHAPE with **no body callback at all**
  (`emitGfunInit key arity` — round 4: an unrestricted frameless body could have opened a
  frame the wrapper never pops); a framed init body (`emitInitFnFramed`, `Gcaf`/`Grec`)
  RETURNS its `(globalKey, value)` candidates and the wrapper owns the whole phase order
  `open → body → pop → permanent roots`, so a "permanent" handle inside a transient frame is
  not expressible (it would be popped away — the hazard the phase order exists to kill). The
  framed wrapper is additionally robust to a body that opens and leaks extra frames: its pop
  restores `roots_len` to the WRAPPER's mark, subsuming anything the body opened (pinned by
  the wrapper-mark golden), and `openFrame`'s minting sites are themselves audit-pinned. The
  entry stub is RootPlan-scoped like any activation and has no path to the permanent tier.

This guards both directions: a local root cannot leak into the caller's frame or the init
region (no token → no `rootLocal`; no pop-with-continuation outside `Root`), and a lowering
recipe cannot borrow the permanent tier to dodge the frame discipline (permanent rooting
exists only inside the wrappers, after the body has already returned).

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
  which is retired into the general discipline. *(Re-sequenced 2026-07-25 into slice 2b: the
  cache's invalidation points are per-recipe safepoint positions — the same declarations the
  2b read-order model needs — so it lands with them, behind the seam. Until then every rooted
  use reloads via `abiGet`; the conservative direction.)*
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

### 6. Slice 2b design — the read-order refinement and the reload cache (amendment, Accepted 2026-07-25 — awaiting maintainer)

The §1 crossing rule (2a) roots every operand of every safepoint node; §3's reload discipline
was re-sequenced here. This section is the owed design. It rests on a pinned runtime handover
contract (whose per-entry verification is 2b-0's deliverable, not a settled fact), adds an
emission-time total check, and then takes the two refinements behind the seam.

**6.1 The handover policy (pinned; per-function ownership, NOT blanket self-rooting).** The
runtime's real contract is ADR-0066 §3's **per-function ownership**: each entry protects its
own view of its operands by ONE of the named policies, and the policy is per-entry (even
per-operand), not universal —

1. **consume/snapshot-before-safepoint** — the raw operand is read into host scalars (or
   fully consumed) before anything can allocate (the Number primops snapshot to scalars
   first; the contract is written at `prim.rs:7`);
2. **self-root + reload** — root the raw operands, allocate, read back from the root slots
   (`new_array`/`new_adt` at `gc.rs:701`/`663`, `new_record` composing them,
   `new_closure_raw` for its env word, `new_pap`, `record_set`);
3. **allocation-free handover to callee ownership** — the exact-saturated `apply` fast path
   deliberately does NOT root the borrowed argv: nothing allocates before `call_code`, and
   from then on the args are the CALLEE's per-its-own-plan responsibility (`apply.rs:11`).
   `apply` as a whole is a COMPOSITE of the policies: the generic exact/over-apply paths hand
   `call_args` to the `CodeFn` allocation-free the same way and root only the over-apply
   LEFTOVERS; the by-need path roots args across the force; under-apply delegates to the
   self-rooting `new_pap`;
4. **discard-before-safepoint** — `pv_settle`'s pending-tail path never uses `r` after the
   stashed tail runs (the placeholder-return convention).

What 2b consumes is the derived guarantee: for every classified row, a raw operand handed
over at the call boundary is safe PROVIDED no safepoint sits between its read and that
boundary — the row's own policy covers everything from the boundary onward. **2b-1 is
BLOCKED on the per-entry/per-operand policy INVENTORY** (every sp-relevant row assigned its
policy against the runtime source — `pv_prim_append`, the remaining allocating `pv_prim_*`
rows and `force_if_byneed`'s cell handling still owed) **and on provider-side evidence PER
INVENTORY ROW**: each entry/operand row either has a dedicated forced-GC (`gc_stress`)
runtime test exercising ITS handover window, or is shown to delegate allocation-free to an
already-tested common primitive — a per-class representative alone cannot catch one entry
silently changing its policy later.

**6.2 Emission-time total check first (2b-0, the net).** The seam's sp-classified emitters
bump a monotonic **safepoint epoch** in `Ctx` — and the check rides WITH each value to its
point of USE, not its point of read: a read-time-only assert would have missed the slice-2a
bug itself (read a direct value at epoch E, a sibling operand's materialisation bumps to
E+1, then the stale raw SSA is stored into argv — the read-time check already passed).
Value operands flow as an **opaque token carrying the epoch at which the SSA value LAST
BECAME VALID — never the epoch at read, and never the epoch of an ANF binding event**:
re-stamping at read time would launder a stale direct value into a fresh-looking token, and
so would an ALIAS binding (`let x = y`, a `CAtom` environment binding that mints NO new SSA)
stamped with the current epoch. The token is a small state machine, pinned:

- **`Imm`/`Raw`** (scalar-literal words, counts, label ids, `alloca`/`ptrtoint` words):
  epoch-free, passes verification always;
- **`Direct`/`Fresh { ssa, validEpoch }`**: `validEpoch` is set ONLY by the events that
  actually produce a new valid SSA value — a non-safepoint instruction's result (stamped
  with the current epoch after its operands verified), a function-entry parameter/capture,
  or a safepoint call's result (stamped POST-bump: verify operands → bump → mint). (A
  reload is the `Rooted` arm's event — it refreshes THAT token's cached SSA, not a `Fresh`
  mint.) An alias/environment binding INHERITS the whole token unchanged; `readVar` never
  re-stamps. Stale at consumption = soundness violation → **crash**;
- **`Rooted { handle }`** (with its cached `{ ssa, epoch }`): cache epoch current → reuse
  the SSA; mismatch → the CONSUMPTION-SIDE checked renderer emits the `abiGet` reload just
  before the consuming instruction and re-caches at the current epoch — the renderer is the
  ONE owner of reloads (this arm IS the 6.4 reload cache: a rooted mismatch is a cache miss,
  never a crash; only `Direct`/`Fresh` staleness is a violation). **Phasing:** the
  reload-on-miss behaviour is 2b-2's; during 2b-0/2b-1 (byte-identical / pre-cache
  emission) the reloads stay at their current choreographed sites, so a rooted token must
  arrive FRESH at consumption — a stale one there indicates a choreography bug and crashes
  exactly like `Direct` until 2b-2 retires those sites and hands the renderer ownership.

`envPseudo`/`envDirect` ride the same token path as any direct binding — no separate
raw-string extraction survives. Verification is at CONSUMPTION and must be TOTAL over
guest-value instruction operands: not only the seam call renderers and the `argBuffer`
stores, but `ret` (the fused terminator forms), the rooting stores (`rootLocal` / the
permanent tier), the inline prims' arithmetic/comparison operands, the `CCase`
`switch`/`icmp`/`phi` operands, and the force/settle helpers' fast-path IR — the norm is
that NO guest-value operand position accepts a bare `String` (early lowering to `String`
erases provenance and is what the token type exists to forbid). **The seam's operand ROLES
are total too**: each `RtOp`/`RtPrim` row carries its argument schema (which positions are
guest values vs raw metadata), checked by the renderer against the supplied kinds — a call
site cannot bypass verification by passing a guest value in a raw-metadata position (per-op
smart constructors or a schema match; the 6.1 inventory's operand roles and the renderer are
tied to the same rows). Only the `sp = true` seam/guest-call emissions bump, after their
operands verify (verify-then-bump) — `argBuffer` stores, `ret`s, prims and every other
non-safepoint consumer verify WITHOUT bumping. Any `Direct`/`Fresh` value whose validity is
separated from its use by ANY safepoint crashes deterministically at emission time,
corpus-wide — the recipe-consistency tests then pin shapes, not carry the whole soundness
argument. 2b-0 lands the epoch + tokens
+ use-point verification on the CURRENT 2a emission (where it must hold: direct = whole
range safepoint-free, rooted values are reloaded fresh) with byte-identical output — net
first, as slice 0 was — and the assertion stays ON in production emission (it never changes
the IR; one Int compare per consumption).

**6.3 The activation-tier read-order refinement (2b-1).** The normative contribution of a
safepoint node N to the crossing set is

> `crossingContribution(N) = liveAfter(N) ∪ preReadHazardOperands(N)`

— the `liveAfter` term is UNCHANGED from 2a (a name used after N always crosses: an early
read does not end the obligation of a later use; handover only covers the operand handed
over). What 2b-1 refines is only the second term: 2a takes ALL of N's operand names,
the refinement takes those whose read has a safepoint BEFORE it inside N — per recipe, a
pinned `preRead` declaration, a FORWARD prefix scan (the sibling of `operandsMayRoot`'s
suffix scan) over the recipe's operand order plus its pre-read machinery flag. The rows,
derived from the emitters (each held by the 6.2 use-point check plus a recipe fixture):

| recipe | read order / pre-read machinery | operands that stay direct-eligible |
|---|---|---|
| `CPrim` (forced, no machinery) | reads L→R, each followed by its force | operand i iff no forced-safepoint among 1..i−1 (`\a b -> a + b`: `a` yes, `b` crosses via `a`'s force) |
| `CApp` generic / `SSentinel` / `SClosureEnv` | `evalAtoms` UNFORCED over (f : args) | operand i iff no boxed-literal/foreign materialisation among earlier operands — the all-vars `fib`-class call keeps everything direct |
| `CApp` `SSelf` | args first, `%env` read after | `envPseudo` crosses iff any arg can materialise-safepoint |
| `CApp` `SForceCell` | callee atom read first; args after earlier atoms | prefix rule with unforced atom safepoints (the roots themselves stay lowering-tier) |
| `CCtor` saturated / `CArray` / `CRecord` (SORTED order) | `evalAtoms` unforced | prefix rule |
| `CCtor` unsaturated | builder closure BUILT before args | no operand direct (pre-read machinery safepoint) |
| `CAccessor` / `CIf` cond / guard force | single operand read FIRST | always direct-eligible — the acc-class elision returns |
| `CUpdate` | base read first; vals after `record_set`s | base only (DELIBERATELY conservative: the FIRST update value is also read before the first `record_set` and is refinable later; the initial 2b-1 does not take it) |
| `CLam` captures / `LetRec` captures | ALL capture reads precede the first alloc (`makeClosure` / `buildGrec` read loops) | every capture — the biggest recovered class, resting directly on 6.1's buffer handover |
| `CCase` scrutinees | scrutinee i after 1..i−1's forces | prefix rule (arm-live conservatism and the dtree lowering tier unchanged) |

**6.4 The reload cache (2b-2, the §3 discipline).** The cache IS the 6.2 token machine's
`Rooted` arm: a rooted token whose cached epoch is current reuses its SSA (no `abiGet`); a
mismatch is a cache MISS — the consumption-side checked renderer (the one reload owner)
emits the reload just before the consuming instruction and re-caches at the current epoch.
2b-2 is the step that RETIRES the current read-site reload choreography (`readVar`'s
immediate `abiGet`, the `getCurrent` sites, `evalAtoms`' reload pass) in favour of this
single owner — until then the Rooted arm runs in verification-only mode (6.2 phasing note).
ANF-level control flow (the `CIf`/`CCase`/guard labels Emit itself emits) conservatively
clears the cache (§3's branch-join rule, taken in its simple always-clear form first); the
seam ops' own internal linear blocks (`fchk`/`rchk`/`schk` chains — single entry, single
exit) do NOT clear, their slow-path safepoints already bump the epoch. This is where the
210k-reload census mass lives.

**Epoch/cache control-flow pins.** The epoch is an EMISSION-ORDER monotone, not a path-exact
one: mutually exclusive branches and seam-internal slow paths bump it unconditionally, which
over-approximates safepoint reachability — always the SAFE direction (a stale-looking token
is re-verified/reloaded, never the reverse). Pinned: (a) operand tokens are verified BEFORE
the consuming emission bumps (verify-then-bump); (b) every `sp = true` row and every guest
call bumps EXACTLY once per emission; (c) an ANF-level label always clears the reload cache;
(d) the cache key is the binding/root-handle IDENTITY, never the source name (shadowing must
miss, not alias); (e) `beginFn` resets both epoch and cache; (f) epoch overflow is
fail-closed (crash, not wrap). If the over-approximation ever produces a false-positive
assert on legitimate emission, the remedy is a path-sensitive epoch join — NEVER weakening
the assert.

**6.5 Sequencing and gates.** 2b-0 (the epoch, the epoch-carrying operand tokens and the
use-point verification, byte-identical on the 2a emission; PLUS the 6.1 per-entry/per-operand
policy inventory and the provider-side forced-GC runtime tests — all gates green) → 2b-1
(read-order crossing refinement, UNBLOCKED only by the completed inventory + provider tests;
re-baselines, behavioural gate incl. stress + debug-ABI, fixpoint, census) → 2b-2 (reload
cache; re-baselines, the full battery again, census + bench — the build-time claim is judged
here). Each step is a separate review checkpoint. Not taken (unchanged residual): `CCase`
arm-live conservatism and the dtree lowering tier, provably-not-by-need force refinement,
lazy rooting, the `CUpdate` first-value refinement.

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
   crossing/branch/closure-escape edge classes (consumed-before-safepoint,
   operand-read-at-safepoint (the 2a crossing rule), crossing, branch-join invalidation,
   capture escape, self-recursive
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
  argument — under the 2a rule the realised share is the identity class; the rest returns
  with the 2b read-order refinement), on top of 0079's per-op cheapening. Both modes'
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

#### Progress (2026-07-23): slice 1 — the liveness analysis and `RootPlan` landed

*(Historical note: this entry describes the slice-1 state under the ORIGINAL use-at-call
boundary; slice 2a — §1's amended crossing rule and the slice-2 progress note below —
replaced that rule and re-pinned the affected tests to the 2a expectations.)*

`Backend.LLVM.Liveness` implements §1+§2 as a pure, emission-independent module:

- **§1 transfers**: `primOpSafepoint` (the table's prim rows, one total function),
  `atomCanSafepoint` / `forcedAtomCanSafepoint` (materialisation + force accounting — a forced
  non-immediate operand is a potential safepoint via `pv_force_if_byneed`'s slow path, and a
  boxed literal / foreign reference allocates at `atom` time), and `cexprCanSafepoint` — the
  per-node summary over each recipe's WHOLE lowering sequence, derived from `Emit`'s recipes.
- **§2 `RootPlan`**: `activationPlan cfg body` — ONE backward pass over the ANF (a pure
  `tailRec` spine collection folded tail-first; recursion depth = control nesting only, §2a)
  returning `{ crossing, loweringMayRoot, anySafepoint }`; `needsFrame` = the two-tier union.
  The §1 use-at-call boundary falls out of the pass shape (at each safepoint the names live
  AFTER the node join `crossing`; the node's own operands are consumed by it). Closure opacity:
  `CLam`/`LetRec` bodies are walked for free variables only. The self `%env` word rides a
  pseudo-name (`envPseudo`) used at self-references. Case/dtree stays the conservative
  lowering-tier fallback (`CCase`: everything live at an arm entry crosses; `mayRoot = true`).
- **§4 by construction**: the plan takes no ABI-profile input — the release/debug equality
  contract is the function signature; the unit test documents it (determinism pinned).
- **Tests** (`Test…LLVM.Liveness` + a `Driver` recipe-consistency case; final unit total after
  the review rounds 429/429, e2e 11/11): consumed-before-safepoint (the sidenote-0011 `fib`
  case: the `sub` result does NOT cross), crossing, use-at-call vs live-after-call on the same
  operand, branch-entry crossing under the condition force, capture escape ending the
  obligation, closure opacity (a nested body's crossings do not leak), `%env` crossing iff a
  safepoint precedes a self-call, case-arm binders (crossing inside the arm vs
  consumed-at-call), sequential guard clauses (incl. the post-guard force), `LetRec`
  construction, frame elision (a scalar leaf body: `needsFrame = false`) and the
  `CUpdate`-accumulator contrast, the `evalAtoms` suffix-scan mirror (`operandsMayRoot`, incl.
  the `CRecord` canonical-order counterexample and the 20k-var linearity regression), and the
  §2a scale fixtures (a 50k-binding `Let` spine, 20k-operand array, 5k-arm case — all on the
  default stack). The recipe-consistency test pins the load-bearing direction against the
  CURRENT emitter: declared-false recipes (`CAtom`/`CAccessor`) emit exactly the prologue
  root blocks, `CUpdate` roots beyond them, and the sorted-order `CRecord` roots its
  var-before-allocating-operand (prologue + 1).

An honest limitation recorded for slice 2 / future refinement: because `CPrim` FORCES its
operands and a forced variable is conservatively a potential safepoint, a leaf like
`\a b -> a + b` still gets `needsFrame = true` through `operandsMayRoot` (the second operand's
force can safepoint, so `evalAtoms` may root the first). The frame-elision win at this slice's
precision comes from single-crossing-free bodies and literal-operand shapes; a
"provably-not-by-need" refinement (params/locals that cannot hold cells) is the follow-up
lever, NOT taken here.

Slice-1 review round (2026-07-23), all folded, 428/428 unit + 11/11 e2e: **[P1] `CRecord`
analyses operands in the emitter's canonical order** — the emitter sorts fields by unsigned
label id BEFORE `evalAtoms` (ADR-0069 §1), so a source order placing the allocating operand
first inverted the suffix scan (analysis said no-root while the emitter roots — exactly the
frame-elision ownership violation §2 forbids); `cexprMayRootLocally` now runs the scan over
`sortRecordFields` order, pinned by a deterministic counterexample derived from
`sortRecordFields` itself in BOTH the helper test and a real-IR recipe-consistency case (the
var on the sorted-first label must be rooted: prologue + 1). **[P2] `operandsMayRoot` is one
reverse fold** (the per-index `Array.drop`/`any` re-scan was O(n²), unexercised by
all-immediate fixtures); a 20k-var regression pins linearity. **[P2] guard clauses are
sequential** — a later clause's live-in now feeds an earlier guard's continuation (a false
guard falls through), so a case-arm binder used only after a failing guard crosses that
guard's safepoints; pinned by the two-clause regression (`b` used only in clause 2 crosses
clause 1's call-guard).

Slice-1 review round 2 (2026-07-24): **[P2] the post-guard force is an independent potential
safepoint** — the emitter forces every guard's VALUE before testing it (the dtree
guard-fallthrough), so a guard expression with no internal safepoint still puts a force
between itself and both continuations; the fold now unions `contAfterGuard` at that force,
exempt only when the guard's tail is an immediate literal (`guardResultForced`, a
spine-iterative walk). Pinned by the variable-guard regression (`b` used only in clause 2
crosses clause 1's `q`-guard force, where the clause-1 guard expression itself cannot
safepoint); 429/429. The earlier progress text above is synced to the final counts/fixtures
(P3).

#### Progress (2026-07-24/25): slice 2 — plan-driven emission + the 2a crossing rule + the §1 seam and §2 capability APIs

**Emission integration (2026-07-24).** `EnvEntry.direct`/`bindDirectVar` (a direct binding is
its raw SSA operand — no slot, no store, no reload), plan-driven `emitFunction` (`LBody` →
`activationPlan` drives param/capture/self-`%env` rooting and frame elision; `readVar`'s
direct branch returns the operand as-is; frame pops at every return/`musttail`/trampoline
site — first landed as a `popFrameIfOpen` helper, since fused into `Root`'s terminator forms,
see round 3 below), `Let` sites root iff crossing. First gate run came back RED — normal legs
included — which root-caused to the §1 use-at-call boundary itself (concrete counterexample:
`mod_17` `showCharImpl$d` stores a `pv_apply` result into a second apply's argv AFTER a
sibling literal's `pv_new_str`; a corpus scan found 557 sites of the class). The rule was
replaced by the 2a crossing rule now normative in §1; `Liveness` implements it (the
intra-node special cases deleted as subsumed), and the slice-1 tests were re-pinned to the 2a
expectations. All gates green after the fix: behavioural gate 5 fixtures × both optimiser
modes × stress legs + the debug-ABI leg; unit 429/429, e2e 11/11, examples 10/10; fixpoint
smoke stage-3 ≡ stage-4 AND C3-link ≡ stage-3, 597/597 each; whole-corpus residual-hazard
scan (raw-operand-store-after-safepoint pattern) 0 hits.

**Slice-2 review (maintainer, 2026-07-24) and the closing work (2026-07-25).** The review
confirmed the 2a design and named the unimplemented Accepted pins as slice-2 blockers; all
landed:

- **§1 classified emission seam = `Backend.LLVM.Safepoint`.** A closed `RtOp` descriptor sum
  (one constructor per runtime entry + `RtPrim PrimOp` over `Prim.primSym`) whose row carries
  symbol / ctx-taking / return kind / **safepoint class**; ALL runtime call text is rendered
  by `rtCall` / `rtCallWith` (caller-supplied result temp, for the boot numbering-order
  sites: `SForceCell`, dtree `extract`, `DswitchLen`, the boxed-literal chain) / `rtCallVoid`,
  and guest calls by `guestDirect`/`guestMusttail`. `Liveness` consults the SAME rows
  (`primOpSafepoint = rtSafepoint (RtPrim op)`; the atom/force/per-node arms reference their
  rows), so analysis-vs-lowering drift now requires editing the shared table.
  **Non-bypassability is layered (round-2 review):** `Monad.emit` REJECTS any line containing
  call text at emission time (a raw `call` crashes deterministically — accidental drift
  cannot survive a single test run); the unchecked emitter is the `unsafe`-prefixed
  `unsafeEmitRawCall`, used only by the seam's renderers and the one ctx-birth line; and
  `tools/seam-audit.sh` (a PureScript-CI step) rejects call-text construction in the backend
  sources outside the exact allowlist. The allowlist, precisely: `Safepoint.purs` (the seam),
  the `%ctx = call ptr @pv_runtime_new` ctx-birth line in `Program` (it produces the `%ctx`
  every seam call renders, and returns `ptr`), and the per-gdef `@…$init(ptr %ctx)` call
  lines `emitInitAll` assembles into `pv_init_all`'s body (module-SKELETON chunk text — the
  entry's own call *to* `pv_init_all` goes through `RtInitAll`). Seam tests: a
  declaration-membership sweep of EVERY row against `Abi.declarations`, with the enumeration
  MECHANICAL off the `Generic` reps of `RtOp`/`PrimOp` (a new constructor is swept
  automatically — no manual list to forget; the counts 29 non-prim + 38 prim are pinned as a
  sanity check on the enumerator itself), the classification pins (`pv_tailcall`
  NOT-a-safepoint; `SetArray` in-place), and render goldens (ctx/no-ctx, void, i32 operands,
  `tailcc`/`musttail`).
- **§2 root-lifetime API = `Backend.LLVM.Root`** (round-2 realization; see the amended §2
  text for the honesty pin). Opaque `FrameToken` minted ONLY by `openFrame`; the raw rooting
  emission is private to the module, and `Abi` no longer exports any rooting/frame operation.
  `Ctx.frame`/`frameOpen` (the Boolean check round 1 flagged) are DELETED: the token is
  threaded lexically through `expr`/`cexpr`/`evalAtoms`/`buildGrec` (`Maybe FrameToken`;
  `Nothing` = the plan elided the frame, and `Emit.root` crashing on it remains the
  under-declared-recipe detector). Round 2 closed the two API gaps: (a) **activation pops are
  fused with their terminators** — `retWith`/`musttailWith`/`tailcallWith` are the only pop
  forms the expression emitters can reach, so pop-then-root is not expressible there; bare
  `popFrame` is confined to `Program`'s three init/entry epilogues (audit-held); (b) the
  permanent tier has **no capability value** — `Root.emitInitFn` scopes the permanent-rooting
  function to the `$init` body callback it wraps (`InitCap`/`initRegion` deleted), so the
  entry stub and activation bodies have no expression for it. Affinity itself is documented
  as out of the type system's reach (§2 honesty pin): emission order ≠ execution order, so
  token consumption is not even the right spec — the structural fusion plus the
  behavioural/stress net carry the per-path property.
- **Refactor identity.** Same-corefn comparison over the self-host corpus, content-paired by
  init symbol across module renumbering: round 1 (seam + tokens) **293/293** modules with
  unchanged corefn byte-identical, round 2 (fused pops + callback-scoped permanent tier +
  emit guard) **295/295** — each time the only differing artifacts are the compiler's own
  edited modules and `entry.ll`'s init aggregation. Post-refactor gates: unit **450/450**
  (new `Test…Safepoint` + `Test…Root` modules; the frame/root goldens moved out of
  `Test…Abi`; the fused-terminator goldens; the `emitInitFn`-callback rooting ≡ `rootLocal`
  single-rooting-emission pin), e2e 11/11, behavioural gate full green including stress and
  the debug-ABI leg (which executes `Root`'s `--debug` entry-call paths).
- **Recipe-consistency strengthening (review P2).** Exact counts, not lower bounds: `ident`
  0, `acc` 1, `upd` **3** (prologue + forced-base fold seed + post-`record_set` accumulator —
  losing the accumulator root now fails), sorted-order `CRecord` 2; plus `lowfr`, the
  two-tier discriminator — an activation with an EMPTY crossing set whose frame exists solely
  because `loweringMayRoot` is true (exactly one lowering root, frame pop asserted).
- **§3 conservative-fallback scopes, as landed (the list §3 requires):** `LClosure` wrapper
  bodies, `Gcaf`/`Grec` init bodies and the entry stub stay on the `rootAll` root-on-create
  fallback (`Gfun` bodies and all lifted `LBody` functions are plan-driven); `CCase`/dtree
  remains entirely lowering-tier (slice-1 state); `CPrim`'s forced-operand conservatism (the
  slice-1 honest limitation) stands. These, with the §3 reload cache and the operand
  read-order precision, are slice 2b's residual.

**Round-3 review closure (2026-07-25).** Two P1s survived round 2; both are now structural
(and the §2 realization text above is the round-3 state):

- **Permanent-root phase order is wrapper-owned.** Round 2's callback-scoped rooting function
  was callable before the transient frame's pop (the review's counterexample was in this
  record's own test). Superseded: init bodies RETURN `(globalKey, value)` pairs; `emitInitFn`
  (frameless) and `emitInitFnFramed` (framed — the wrapper owns `open → body → pop →
  permanent roots`) plant them strictly post-body/post-pop. With `entryTeardown` fusing the
  entry epilogue, bare `popFrame` disappeared from `Program` entirely and went
  **private to `Root`**. The framed-init golden pins the phase order as one emission
  (transient root inside the frame, pop, THEN the permanent block).
- **The audit is exact and self-testing, and the module buffer is guarded.** `Monad.emitModule`
  now rejects call-carrying chunks like `emit`; guest `define` blocks go through `emitDefine`
  (an opaque validated `FnBody` between call-checked wrapper texts — call text cannot re-enter
  the module buffer unvalidated), and the one call-carrying skeleton chunk (`pv_init_all`'s
  body) is isolated behind `unsafeEmitRawModule`. `tools/seam-audit.sh` pins, per file ×
  construction shape × exact count: the 4 seam renderers, `Program`'s two constructions (each
  matched against its pinned source shape), every `unsafeEmitRawCall`/`unsafeEmitRawModule`
  use site, and `popFrame`'s confinement to `Root` — and SELF-TESTS on every run (seven
  violation classes injected into a scratch copy must each be rejected, and a clean copy must
  pass, before the real tree is checked). Negative unit tests pin the runtime guards
  themselves (`emit`/`emitModule`/`emitDefine` each crash on call text).

Round-3 verification: unit **455/455** (the forbidden-ordering test replaced by the
phase-order golden; guard negative tests added), audit + self-test green, same-corefn
identity **296/296** unchanged-input modules byte-identical (differs only in the four
re-edited compiler modules), behavioural gate full green again (both modes, stress, debug-ABI
— `OK gc29612` on the debug leg), fixpoint smoke re-run green (C3-link ≡ stage-3 and
stage-3 ≡ stage-4, 601/601 each).

**Round-4 review closure (2026-07-25).** Two P1s on boundary closure (not new liveness
counterexamples — the 2a rule and current output remained sound; the findings were that the
boundary protecting them from future emitter changes was weaker than claimed):

- **The frameless init body existed to be abused.** An unrestricted `Codegen` body given to
  the frameless wrapper could `openFrame` (public) and the wrapper would plant permanent
  roots without ever popping it. Closed by elimination: the `Gfun` init is fully determined
  by `(key, arity)`, so `emitGfunInit` is now a FIXED SHAPE with no body callback (the
  generic frameless `emitInitFn` is deleted). The framed wrapper is robust to body-opened
  frames by pop-to-mark semantics — pinned by a golden asserting the pop stores the
  WRAPPER's mark with a body-leaked frame in between — and `openFrame` minting sites are
  audit-pinned per file × count (Root 4 / Emit 2 / Program 2 / others 0).
- **Remaining seam bypasses closed.** (a) The unsafe emitters are audited across ALL of
  `compiler/src` recursively (outside the backend directory the identifiers must not appear
  at all — an import from a sibling directory no longer passes); (b) free-form `emitGlobal`
  is deleted — the globals buffer is reachable only through the typed `emitStringConstant`
  (name/len/escaped rendered into the fixed `c"…"` constant shape, which cannot express an
  instruction); (c) call detection is line-start-normalised (`containsCallText`: a
  leading-space prepend plus newline/tab starts), so a column-zero `call i64 …` cannot slip
  past — with negative unit tests for exactly that, and new audit self-test classes
  (column-zero call, unsafe-emitter-outside-backend, openFrame drift).

Round-4 verification: unit **458/458**, e2e 11/11, audit + self-tests green (ten in-dir
violation classes + the wide-scan class), same-corefn identity **296/296** unchanged-input
modules byte-identical (differs only in the four re-edited compiler modules), behavioural
gate full green (both modes, stress, debug-ABI `OK gc29612`), fixpoint smoke re-run green
(C3-link ≡ stage-3 and stage-3 ≡ stage-4, 601/601 each).

**Round-5 review closure (2026-07-25).** One P1 remained on the seam: the round-4
`emitStringConstant` still took `name`/`escaped`/`len` as raw parts, so a hostile `name`
(embedded newline + IR text) could have voided the fixed suffix and injected arbitrary IR
through the globals buffer. Closed by derivation: the emitter now takes ONLY the raw guest
`String` and derives everything itself (`freshStrName` for the name,
`Mangle.escapeStringBytes` for length/bytes; the empty string returns `Nothing`, boot's
null-pointer case) — `freshStrName` is un-exported, `Emit.stringConstant` consumes the
returned `{name, len}`. Call-looking TEXT may legitimately survive inside the `c"…"` bytes
(guest data, not an instruction position); what the shape forbids is structural breakout, and
the hostile-string unit test pins exactly that (one well-formed line, derived name prefix,
intact closing quote — a raw newline or quote cannot appear). [P2] `openFrame` joined the
compiler-src-wide audit scan (outside the backend directory the identifier must not appear;
smuggle self-test added). [P3] the entry-stub comment's stale `emitInitFn` reference synced
to the wrapper-owned wording. Round-5 verification: unit **460/460**, e2e 11/11, audit +
self-tests green, same-corefn identity **298/298** unchanged-input modules byte-identical
(differs only in the two code-edited modules, `Monad`/`Emit`), behavioural gate full green
(both modes, stress, debug-ABI `OK gc29612`), fixpoint smoke re-run green (C3-link ≡ stage-3
and stage-3 ≡ stage-4, 601/601 each).

#### Progress (2026-07-26): slice 2b-0 — the token net landed (§6.2) + the §6.1 inventory

**Compiler side (§6.2), byte-identical and corpus-verified.** `Types.Val` (`VRaw` |
`VFresh { ssa, epoch }` — the `Rooted` operand arm is 2b-2's; a rooted BINDING is
`BindingV`'s `RootedV handle`, its read reloads and the reload's result flows as `VFresh`),
`Ctx.spEpoch` + `currentEpoch`/`bumpEpoch` (overflow fail-closed) / `useVal`/`useValAt` /
`mintFresh` in `Monad`; the seam's `RtArg` gained the guest-value arm (`V Val`) and every row
an **argument schema** (`Fixed` roles | `AllVals` for the prim family) checked by the
renderer; `rtCall`/`rtCallWith` return the result TOKEN (verify-then-bump-then-mint);
`Abi.abiSettle`/`forceValue` and the `CIf`/`CCase` phis verify each incoming against its
**per-arm epoch snapshot** (the §6.4 path-sensitive join — needed from day one: the
emission-order epoch is over-approximate across mutually exclusive arms, and a then-arm value
would otherwise false-positive at the merge whenever the else-arm safepoints); `argBuffer`
verifies at each store WITHOUT bumping; alias bindings inherit tokens by construction
(`bindDirectVar` stores the given `Val`; `readVar` of a direct binding hands it back
unre-stamped). Every guest-value operand position in `Emit`/`Program`/`Root`/`Abi`/`Prim` now
consumes tokens; `bumpEpoch` joined the audit (per-file counts + wide scan + self-test
class). Results: unit **470/470** (all emission goldens UNCHANGED; new negative tests — stale
crash, alias-staleness survival, `useValAt` phi form, seam-level stale-after-bump,
fresh-into-own-sp-row, three schema violations), e2e 11/11, **full-corpus emission with
verification ACTIVE: zero false positives, 292/292 unchanged-input modules byte-identical**
(the differing artifacts are exactly the compiler's own edited modules).

**Runtime side (§6.1): the per-entry/per-operand handover inventory, verified against source
and evidenced per row.** Policies: (1) consume/snapshot — the boxed-`Number` family and the
heap primops per the `prim.rs` module contract (inputs to locals/vectors before any
allocation); array `Append` (snapshot-then-delegate, `prim.rs:304`); `pv_tailcall` (owned
`Vec` copy before returning; the stash-to-take no-alloc window is the §1 pin);
`pv_prim_new_array` (immediate length, delegates). (2) self-root+reload — `new_array`
(`gc.rs:701`), `new_adt` (`:663`), `new_closure_raw`'s env (`:737`), `new_pap` (`:753`),
`new_record` (composition, `:1294`), `record_set`/`record_delete`/`record_union`
(`record.rs:102/128/155`), `str_append2` (`gc.rs:1221`, the ADR-0103 root→alloc→re-derive),
`force`'s cell (`byneed.rs:18`). (3) allocation-free handover to callee ownership — `apply`'s
saturated fast path and the generic exact/over-apply `call_args` (leftovers rooted; by-need
arm roots args across the force; under-apply delegates to `new_pap`) — `apply.rs`; guest
direct/`musttail` calls (params rooted per the callee's own plan). (4) discard — `pv_settle`'s
pending-tail placeholder. `pv_run_effect` delegates to `apply`. Provider-side evidence: seven
NEW forced-GC (`gc_stress`) runtime tests — `new_closure` env, `new_record` composition,
the record-op trio, `force`'s cell, the by-need-callee arg arm, over-apply leftovers, the
Number-family snapshot, array-append snapshot-then-delegate — joining the existing
`new_array` GC-firing fixture (ADR-0103) and the slice-0 survivor-integrity test (which
covers `new_adt` + `new_str`); runtime suite **160/160**, the handover tests Miri-clean,
`cargo fmt` clean.

2b-0 gates, all green: behavioural gate full (both optimiser modes × stress legs + the
debug-ABI leg, `OK gc29612`), fixpoint smoke C3-link ≡ stage-3 AND stage-3 ≡ stage-4
(**601/601** each — the whole self-host cycle ran with the token verification ACTIVE in every
emission).

**2b-0 review round 2 (2026-07-26): the mechanically-non-bypassable boundary, closed.** The
round-1 net verified correctly but its API could be walked around (re-wrap a stale SSA as an
immune raw, extract with `valKey` — `Root` did — or hold `useVal`'s bare `String` across a
bump; `useValAt` took caller-chosen epochs; `mintFresh` re-stamped freely); and the §6.1
evidence had per-row gaps. Closed:

- **`Value` kernel (new module):** `Val`'s constructors are PRIVATE; `vImm` REJECTS
  `%`-prefixed text (an SSA register cannot be laundered as epoch-immune); the pure
  `verifyAt`/`mintAt` primitives are audit-caged to `Monad`'s tracked-epoch wrappers, `keyOf`
  to `Types` (which stamps `EnvEntry.key` at bind time — `directTarget` compares keys, never
  extracts operands); `unsafeTestVal`/`unsafeValText` exist for test goldens with a pinned
  zero-use count under `src`.
- **Fused verify+emit renderers:** `emitGuestStore`/`emitGuestRet`/`emitPayloadAshr`/
  `emitLowBitAnd`/`emitGuestSwitch` take the token and emit the consuming instruction in one
  step; phis flow as the opaque **`PhiIncoming`** — `phiArm` verifies AT the arm's end (the
  freeze IS the verification) and `emitPhi` renders+mints, so `useValAt` no longer exists.
  The seam's own arg rendering and the inline prims use the audit-caged
  `unsafeUseVal`/`unsafeMintFresh` bridges (their sequences cannot bump before interpolation).
- **`machineryHandleCall`** (allowlist `RtFrame`/`RtRoot`) returns raw HOST-index handles —
  the reviewer-named alternative that removes `Root`'s need for any token unwrap.
- **Prim schema arity:** `Vals n` via `Prim.primArity` — kind AND count are checked (an
  under/over-applied prim is a seam violation, with negative tests), closing the P2.
- **§6.1 per-row evidence completed:** `sub`/`mul`/`div` Number (independent
  implementations), `pv_prim_record_set`'s DYNAMIC `Str` key + absent-label INSERT branch
  (`record_unsafe_set` → `record_insert`), under-apply's `new_pap` heap captures, and
  `pv_settle`'s discard policy (a stashed tail run under stress, placeholder dropped) —
  runtime **164/164**, the handover tests Miri-clean.
- Audit v4: the kernel identifiers joined the per-file × count cage and the wide scan
  (self-test classes now fifteen + the wide pair), rewritten as a one-pass-per-file counter.
- Verification: unit **472/472** (all emission goldens unchanged; new negatives:
  phi-freeze-after-bump, `vImm` laundering, prim arity), same-corefn identity **293/293**
  unchanged-input modules byte-identical, e2e 11/11, behavioural gate full green (both modes,
  stress, debug-ABI `OK gc29612`), fixpoint smoke green — C3-link ≡ stage-3 and stage-3 ≡
  stage-4, **603/603** each (the count grew by the `Value` module; the whole self-host cycle
  ran with the opaque-token verification active).

**Pinned 2b-2 blocker (round-2 P2): the `musttail` prepared-call split.** Today
`musttailWith` pops and THEN renders the guest call — sound while the choreography hands it
fresh values, but 2b-2's renderer-owned reload would fire on a `Rooted` cache miss AFTER
`popIfOpen`, when the root handle is already dead. 2b-2 MUST introduce the two-phase form
first (an opaque `PreparedCall`: operands verified/reloaded BEFORE the pop; after the pop
only the already-rendered call is emitted) — this joins §6.4's phasing note as an explicit
precondition, not an implementation detail.

#### Progress (2026-07-29): slice 2b-1 — the §6.3 read-order refinement landed

`Liveness` now computes `crossingContribution(N) = liveAfter(N) ∪ preReadHazardOperands(N)`:
`prefixHazards` (the forward fold) + the per-recipe `preReadHazards` table exactly as pinned
in §6.3 — `CIf`'s condition and `CLam`/`LetRec` captures dropped from the crossing term,
`CCase` scrutinees on the forced prefix, `CApp` hazards `envPseudo` iff the callee is the
self name AND some argument materialises, `CUpdate` stays base-only conservative. The §6.2
net did its job: the full corpus (302 objects) emitted under the refined plan with **zero
token-verification failures** — every no-hazard claim held against the real emission — and
the test shifts were exactly the predicted classes (the fib case, use-at-call, the
condition, captures, `%env` with var-args, the `CUpdate` base, `acc` 1→0 / `upd` 3→2 /
`rec` 2→1; the slice-1 goldens needed NO re-baseline). The review round added the §6.3
per-recipe FIXTURE MATRIX — eight positive/negative `activationPlan` pairs covering every
independent table branch (generic-`CApp` allocating-earlier vs reverse, saturated
`CCtor`/`CArray` both directions, unsaturated `CCtor`'s machinery, `CRecord` in CANONICAL
order both directions with a fail-closed label-shape guard, multi-scrutinee forced prefix,
`CUpdate`'s variable value vs base, `LetRec` captures) — so a silently-emptied branch fails
the units directly, not by corpus luck; final unit count **481/481**. Results: e2e 11/11,
behavioural gate full green (both modes, stress, debug-ABI `OK gc29612`), fixpoint smoke
**603/603** both compares. **Census (vs the 2a emission, same corpus): IR 100.0 → 78.2 MB
(−21.8 %), root blocks 84,105 → 53,851 (−36.0 %), ctx reload heads 208,252 → 147,764
(−29.0 %), frames elided 2,593/10,985 (23.6 %, was 18.1 %).** The remaining choreography
mass is 2b-2's (the reload cache, behind the `PreparedCall` blocker).

**2b-0 review round 3 (2026-07-29): the last three net holes.** [P1] `vImm` validated only
the first character, so leading whitespace could smuggle an SSA register (`vImm " %t9"` →
a valid `i64  %t9` operand) — the constructor now validates the WHOLE production grammar
(optional `-`, one or more digits, nothing else; negatives for space/tab/suffix/empty/lone
`-`). [P1] a public `phiArm` freeze did not prove the arm actually ENDED there (a same-arm
bump after the freeze rode through) — `phiArm` is now private and the exported forms fuse the
freeze with the arm boundary it proves: `closeHopArm` (verify + `br hop`/`hop:`/`br merge`,
the CIf/CCase idiom), `armIncomingAt` (freeze fused with the NEXT arm's label — the Abi fast
arm, whose own terminator is already emitted), and `armIncomingClosing` (the last arm's
freeze fused with its closing branch and the merge label; an interloper before `emitPhi`
would make the phi a non-first instruction — invalid LLVM, rejected deterministically).
[P1] the audit globbed only the backend root while the wide scan excluded the whole backend
subtree, so a NESTED `Backend/LLVM/Internal/Evil.purs` escaped both — the audit now walks the
backend RECURSIVELY keyed on relative paths (a nested file named `Monad.purs` does not
inherit the root allowlist), with nested-smuggle self-test classes. Verification: unit
**473/473**, audit + 17 self-test classes green, same-corefn identity **297/297**
unchanged-input modules byte-identical (differs only in the four edited modules —
`Value`/`Monad`/`Abi`/`Emit`), e2e 11/11, behavioural gate full green (both modes, stress,
debug-ABI `OK gc29612`), fixpoint smoke green (C3-link ≡ stage-3 and stage-3 ≡ stage-4,
**603/603** each).

**Measurements (2a, same-corefn vs pre-slice-2 HEAD).** IR 97.8 → 94.6 MB (−3.3 %), root
blocks 88,555 → 83,666 (−5.5 %), ctx reload heads 212,569 → 206,881 (−2.7 %), frames elided
on 2,014/10,961 `$d` functions (18.4 %) — the identity-class share; the choreography bulk is
2b's. Bench llvm leg (fib/quicksort): no regression — every overlapping size faster than the
7/11 baseline (exponents 0.96/1.05; the baseline predates ADR-0103, so the absolute speedup
conflates tracks — the operative claim is strictly-no-regression). Post-seam fixpoint smoke:
re-run green — C3-link ≡ stage-3 AND stage-3 ≡ stage-4, **601/601** artifacts byte-identical
each (300 module `.ll` + `entry.ll` + 300 `.pmi`; the count grew by the two new modules) —
and re-verified green after the round-2 refactor (fused pops + callback-scoped permanent
tier + emit guard), alongside the full behavioural gate (both modes, stress, debug-ABI).
