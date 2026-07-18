# 0104. Retire the boot byte-identity gate: the port objective is achieved; correctness anchors move to the behavioural oracle and the self-host fixpoint

- Status: ~~Proposed~~ **Accepted** _(2026-07-17: accepted by maintainer after five review rounds — §1 freeze scoping incl. the codegen_ml leaf table, §2 Level-2 behavioural gate owed before bridge removal + C2→stage-3→stage-4 fixpoint procedure, §3 bridge decomposition, §4 golden classes + binary-format/`.pmi`-content forward pins, §5 seam pins)_
- Date: 2026-07-16

## Context

[ADR-0082](0082-native-codegen-port-to-level-2.md) ported the native (LLVM) code generator to the
Level-2 compiler under a deliberately strong verification instrument: the Level-2 backend had to
emit **byte-identical `.ll`** against frozen boot's `--no-opt` reference, turning "was the
transcription faithful?" into a bit-for-bit diff. That instrument did its job and the objective is
**achieved and recorded**: the Level-2 compiler self-compiles with every artifact byte-identical to
boot's (488/488 `.ll` objects; earlier, the L1↔L3 `.pmo`/`.pmi` differential at 227/227), a state
that has been re-verified as a standing gate through the subsequent runtime/codegen arcs
([0076](0076-direct-known-arity-calls-musttail.md)–[0079](0079-ctx-header-abi-inline-rooting-fast-paths.md)
Progress notes).

Since then the roles have inverted. boot is **frozen** (post-0079 policy): it will never track a
Level-2 improvement. A byte-identity gate against a frozen reference is therefore no longer a
verification instrument — it is a **ratchet that pins the Level-2 backend to boot's 2026-07 codegen
choices forever** (conservative root-on-create emission, emission order, fresh-name order, the
whole-program `DictElim` bridge shape). Two of the walls the current performance work must move are
sitting directly on that ratchet:

- **Liveness-based rooting** (the [sidenote 0011](sidenotes/0011-v1-gap-anatomy-post-0079.md)
  lever 1; the measured `.ll`-size wall — ~70 % of emitted IR text is rooting choreography,
  96 MiB / 18 min of `clang` on the compiler closure) is *impossible* under byte-identity: boot
  emits conservative rooting, boot is frozen, so Level-2 may never emit anything else.
- The `--no-opt` **boot-parity `DictElim` bridge** ([0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md)
  Addendum — self-described as *transitional, scheduled for removal when the port completes*)
  needs whole-program machinery facts, which the current implementation bundles — together with
  `gkeys`/surface — into one **eager whole-closure context** derived before the per-module fold
  (with an accidental duplicated lowering), contradicting the per-module, summary-threaded build
  shape [0085](0085-native-build-orchestration-inmemory-summary-env.md)/[0087](0087-backend-neutral-build-driver-compileraction.md)
  intended.

There is in-repo precedent for exactly this move:
[0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md) already released the **VM
backend** from boot byte-identity when the optimiser seam made boot's bytecode shape obsolete. This
record is the same release for the LLVM backend — plus the explicit statement of what replaces the
retired gate.

Maintainer decision (2026-07-16), which this record captures: *boot byte-identity is considered
achieved; boot is fully frozen; Level-2+ backends are driven by functional improvement, with
correctness anchored on behavioural invariance against the execution oracle and on level-to-level
byte identity.* (Originally phrased as "the CESK oracle"; the 2026-07-18 §2 amendment pins the
oracle as the frozen boot VM + fixture-owned expected traces and retires the CESK-leg
requirement.)

## Decision

### 1. The ADR-0082 byte-identity objective is declared achieved and the gate is retired

No Level-2 output — `.ll`, `.pmo`, `.pmi` — is held to byte-equality against boot from this record
on. boot remains **fully frozen** (now including: never re-baselined to match Level-2), stays the
**bootstrap seed** (L1 still cold-starts the toolchain: `boot` builds the Level-2 compiler), and
remains available at its pinned history as an archival reference — the retirement removes the
standing *gate*, not the artifact.

**"Fully frozen", scoped precisely** (review round 1 — the wording collided with
[ADR-0103](0103-native-string-substrate-zero-copy-slices.md)'s parity leaves; round 2 — corrected
against 0103's *implemented* shape): the absolute freeze covers boot's **codegen shape and
optimiser** — lowering, emission order, naming, instruction selection are never extended, never
re-baselined. Boot's **host-FFI leaf tables** stay open to the *existing* Level-2+-blocking
exception class (the post-0079 freeze policy; precedent: `foreign-sig-dump`), which is exactly
where ADR-0103's bulk string parity leaves landed. Implementation showed those tables are **two**
boot surfaces, not one:

- `Ffi.host` (the CESK/VM differential leaves) — interpreter-side; changes no emitted artifact
  byte.
- `codegen_ml`'s `foreign` table — part of the runtime-support prelude the OCaml backend emits
  **verbatim** into generated programs, so a leaf addition *does* change the OCaml backend's
  emitted bytes. That backend was never under an identity gate (the ADR-0082 gate is `.ll`-only),
  and a new arm in a runtime leaf *table* is data, not a codegen-shape change — the freeze's
  purpose (a stable transcription reference) is untouched.

The LLVM path needs no boot change at all for new leaves (they resolve as link-time `pvf_`
symbols with arity from `Ffi.foreign_arity`), so the identity gate — while it still stands — is
never in conflict. Scoped rule, stated once: **a leaf-table addition mirrored across all
providers is within the freeze; anything that changes what code boot *generates for guest
terms* is not.**

### 2. Level-2+ correctness anchors

- **Behavioural invariance against the execution oracle** — values + `Effect` order, the
  discipline of [0023](0023-effect-runtime-oracle.md)/[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §7.
  The VM and the boot-built binaries stay in the differential as long as boot remains in the build
  path — behavioural agreement never required byte agreement.

  **The oracle, pinned (amendment 2026-07-18, maintainer decision):** for the current
  *deterministic, sequential* language, the Level-2 behavioural gate's oracle is the **frozen boot
  VM plus fixture-owned expected stdout** (the expected traces reduce the risk of Level-2 and the
  VM sharing one error). A **direct CESK leg is NOT required**: the frozen CESK runner is an
  *implementation* of the current sequential semantics, not a permanent normative semantics —
  Level-2 has no CESK runtime of its own, so requiring that leg would quietly re-anchor
  correctness to frozen boot, the very ratchet this record retires. The scenario that would
  genuinely need a reference semantics — **concurrent primitives** — cannot reuse the current
  runner anyway: with concurrency a program's meaning is a *set* of admissible observable traces,
  not one output, so single-run stdout equality stops being meaningful. Any future concurrency
  work therefore starts with its own ADR defining the verification contract — what is observable,
  whether the scheduler is part of the semantics, trace refinement
  (`Traces(impl) ⊆ Traces(spec)`) vs deterministic-scheduler differentials vs happens-before
  normalisation vs linearizability, and the safety/liveness split — **before** selecting or
  extending a reference machine. Deciding for the existing CESK first would let an implementation
  dictate the semantics backwards.

  **The owed Level-2 native behavioural gate (review round 2 — the existing nets do NOT yet cover
  a Level-2 emitter bug):** today's forced-GC fixtures live in boot's e2e (they verify *boot's*
  emitter) and in the runtime's unit tests; `tools/native-run-diff.sh` runs a handful of
  Prim-only pure-value fixtures, `--no-opt` only, comparing boot-native vs Level-2-native vs a
  fixed expected value — it covers no `Effect` order or dictionary dispatch, forces no
  collection, and the
  Level-2 native harness is not CI-wired (`examples-ci` excludes Level-2). So "rooting-emission
  bugs are caught behaviourally" is **not yet true of Level-2-generated binaries**. Pinned:
  **before the FIRST intentional emission divergence — which is bridge removal, not liveness
  rooting** (review round 3: deleting `dictElimExpr` changes the emitted ANF/LLVM under
  `--no-opt` and productionises the dynamic-dictionary-dispatch path native `--no-opt` has never
  seriously exercised), **the full Level-2 behavioural gate lands as its own step in the §5
  sequence, after the identity-preserving restoration and before bridge removal**: it compiles
  fixtures with Level-2, runs the resulting native binaries under a deliberately small heap
  (`PURVASM_HEAP_WORDS` — the existing knob, so the mechanism is unambiguous), and compares
  values + `Effect` order against the oracle above (the boot-VM leg AND the fixture's own
  expected stdout) — in BOTH `--opt` and `--no-opt`, with `PURVASM_STATS` asserting that
  collections actually fired (a run where no collection happened is not GC coverage), and with
  dictionary-dispatch-heavy fixtures included (the very path bridge removal turns on). The gate
  is a **standing** net, so it must be **CI-wired** (its absence from CI is one of the defects
  this paragraph lists) no later than the change that removes the bridge. **Bridge removal and
  everything after it is blocked on this gate**, not merely on this record's acceptance.
- **Level-to-level byte identity (the self-host fixpoint)** — pinned procedure (review round 2:
  "stage N vs stage N+1" was ambiguous; comparing the boot-built compiler's *own* `.ll` against
  that compiler's *output* is the wrong reading — those legitimately diverge at the first
  intentional codegen change):

  1. boot builds the Level-2 compiler → the stage-2 binary `C2` (bootstrap seed; **its own bytes
     are not part of the comparison**).
  2. `C2` compiles the pinned CoreFn closure → **stage-3 artifacts**, from which the stage-3
     binary `C3` is linked.
  3. `C3` compiles the *same* pinned closure → **stage-4 artifacts**.
  4. The gate: **stage-3 artifacts ≡ stage-4 artifacts, byte-for-byte.**

  Comparison set, pinned: every per-module `.ll`, `entry.ll`, and every `.pmi` (plus the `.pmo`s
  whenever the VM backend is part of the run); inputs pinned to the identical CoreFn `output/`
  and identical staged ulib; the two compiler runs use the same mode and the same heap/debug
  settings. Mode profiles, named (review round 3 — "`--no-opt` is the cheap default" collided
  with §3, which makes `--no-opt` binaries slower post-bridge and `--opt` the daily path;
  emission may be cheaper under `--no-opt` while `C3`'s *compile* leg is faster under `--opt`,
  and the determinism worth pinning is the recommended self-host path's):
  - **smoke profile** — `--no-opt`;
  - **required milestone profile** — `--opt` (the recommended self-host path; also the leg that
    pins the optimiser's determinism);
  - which one the script *defaults* to is decided by measurement after the script exists, not
    here.

  The **linked binaries are excluded** (linker determinism is not Level-2's contract; `C3` is
  already transitively pinned by the artifact identity over the compiler closure). This is the identity that survives boot's retirement, promoted to a
  **checked-in script + standing gate** (`tools/selfhost-fixpoint-diff.sh`, owed with this
  record's implementation). Until the L3 build is fast enough for per-PR CI (the very track this
  record unblocks), it runs as a milestone gate (release/ADR-landing checkpoints), with the
  per-PR net carried by the unit goldens and the behavioural e2e.

  **Acceptance ordering (review round 1):** this record is sequenced **after
  [ADR-0103](0103-native-string-substrate-zero-copy-slices.md)** — the fixpoint gate's practical
  routine (a whole-closure L3 run) is only affordable once the string-substrate quadratic falls,
  so the gate's replacement must not be declared before its successor is exercisable. (The
  independent §5-1 per-module restoration is identity-preserving and may land in any order.)

### 3. `--no-opt` is redefined; the boot-parity `DictElim` bridge is removed

`--no-opt` becomes "the optimiser-free reference lowering" (the bisection aid separating codegen
bugs from optimiser bugs) — **not** "boot's bytes". The backend-private bridge
([0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) Addendum) is deleted, which
aligns native `--no-opt` semantics with the VM's (dictionaries stay dynamically dispatched) and
**removes the machinery channel that the §5 step-1 restoration has already made unnecessary**
(review round 4 — the bridge never *required* the eager whole-closure context, only machinery
facts; step 1 supplies those from the keyed, import-closure-projected context, so removal is the
deletion of the last consumer, not the enabler of the restoration).

**What "the bridge" is, decomposed (review round 2 — deletion must not take required lowering
with it):** `nativeByteIdentityBridgeDictElim` (`Backend/LLVM/Driver`) bundles three passes:

1. `resolveNativeForeigns` — native leaf resolution (`AtomVar → AtomForeign`): **required**
   native lowering, independent of boot parity;
2. `resolveLitBuiltins` — `unit`/`undefined` literalisation: likewise **required**;
3. `dictElimExpr` over the whole-program `machinery`/`gkeys` — the boot-parity piece, and it
   alone.

Only (3) is removed, together with `LlvmContext.machinery` and the whole-program machinery
derivation that feeds it. (1)+(2) survive as an explicitly named backend-required lowering
(shape: `nativeRequiredLowering leaves = resolveLitBuiltins <<< resolveNativeForeigns leaves`),
applied in **both** modes, and `synthForeignGdefs` remains — intrinsic foreigns still need their
gdef materialisation.

Sequencing caveat, pinned: bridge removal makes `--no-opt` binaries slower (dispatch stays
dynamic), so it lands **together with or after** the `--opt` self-host path being the recommended
default — i.e. coordinated with the ADR-0089 sticky-quarantine addendum landing, so day-to-day
self-hosting is `--opt` before `--no-opt` loses its accidental speed. **Status note (review
round 2): already satisfied** — the 0089 addendum has landed and the CLI's default path is
`--opt` (`opt: not opts.noOpt`), so this caveat gates nothing further.

### 4. Byte-identity guard inventory and migration

The repository's identity guards, classified (the 2026-07-16 sweep):

**(A) Boot-anchored byte guards — retired/retargeted by this record:**

| guard | disposition |
|---|---|
| `tools/llvm-diff.sh` (boot `.ll` vs L2 `.ll`, the 0082 harness) | retired from standing use; kept in-tree with a header note pointing here (revivable against pinned history) |
| `compiler/test/fixtures/slice1/{mod_0,entry}.ll` + `Test.Unit….LLVM.Driver` "byte-identical … objects" | goldens become **L2-owned**: re-baselined (same PR) on the first intentional codegen change, with the behavioural differential green as the license to re-baseline |
| `Test.E2E.Purvasm.Compiler` `refPmoDiaA`/`refPmiDiaA` (boot's exact `.pmo`/`.pmi` bytes) | same L2-owned golden policy (they already are the VM-side goldens in spirit, [0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md)) |
| `Test.Unit….Compiler.Compile` (`refPmo`/`refPmi`/`refRecPmo`… — "== boot's `.pvmo`/`.pvmi`" fixtures; review round 2, missed by the sweep) | wording de-anchored from boot; **composite** — instruction/lowering content is emission-class, serialisation (schema/opcode encoding/`version`/hash) is format-class (rule below) |
| `Test.Unit….Bytecode.Artifact` (hand-built serialisation fixtures incl. `version` and the MD5 interface hash) | **format-class golden** (below) — NOT freely re-baselineable |
| `Test.Unit….Compiler.Link` (`refAppPvm`/`refDce` — linked `.pvm` image bytes) | boot wording dropped; **composite** — same split as `Compiler.Compile` (image schema is format-class, the gdef instruction content is emission-class) |
| `CLI.Compile`'s doc contract ("write the artifacts (byte-identical to boot)") | prose retargeted to the §2 anchors |
| the manual boot-vs-L2 self-compile artifact diff (the "488/488" procedure) | superseded by the §2 stage-fixpoint script |
| the `--no-opt` bridge byte-parity obligation ([0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) Addendum) | removed with the bridge (§3) |

**(B) Boot-involved but behavioural — kept unchanged (they never depended on byte equality):**
`tools/native-run-diff.sh` (run-value agreement), `tools/ffi-e2e.sh`,
`tools/foreign-sigs-diff.sh` (FSR shape agreement vs boot's registry, ADR-0080 §2),
`examples-ci` (4-backend output equality), the benchmark harness's cross-leg output self-check,
and boot's own e2e/unit suites (boot-internal; frozen boot keeps passing them untouched).

**(C) Level-2-internal identities — kept, none reference boot:**
the `.pmi` mode-stability guard (`--opt` vs `--no-opt` `ExportKind`/hash,
[0099](0099-generalized-effect-reflection-cperform.md) §4a), the `opt-effect` size-ratio gates and
self-compile size leg ([0089](0089-nbe-general-inliner.md)), the LLVM-vs-bytecode interface
consistency test, and the inline expected-IR unit fixtures in `Emit`/`Program`/`Abi`/`Prim` tests
(L2-owned by definition; updated with the intentional change that moves them).

**Golden classes and their re-baseline rules (review round 2 — one rule was too coarse; an
emission golden and an ABI golden must not share a licence):**

- **Emission-shape goldens** — expected IR/lowering text whose exact bytes are a *choice* of the
  Level-2 emitter: `slice1/{mod_0,entry}.ll`, the inline expected-IR fixtures in
  `Emit`/`Program`/`Abi`/`Prim` tests, `refPmoDiaA`'s instruction sequences. Re-baselined only in
  the same change that intentionally alters emission, with the behavioural differential
  (forced-GC fixtures included, once the §2 Level-2 gate exists) green as the licence. A golden
  diff without an intentional emission change is a regression, full stop.
- **ABI / persistent-format goldens** — bytes that are a *contract* with other providers,
  consumers, or on-disk history: the symbol mangling (`Backend.LLVM.Mangle`, `pvf_` names), the
  artifact `version`/JSON schema/opcode encoding/MD5 interface-hash discipline
  (`Bytecode.Artifact`, `Compiler.Compile`, `Compiler.Link` image shape), the `.pmi`
  mode-stability rule. **A green behavioural run is NOT a licence to change these**: they move
  only with a format/ABI ADR (or an explicit version bump) and the coordinated update of every
  producer and consumer.

  **Composite goldens split by content, not by fixture (review round 3):** a `.pmo`/`.pvm`
  golden string is BOTH things at once — the *instruction sequences* inside it are
  emission-shape (a codegen improvement may change them under the emission rule above, same
  change + behavioural green), while its *serialisation* (JSON schema, opcode encoding,
  `version`, the hash discipline) is format-class. Re-baselining a `.pmo` golden because the
  emitted instructions changed is routine; it does NOT require a format ADR — that is required
  only when the schema/encoding/version themselves move.
- **Formerly boot-anchored, now L2-owned** — fixtures whose *prose* says "== boot's bytes"
  (`Compile`/`Artifact`/`Link`/CLI doc): the wording is updated to name the pinned format or the
  §2 anchors; each fixture then lives under whichever class its content belongs to above.

This keeps the cheap textual net that byte-identity used to provide, aimed at Level-2's *own*
previous output instead of boot's — without laundering ABI changes through it.

**Anticipated first format-class change: the binary `.pmo`/`.pmi` format.** The long-planned
binaryisation of the artifacts (JSON serialisation was a deliberate stopgap,
[0049](0049-pmo-artifact-construction.md)/[0051](0051-flatten-json-serialization.md) lineage) is
exactly the kind of change the format rule exists for, and this record makes it *possible*: boot
is frozen on JSON, so under the retired gate the formats may now diverge — boot's artifacts stay
frozen with boot (bootstrap-seed internal), while Level-2's on-disk format moves by its own ADR
with a `version` bump and every Level-2 producer/consumer of `.pmo`/`.pmi` (compiler, disk
readers/linker, tooling — NOT the VM runner, which reads `.pvm`, untouched here; it joins the
update set only if a future [0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md)
phase (b) ADR moves `.pvm` itself) updated together. Two pins travel with it: the encoder must stay **byte-deterministic** (the §2
fixpoint compares whatever bytes the format produces — the gate is format-agnostic, but only for
a deterministic encoder), and the format-class goldens are re-baselined **in that ADR's change**,
not before.

The same format ADR also extends the `.pmi` **content**, so a dependent's per-module compilation
gathers its facts by reading its deps' `.pmi`s instead of re-deriving them from sources. Pins
(review round 5 — the first draft of this paragraph re-collapsed the 0084/0085 phase and
lifetime separations):

- **Two sections, not one** — the phases differ and must not be merged:

  ```text
  interfaceFacts    -- always present; derived from the RAW pre-optimisation ANF;
                    -- mode-independent (export surface, exported foreign shapes,
                    -- the §5 step-1 backend facts)
  optimizerSummary  -- optional; post-optimisation ANF; mode-keyed
                    -- (the 0084 channel as accepted: inline candidates, effect
                    --  facts, optimiser dict machinery)
  ```

  [0084](0084-binary-pmo-pmi-and-cross-module-summary.md)'s summary stays what it is (post-opt,
  `--opt`-only); the format ADR adds the *stable* section beside it rather than generalising the
  unstable one. (Post-bridge there is no consumer that needs dict machinery persisted for the
  `--no-opt` backend at all.)
- **Each `.pmi` persists its module's OWN contribution, never the accumulated closure**:
  `pmi[M] = facts contributed by M`; the build holds `context = Map ModuleName ModuleFacts`; a
  consumer sees `projection(context, importClosure(M))`. `M`'s facts may be *computed* under its
  deps' facts, but what is *stored* is `M`'s contribution — storing closures would give quadratic
  size, needless cascades, sibling leakage, and self-pollution (0084's own pin).
- **Cold/warm reuse requires the invalidation contract in the same format ADR** — today's `.pmi`
  `hash` covers only the export surface, and 0084 is safe only because `--opt` always recompiles.
  Reading persisted facts across builds needs, at minimum: a fingerprint over the module's own
  facts; the consumed **direct-dependency fingerprints** (giving the transitive Merkle cascade);
  a cache key over mode + compiler/schema version + target/ABI/options; and rejection on schema
  skew or stale deps. **Until that contract lands, persisted facts serve fresh builds'
  serialisation only — no cross-build reuse.**
- **Scope boundary vs the VM (`.pvm`)**: this note covers the `.pmo`/`.pmi` **on-disk encoding
  and `.pmi` content** only. The `.pvm` image stays boot-`purvm`-runnable, and any change to the
  `.pmo`/`.pvm` *semantic object shape* (the `decls + init` restructuring) waits on
  [0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md) phase (b) / the owned
  VM. Encoding-only means the update set is the compiler, the disk readers/linker, and tooling —
  the VM runner does not read `.pmo`.

### 5. First consumers (sequenced, each its own change)

1. **Per-module build restoration** (identity-preserving even before §3): derive backend facts
   dependency-directed, eliminate the duplicated `declsOfModule` lowering — restores the
   [0085](0085-native-build-orchestration-inmemory-summary-env.md) shape. Seam pins (review
   round 2 — today's `Backend.context :: Array ContextModule -> c` is an eager whole-closure
   API and the pins keep the restoration from re-inventing it):
   - the backend context starts **empty** and is **extended in dependency order** through the
     build fold (the `BuildEnv` summary-threading shape);
   - context facts are derived from the **raw pre-optimisation `AnfModule`**;
   - that same `AnfModule` value is what the optimiser receives — `declsOfModule` runs **once**
     per module;
   - facts are held **keyed by module**, and a module's lowering consults the **projection over
     its import closure** — own facts + transitive dependencies' facts, nothing else. (Review
     round 3: a single accumulated context extended in dependency order is a monotone superset
     of the processed prefix — on a diamond it also contains already-processed *sibling* facts,
     so the projection, not the fold order, is what enforces "dependencies only"; the keyed form
     is the one faithful to separate compilation.)
   - `lowerEntry` consults the **final accumulated** context (the whole-closure projection is
     correct there by definition — the entry imports the world).

   (This slice may alternatively be re-homed to its own ADR; the five pins carry over verbatim.)
2. **The Level-2 native behavioural gate** (§2) — lands here, between the identity-preserving
   restoration and the first divergence: small-heap (`PURVASM_HEAP_WORDS`) forced-GC runs of
   Level-2-built binaries vs the execution oracle (the frozen boot VM + fixture-owned expected
   traces, per the §2 amendment), both modes, dictionary-dispatch fixtures included,
   `PURVASM_STATS`-verified collections.
3. **Bridge removal** (§3) — **the first intentional emission divergence** (review round 3: the
   `--no-opt` ANF/LLVM changes and dynamic dictionary dispatch goes production on native), so it
   is blocked on the step-2 gate; the §3 `--opt`-default caveat is already satisfied.
4. **Liveness-based rooting** — the second intentional divergence, gated by the §2 anchors + the
   emission-shape re-baseline rule (§4), with the step-2 gate's forced-GC legs as the specific
   net for rooting. Its own ADR (the codegen/code-size track).

## Consequences

- The Level-2 backend can improve again: liveness rooting (the `.ll`-size/`clang`-time/emit-volume
  lever), future emission-order and direct-call improvements — all previously frozen by proxy.
- The native build driver can drop its whole-program pre-pass, restoring per-module,
  leaves-first compilation with only interface/summary facts flowing forward.
- `--no-opt` loses its boot-anchored meaning; bisection now leans on the behavioural
  differential against the execution oracle (the VM leg + fixture-owned traces),
  `PURVASM_TRACE`, and the stage fixpoint. The bridge's deletion also deletes the
  last whole-program byte-identity machinery from the seam.
- The stage-fixpoint gate is only as good as the L3 build's viability — acceptable circularity:
  the gate runs at milestones until the build-performance track (which this record unblocks) makes
  it CI-cheap, while per-PR coverage stays on goldens + behavioural e2e.
- boot's value narrows honestly to bootstrap seed + archive; no future work is spent keeping it
  representative.

## Alternatives considered

- **Keep the gate and unfreeze boot** (port each Level-2 improvement back). Rejected: doubles every
  codegen change across two languages forever, and contradicts the freeze that made boot a stable
  reference in the first place — the gate's value premise is already gone.
- **Keep byte-identity for `--no-opt` only, improve codegen under `--opt` only.** Rejected: rooting
  emission is mode-independent codegen (forking it per mode doubles the emitter and halves the
  benefit — the measured giant `.ll` builds are `--no-opt`), and the bridge's whole-program pass
  would survive precisely where the build-shape problem lives.
- **Normalized/"identity modulo known transforms" diffing** (compare boot vs L2 after erasing
  rooting/naming differences). Rejected: the normalizer becomes an unverified second codegen; weak
  guarantee at real tooling cost.
- **Retire the gate without a replacement identity.** Rejected: the stage fixpoint is nearly free
  mechanically (it is the existing 488/488 procedure re-anchored) and is the classic self-hosting
  correctness anchor; behavioural tests alone would drop the cheap whole-closure textual net.

#### Progress (2026-07-17): §5-1 per-module restoration implemented; identity verified

The step-1 restoration landed on the pinned seam: `Backend` moved from
`context :: Array ContextModule -> c` to `emptyContext` / `mergeContext` / `moduleContext` — an
**idempotent commutative monoid on valid fact sets** (the load-bearing law, pinned in the seam
docs after review: the driver merges *overlapping* projections on diamonds, so a merely-disjoint
merge contract would be wrong) — with the driver accumulating module-keyed contributions through
the fold, memoising each module's import-closure projection
(`visible(M) = merge(direct deps' visibles) ∪ contrib(M)`), and handing `lowerEntry` the final
accumulation. `declsOfModule` runs once per module (the duplicated lowering is gone). A
context-aware mock fixture pins the projection directly on the diamond (`B` sees `C` but never
the already-processed sibling `A`; the entry sees the world) — coverage the byte-identity gates
cannot provide, since well-typed bodies never reference sibling keys.

Two results, recorded separately:

- **§5-1 correctness** — same-input artifact identity, Node-hosted before/after compilers over
  the identical CoreFn corpus + staged ulib: `--no-opt` 298/298 `.ll` + 297/297 `.pmi`
  byte-identical, `--opt` likewise 298/298 + 297/297; unit 404 / e2e 11 green. Method note for
  the future fixpoint script: the corpus IS the compiler, so a naive before/after run compiles
  *different* corefn (the changed modules) and self-invalidates — hold the input fixed and vary
  only the compiler build (the before-leg here: sources restored via `git show HEAD:` into a
  scratch tree, built with shared package caches).
- **Build performance (pre-existing debt, not a §5-1 regression)** — the *before*-leg native L3
  `--opt` self-build was aborted after > 48 min at 100 % CPU inside one tail module (mod_282,
  ~`LLVM.Emit`); completion time unknown. The comparison was moved to Node-hosted legs. This is a
  measured data point for the §2 fixpoint profiles (smoke = `--no-opt`; the `--opt` milestone
  leg's native flavour is currently impractical) and for the build-performance track this record
  unblocks.

#### Progress (2026-07-18): §5 step 2 — the Level-2 native behavioural gate landed, CI-wired

`tools/l2-native-behavioural.sh` + `test-fixtures/l2-behavioural/` (a workspace member, so the
stress fixtures stay out of `examples/` per the repo's examples policy) + the
`l2-behavioural-ci.yaml` workflow (trigger set includes `compiler/`/`cli/` — a Level-2 emitter
change is what the gate exists to catch; required status: `l2-behavioural-ci-gate`).

The gate implements the §2 pin as amended (2026-07-18): oracle = **frozen boot VM + fixture-owned
expected stdout** (the expected traces were generated from the **JS backend** — a third,
purvasm-independent source — and are committed as fixture-owned truth); a direct CESK leg is not
required. Four fixtures (GC churn / `Effect` order / dictionary-dispatch-heavy incl. a local
class + 500-call bulk / mixed records-closures-effects), each run in `--opt` AND `--no-opt`
under `PURVASM_HEAP_WORDS=65536` with a schema-checked `purvasm-stats:v1` line and
`gc_collections >= 1` asserted per run. Review hardening: the churn's allocations are
data-dependent on the loop variables and land in printed checksums (constant-hoisting cannot
vacate the GC coverage), and the no-collection case FAILS — demonstrated live when the first
fixture sizing under-allocated and the gate reported `NO-GC(0)` instead of passing. Result:
8/8 legs behaviour-identical to the oracle with 2–8 collections each. **The §5 step-3 (bridge
removal) blocker is released once this lands with review.**

#### Progress (2026-07-18): §5 step 3 — the boot-parity `DictElim` bridge removed

The §3 decomposition executed exactly: `dictElimExpr` over the whole-program machinery is gone
from `Backend/LLVM/Driver`, together with `LlvmContext.machinery`, its `moduleContext`
derivation (`machineryOf` over deps) and `mergeContext` merge; `noForeignLift` — the bridge's
lift policy, with no other production consumer — went with it (its DictElim unit test retired;
the `AtomForeign`-always-liftable property test was re-anchored on `intrinsicLift`, being a
property of `liftable`, not of the bridge policy). The survivors are exactly the §3 pins:
`nativeRequiredLowering leaves = resolveLitBuiltins <<< resolveNativeForeigns leaves`, applied
in BOTH modes, and `synthForeignGdefs` untouched. The `ForeignLift` parameter stays parametric
(a call-site property); the seam's `moduleContext deps` projection stays in the contract (the
LLVM backend simply no longer uses it). Prose de-anchored in the same change: seam docs
(`Compiler`/`Optimizer`), CLI `--no-opt`/`--emit-llvm` help, and the slice1 golden test's
header (now explicitly the §4 L2-owned-golden contract).

**The first intentional emission divergence, measured** (same-corefn, pre/post compilers per
the §5-1 method note): `--no-opt` 47/104 objects diverge on the dict-dispatch fixture closure —
the diff shape is exactly dispatch un-collapsing (direct `concatString`/`showIntImpl`/
`Purvasm.Int.add` references replaced by `semigroupString`/`semiringInt` dictionary roots and
`append$d`/`add$d`/`foldl$d` accessor calls); `--opt` is **0/104** — the bridge was already a
no-op behind the real optimiser pass, so the daily (`--opt`) path's emission is byte-unchanged:
no *generated-code* performance regression is possible on the measured closure (compile-time
behaviour — e.g. the dropped machinery derivation — is a separate axis this diff does not
speak to). Gates: behavioural gate green post-removal (8/8 legs ≡ oracle,
gc 2–8 — dynamic `--no-opt` dispatch now production and covered by the very fixture class the
gate was built for), unit green (slice1 goldens needed **no** re-baseline: the fixture has no
dispatch), e2e 11/11 (VM-side goldens unaffected — the VM never had a bridge), examples sweep
10/10.

Review round (P1): the removal itself is now pinned by an automated fixture — the behavioural
gate is bridge-invariant (same values either way) and slice1 has no dispatch, so nothing above
would catch a bridge *reintroduction*. `test/fixtures/dict-retire` (a self-contained local
class whose instance member references a top-level impl — exactly the liftable shape the bridge
collapsed) + a `Test.Unit….LLVM.Driver` case asserting the `--no-opt` use site loads the
accessor + instance-dictionary roots and never references the impl directly, with `--opt` as
the contrast leg (the optimiser's `DictElim` must eliminate the dispatch). Discrimination
verified live: the same test run against the pre-removal compiler fails on exactly the
`--no-opt` leg (direct `speakDogImpl$d` call, no dictionary roots). Unit total: 404/404.
