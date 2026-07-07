# 0084. The cross-module optimiser summary — an `--opt`-only `.pmi` extension

- Status: ~~Proposed~~ **Accepted** _(2026-07-07: accepted by maintainer; JSON additive-field summary + `hash`-untouched/always-recompile invalidation per the revised §4/§5)_
- Date: 2026-07-07

## Context

[0082](0082-native-codegen-port-to-level-2.md) names a **prerequisite** it does not itself define: a
per-module channel that publishes what a *dependent's* optimiser needs about its imports. Today the
Level-2 interface (`Bytecode.Artifact.Interface`) carries only `(ExportKind = kind + arity, hash)` —
enough for linking and cross-module direct calls ([0077](0077-cross-module-direct-calls-pmi-arity.md))
but far too thin for a dependency-directed optimiser. Two forces make the channel a prerequisite of
[0082](0082-native-codegen-port-to-level-2.md)'s **optimiser** track (not its backend track):

- **Foreign shapes are consumed every build.** A foreign's *type* is **not** in `corefn.json` (it is
  type-erased), so its `(arity, vsat, ret_vsat)` genuinely needs the CST —
  [0080](0080-foreign-signature-reconstruction-cst.md)'s reconstruction re-parses the foreign frontier
  through the pure-PS engine under the v1 call tax, minutes per native build. Its Progress already
  upgraded publication of the shape from a deferred nicety to a **port prerequisite**.
- **Cross-module inlining / specialization / impurification need the callee's *body* — the cost, not
  availability, is the issue.** The body is **always derivable from `corefn.json`** (source-less
  distribution does not change that — `corefn.json` is the compiler's input, not `.purs`). What the
  channel removes is the **per-build re-derivation cost**: a dependent that inlines/specializes against
  a dependency's `mapList` would otherwise re-run `CoreFn → CESK → ANF` (`transl`/`normalize`) *and*
  re-optimise that dependency **every build**, for **every** dependency it draws a body from.
  Publishing the summarised, already-lowered body once — the purs-wasm `summarize` model, a
  GHC-`.hi`-unfolding analog — lets a dependent **read** it instead of re-deriving it. The input flows
  **callee → caller**, the direction separate compilation already runs in, so it publishes soundly.

This record defines that channel. Because it publishes bodies and grows the artifact schema (novel,
with choices), it is its own ADR rather than a bullet in [0082](0082-native-codegen-port-to-level-2.md).

## Decision

### 1. The summary is a pruned module, at a fixed IR stage, closed by opaque refs

The channel carries a **`summary` — a pruned `M.Module`** (the `summarize` pass, purs-wasm's model),
keeping exactly the decls a dependent might need the *body* of and dropping the rest. Keep predicate
(the union): recursive groups (`M.Rec`); **inline candidates** ∪ **impure / memory-effecting bindings**
(effect visibility is *body*-carried — a dependent reads the kept body to place effects,
[0034](0034-effect-analysis-impurification.md); not a purity *bit*) ∪ **specialization callees** (the
higher-order functions [0082](0082-native-codegen-port-to-level-2.md) §1 specializes) ∪ **dictionary
machinery**; and small bindings (≤ an inline cap) + record/ctor shapes. Dropped: only *large,
multi-use, pure* cross-module bodies (never inlined/specialized anywhere) — ≈ 7/8 of the module by
purs-wasm's metatheory measure (summaries ≈ **1/8 of finalized MIR**, 1.46 MB vs 11.8 MB). Two things
the prune alone does not fix and this record must pin:

- **IR stage — the module's *per-module-optimised* ANF.** Each kept binding is published at a
  *fixed, documented* stage: the ANF **after the module's own per-module optimiser passes** (`DictElim`
  → `Simplify`/`Dbe`/`EffectAnalysis` → caller-homed specialization → the NbE inliner), *the same body
  its own codegen consumes*, itself having consumed *its* dependencies' summaries. This is a fixpoint
  over the (acyclic) dependency DAG — leaves summarised first — so a summary is a pure function of
  `(own corefn, dependencies' summaries)` and never embeds a *cross-consumer* decision. It is
  emphatically **not** the raw `Normalize` output (which would force the consumer to re-optimise), nor
  a consumer-specialised form (a higher-order callee keeps its function parameter — specialization is
  the *consumer's* caller-homed job). A cyclic module group is summarised by the same fixpoint (or,
  if that does not converge, treated conservatively as opaque — below).
- **Closure — bounded, via opaque external references, backed by a link-only export.** A kept body may
  reference a binding the prune *dropped* (a large private helper). The summary is **closed by leaving
  such references as opaque cross-module calls the consumer does not inline through** — **not** by
  transitively keeping the referenced closure (which would defeat the prune and re-admit the large-pure
  mass). A consumer's inlining/specialization is therefore bounded exactly by what is published: always
  sound (an un-inlined call is correct), and the keep-predicate + inline cap are the sole reach knobs.
  But the opaque call needs a **linkable target**, which pins a codegen obligation: a binding
  *referenced by* the summary's kept bodies but *dropped from* the summary is emitted by its defining
  module as a **link-only export** — **external linkage + a stable mangled name**, so a consumer's
  inlined body links against it — while staying **out of the interface surface** (it does not widen the
  public API/ABI, and is not a direct-call target [0077](0077-cross-module-direct-calls-pmi-arity.md))
  and **out of the summary** (not inlinable). Reachability DCE ([0021](0021-reachability-dce.md)) then
  keeps it iff some consumer actually calls it — a private helper no inlined body reaches is still
  stripped. So the module's *link-only export set* is exactly the free references of its summary's kept
  bodies that the prune dropped: computable per-module, no consumer knowledge.

### 2. Foreign shapes are published as data

Foreigns have **no body**, so their `(arity, vsat, ret_vsat)` shape
([0080](0080-foreign-signature-reconstruction-cst.md)) rides the channel as data (for the optimiser's
`EffectAnalysis`). Reconstruction stays the **correctness reference** and the `--check-foreign-sigs` /
`foreign-sigs-diff` cross-check; publication is the fast every-build channel the *optimiser* reads.
(The *backend*'s `--no-opt` calling-convention arity keeps coming from reconstruction — byte-identity
with boot requires it to match boot regardless of source — so this channel is not on the backend's
critical path; §3.)

### 3. `--opt`-only; the `.pmi` core stays boot-byte-identical; core binarisation is deferred

The channel is scoped so it **does not disturb the port's forcing function**. The two-gate split
([0082](0082-native-codegen-port-to-level-2.md) §2) already separates `--no-opt` (byte-identity with
boot) from `--opt` (behavioural). This record applies it to the interface:

- the **`.pmi` *core*** (exports / kind / arity / the existing `hash`) stays **byte-for-byte identical
  to boot's `.pvmi`** on the `--no-opt` path (and on the bytecode path, which runs no optimiser) — the
  gate the port depends on is untouched;
- the **summary + published shapes are an `--opt`-only extension** — an additive section absent,
  byte-for-byte, on the `--no-opt`/boot-identical output — and `--opt` output is **behaviourally**
  gated, not byte-identity-gated, so it is free to carry it.

So the **backend track and optimiser track stay parallel**: the backend consumes only the boot-identical
`--no-opt` core; the optimiser owns and consumes the `--opt` summary. **Binarising the `.pmi` *core*
is out of scope** and deferred to *after boot retires* (when byte-identity is no longer the gate) — a
future format record ([lv2-native-build]'s "binary `.pmo` = future ADR"). The **summary section itself
has no boot counterpart**, so it rides the existing JSON `.pmi` as an additive, `--opt`-only field (§5).

**Mode-keyed cache — the two modes never cross-serve.** Because the same module has two artifact shapes
(`--no-opt`: summary-absent; `--opt`: summary-present), the build cache **keys the artifact by
optimisation mode** so they are *distinct entries* that never clobber or substitute for one another.
A build carries its mode into the cache key (a namespace / an explicit `mode` tag in the build cache
key); an `--opt` compile that finds a dependency cached in `--no-opt` mode (no summary) treats
it as a **cache miss** and rebuilds that dependency `--opt`, rather than silently degrading to
conservative-unknown against a summary-less artifact or reusing a stale one — and symmetrically a
`--no-opt` build never reads an `--opt` artifact. (This keeps the boot byte-identity check honest: the
`refPmi` comparison is against the `--no-opt` entry, never an `--opt` one that carries the extra
section.)

### 4. Invalidation — the `hash` field is untouched; `--opt` always recompiles

`Interface.hash` today covers only the export surface (`Bytecode.Artifact`). Consuming a dependency's
summarised body creates a **stale-optimisation hazard** — a dependent that inlined that body must not
keep a stale copy after it changes. An earlier draft closed this precisely: fold a foreign-shape /
summary fingerprint + schema version into the `--opt` cascade key and have each dependent record the
`.pmi` fingerprints it consumed. This record takes the **simpler, sound** route for now and defers the
precise contract:

- the **`hash` field is left exactly as-is** — the export-surface MD5, **the same value in both modes**
  (so `--no-opt` byte-identity with boot is trivially preserved, §3, and *no* summary fingerprint is
  mixed into it — the deliberately-avoided complication);
- the **`--opt` path does no hash-based incremental reuse — it always recompiles.** Rebuilding every
  module on `--opt` sidesteps the stale-optimisation hazard *by construction* (no cached optimised
  output to go stale, and every dependency summary read in a build is freshly produced *this* run). It
  is slower, not wrong, and composes with the §3 mode-keyed cache: a non-fresh `--opt` dependency is
  simply rebuilt;
- the **precise per-consumer invalidation** the earlier draft pinned (fingerprints in the cascade,
  dependents recording consumed hashes) is **deferred to a future incremental-`--opt` lever**, added
  only if the always-recompile cost is measured to bite.

**Self-pollution invariant (still load-bearing).** Compiling module `M` **must never read `M`'s own
`.pmi` summary — only its *dependencies'* summaries.** `M`'s summary is an *output*; reading it back
would fold `M`'s previous build into its current one (stale, and non-deterministic across rebuilds).
This holds under always-recompile too, and is [0033](0033-separate-compilation.md)'s per-module
discipline, restated because 0084 introduces the summary *read*.

### 5. The summary is an additive JSON field, not a binary envelope

The `--no-opt` core keeps its current textual form (byte-identical to boot, §3), so **there is no
migration of the existing `.pmi`** and the boot byte-identity tests (`refPmi`) are unaffected. The
summary rides the **same JSON `.pmi`** as one **additional field** on the existing object — *not* a
binary envelope — reusing the existing `Json` / `stringify` / `gdefToJson` machinery (the summary is
pruned ANF/bytecode bodies, which already have a JSON encoding). Byte-identity is preserved **not** by
JSON's tolerance of extra fields (adding a field *always* changes the bytes) but by **conditional
emission**:

- on the **`--no-opt`** path the field is **entirely absent** — the object is the current five keys
  (`version` / `name` / `exports` / `imports` / `hash`), byte-for-byte boot's; **not** `"summary":null`
  nor `"summary":{}` (either would change the bytes and break `refPmi`);
- on the **`--opt`** path the field is **appended after `hash`** (the `Json` object is order-preserving,
  so the first five keys stay byte-identical) and the object's `version` stays **`3`, unbumped** — a
  reader that does not know the field ignores it, and `--no-opt` output never carries it.

The summary therefore needs **no magic / envelope / varint / separate `formatVersion`** an earlier
draft proposed: it is JSON under the existing `version: 3`. The cross-build **schema-skew** that
envelope versioning would have guarded against is dissolved by the §4 *always-recompile* decision —
within one `--opt` build every summary is produced by the *same* compiler, so no module ever reads a
summary written to a different schema. (A small in-field schema tag may be kept for defensive
debuggability, but is not load-bearing.)

### 6. What it does *not* carry — cross-module unboxing

Representation (unboxing) is **out of scope**: a *parameter* rep is the join over every call site
(caller → callee, *against* compilation), unprovable by a separately-compiled callee. That is a
link-time concern (LLVM `(Thin)LTO` + the purs-wasm ADR-0037 result-rep / worker-wrapper ladder), not
a summary one ([0082](0082-native-codegen-port-to-level-2.md) §4).

## Consequences

- [0082](0082-native-codegen-port-to-level-2.md)'s optimiser track gets its cross-module inputs from
  one `--opt`-only channel; the `--no-opt` backend `.ll` track does **not** depend on it (boot-identical
  core, no summary read), so the two proceed in parallel and this record is a dependency but **not an
  acceptance blocker** for [0082](0082-native-codegen-port-to-level-2.md).
- A dependency's summary now carries bodies, so a summarised body change must reach the dependents that
  consumed it — for now via §4's *always-recompile* on `--opt` (the precise fingerprint-based
  [0033](0033-separate-compilation.md) cascade invalidation is deferred); the `summarize` prune keeps
  the rebuilt mass off the *large-pure* bodies that dominate MIR.
- Retires the per-build reconstruction tax for shapes on the optimiser path (read, don't re-derive) —
  the [0080](0080-foreign-signature-reconstruction-cst.md) Progress owed item — while leaving the
  backend's `--no-opt` byte-identity untouched.

## Alternatives considered

- **Binarise the whole `.pmi` (core + summary) now.** Rejected for the port: the `.pmi` core is
  byte-identical to boot's JSON `.pvmi` and that identity is the port's forcing function (`refPmi`).
  Binarising the core would forfeit that gate mid-port. The core stays as-is, JSON; the additive
  `--opt` summary is a JSON field on it (§5), not binary. Core binarisation is a post-boot-retirement
  format record.
- **A binary envelope for the summary section alone** (magic + its own `formatVersion` + a varint body —
  this record's earlier §5). Rejected for the port: the summary reuses the existing `Json` / `gdefToJson`
  encoders (its bodies are already JSON-encodable), so a binary envelope adds a second encoder/decoder
  and a second version axis for **no** byte-identity benefit (the core stays JSON regardless), and the
  §4 always-recompile decision removes the cross-build schema-skew the envelope's versioning existed to
  guard. Compactness / parse-speed of an `--opt`-only section is a later lever, not a port concern — a
  plain additive JSON field (§5) is simpler and reuses the pipeline the port already has.
- **Keep the thin `(ExportKind, hash)` interface; re-derive bodies from `corefn.json` on demand (or at
  a release-only whole-program step).** `corefn.json` *is* available, so this is possible — but it
  re-pays `transl`/`normalize` + re-optimisation for every dependency body drawn, **every build** (the
  cost this record removes), and a release-only variant additionally forgoes **incremental**
  cross-module optimisation (dev builds get none). The published summary caches the derived+optimised
  body once per module on the cascade — strictly cheaper and incremental.
- **Transitively keep the referenced closure of every kept body** (rather than opaque refs, §1).
  Rejected: it re-admits the large-pure mass the prune exists to drop, unbounding the summary; opaque
  refs bound the consumer's reach to exactly what is published, soundly.
- **A separate "externs" body channel distinct from the summary.** Rejected on purs-wasm's evidence:
  the bodies a dependent needs are just decls in the summary — one channel, not two.
- **An effect-summary *bit* instead of kept bodies.** Insufficient for impurification, which needs the
  effect *placement* in the body, not just "is effectful"; body-carried is the purs-wasm choice.
