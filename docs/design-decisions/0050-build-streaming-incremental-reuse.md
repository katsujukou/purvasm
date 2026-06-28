# 0050. Level-2 `build`: per-module streaming orchestration and incremental artifact reuse

- Status: Proposed
- Date: 2026-06-28

> **Correction / re-scope (2026-06-29), after ADR-0049 + ADR-0051 landed and the native build was
> profiled end-to-end.** Two premises of this record were measured to be wrong, and its priority
> has shifted:
>
> 1. **The decode-reuse argument is void on native.** The Context below claims decode is ~40–55 %
>    of per-module time (a *Node* figure); on native, decoding the *entire* ~227-module closure is
>    ~3 s — decode is not a meaningful cost, so "reuse also skips re-decoding" is not a real win.
> 2. **The per-module "stall at link" was serialization, now fixed by ADR-0051** (flat
>    `stringify`, ~10× — per-module serialize 43–57 s → 3.5–5.5 s). Post-0051, a cold build
>    compiles all 227 modules in ~207 s and *then* does not finish the **link step** itself
>    (>213 s, no `app.pvm`). The dominant remaining cold-build cost is the link: the in-memory
>    name-graph merge + reachability walk + the single whole-program `imageToString`.
> 3. **Incremental reuse helps only *warm* rebuilds**, which is orthogonal to the maintainer's
>    stated priority that the *cold* build be fast. So the centerpiece of this record (mtime+version
>    reuse) does not address the current bottleneck.
>
> **Resulting plan.** The link-step cold-build cost will be addressed by a **new, profile-based
> ADR** — first pinpoint where `link` spends its time (merge vs. reachability vs. `imageToString`,
> the same decode→serialize method that found the real culprit here), then design against the
> measured bottleneck rather than assume streaming/read-from-disk is the fix. This record is
> thereby **narrowed** to its still-valid, lower-priority concern — per-module streaming
> (peak-memory) and incremental reuse for warm rebuilds — and should not be accepted as the
> cold-build remedy. The streaming/peak-memory part may be folded into the forthcoming link ADR if
> the link profile shows residency is the constraint.

## Context

The Level-2 (PureScript) `build` (`CLI/Build.purs`) is structured as an **all-in-memory,
always-full** pipeline:

1. `loadClosure` loads the entry's whole transitive `corefn.json` closure into a
   `Map String Module` (all sources resident at once).
2. `map compileModule (depOrder mods)` compiles **every** module **every** build into an
   in-memory `Array ModuleArtifact` — there is no reuse of a previously emitted `.pmo`.
3. the `.pmo`/`.pmi` are written, and `link` merges the whole artifact array, after which the
   entire `app.pvm` is held as one in-memory JSON string before it is written.

So at the link step the closure is resident three times over (all CoreFn ASTs + all artifacts +
the whole image string), and a one-line edit recompiles all 227 modules. This is the
["all-in-memory build stalls at link"] problem recorded in [[selfhost-build-streaming-architecture]].
ADR-0049 removes the *algorithmic* super-linearity in artifact construction; this record removes
the *orchestration* cost (redundant recompilation) and the *space* cost (peak residency), so
Level-3 `build` reaches Level-1-comparable time **and** memory. ~~It also recovers a cost ADR-0049
explicitly leaves on the table: decode (`parseModule`) is ~40–55 % of per-module time and roughly
linear, so it is not an *algorithmic* target — but reusing a fresh `.pmo` skips reading and
decoding that module's `corefn.json` entirely, so on an incremental rebuild the unchanged modules
pay neither compile nor decode.~~ _(Corrected 2026-06-29: the 40–55 % was a Node measurement; on
native, decoding the whole closure is ~3 s — decode is not a meaningful cost, so this reuse
benefit does not hold. See the note at the top.)_

The Level-1 reference already does this (`boot/bin/main.ml`): it compiles per module and emits
each artifact immediately, and it **reuses a `.pvmo` when the object is newer than its source and
still loads at the current `Image.format_version`** (`main.ml:183-209`), recompiling otherwise.
ADR-0033 specifies the artifact/interface machinery and the three `purs-wasm` soundness rules
this reuse must honour. A prerequisite gap: the Level-2 side has **serialisers only** —
`Artifact.purs` has `moduleToString`/`interfaceToString` but no reader; boot has
`Pvm.Artifact.module_of_string`. Reuse needs a `.pmo`/`.pmi` **deserialiser** on the PureScript
side to read a cached object back for linking.

## Decision

Restructure `build` as a **dependency-ordered, per-module stream with mtime-based incremental
reuse**, mirroring boot's `build` and reusing ADR-0033's artifact/interface contract.

- **Per-module step (stream, don't accumulate sources).** Walk modules in dependency order
  (the now-linear `depOrder`/`topo`, ADR-0049). For each module obtain its `ModuleArtifact`
  either by **reuse** or by **compile**, write its `.pmo`/`.pmi`, and do not retain its CoreFn
  AST afterwards. Peak residency becomes one CoreFn module at a time plus the artifacts needed
  for linking, not the whole source closure simultaneously.

- **Incremental reuse (mtime + version), mirroring boot.** Reuse the on-disk `.pmo` iff it
  exists, is newer than its `corefn.json`, **and** still deserialises at the current
  `formatVersion`; otherwise recompile and re-emit. The version gate makes a codegen/encoding
  change (which bumps `formatVersion`, ADR-0033's convention) invalidate stale objects rather
  than silently serve them.

- **`.pmo`/`.pmi` deserialiser (new, enabling).** Add the PureScript reader counterpart to the
  existing serialisers — the inverse of `moduleToString` / `interfaceToString` over the shared
  `Json` tree (boot's `module_of_string` / `interface_of_string`) — so a reused object can be
  read back into a `ModuleArtifact` for linking. The reader is exercised by a round-trip test
  (`serialise ∘ deserialise = id` on the artifact, and byte-identity preserved).

- **Linking from the per-module artifacts.** `link` consumes the artifacts produced by the
  stream (freshly compiled or reused). To keep peak memory bounded, artifacts may be read from
  disk for the link rather than all retained in memory; the reachability-DCE name-merge
  (ADR-0021/0033) is unchanged, and `app.pvm` stays byte-identical.

- **Soundness rules carried from ADR-0033.** The three `purs-wasm` invariants hold: a module's
  compilation never consumes its own `.pmi` (no self-pollution); `.pmi` is a superset of what a
  dependent consumes (interface completeness); the `.pmi` hash moves iff downstream-relevant
  content moves (hash stability). This record matches boot's *current* reuse policy — **mtime +
  version only**; the `.pmi`-hash *cascade* (skip a dependent when a dependency's interface is
  unchanged) stays dormant, exactly as in ADR-0033, until a cross-module optimiser needs it.

- **Validation.** Differential/byte-identity: `app.pvm` and every `.pmo`/`.pmi` are unchanged
  versus the current full build and versus boot's artifacts (E2E suite). Behavioural: a no-op
  rebuild recompiles **zero** modules; editing one module recompiles only it (and relinks);
  build time scales ~linearly in module count and peak memory no longer holds the whole closure
  thrice. These are the acceptance signals for "Level-1-comparable".

## Scope

- **In:** the per-module streaming driver, mtime+version reuse, the `.pmo`/`.pmi` deserialiser,
  and linking from per-module artifacts, all in `CLI/Build.purs` / `Bytecode/Artifact.purs`
  (+ a shared `Json` parser if not already present). Depends on ADR-0049 for the linear
  `depOrder`/`topo`.
- **Out:** the `.pmi`-hash recompilation cascade (deferred with ADR-0033); cross-module
  optimisation ([[optimizer-roadmap]]); any change to artifact/image *formats* or emitted
  bytecode (byte-identity is an invariant here); `ulib` build orchestration (ADR-0043's
  `ulib-tools`, a separate toolchain).

## Consequences

- Rebuilds become incremental (only changed modules recompile, then relink), and a clean build
  no longer holds the whole CoreFn closure + all artifacts + the image string at once — directly
  addressing the time and space of the >10-min self-host build.
- The Level-2 toolchain gains a real `.pmo`/`.pmi` **reader**, closing the asymmetry with boot
  and enabling any future tool that must consume artifacts (a linker that reads from disk, a
  cross-module optimiser, an interface inspector).
- Establishes that Level-2 `build` matches boot's incremental contract, so the two toolchains
  stay differentially comparable as the compiler evolves.

## Alternatives considered

- **Keep all-in-memory; rely only on ADR-0049.** Fixes construction cost but still recompiles
  every module each build and still triples peak residency at link — neither the rebuild-time
  nor the space problem is solved.
- **Content-hash reuse (rebuild iff source/consumed-`.pmi` hash changed) now.** Stronger than
  mtime (immune to clock skew, robust to no-op touches), but it is the deferred `.pmi`-cascade of
  ADR-0033 and concentrates the `purs-wasm` separate-compilation pitfalls; matching boot's
  mtime+version policy first keeps the two toolchains aligned and the cascade a single later step.
- **Link by streaming artifacts straight from the compile step without re-reading from disk.**
  Lower I/O, but it re-couples link residency to "all artifacts in memory"; reading reused
  objects from disk is what bounds peak memory, and the freshly-compiled ones can be handed
  through directly — a hybrid the driver can choose without changing observable output.
