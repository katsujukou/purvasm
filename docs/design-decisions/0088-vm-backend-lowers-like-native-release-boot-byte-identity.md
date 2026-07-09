# 0088. The VM backend lowers the optimiser seam like native, released from boot byte-identity

- Status: ~~Proposed~~ **Accepted** _(2026-07-10: accepted by the maintainer)_
- Date: 2026-07-10

## Abstract

Release the VM/bytecode backend's `--no-opt` **byte-identity-to-boot** constraint for the VM **object and
image** (`.pmo == boot's .pvmo`, `app.pvm == boot's .pvm`) — **not** the `.pmi`, which stays a
backend-neutral interface byte-identical to boot (native still needs it, ADR-0084/0086) — and lower the VM
**the same way as native** (the ADR-0087 `Backend`): each module's `.pmo` becomes a **list of
`(name, seam-output decl)` + an init**, and the linked `.pvm` becomes **all reachable `(name, seam-output
decl)` + a reachability-aware initialiser** (built by `lowerEntry`, not the linker) — the VM analogue of the
LLVM backend's per-decl object + `pv_init_all`-over-`reachableGdefs`. Rationale, in one point: **boot's
bytecode shape is debt from rushing to completion**, and pinning Level-2 to it (a) forces the *un-optimised*
output, so the VM cannot be the optimiser's effect-measurement field (ADR-0086 Addendum), (b) blocks a REPL
(the whole-program-derived shape has no per-decl init/evaluation), and (c) defers — does not avoid — a large
refactor to boot retirement. Freed from boot-matching, `bytecodeBackend` consumes the driver's **seam-output**
`AnfModule` (optimised under `--opt`, normalise-only under `--no-opt`; so a decl's `deps` — its free
**global-key** refs, the reachability edges; foreign refs recorded separately as required foreigns — come
from the ANF, with no CESK-term-free-var matching), and sits **symmetric to `llvmBackend`** in the
`Backend`/`CompilerAction` machinery. VM correctness is regated
**behaviourally** (VM run output vs the oracle / the VM≡LLVM≡JS differential), not by bytes-to-boot.

## Context

- boot's VM codegen already decomposes the CESK term into gdefs, lowers each to a `CodeBlock`, and links a
  reachability initialiser — structurally close to ideal. But Level-2 pins its `.pmo`/`.pmi`/`app.pvm`
  **byte-identical to boot** (the E2E "diamond/DiaA byte-identical to boot" gate; the unit `.pmo`/`.pmi`
  gates in `Backend.LLVM.Driver`/`Bytecode.Artifact`/`Link`). That pin predates the shared seam.
- The shared optimiser seam (ADR-0086, as revised by its 2026-07-10 Addendum) now yields an **optimised,
  backend-neutral `AnfModule`**, and the neutral build driver (ADR-0087) already drives the LLVM backend off
  it. The VM is the intended **optimiser-effect measurement field** — `--opt` VM must run and emit the real
  optimiser's output (DictElim/Simplify/…). Byte-identity to boot's *un-optimised* bytecode is directly at
  odds with that.
- Today's Level-2 VM path (`Compile.compileModuleWith` → `Link.link`) is a **second** lowering pipeline
  (its own `translExpr → normalize → Simplify → gdefOfExpr`), parallel to and divergent from the seam — kept
  only to reproduce boot's bytes. Re-homing it onto the library `build` (ADR-0087 §5) is blocked precisely
  by the boot-matching requirement (the `.pmo` `deps` must equal boot's CESK-term `freeVarsSet`, which the
  seam's post-normalise `AnfModule` does not carry).

## Decision

### 0. Implementation scope: (a) now, (b) deferred to an owned VM interpreter

The `app.pvm` **executable** format is consumed by boot's OCaml VM (`boot/lib/vm/machine.ml`), which is
**frozen** — the Level-2 side only *produces* `app.pvm`, it does not interpret it. So this record splits into
two parts by whether the *executed* format changes:

- **(a) — implemented now** (no new interpreter): release the byte-identity **gate** and route the VM through
  the seam + library `build`, but keep the emitted `.pvm` in **boot's current, purvm-runnable `Image` shape**
  (`{gdefs, main}`). boot's VM runs *optimised* boot-shape bytecode just as it runs un-optimised, so `--opt`
  VM becomes the optimiser measurement field with no interpreter change. Concretely: `bytecodeBackend`
  consumes the seam's output `AnfModule` and emits the current `ModuleArtifact`; **`Link.link` keeps owning
  reachability + the linked image** (its today's behaviour); `bytecodeBackend.lowerEntry` is a **placeholder**
  (the entry is the link-time `mainTerm`, per ADR-0087 §4). `.pmo` `deps` come from the ANF (this record's
  §2), which is why the gate must lift. This achieves the core value — VM consumes the optimiser.
- **(b) — deferred** (needs an **owned VM interpreter**, PS or Rust): the `.pmo`/`.pvm` **decls + init** format,
  `lowerEntry` owning reachability + the initialiser, and the REPL substrate. Changing the *executed* `.pvm`
  shape is impossible while boot's frozen `purvm` is the only interpreter, so §2's `lowerEntry`-owns-reachability
  and §3's format change land **with** the owned VM, in a follow-up. Until then, §4's *current* `Link.link`
  ownership stands.

The rest of this section (§1–§4) describes the **(b) target**; §0 is the authoritative scope for what ships
now. REPL is out of scope entirely this pass.

1. **Release the VM boot byte-identity constraint — for the VM object/image only.** Retire the
   `.pmo == boot's .pvmo` and `app.pvm == boot's .pvm` gates. **The `.pmi` is out of scope**: it is a
   *backend-neutral* interface the **native** backend still needs byte-identical to boot (ADR-0084 core /
   ADR-0086), so this record does **not** change the `.pmi` format, version, or bytes — the VM's `.pmi`
   stays byte-identical to boot, computed by the same `interfaceFromExports` as native. Only the VM object
   (`.pmo`) and linked image (`.pvm`) formats change. VM correctness is gated **behaviourally** — the VM run
   output against the oracle, and the `--opt ≡ --no-opt ≡ oracle` differential over Level-2's own VM/LLVM/JS
   legs (ADR-0082 §2). boot bytecode remains a runnable reference for **behaviour**, not bytes.

2. **The VM lowers like native.** `bytecodeBackend` mirrors `llvmBackend` as an ADR-0087 `Backend`, consuming
   the driver's **seam-output** `AnfModule` (optimised under `--opt`, normalise-only under `--no-opt`):
   - `lowerModule` : the module's `AnfModule` → the module object — a **list of `(name, bytecode-Gdef)` decls
     + a per-module init** (the module's own CAF/closure initialisation order). Each decl records its free
     references — names bound **outside** the decl (the decl body's free variables minus its own params /
     local `let` / `case` binders; prim ops are not names) — split by **kind** (as the LLVM backend already
     does — reachability over `fv ∩ gkeys`, foreigns via a separate accumulation):
     - **`deps`** = the referenced **global keys** (top-level decls, own-module and imported, qualified).
       *These are the reachability-graph edges* — every one resolves to a decl node.
     - **`foreignRefs`** = the referenced **foreign keys** — recorded as *required foreigns* (host/runtime
       resolution, the FFI ladder), **not** graph nodes: `reachableGdefs` never traverses them, so a foreign
       ref is never mistaken for a missing decl. They are accumulated for the run/link foreign manifest.
   - `lowerEntry` : the **whole** reachable module set + the entry → the **entry object**, and **owns
     reachability + initialiser construction** (the VM analogue of LLVM `entryLl` — pure whole-program
     codegen the driver runs once after the fold): compute `reachableGdefs` from the entry over the
     **global-key `deps`** graph (foreigns are not nodes), then emit the **reachability-aware initialiser**
     (the reachable decls' init in dependency order) + the entry runner. Reachability lives **here, not in
     link** (§4).
   - `interfaceOf` : the module's `.pmi` export surface — **unchanged**, still byte-identical to boot (§1).
   - `context` : the whole-program facts `lowerEntry` needs (the `deps`/reachability graph). **No `DictElim`
     bridge** — the VM has none (ADR-0086 Addendum); `--no-opt` VM is normalise-only, `--opt` runs the optimiser.

3. **The `.pmo`/`.pvm` format is decls + init** (dropping boot's exact bytes; the `.pmi` is unchanged, §1):
   per-module `.pmo = { name, imports, exports, decls, init }`; linked
   `.pvm = { decls (all reachable), initialiser, entry, isEffect }`, where the decls are the driver's
   **seam output** (optimised or normalise-only per mode). Consumable by both `purvasm run` **and** a future
   REPL (per-decl evaluation / incremental init) — the reason the whole-program-derived boot shape is
   inadequate. A `format_version` bump marks the break, **scoped to the `.pmo`/`.pvm` encoding**; the
   `.pmi` version and core are untouched (§1).

4. **Finalisation (link) stays CLI per-target** (ADR-0087 §4), and is **assembly only — no reachability, no
   codegen**. The VM link (`Link.link`, evolved to the new format) **merges** the per-module decl objects
   (`lowerModule`) with the entry object (`lowerEntry`) into one `.pvm` image; it does **not** compute
   reachability or build the initialiser — those are `lowerEntry`'s (§2), exactly as LLVM's `NativeLink`
   only combines objects while `entryLl` owns `pv_init_all`. Native link is the `clang`/`lld` subprocess. No
   shared `link` capability.

## Consequences

- **Phase B (VM onto the library `build`) unblocks cleanly.** `bytecodeBackend` consumes the driver's
  seam-output `AnfModule`; `.pmo` `deps` come from the ANF; `lowerEntry` owns reachability + the initialiser
  — resolving both open questions (deps source; the `lowerEntry`/link responsibility split) the boot pin
  created.
- **The VM becomes the optimiser measurement field.** `--opt` VM runs the real optimiser and emits *that*;
  the effect of DictElim/Simplify/… on the VM is observable end to end.
- **The duplicate lowering pipeline dies.** `Compile.compileModuleWith`'s parallel `translExpr → normalize →
  Simplify → gdefOfExpr` is replaced by the seam + `bytecodeBackend`; there is one middle-end.
- **The boot `.pvmo`/`.pvm` (object/image) byte-identity gates are retired**, replaced by behavioural gates.
  The **`.pmi` gate stays** (§1: the interface is unchanged, still byte-identical to boot for native). This
  is the deliberate, scoped removal of a standing forcing function — logged as such, not silently dropped.
- **REPL substrate.** Per-decl init/evaluation is expressible on the decls-+-initialiser format.
- **Post-boot-retirement refactor avoided.** The VM format is chosen for the VM, not inherited from boot, so
  boot's retirement removes code rather than triggering a reformat.
- **Backend symmetry.** `bytecodeBackend` and `llvmBackend` are structurally parallel (both seam-consuming,
  both `{decls + init}` per module + a reachability initialiser), so `Backend`/`build` serve both without
  special-casing — which is what the CompilerAction machinery was shaped for.

## Alternatives considered

- **Keep the VM byte-identical to boot.** Rejected (the thesis): it forces un-optimised, boot-shaped output —
  the VM cannot be the optimiser field — blocks a REPL, and defers a large refactor to boot retirement
  instead of avoiding it.
- **Release the gate but keep the current `Image` shape (`gdefs + main`) with minimal wiring.** Viable as a
  short hop, but leaves the VM without an explicit per-decl init (no clean REPL substrate) and asymmetric
  with native. The decls-+-reachability-initialiser is the small extra step that makes both backends fit the
  same `Backend` shape — worth doing once rather than reworking later.
- **A new bytecode instruction set.** Out of scope: this record changes the object/link **shape** (decls +
  init) and the **gate**, not the ISA (`Bytecode.Instruction` is unchanged); the existing `CodeBlock`/`Gdef`
  lowering is reused.
- **Defer until boot retires.** Rejected: the VM is needed *now* as the optimiser measurement field, and the
  seam already produces the optimised module the VM should consume; waiting keeps the duplicate pipeline and
  the boot pin alive for no gain.
