# 0082. Porting the native (LLVM) code generator to the Level-2 compiler

- Status: Proposed
- Date: 2026-07-07

> **Revision (2026-07-07, maintainer):** the gate is **split by layer** rather than a single
> whole-pipeline byte-identity. The backend + required lowering (`codegen_llvm` + `DictElim`) is a
> faithful, `.ll`-byte-identical transcription; the optimiser (`Simplify`/`Dbe` + the newly-built
> **NbE general inliner**) is rebuilt best-in-class and gated on **behavioural** equivalence, not
> byte-identity with boot. The two are separated by a **`--no-opt`** flag on both Level 2 *and
> boot* (the boot side is the freeze's Level-2-blocking exception): `--no-opt` = `Normalize` +
> `DictElim` (byte-identity gate); `--opt` adds the optimiser (`--opt` ≡ `--no-opt` ≡ oracle
> gate). The two layers develop as **parallel tracks**, not a strict sequence (§3).
>
> **Updated by [0083](0083-match-compilation-to-anf-middle-end.md):** match compilation moves to a
> required ANF→ANF lowering pass, so post-0083 `--no-opt` = `Normalize` + `DictElim` + `MatchCompile`
> and `--opt` inserts the optimiser *before* `MatchCompile`. The byte-identity gate is unchanged in
> kind (both sides still feed codegen the same lowered ANF) and is re-baselined by 0083. This port is
> **deferred until 0083 lands**, after which the native backend lowers a decision tree rather than
> transcribing the CPS cascade.

## Context

Wall 3 of the [0059](0059-native-abi-value-representation.md) roadmap — *bootstrap natively with
boot* — needs one thing Level 2 does not yet have: **the native backend, in PureScript.** Today
the whole native path lives only in boot (OCaml):

- the ANF optimisation passes the native lowering assumes — `Dict_elim` (158 loc), `Simplify`
  (169), `Dbe` + `Effect_analysis` ([0034](0034-effect-analysis-impurification.md), 19 + 311);
- the LLVM code generator `codegen_llvm.ml` (2465 loc — [0071](0071-codegen-runtime-c-abi.md) /
  [0072](0072-anf-to-llvm-lowering.md) lowering, with [0076](0076-direct-known-arity-calls-musttail.md)
  direct calls, [0077](0077-cross-module-direct-calls-pmi-arity.md) cross-module, and
  [0079](0079-ctx-header-abi-inline-rooting-fast-paths.md) inline rooting folded in);
- the `Native_link` driver (~430 loc — `clang -c`/dead-strip link, ulib `.c` compilation,
  the init/entry object).

Level 2 already has the front half faithfully (CoreFn → CESK → ANF: `CESK.Translate`,
`MiddleEnd.ANF`, `MiddleEnd.Normalize`) — proven by the bytecode path's 528/528 `.pmo`/`.pmi`
byte-identity. What it lacks is everything *after* ANF on the native branch. Its provider ladder
also omitted the native rung and the foreign **shape** metadata the native lowering needs; that
gap is now closed — [0080](0080-foreign-signature-reconstruction-cst.md) gives Level 2 each
foreign's `(arity, vsat, ret_vsat)` in PureScript. So the port is unblocked.

Two standing facts shape the approach. **Boot is frozen** ([boot-freeze], no codegen investment
except Level-2-blocking fixes) precisely so it is a *stable golden reference* for this port. And
the bytecode port already proved the method: **per-artifact byte-identity against boot** as the
forcing function, grown construct by construct, with fresh-name/emission order mirrored exactly
(the [byte-identity-fresh-name-order] discipline).

## Decision

### 1. Two layers, two different mandates

The port splits cleanly by *what boot is a good reference for*:

**The backend + required lowering — a faithful, byte-identical transcription.** The `codegen_llvm`
lowering (ANF → `.ll`) and the required-lowering pass `Dict_elim` (dictionary-newtype erasure,
[0018](0018-corefn-lowering.md) — not an optimisation: without it dict access is wrong-shape and
stuck, [dicts-are-newtypes]) are transcribed to reproduce boot's output **byte-for-byte**. This
is *not* the place to improve the codegen: the v1-gap levers (liveness rooting, inline
`pv_read_field`, allocation inlining — [sidenote 0011]) are **post-port** PureScript work. A
faithful transcription keeps boot the golden reference for the whole backend.

**The optimiser — best-in-class PureScript, not a boot transcription.** `Simplify`/`Dbe` and,
newly, the **long-deferred NbE general inliner** ([general-inliner-study-first] /
[inliner-blowup-reduction-aware]: study `purs-backend-optimizer` first, then a per-reference
inline gate) are written fresh for the *current* infrastructure — boot's optimiser was built
against a different one, and blindly transcribing it would forgo the redesign this arc is the
right moment for. Because the optimiser is not tracking boot, **byte-identity with boot is
explicitly not its goal** (§2). `EffectAnalysis` (the effect-placement soundness the inliner and
`Dbe` need, [0034](0034-effect-analysis-impurification.md)) is part of this layer and consumes
the [0080](0080-foreign-signature-reconstruction-cst.md) `ForeignShape` `(arity, vsat, ret_vsat)`.

Rough module map (split per the >100-loc cohesion rule):

- `Purvasm.Compiler.MiddleEnd.DictElim` — required lowering (faithful, byte-identity-gated);
- `Purvasm.Compiler.Backend.LLVM.*` — the `codegen_llvm.ml` transcription (lifting, per-module
  root-handle globals, rooting emission, the direct-call/cross-module/inline-ABI machinery),
  split by responsibility (emitter core, match compiler, `program_split`); faithful;
- `Purvasm.Compiler.Backend.NativeLink` — the `clang`/link driver, over the CLI `PROC`/`FS`
  effects (shells out; the one effectful, non-byte-identity-critical layer);
- `Purvasm.Compiler.MiddleEnd.Optimizer.*` — `Simplify`, `Dbe`, `EffectAnalysis`, the NbE
  inliner; best-in-class, behavioural-equivalence-gated.

### 2. Two gates, split by layer — via a `--no-opt` mode on both compilers

The two layers get the two different checks they warrant, separated by a **`--no-opt`** flag
(disable the optimiser; keep required lowering) added to **both** the Level-2 CLI *and boot* —
the boot side is the freeze's Level-2-blocking exception (it exists only to make Level 2's
verification possible). `--no-opt` = `Normalize` + `DictElim` → codegen; `--opt` additionally
runs `Simplify`/`Dbe`/NbE. (Post-[0083](0083-match-compilation-to-anf-middle-end.md) both paths also
end in the required `MatchCompile` lowering — see the Revision note above.)

**Backend + required lowering — `.ll` byte-identity, on the `--no-opt` path.** For every module,
the Level-2 `--no-opt` `.ll` (each `program_split` object — module objects + the init/entry
object) is **byte-identical to boot `--no-opt`**. Because Level-2 `Normalize` ≡ boot `transl`
(proven by the bytecode 528/528) and `DictElim` is faithfully transcribed, both sides feed codegen
the *same* un-optimised ANF — so the `.ll` diff isolates exactly the backend transcription, with
the optimiser out of the picture. `.ll` is a far more detailed artifact than `.pmo` (SSA
temporaries, labels, per-function emission), so byte-identity is a strict, early-failing check a
single mis-ordered emission cannot slip past. It **requires the PS codegen to mirror boot's
emission order exactly** — the SSA/label/temp counters advance on the same events in the same
order ([byte-identity-fresh-name-order], now at `.ll` scale). The deterministic embedded values
already have portable Level-2 producers (FNV-1a label ids, `Util.MD5`, `Util.Int64Decimal` float
bits, string-constant escaping).

**Optimiser — behavioural equivalence, against the *same* Level-2 compiler.** The optimiser is
correct iff, for the **same** Level-2 build, `--opt` and `--no-opt` produce **the same program
input/output behaviour** — the optimiser preserves semantics. Boot is *not* the reference here (it
is being replaced/improved, so byte-identity would be the wrong demand). This rides the existing
**four-way differential** (VM / OCaml-native / LLVM-native / JS, oracle-checked): running the
native leg both `--opt` and `--no-opt` and holding both to the CESK oracle proves `--opt` ≡
`--no-opt` ≡ oracle. The NbE inliner's known blow-up hazards ([inliner-blowup-reduction-aware])
are guarded by the differential plus the steps/allocs regression bench ([bench-regression]).

### 3. Two parallel tracks, joined at the native self-host

The two layers develop **in parallel** (their gates are independent), not in a strict sequence.

**Backend track — grow `--no-opt` `.ll` byte-identity by construct-slice**, as the
bytecode/matcher port did (never a big-bang transcription, unverifiable until the end). Rough
order, each slice its own `.ll` byte-identity checkpoint:

1. a pure first-order module (Int/Bool arithmetic, `let`, `if`) through `program`/`program_split`
   — establishes the emitter core, the entry stub, rooting, `pv_abi_check`/stamp, boot's
   `--no-opt`, and the `.ll` diff harness;
2. functions and closures (lifting, captures, the direct/generic two-entry shape);
3. ADTs, records, arrays, `case` (the match compiler);
4. `Effect` and the loop combinators (the foreign shapes, [0080](0080-foreign-signature-reconstruction-cst.md));
5. dictionaries (pulls in `DictElim`, and most real code);
6. cross-module (`program_split` multi-object, [0077](0077-cross-module-direct-calls-pmi-arity.md))
   and the inline-ABI fast paths ([0079](0079-ctx-header-abi-inline-rooting-fast-paths.md)).

**Optimiser track — build best-in-class, gate on behavioural equivalence.** Independently:
`Simplify`, `Dbe`/`EffectAnalysis`, then the NbE inliner (study-first, then a per-reference inline
gate). Each is validated by `--opt` ≡ `--no-opt` ≡ oracle on the differential corpus, and watched
for regressions on the steps/allocs bench — no dependence on the backend track's construct order.

**The join = the wall-3 native self-host.** The Level-2 compiler compiles **itself** natively:
`--no-opt` `.ll`-byte-identical to boot `--no-opt` across all modules (backend track complete),
and `--opt` behaviourally identical to `--no-opt` (optimiser track complete). That is the
bring-up milestone at which boot can retire.

### 4. Shapes now by reconstruction; `.pmi` caching is a later, orthogonal record

The port consumes [0080](0080-foreign-signature-reconstruction-cst.md) shapes via the existing
per-module resolution (`ForeignSigs.moduleForeignSigs`). That is a *correctness*-complete input
today; the channel's build-time **cost** (minutes on native, under the v1 tax) is a **perf**
concern the planned binary-`.pmo`/`.pmi` record addresses (per-module shape publication + the
[0033](0033-separate-compilation.md) hash cascade — [0080](0080-foreign-signature-reconstruction-cst.md)
§4). The port does not block on it: byte-identity is verified with reconstruction-based shapes
(slow but correct); the caching lands as its own record and speeds the port up without changing
its output.

## Consequences

- Wall 3 becomes reachable: the Level-2 compiler gains a native backend and can self-host
  natively — `--no-opt` `.ll`-byte-identical to boot and `--opt` behaviourally equivalent — at
  which point **boot retires** and the freeze pays off (every post-port investment lands only in
  the surviving PureScript codebase).
- The optimiser is *rebuilt*, not transcribed: Level 2 gets a best-in-class pass suite (finally
  the NbE general inliner) instead of a port of boot's older design — the v1-gap codegen levers
  ([sidenote 0011]) then layer on top, all in PureScript.
- `--no-opt` becomes a durable capability, not just a test scaffold: an un-optimised native build
  for bisecting a codegen bug away from an optimiser bug (the two gates keep them separable).
- The backend transcription (~2600 loc OCaml → PureScript) under a strict byte-identity gate —
  long, but incrementally verified and bisectable (a diverging `.ll` names the module and the
  first differing line). The optimiser is new work, gated behaviourally, on its own schedule.
- A second consumer of the foreign-shape metadata appears (the `EffectAnalysis` in the new
  optimiser), so [0080](0080-foreign-signature-reconstruction-cst.md)'s shapes must be faithful;
  the differential is the guard.

## Alternatives considered

- **One byte-identity gate over the whole pipeline** (require `--opt` `.ll` byte-identical to
  boot too). Rejected: it would force the optimiser to be a *transcription* of boot's — forfeiting
  the redesign (and the NbE inliner) this arc is the right moment for. Splitting the gate by
  `--no-opt` lets the backend keep boot as a structural reference while the optimiser is rebuilt
  and checked behaviourally.
- **Semantic-differential-only for the backend too** (drop `.ll` byte-identity entirely). Looser
  per construct, but discards boot as a *structural* reference for the codegen — a backend
  divergence would surface only as a runtime mismatch on some input, not a named `.ll` line, the
  late-and-vague failure mode the bytecode port avoided. Kept as byte-identity for the backend.
- **Transcribe boot's optimiser faithfully too** (byte-identical `--opt`). Rejected: boot's
  optimiser was built against a different infrastructure, and a faithful port would bank its
  older design and skip the deferred NbE inliner — double work later. Rebuild best-in-class now.
- **Big-bang transcription** (port all ~3500 loc, then test). Unverifiable until the end and
  un-bisectable; rejected for the construct-slice growth the bytecode port validated.
- **Keep the native backend in boot indefinitely** (Level 2 stays bytecode-only). Abandons wall 3
  and self-hosting, and permanently splits the native investment across two languages. Rejected —
  it is the thing this arc exists to end.
