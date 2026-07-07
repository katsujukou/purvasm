# 0082. Porting the native (LLVM) code generator to the Level-2 compiler

- Status: Accepted
- Date: 2026-07-07

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
foreign's `(arity, vsat, ret_vsat)` in PureScript. And [0083](0083-match-compilation-to-anf-middle-end.md)
has **landed**: match compilation is a shared `Middle_end.Match_compile` `CCase → dtree` builder called
at each backend's `case` site — `Anf.cexpr`/`CCase` and the `--no-opt` pipeline are unchanged, and the
native backend now lowers a *shared decision tree* (no per-backend matcher to transcribe). So the port
is unblocked.

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

**The optimiser — best-in-class PureScript, not a boot transcription.** `Simplify`/`Dbe`,
**caller-homed specialization**, and, newly, the **long-deferred NbE general inliner**
([general-inliner-study-first] / [inliner-blowup-reduction-aware]: study `purs-backend-optimizer`
first, then a per-reference inline gate) are written fresh for the *current* infrastructure —
boot's optimiser was built against a different one, and blindly transcribing it would forgo the
redesign this arc is the right moment for. Because the optimiser is not tracking boot,
**byte-identity with boot is explicitly not its goal** (§2). `EffectAnalysis` (the effect-placement
soundness the inliner and `Dbe` need, [0034](0034-effect-analysis-impurification.md)) is part of
this layer and consumes the [0080](0080-foreign-signature-reconstruction-cst.md) `ForeignShape`
`(arity, vsat, ret_vsat)` — of the current module *and of its dependencies' directly-imported
foreigns* (§4), since the analysis is dependency-directed.

**Caller-homed specialization** ([optimizer-roadmap]; the **higher-order specialization** /
**static-argument transformation** purs-wasm implements, the step between effect analysis and the
general inliner — *not* dictionary erasure, which is `DictElim`'s separate job) targets a recursive
higher-order callee that threads a *function* parameter **unchanged** through its own recursion
(`mapList`, `filterBy`, `foldlList`). At a call with a **known lambda**, instead of heap-allocating
the lambda as a closure and applying it per element through an indirect call with boxed operands, the
pass emits a **copy of the callee with the lambda's body inlined and the function parameter removed**
— the lambda's free variables threaded as leading parameters (lambda-lift style). `mapList (\x -> x +
1) xs` becomes `mapList$spec0 xs`, with `mapList$spec0` recursing over `x + 1` inlined; the closure
and the indirect call vanish. Specializations dedup by callee + the lambda's *shape* (free variables
abstracted).

It is **caller-homed = homed in the *consuming* module** (the call site's module), never the callee's
defining module: a module's specialized output then depends only on its own source **plus its
dependencies' bodies**, never on its consumers — which is what keeps the pass module-local and its
build cache per-module ([optimizer-modular-not-whole-program]). Two different consumers that
specialize the same callee+lambda each home an identical copy. **Intra-module** duplication is deduped
at emission (by callee + lambda-shape), so the residual is only the **cross-module** identical copy —
and it is **size, not correctness**. Note the mitigation must be pinned to the *actual* toolchain:
the default linkers do **not** general-function-fold — macOS `ld64` has no ICF, GNU `bfd ld` none;
only `lld`/`gold` with an explicit `--icf`, which `Native_link` does not pass (it uses only
`-dead_strip` / `--gc-sections`, no `-fuse-ld=lld`, triple `arm64-apple-darwin`). So purvasm
**accepts the bounded cross-module duplication for now** (cross-module identical specs are rarer than
the intra-module copies already deduped), with `lld --icf` or a link-time dedup pass as a *future
size lever* if measured — not a relied-on mechanism. (purs-wasm could dedup inside Binaryen; purvasm's
`clang`/`ld` path cannot assume it.) Note the input this needs: specializing a callee defined **in a
dependency** requires
that callee's **body** — and (per purs-wasm) that body is **already carried in the dependency's
`.pmi` summary**, not a separate "externs" channel. purs-wasm's summary is a *pruned `M.Module`* (via
a `summarize` pass) that keeps exactly the bindings a dependent might need the body of — the
specialization callees among them (`specializationCalleeKeys`), alongside inline candidates, impure /
memory-effecting bindings, dictionary machinery, and small bindings. So a module's specialized output
is a pure function of `(own corefn, dependencies' summaries)`, and cross-module specialization needs
no channel beyond the `.pmi` summary §4 already makes the port's prerequisite.

**Why it must be its own pass (not folded into the inliner).** The general NbE inliner **excludes
recursive bindings from its inline set** (a self-referential callee stays a call — `purs-backend-es`
guards this with `selfRef`/`visited`, [sidenote 0001 §6]). So a recursive higher-order callee like
`mapList` is *never* inlined-then-β-reduced against its lambda — the general inliner cannot produce
`mapList$spec0`. Higher-order specialization is therefore a **distinct pass by necessity**, not just
by the [optimizer-roadmap] ordering.

**NbE inliner — requirements beyond the "per-reference gate."** Porting `purs-backend-optimizer`'s
NbE reducer carries several requirements the ADR must name as co-equal, not just the inline gate:

- **Preserve the reducer's own sharing** (a *second* exponential, distinct from output blow-up). A
  naive NbE reducer is `Θ(2^depth)` even when its *output* is linear — `eval` recomputes a shared
  value per use and `quote` re-normalises per path ([sidenote 0002] / [sidenote 0005]).
  `purs-backend-optimizer` avoids this with **`Lazy` `BackendSemantics` (`SemRef` deferred bodies /
  `force`) + HOAS sharing + `makeLet` floating** ([sidenote 0012 §3]); a PS port that drops that
  sharing exponentiates *reducer time* even with a correct gate. Hence the depth-`d` diamond fixture
  (§2) asserts **reducer time/alloc linear**, not only output size.
- **`case`-of-known-constructor / `case`-of-`case` on `CCase` are in-scope and non-trivial.** §3 keeps
  `case` structured through the optimiser precisely so these fire — but `purs-backend-optimizer` does
  them on its *flattened* `Branch` (cond-chain) via ~15 `simplifyCond*` / `shouldDistributeBranch{Apps,
  Accessor,PrimOp*}` functions ([sidenote 0012 §6/§9.1]). purvasm keeps `CCase` (the shared `dtree`
  builder runs *after* the optimiser), so **the NbE track must reimplement this whole group directly
  on `CCase` nodes** — a sizeable, explicitly-scoped part of the track, not a freebie.
- **Fuel + termination.** The rewrite fixpoint needs a fuel cap (`purs-backend-es` uses
  `rewriteLimit = 10000`, crashing on overrun, [sidenote 0012 §1]); the port owes the same bound.
- **Inline directives — decided: no channel by default, verify heuristics-alone first.**
  `purs-backend-optimizer` leans on `@inline arity=n` instance-method directives for wins heuristics
  alone miss (dictionary methods not saturating), and Level 2 *could* build the same
  (`Directives`/`InlineDirectiveMap`, feasible on the existing CST engine
  [0080](0080-foreign-signature-reconstruction-cst.md) / [0081](0081-purvasm-regex-pure-ps-es-engine.md)).
  But purvasm needs them **less**, on two structural grounds. (i) `DictElim` is a *required-lowering*
  pass ([0027](0027-dictelim.md), byte-identity-gated), so the primary directive win — instance-method
  saturation to a primitive (`eq → intEq`) — is front-loaded off the heuristic path (in
  `purs-backend-optimizer` there is no separate `DictElim`; directives do that work). (ii) The first
  corpus is a **fixed self-host set** (compiler + Prelude/ulib), not arbitrary user code, so a residual
  can be measured and hand-fixed rather than exposed as a general tuning surface. The one structural
  gap — a curried partial like `genericShow dict1 dict2` (arity 3, 2 applied) sits size-1-opaque and is
  not specialised ([sidenote 0005 §2]) — is an **accepted missed-opt residual, not a blow-up**: the
  unsaturated instance body stays a *call*, while saturated live `show` sites inline and reduce
  naturally, and the pathological (opaque × dead-over-export) side is bounded by the size backstop
  above. This is the purs-wasm outcome — its `genericShow` partial was left un-specialised and judged
  fine because the saturated real-world `show` uses inlined. **Decision: ship no directive channel by
  default; verify the heuristic wins on the self-host corpus, and add one only on a *measured*
  under-optimised hot dispatch — then as local-only `@inline` or a curated default-directive set,
  never `@inline export`** (a frozen cross-module ABI surface, cf. [ulib-instance-context-abi]).

Rough module map (split per the >100-loc cohesion rule):

- `Purvasm.Compiler.MiddleEnd.DictElim` — required lowering (faithful, byte-identity-gated);
- `Purvasm.Compiler.Backend.LLVM.*` — the `codegen_llvm.ml` transcription (lifting, per-module
  root-handle globals, rooting emission, the direct-call/cross-module/inline-ABI machinery),
  split by responsibility (emitter core, match compiler, `program_split`); faithful;
- `Purvasm.Compiler.Backend.NativeLink` — the `clang`/link driver, over the CLI `PROC`/`FS`
  effects (shells out; the one effectful, non-byte-identity-critical layer);
- `Purvasm.Compiler.MiddleEnd.Optimizer.*` — `Simplify`, `Dbe`, `EffectAnalysis`, caller-homed
  specialization, the NbE inliner **including the `case`-of-`case` / `case`-of-known-ctor group on
  `CCase`** (above); best-in-class, behavioural-equivalence-gated.

**Blow-up's `clang -O2` backstop — two *different* mechanisms, not one.** `clang -c -O2` is
**super-linear in single-function size** (measured exponent 1.18→1.70 — GVN/InstCombine/MachineCSE/
RegAlloc, [sidenote 0012 §10]), and purvasm cannot drop `-O2` (its IR is naive), so a single huge
lifted function is a compile-time cliff. The standing conclusion ([sidenote 0005 §6]) — "gate in the
reducer **plus** an explicit codegen-side guard" — actually names two mechanisms addressing two
*causes*, which the size guard must **not** conflate:

- **(a) Per-reference inline gate — optimiser side, *pre*-materialise, bounds *each* inline.** For a
  function that would be huge *because the inliner materialised a large dispatch*, the only place to
  keep it a runtime call is **before** materialising: once the NbE reducer quotes, the pre-inline call
  form is gone, and un-materialising at codegen means re-deriving the discarded reference — not cheap.
  This is the existing per-reference gate — but its threshold (`~16`) is a **reduction-worthiness**
  calibration, **not** a compile-time-cliff one. The point is to keep it **conservative *because* the
  backend is super-linear** — *not* to raise it toward the cliff (`~2000` lines), which would admit
  huge single inlines and degrade reduction quality. So (a) bounds an **individual** inline's size.
- **(b) Emitted-size downgrade — codegen side, via a dedicated `-O0` translation unit.** Two things
  escape (a) entirely: an **inherently large source function** (a generated giant `case` table) or a
  dead over-exported one — no dispatch to defer; **and the compounding path** — many single-use
  *small* inlines, each under the gate, **accumulating** in one lifted function past the cliff, which
  no *per-inline* gate can see. Both are visible only in the **emitted function size**. The realisation
  must be pinned to the *actual* driver: today `Native_link` `clang -c`s the whole module `.ll` at a
  single `-O2` — there is **no per-function opt knob**, and `optnone` does **not** suffice (it drops
  the IR passes but not the *machine* passes — MachineCSE / Greedy RegAlloc, which [sidenote 0012 §10]
  measured as a large share of the cliff, are module-level). So the concrete lever is to **emit the
  outlier function into its own `.ll`/`.o` and `clang -c -O0` it** (fast isel + fast regalloc — truly
  linear) — a natural extension of `program_split`'s per-object emission — with outlining/acceptance as
  fallbacks. *This* is the mechanism the two-track split does not own (a codegen decision on an emitted
  size measurement) and it is **purvasm-specific**: `purs-backend-es` carries **no** per-function
  budget (its safety is reduction gate + share-don't-copy + a *linear* Binaryen backend); purvasm needs
  (b) only because `clang -O2` is not linear.

For the dead-over-export case (0004 Lesson 5.1 — the purs-wasm hang, reproducible on LLVM), the clean
**primary** fix is upstream of both: **don't feed dead code to `-O2` at all** — link-time DCE and not
over-exporting synthetic decls ([sidenote 0005 §6]'s "SMO amplifier"); (b) is only the backstop after
that. And note the residual is small: with the per-reference gate (a) working, an over-export module's
generic dispatch stays a *call* (a small function), so (b)'s real target narrows to **truly large
source functions** and **compounding** — rare. Ordering these (DCE/non-over-export → gate (a) → codegen downgrade (b))
keeps the relationship to §4 clean.

### 2. Two gates, split by layer — via a `--no-opt` mode on both compilers

The two layers get the two different checks they warrant, separated by a **`--no-opt`** flag
(disable the optimiser; keep required lowering) added to **both** the Level-2 CLI *and boot* —
the boot side is the freeze's Level-2-blocking exception (it exists only to make Level 2's
verification possible). `--no-opt` = `Normalize` + `DictElim` → codegen; `--opt` additionally
runs `Simplify`/`Dbe`/caller-homed specialization/NbE. (Post-[0083](0083-match-compilation-to-anf-middle-end.md) codegen's
`case` lowering consumes the shared `dtree` builder on both paths — there is no separate
`MatchCompile` pipeline stage ([0083](0083-match-compilation-to-anf-middle-end.md)).)

**Backend + required lowering — `.ll` byte-identity, on the `--no-opt` path.** For every module,
the Level-2 `--no-opt` `.ll` (each `program_split` object — module objects + the init/entry
object) is **byte-identical to boot `--no-opt`**. Because Level-2 `Normalize` ≡ boot `transl`
(proven by the bytecode 528/528) and `DictElim` is faithfully transcribed, both sides feed codegen
the *same* un-optimised ANF (still `CCase`-shaped) and lower its `case`s through the *same* shared
`dtree` builder ([0083](0083-match-compilation-to-anf-middle-end.md)) — so the `.ll` diff isolates
exactly the backend transcription, with the optimiser out of the picture. `.ll` is a far more detailed artifact than `.pmo` (SSA
temporaries, labels, per-function emission), so byte-identity is a strict, early-failing check a
single mis-ordered emission cannot slip past. It **requires the PS codegen to mirror boot's
emission order exactly** — the SSA/label/temp counters advance on the same events in the same
order ([byte-identity-fresh-name-order], now at `.ll` scale). The deterministic embedded values
already have portable Level-2 producers (FNV-1a label ids, `Util.MD5`, `Util.Int64Decimal` float
bits, string-constant escaping).

**Optimiser — behavioural equivalence, against the *same* Level-2 compiler.** The optimiser is
correct iff, for the **same** Level-2 build, `--opt` and `--no-opt` produce **the same program
input/output behaviour** — the optimiser preserves semantics. Boot is *not* the reference here (it
is being replaced/improved, so byte-identity would be the wrong demand). Concretely the gate runs
**Level 2's *own* legs** against the CESK oracle — its **VM (bytecode) / LLVM-native / JS** outputs,
each built both `--opt` and `--no-opt` — proving `--opt` ≡ `--no-opt` ≡ oracle. The OCaml-native leg
is **boot's** (boot has its own optimiser and does not consume Level 2's optimised ANF), so it is
*not* part of this gate; it remains a fourth independent leg of boot's own e2e harness through the
freeze period ([0083](0083-match-compilation-to-anf-middle-end.md) §3 / §5), not of the Level-2
optimiser check.

**But behaviour + runtime-bench is *blind to blow-up* — a third, size/time gate is mandatory.** The
NbE inliner's hazard ([inliner-blowup-reduction-aware]) is not a wrong answer but a code-size /
compile-time explosion, and **neither existing gate measures that dimension**: the differential
proves `--opt ≡ --no-opt ≡ oracle` — a `2^depth` diamond and a `~33×` generic-DAG bulk each compute
the *right value*, so they **pass**; and the steps/allocs bench ([bench-regression]) measures the
*compiled program's* runtime cost on the oracle, not the optimiser's output size or compile time.
[sidenote 0003 §2] / [sidenote 0004 Lesson 6] record this exactly — a `genericShow` blow-up while
unit/e2e/diff/bench all stayed green, because "no fixture exercised the property the new architecture
stresses." So the optimiser track carries a **third gate: size/time-asserting fixtures** (owed by
[sidenote 0004 Lesson 6] / [sidenote 0005 §6]), first-class alongside the differential and the bench:

- a **depth-`d` diamond** — assert the normal form is *linear* in `d` **and** the reducer's own
  time/allocation is linear (not just the output — see the reducer-sharing requirement in §1);
- a **recursive-`Generic`/`Eq` module** — assert by *size* that the large dispatch is **never
  constructed** (fix "no blow-up" as a size bound, not a value diff);
- **positive fixing of the win patterns** in the bench corpus — State-monad collapse, dict-elim
  (`eq → intEq`), `Effect` collapse, fusion convergence — since a behavioural gate passes a
  regression that silently *loses* a collapse (still correct, just slow/large).

### 3. Two parallel tracks, joined at the native self-host

The two layers develop **in parallel** (their gates are independent), not in a strict sequence.

**Backend track — grow `--no-opt` `.ll` byte-identity by construct-slice**, as the
bytecode/matcher port did (never a big-bang transcription, unverifiable until the end). Rough
order, each slice its own `.ll` byte-identity checkpoint:

1. a pure first-order module (Int/Bool arithmetic, `let`, `if`) through `program`/`program_split`
   — establishes the emitter core, the entry stub, rooting, `pv_abi_check`/stamp, boot's
   `--no-opt`, and the `.ll` diff harness;
2. functions and closures (lifting, captures, the direct/generic two-entry shape);
3. ADTs, records, arrays, `case` (the LLVM lowering of the shared `dtree`, [0083](0083-match-compilation-to-anf-middle-end.md));
4. `Effect` and the loop combinators (the foreign shapes, [0080](0080-foreign-signature-reconstruction-cst.md));
5. dictionaries (pulls in `DictElim`, and most real code);
6. cross-module (`program_split` multi-object, [0077](0077-cross-module-direct-calls-pmi-arity.md))
   and the inline-ABI fast paths ([0079](0079-ctx-header-abi-inline-rooting-fast-paths.md)).

**Optimiser track — build best-in-class, gate on behavioural equivalence *and size/time*.**
Independently, in the [optimizer-roadmap] order: `Simplify`, `Dbe`/`EffectAnalysis`, then
**caller-homed specialization**, then the NbE inliner (study-first, preserving the reducer's `Lazy`
sharing §1, a **conservatively-thresholded** per-reference inline gate, with the codegen-side
**emitted-size downgrade via a dedicated `-O0` translation unit** (for compounding +
inherently-large functions) as the boundary backstop §1). Each is
validated by `--opt` ≡ `--no-opt` ≡ oracle on the differential corpus, **and by the §2 third gate —
the size/time-asserting fixtures (diamond, recursive-`Generic`, win-pattern fixing)** the behavioural
gate and the runtime bench are blind to — and watched for runtime regressions on the steps/allocs
bench. No dependence on the backend track's construct order.

**The join = the wall-3 native self-host.** The Level-2 compiler compiles **itself** natively:
`--no-opt` `.ll`-byte-identical to boot `--no-opt` across all modules (backend track complete),
and `--opt` behaviourally identical to `--no-opt` (optimiser track complete). That is the
bring-up milestone at which boot can retire.

### 4. The cross-module channel is one pruned-module `.pmi` summary — a port prerequisite

Every dependency-directed optimiser input — foreign shapes, cross-module effect visibility, and
specialization/inline callee bodies — rides **one** channel: a **pruned-module summary** published
in each module's binary `.pmi`, the design purs-wasm proves and this port adopts. (This supersedes an
earlier draft that split it into three separate channels — a foreign-shape field, an effect-summary
*bit*, and an "orthogonal externs/Layer-B" body channel; purs-wasm shows they are one summary.)

**The summary is a *pruned `M.Module`*, not the interface Level 2 has today.** The current
`Interface` publishes only `(ExportKind = kind + arity, hash)` (`Bytecode.Artifact`) — far thinner
than what cross-module optimisation needs. purs-wasm's summary (a `summarize` pass over the module)
keeps exactly the decls a dependent might need the **body** of, and drops the rest:

- **kept:** recursive groups (`M.Rec`); the keep-keys — inline candidates ∪ impure / memory-effecting
  bindings ∪ **specialization callees** (`specializationCalleeKeys`, the higher-order functions §1
  specializes) ∪ dictionary machinery; and small bindings (≤ an inline cap) + record/ctor shapes;
- **dropped:** only *large, multi-use, pure* cross-module bodies (never inlined/specialized anywhere).

So effect visibility is **body-carried** (a dependent sees an import's effects by reading the impure
binding's kept body — impurification needs the placement, not just a bit), and specialization callee
bodies are ordinary decls in that same summary — *no separate externs channel*. Measured on purs-wasm
(ADR-0034's metatheory corpus), the summaries total ≈ 1/8 of the finalized MIR (1.46 MB vs 11.8 MB).

**Foreigns are the one thing with no body**, so their `(arity, vsat, ret_vsat)` shape rides the
summary as data, from [0080](0080-foreign-signature-reconstruction-cst.md) — reconstructed **per
dependency, no closure sweep** as the correctness reference / `--check-foreign-sigs` cross-check, but
**published into the `.pmi`** for the fast every-build read. [0080](0080-foreign-signature-reconstruction-cst.md)'s
Progress already upgraded that publication from a deferred nicety to a **prerequisite of this port**
(reconstruction re-parses the frontier through the pure-PS CST engine under the v1 call tax — minutes
per native build — so the port must *read* shapes, not re-derive them each build). The prerequisite is
scoped precisely: it gates **full wall-3 self-host and the optimiser track's cross-module precision**,
**not** the backend `.ll`-byte-identity track (which precedes it on reconstruction — next paragraph).
The port **depends on** the binary-artifact record — **[0084](0084-binary-pmo-pmi-and-cross-module-summary.md)**,
carrying the pruned-module summary + published foreign shapes on the
[0033](0033-separate-compilation.md) hash cascade — rather than superseding
[0080](0080-foreign-signature-reconstruction-cst.md); the record changes only *where* an input is read
from, never the port's output. Together with **caller-homed** placement (§1), the
summary makes each module's optimised output a pure function of `(own corefn, dependencies'
summaries)` — module-local, per-module-cacheable, no upward (consumer) dependency.

**What is *not* blocked, and the phased option.** The backend `.ll`-byte-identity track starts
immediately: it consumes foreign **shapes** (via reconstruction while the summary channel is built —
slow but correct; byte-identity holds regardless of source) and **no** cross-module summary at all
(it runs no optimiser, so its `.ll` never depends on one). The optimiser track's cross-module
precision is what rides the summary; before the pruned-module summary lands it may start with
imported ordinary definitions treated **conservative-unknown** (sound, only a missed optimisation)
and intra-module specialization only, tightening as the summary channel comes online.

**The one cross-module optimisation the summary *cannot* carry — unboxing.** Everything above works
per-module because its input flows **callee → caller**, the direction separate compilation already
runs in: a dependency's kept body / effect placement / spec-callee is fixed by that dependency and
published in its `.pmi` summary. **Cross-module representation (unboxing) is the exception**, because
a *parameter*'s unboxed rep is the join over **every call site** — it flows **caller → callee,
against compilation** — and a separately-compiled callee cannot prove all its callers pass an unboxed
scalar (feeding a boxed value to an unboxed parameter faults). This is purs-wasm's measured wall
(its ADR-0037 Addendum, *separate per-module codegen and linking*): *result* reps are
`.pmi`-publishable soundly (callee → caller, no fixpoint), *parameter* reps are inherently
whole-program or speculative. So
purvasm's cross-module unboxing is a **link-time** concern, not a summary one — the LLVM `(Thin)LTO`
the native output rides. Caveat: purvasm boxes behind runtime calls (`pv_new_number` / `pv_read_*`),
semi-opaque to LTO, so LTO will not auto-unbox cross-module numerics; the sound-and-cheap first step
is still summary-adjacent — **publish result reps** — with the worker/wrapper and parameter-rep rungs
(the 0037 ladder) taken only if a hot cross-module scalar path is measured to regress. This is out of
the port's critical path (a perf lever, gated behaviourally), noted so the boundary between "summary
carries it" and "link-time recovers it" is explicit.

## Consequences

- Wall 3 becomes reachable: the Level-2 compiler gains a native backend and can self-host
  natively — `--no-opt` `.ll`-byte-identical to boot and `--opt` behaviourally equivalent — at
  which point **boot retires** and the freeze pays off (every post-port investment lands only in
  the surviving PureScript codebase).
- The optimiser is *rebuilt*, not transcribed: Level 2 gets a best-in-class pass suite (caller-homed
  specialization and, finally, the NbE general inliner) instead of a port of boot's older design —
  the v1-gap codegen levers ([sidenote 0011]) then layer on top, all in PureScript.
- `--no-opt` becomes a durable capability, not just a test scaffold: an un-optimised native build
  for bisecting a codegen bug away from an optimiser bug (the two gates keep them separable).
- The backend transcription (~2600 loc OCaml → PureScript) under a strict byte-identity gate —
  long, but incrementally verified and bisectable (a diverging `.ll` names the module and the
  first differing line). The optimiser is new work, gated behaviourally, on its own schedule.
- A second consumer of the foreign-shape metadata appears (the `EffectAnalysis` in the new
  optimiser), so [0080](0080-foreign-signature-reconstruction-cst.md)'s shapes must be faithful;
  the differential is the guard.
- The port acquires a **prerequisite — [0084](0084-binary-pmo-pmi-and-cross-module-summary.md)**: the
  binary-`.pmo`/`.pmi` artifact record carrying a **pruned-module summary** (purs-wasm's `summarize` —
  kept bodies for cross-module spec / inline / impurify / dict-elim, plus published foreign shapes; ≈
  1/8 of MIR), aligning with [0080](0080-foreign-signature-reconstruction-cst.md)'s Progress. Scoped:
  it gates **full wall-3 + optimiser cross-module precision**, not the backend `.ll` track — that
  proceeds on reconstruction meanwhile, and the optimiser track can start conservative-unknown +
  intra-module-only until the summary lands (§4). So [0084](0084-binary-pmo-pmi-and-cross-module-summary.md)
  is a real dependency but **not an acceptance blocker** for this record. Sequencing pinned, not
  hand-waved.

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
- **Supersede [0080](0080-foreign-signature-reconstruction-cst.md) and run the whole port on
  reconstruction** (no binary shape publication prerequisite). Rejected: reconstruction re-parses the
  foreign frontier through the pure-PS CST engine under the v1 call tax — minutes per native build —
  which the port pays *every* build (§4), and [0080](0080-foreign-signature-reconstruction-cst.md)'s
  Progress already pinned publication as a prerequisite for exactly this reason. Reconstruction is
  kept as the correctness reference and `--check-foreign-sigs` cross-check, not the build-time
  channel.
- **Block the optimiser track until the pruned-module summary lands** (no conservative-unknown /
  intra-module-only phase). Workable but needlessly serialises the optimiser behind the format bump;
  §4's conservative-unknown + intra-module-only default lets the optimiser track start (soundly) and
  tighten when the summary arrives, keeping the two tracks parallel.
- **A separate "externs" body channel, distinct from the `.pmi` summary** (my earlier framing).
  Rejected on purs-wasm's evidence: the bodies a dependent needs for specialization/inlining are just
  decls in the `summarize`d `.pmi` summary — one channel, not two; a second one would duplicate the
  summary and re-introduce a cross-module body dependency the summary already carries.
