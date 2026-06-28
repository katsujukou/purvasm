# 0049. Eliminate super-linear construction in bytecode lowering, match compilation, and link ordering

- Status: ~~Proposed~~ **Accepted** _(2026-06-28: maintainer explicitly accepted; implemented same day)_
- Date: 2026-06-28

> **Progress (2026-06-28).** Implemented. `lists` added as a direct dep of `compiler` and `cli`.
> `Bytecode/Lower.purs` now lowers through `List`-space workers (`lowerExpr'`/`lowerCexpr'`/
> `lowerValue'`) with tail-sharing `<>` and `List.length` for `CIf` offsets, materialising to an
> `Array` only at the chunk boundaries (`lowerExpr`/`fnChunk`, the `Closure`/`MakeRec` embeddings,
> and the `CCase` seam back from `Match`); the public `lowerCexpr`/`lowerValue` are kept as thin
> `Array` wrappers for the unit tests. `Bytecode/Lower/Match.purs`'s assembler buffer is a reversed
> `List Pseudo` (cons + one reverse), and `resolve` builds its output by cons + reverse; the
> Maranget matrix-rewrite `<>`/`concat` were deliberately left (intrinsic, program-size-independent).
> `Bytecode/Codegen.purs`'s `program` (both `Let` and `LetRec` arms), `Link.topo`, and
> `CLI/Build.purs`'s `depOrder` use reversed-list accumulation. **Validation:** the full unit
> (59/59) and E2E (3/3) suites pass, including the `.pmo`/`.pmi`/`app.pvm` byte-identity-vs-boot
> tests — output is unchanged, no `format_version` bump. The synthetic `let`-chain probe's exponent
> dropped from ≈1.5 to **1.003** (N 250→4000 = 16×, time 16.1×); the heaviest real binding
> (`Bytecode.Instruction`, a 378-instruction chunk) fell from 14.8 ms to 5.3 ms of lowering on V8
> (the gain is far larger on native, which lacks V8's optimised `concat`). The `Data.List` native
> linkability prerequisite was checked: `lists` is pure PureScript (no `foreign import`), and both
> `Data.List.Types` and `Data.List` compile cleanly through boot's `purvm compile` to `.pvmo`. The
> observed non-stack-safety of the lowering recursion on synthetic chains (~8000 deep) is unchanged
> from boot and out of scope (real chunks are ≤ a few hundred).

## Context

The Level-2 (PureScript) compiler's `build` is super-linear in program size: building the
self-hosting closure (227 modules) with the Level-1 native `purvm` takes >10 min, and peak
memory is large. The Level-1 (OCaml) compiler does the same work near-linearly. The Level-2
algorithms are *ported* from boot but lost boot's accumulation discipline: OCaml accumulates
emitted instructions / orderings in **lists** (`x :: acc`, O(1)) or a mutable **buffer**, then
materialises once. The PureScript port accumulates in **`Array`** via `Array.snoc` / `<>`, and
on this target every `Array` append allocates a fresh array and copies all elements (it is *not*
amortised). Repeated append in a fold or right-recursion is therefore quadratic. The concrete
hot spots (all verified against the current source):

- **Bytecode lowering — `O(L²)` per code block** (`L` = instructions in a function body).
  `Bytecode/Lower.purs` builds a `CodeBlock` (`Array Instruction`) by left-associative `<>`
  whose right operand is the recursive tail (`lowerCexpr c <> [Bind x] <> lowerExpr rest`).
  After ANF normalisation every compound subexpression is a `let`, so a body is a long
  `let`-spine; the tail array is re-materialised and re-copied at every level. This fires for
  **every binding in every module** and is the dominant cost *within compile* (decode is a
  separate, co-equal constant — see Profiling), since the largest functions (e.g. the compiler's
  own decoders) dominate.

- **Pattern-match compilation — `O(M²)` per `case`, twice** (`M` = emitted pseudo-instructions).
  `Bytecode/Lower/Match.purs` emits via `emit p = … buf = Array.snoc s.buf p` (one snoc per
  pseudo), and `resolve` rebuilds the final chunk with `Array.snoc acc.out …` (a second
  `O(M²)`). Fires for every `case`.

- **Link/build ordering — `O(N²)` in module count** (`N` = modules). `Link.topo` accumulates the
  topological order with `snoc ord1 a`; `CLI/Build.purs`'s `depOrder` accumulates with
  `Array.snoc done1 m`. This is the genuinely *module-count*-dependent super-linearity.

- **(Latent) whole-program spine split — `O(K²)`.** `Bytecode/Codegen.purs`'s `program`
  accumulates `gdefs` with `Array.snoc`. It is not on the Level-2 `link` hot path (it runs only
  over the tiny `main` term there), but the same trap should be removed pre-emptively.

These are algorithmic, not encoding, problems: the *emitted bytecode is byte-identical*; only
the cost of building it is wrong. Byte-identity of `.pmo`/`.pmi`/`app.pvm` with boot's
`.pvmo`/`.pvmi`/`.pvm` is an invariant the E2E suite asserts and must be preserved
([[byte-identity-fresh-name-order]]).

This record addresses (A) per-code-block and (B) per-program construction costs. The
orchestration-level costs — recompiling every module each build, and holding the whole closure
in memory — are a separate decision ([[selfhost-build-streaming-architecture]], proposed in
ADR-0050), which depends on the linear `topo`/`depOrder` established here.

**Profiling (2026-06-28).** The dominance claim above was unverified, so it was measured. The
Level-2 compiler also compiles to JS under stock `purs`, and on that target `Array` append is
likewise an O(n) copy (`snoc` slices, `<>` is `concat`), so the same quadratics manifest — fast
enough to profile without the 10-min native run. Numbers are V8 wall-clock used only to rank the
*algorithmic* terms; native constants are ~3 orders larger and, lacking V8's optimised `concat`,
push the exponents higher.

- **Within `compileModule`, lowering dominates.** Splitting compile into `translExpr` / `freeVars`
  / `normalize` / lowering (`gdefOfExpr`): lowering is 54–89 % of compile time
  (`Data.Map.Internal` 21.5 / 32.5 ms; `PureScript.CoreFn.Expr` 18.8 / 22.2; `Bytecode.Instruction`
  18.6 / 21.0, whose single largest binding — a 378-instruction chunk — alone took 14.8 ms).
- **Lowering is super-linear in chunk length.** A synthetic `let`-chain of `N` bindings: as `N`
  grows 250 → 4000 (16×), `lowerExpr` time grows ~61× — an empirical exponent ≈ 1.5 on V8,
  trending toward 2 on native. (It also overflowed the V8 stack at `N` ≈ 8000, since `lowerExpr`
  recurses to chunk depth; real chunks are small — max ~hundreds — and boot recurses identically,
  so this is noted, not a target.)
- **Decode is comparable but appears linear, and is out of scope.** `parseModule` (argonaut
  `jsonParser` + `decodeModule`, over `Foreign.Object` = `Data.Map`) is ~40–55 % of total
  per-module time (e.g. `Data.Map.Internal` decode 44.8 ms vs whole compile 31.2 ms), but its
  cost-per-KB does not grow with module size — it is a large *constant*, not the super-linear
  term. Neither ADR touches it; ADR-0050's incremental reuse nonetheless avoids re-decoding
  unchanged modules, and the decode path itself is flagged as separate future work.

So the dominance is confirmed *for the compile stage*, with the honest caveat that decode is a
co-equal constant the algorithmic fixes here do not address.

## Decision

Eliminate every growing-`Array`-`<>`/`snoc` accumulator, materialising to an `Array` **once** at
the boundary with exact element order preserved. The mechanism differs by regime — there are two,
and conflating them is itself a hazard:

- **Forward-order build with length introspection (lowering): transliterate boot's list `@`, not
  a reverse-cons / push-buffer.** `lowerExpr` is forward-order *and* reads the length of
  already-emitted code to compute jump offsets — `CIf` does `JumpUnless (length tc)` over the
  then-branch before appending the else-branch (`Lower.purs:55-57`), exactly as boot's
  `gen_atom a :: Jump_unless (len tc) :: (tc @ ec)`. Boot's linearity comes from OCaml `@`:
  `small @ big_tail` copies only `small` and **shares** `big_tail`. A reverse-cons-then-reverse or
  a single forward push-buffer is *wrong* here: it cannot serve `length tc` mid-stream without
  introducing label back-patching that Lower deliberately does not have, which would also threaten
  byte-identity. So **change `CodeBlock` accumulation in `Lower` from `Array Instruction` to
  `Data.List Instruction`** — whose `Semigroup` `append` is `foldr (:) ys xs`, i.e. left-O(n),
  tail-sharing, identical to OCaml `@` (verified) — keeping `lowerExpr`/`lowerCexpr`/`lowerValue`/
  `fnChunk` structurally line-for-line with boot (`length tc` becomes `List.length tc`), and call
  `Array.fromFoldable` exactly once at each chunk boundary (`fnChunk`, the `Closure`/`MakeRec`
  embeddings, and `program`'s `main`). Postorder (ADR-0003) and every offset are unchanged.
  - *Prerequisite (verify before implementing):* `Data.List` is pure PureScript and already
    produced under `output/`, but it is **not** in `ulib`/`purvasm_lib`. Confirm it compiles
    cleanly through `purvm` and is in the native self-host link closure (add it to the linked set
    if the native build requires it there). If that proves troublesome, a bespoke local catenable
    type with the same `@`-semantics is the fallback — but `Data.List` is preferred.

- **Pure end-append accumulation (everything else): reverse-cons + one reverse (or `ST` push).**
  Where order is forward but no mid-stream length is read, accumulate by consing and reverse once
  (boot's `instrs := i :: !instrs; List.rev`), equivalently an `ST` push+freeze (boot's `Buffer`):
  - **`Bytecode/Lower/Match.purs` — assembler only.** Replace the `State.buf` `snoc` and
    `resolve`'s `snoc out` with this discipline. The two-pass label back-patching (`labelpos`
    then offset resolution) is retained; only the buffer representation changes. **The pattern-
    *matrix* rewriting in the same file (`row.binds <> bindsOf …` on each descent in
    `compileCtor`/`compileLit`/`compileArr`/`compileRec`/`wildRows`, and `replaceCol`'s `concat`)
    is intentionally *not* changed:** its cost is intrinsic to Maranget specialisation —
    proportional to pattern nesting × width, independent of program size — and boot's list version
    is isomorphic, so it is not a super-linear-in-program-size term. This is called out explicitly
    so the file's remaining `<>`s are understood as deliberate, not missed.
  - **`Link.topo` and `CLI/Build.purs`'s `depOrder`.** Accumulate the DFS post-order by consing
    and reverse once (O(N)). Ordering semantics (imports before importers; a module's own decls in
    source order) unchanged.
  - **`Bytecode/Codegen.purs`'s `program`.** Both growing accumulators here are fixed, not just
    the obvious one: the `Let` arm's `Array.snoc acc …` (`Codegen.purs:43`) **and** the `LetRec`
    arm's `acc <> map … binds` (`:44`), which also full-copies `acc` each step. Boot's
    `List.rev_append defs acc` removes both; reverse-cons here does the same. (Off the Level-2
    `link` hot path today — it runs only over the tiny `main` term — but the latent `O(K²)` is
    removed pre-emptively.)

- **Validation.** The existing E2E byte-identity tests (`.pmo`/`.pmi`/`app.pvm` equal boot's
  artifacts on the cross-module fixtures, ADR-0033) are the correctness gate: they must stay
  green with no change, since output is unchanged. For the perf intent, use a concrete yardstick
  rather than an abstract "roughly linear": re-run the synthetic `let`-chain probe (the exponent
  must drop from ≈1.5 to ≈1.0), and compile the heaviest real module — `Bytecode.Instruction`
  (lowering 18.6 ms, 89 % of its compile, single 378-instruction binding 14.8 ms on V8) — whose
  per-binding lowering should become flat in chunk length. No `Image.format_version` bump — the
  encoding and emitted bytecode are identical (ADR-0033's versioning convention is about output
  meaning, which does not change here).

## Scope

- **In:** the accumulation-discipline change in `Lower`, `Lower/Match`, `Codegen.program`,
  `Link.topo`, and `Build.depOrder`; one shared accumulator utility if helpful. Output bytes,
  ordering, and the `.pmo`/`.pmi`/`.pvm` formats are unchanged.
- **Out:** any change to *what* is emitted (no new optimisation passes — those remain absent in
  Level-2 and are tracked separately, [[optimizer-roadmap]]); orchestration/incrementality/peak
  memory (ADR-0050); the underlying `ulib` `Array.snoc`/`<>` cost itself (a general-purpose array
  append is inherently O(n)-copy on this target — confirmed: it is *not* amortised, on native
  per ADR-0009/0019 nor on JS where `<>` is `concat` and `snoc` is `slice`; native `SetArray`'s
  in-place O(1) is for the mutable-build primitive and does not change `<>`/`snoc` semantics — so
  the fix is to stop using them as accumulators, not to alter `Data.Array`); and the **decode**
  path (`parseModule`), a co-equal but apparently-linear constant (see Profiling) left for
  separate future work on the `argonaut`/`Data.Map` ingestion.

## Consequences

- Per-code-block construction drops from `O(L²)` to `O(L)` and per-`case` from `O(M²)` to
  `O(M)`; link/build ordering from `O(N²)` to `O(N)`. Combined with ADR-0050 this is what brings
  Level-3 `build` to Level-1-comparable scaling.
- Establishes a project convention — *never accumulate an `Array` with `snoc`/`<>` in a
  growing fold or recursion; use a cons-list or `ST` buffer and materialise once* — that applies
  beyond these files (the same trap recurs across `ulib`-backed code).
- Touches the most heavily-tested codegen path; the byte-identity E2E suite makes the refactor
  safe to land independently of ADR-0050.

## Alternatives considered

- **`Array.concat [piece₁, piece₂, …]` instead of `a <> b <> c`.** A single flat `concat`
  precomputes its size and copies each element once, but it does **not** fix the lowering
  recursion: the tail returned by the recursive call is already a materialised array and is
  copied again by the enclosing `concat` at every level — still `O(L²)`. Only an accumulator that
  never re-copies the tail is linear. (A flat `concat` *is* the right tool for the rare
  fixed-arity, non-recursive joins.)
- **A reverse-cons-then-reverse or a forward `ST` push-buffer for `Lower` too** (one uniform
  mechanism everywhere). Rejected: `Lower` reads `length tc` mid-stream for `CIf` jump offsets, so
  a reversed accumulator has the wrong prefix lengths and a single push-buffer would force adding
  label back-patching that Lower does not currently have — both new ways to break byte-identity.
  Boot does not do this; it uses forward list `@`, and the faithful port is `Data.List`. The
  uniform mechanism is right for the *other* sites (no mid-stream length read), hence the two
  regimes.
- **Make `Data.Array` append amortised / use a persistent vector in `ulib`.** Changes a
  general-purpose library's representation and cost model for one consumer's benefit; far larger
  blast radius, and unnecessary once accumulation stops abusing `snoc`/`<>`.
- **Leave it and only fix orchestration (ADR-0050).** Incremental reuse hides the per-function
  quadratic on *rebuilds* but a clean build still pays it on every module; the dominant cost (A)
  would remain.
