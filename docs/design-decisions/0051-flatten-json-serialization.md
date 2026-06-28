# 0051. Flatten JSON serialization to a single join (the native serialize bottleneck)

- Status: ~~Proposed~~ **Accepted** _(2026-06-29: maintainer explicitly accepted; prototype validated during investigation)_
- Date: 2026-06-29

> **Progress (2026-06-29).** Implemented in `Bytecode/Image.purs`: `stringify` flattens the `Json`
> tree into a reversed token `List` (each leaf — `jstr`, `show n`, bracket, separator — built once
> and consed O(1)), reversed once, and `joinWith ""`-ed a single time; `jstr` is unchanged (a
> fast path measured as no help). **Validation:** unit 59/59 + E2E 3/3 green, including the
> `.pmo`/`.pmi`/`app.pvm`-equal-boot byte-identity tests — output unchanged, no `format_version`
> bump. **Native effect:** per-module serialize 43–57 s → 3.5–5.5 s (~10×); cold compile-all of the
> 227-module self-host closure went from *not finishing* to ~207 s. The canonical native Level-3
> compiler (`output-lv3/app`) was rebuilt with the fix. The remaining cold-build cost is now the
> **link step** (in-memory merge + reachability walk + whole-program `imageToString`) — the next
> target (see ADR-0050 review).

## Context

After ADR-0049 removed the algorithmic super-linearity in artifact *construction*, a native
Level-3 build still felt no faster, so the per-module native cost was profiled end-to-end (built
old vs new Level-3 with `purvm native -m Purvasm.CLI.Native --ulib dist/ulib`, timed single-module
`compile`). The result overturned the working assumption that decode dominates:

- **Decode is cheap.** `loadClosure` (argonaut `jsonParser` + `decodeModule` over every module in
  the entry's closure) takes ~3 s for the *whole* ~227-module closure — not the bottleneck. (The
  ulib `Json.Core.Parser` is array-index based and measured perfectly linear, ~0.10 µs/byte, on
  the JS path.)
- **Compile is cheap.** Decode + `compileModule` (translate → normalize → lower, including the
  ADR-0049 lowering) is ~0.5 s per module, even for the heaviest ones.
- **Serialization is ~all of it.** `moduleToString` (the `.pmo` JSON) alone is **43–57 s** for a
  big module (`Bytecode.Instruction` 0.48 s compile-only → 43 s with `moduleToString`;
  `CoreFn.Expr` 57 s; `Data.Map.Internal` 44 s) — ~98 % of per-module time, ~0.5 s per KB of
  output on native. Splitting it further: building the `Json` tree (`moduleToJson`) is ~0.5 s; a
  per-character `jstr` fast path made **no** difference; the cost is entirely in **`stringify`**.

Root cause: `stringify` builds the output recursively as
`"[" <> joinWith "," (map stringify xs) <> "]"` (and the object analogue). Each array/object node
re-concatenates its *whole subtree string* — twice for the brackets, and again as an element of
its parent's `joinWith`. On a backend where `String` is a flat UTF-8 byte sequence and `<>`
(`concatString`) is an **O(n) byte copy, not a V8-style rope** (ADR-0006), a byte at depth `d` is
copied O(d) times, so a tree with tens of thousands of nodes costs ~O(output × depth) with a
catastrophic constant. This is the **same class of defect as ADR-0049** (repeated `<>`/`snoc`
accumulation) — on the *output* side, which ADR-0049 explicitly scoped out — and it is invisible
on the JS/V8 path (ropes make nested `<>` cheap), which is why it was never caught.

This is the dominant term in *cold* build time (incremental reuse, ADR-0050, does not help a cold
build), so it is the right thing to fix first.

## Decision

Rewrite `stringify` (`Bytecode/Image.purs`, the encoder shared by `moduleToString` /
`interfaceToString` / `imageToString`) to **flatten the `Json` tree into an ordered token list,
then `joinWith ""` exactly once.** Each leaf string (`jstr`, `show n`, a literal bracket /
separator) is built once and prepended O(1) to an accumulator (reversed cons, reversed once — the
ADR-0049 discipline); `joinWith` already precomputes the total length and fills a single buffer in
one pass, so every byte is copied a constant number of times. The token sequence is exactly the
old concatenation order, so **output is byte-for-byte identical** — the same `.pmo`/`.pmi`/`.pvm`
bytes, no `Image.format_version` bump.

`jstr` is left as-is: profiling showed the per-string escaping is not the cost (a fast path for
escape-free strings made no measurable difference), so adding one would be unjustified complexity.

## Scope

- **In:** the `stringify` rewrite in `Bytecode/Image.purs`. It benefits all three serialisers that
  share it, including the linker's `imageToString`.
- **Out:**
  - The **link step**, now the next dominant cost: a cold full build compiles all 227 modules in
    ~207 s (was: it could not finish 88 in 240 s) and then stalls in `link` — the all-in-memory
    name-graph merge + reachability walk + the single giant `imageToString` of the whole program.
    `imageToString` gets faster for free here, but the in-memory merge and peak residency are
    ADR-0050 territory; a follow-up should profile the merge itself.
  - A **single-buffer, two-pass serialiser** (pass 1 sums the exact byte length, pass 2 fills one
    preallocated `Purvasm.String` buffer with no intermediate token strings at all) — a further
    constant-factor win over flatten-then-join, deferred as a larger rewrite if link-time
    serialization proves to still matter.

## Consequences

- Per-module serialization drops ~10× on native (43–57 s → **3.5–5.5 s**); cold compile-all of the
  227-module self-host closure goes from *never finishing* to **~207 s**. Byte-identity is
  preserved (unit 59/59 + E2E 3/3 green, including the `.pmo`/`.pmi`/`app.pvm`-equal-boot tests).
- Confirms the ADR-0049 principle as a project-wide rule with teeth: *never build a large string by
  nested/repeated `<>` on this backend* — `<>` is an O(n) copy, so accumulate flat and join once.
  The same trap to watch for anywhere output is assembled.
- Re-points the optimisation effort: decode and compile are not the levers; after this, the link
  step (merge + whole-image serialize) is the next cold-build target.

## Alternatives considered

- **Single mutable byte-buffer, two-pass (size then fill).** Fastest (no intermediate token
  strings), but a larger rewrite and more error-prone for byte-identity; flatten-then-`joinWith`
  is the minimal change that removes the asymptotic re-copying and already wins ~10×. Kept as a
  deferred further step.
- **A per-string `jstr` fast path** (skip per-char escaping when none is needed). Measured: no
  effect — `jstr` is not the bottleneck — so rejected as needless complexity.
- **Leave serialization; rely on incremental reuse (ADR-0050).** Rejected: reuse skips *unchanged*
  modules but does nothing for a cold build, where every module is serialized once; the maintainer's
  requirement is that the cold build itself be fast.
- **Bump `format_version`.** Unnecessary: the emitted bytes are identical; the version stamp tracks
  output *meaning*, which does not change (ADR-0033 convention).
