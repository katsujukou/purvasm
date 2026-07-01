# 0067. v1 Effect execution: thunks as closures, native leaves as Rust `CodeFn`s, and `run_effect`

- Status: ~~Proposed~~ **Accepted** _(2026-07-02: accepted by the maintainer)_
- Date: 2026-07-02

## Context

The v1 runtime can allocate, collect, and `apply` closures, but it cannot yet **run a program**: a
compiled entry is `main :: Effect Unit`, and there is no driver to execute it, no way to express the
effectful **leaves** (`Console.log`, `Effect.Ref`), and no realisation of the `Effect` monad.

The semantics are already fixed by two Accepted records:

- **[0023](0023-effect-runtime-oracle.md)** — `Effect a` is a **nullary thunk** `Unit -> a`:
  `pureE a = \_ -> a`, `bindE a f = \_ -> f (a unit) unit`. The whole monad + combinators are
  **structural guest terms** (ADR-0020 rung), produced by the *compiler*, not the runtime; correct
  sequencing falls out of strict evaluation. A leaf (`Console.log`) resolves to a foreign that
  **returns the `Effect` thunk**, and *forcing that thunk performs the real effect* — the "force is
  the `Perform`" contract. A top-level `main` runs by applying it to `unit`.
- **[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §6** — for the native runtime:
  `run_effect` applies `main` to unit; synchronous `Effect` runs direct-style; native leaves are
  `extern "C"` functions kept at parity with `boot`'s `Ffi.host` by the differential.

What §6 leaves to a v1 realisation (this record): the concrete **thunk representation**, how leaves
are provided **before codegen exists** (there is no `extern "C"` caller yet, mirroring the code-table
stand-in of [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3), where **effectful
runtime state** (an output sink) lives, and how effect output is **captured** for the differential.

## Decision

### 1. `Effect a` is an ordinary closure — no new object kind

A nullary thunk `Unit -> a` is exactly an **arity-1 `Closure`** over its
captured environment. `pureE x` is a closure capturing `x`; `bindE`/combinators are the compiler's
structural guest closures ([0023](0023-effect-runtime-oracle.md)). **The runtime implements none of the
`Effect` monad** — it only *forces* thunks, and forcing is `apply(thunk, [unit])`. No `Effect`/thunk
`Kind`, no `ByNeed` (that is for recursive-binding black-holing, ADR-0024), no new value form.

### 2. `run_effect` is the driver

`run_effect(main) = apply(main, unit())` — evaluate the entry thunk by applying it to the immediate
`unit`, and return the final value. Direct-style: the structural `bindE` chain drives sub-effects in
program order under strict `apply`; the driver adds no scheduling (that is v2,
[0062](0062-mn-work-stealing-scheduler-fibers.md)).

**Scope of the first run target.** This record fixes `run_effect`, the leaves, and the *forcing* of
`Effect` thunks — nothing more. It does **not** implement the **by-need recursive-CAF** mechanism
(`Grec` → a black-holed `ByNeed` cell forced on first use and memoised, ADR-0024) that a *compiled*
program needs to build the recursive **`Monad Effect` instance dictionary**:
[0032](0032-vm-native-foreign-effect.md) records that `ap monadEffect` saturates and **forces** a
sibling *during* the group's construction, so the group must be built **by-need**, not eager. v1's
first run targets are therefore **hand-built / leaf-driven `Effect` terms** (as the unit tests
construct them) that do not thread the recursive dictionary. Running a **full compiled program**
through the ordinary `Monad Effect` dictionary is gated on the **by-need `ByNeed`-force increment**
(the `Kind::ByNeed` cell exists, but its force + black-hole are not yet built) — a **named
prerequisite tracked separately**, not folded in here. Keeping `run_effect` correct is independent of
that gate; conflating them would stall this slice on the CAF machinery.

### 3. Native leaves are Rust `CodeFn`s (a v1 stand-in for `extern "C"`)

A native leaf is a `CodeFn` like any other closure body (interned in the code table,
[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3): the runtime constructs closures
whose bodies perform the leaf. The `extern "C"` C-ABI arrives with codegen — until then leaves are
plain Rust `CodeFn`s, so the parity target is behaviour, not the calling convention. A **curried**
leaf (`log :: String -> Effect Unit`) is an outer `CodeFn` `\s -> thunk` returning the **thunk**
closure whose body performs the effect when forced — the [0023](0023-effect-runtime-oracle.md)
"foreign returns the thunk; its force performs" shape, realised with two `CodeFn`s.

**Leaf rooting follows [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §3.** A native leaf is
an ordinary `CodeFn` and is bound by the same per-function rooting contract. In particular, a leaf that
**allocates a returned thunk** captures its arguments into that thunk's env, and *allocating the thunk
is a safepoint*, so the captured `Value`s must survive it. The **self-rooting constructors** discharge
this by construction: `log s` builds its thunk with `new_closure`, whose `env` is self-rooted, so the
common **single-capture** case needs no extra ceremony. A leaf with **multiple heap captures** must
place them in a **rooted env object built before any allocation crosses a safepoint** — root the
captures, build the env `Array` (itself self-rooting), then the closure — never a `new_closure_raw`
with a hand-assembled env `Value` held live across an intervening safepoint.

### 4. Lead with `Effect.Ref` — value-observable, no IO sink

Per [0023](0023-effect-runtime-oracle.md), the first, primary vehicle is **`Effect.Ref`**
(`new`/`read`/`write`/`modify`) over the existing `Ref` cell (`new_ref` exists; `read`/`write` are the
value-slot accessors). A program like `do r <- new 0; modify (_+1) r; read r` returns `1`, so
**sequencing and state are asserted as the returned value** — no stdout, no new machinery beyond the
leaves. This is the clean first slice.

**`modify f` must root across the `apply` (the earliest safepoint trap).** `modify f` is
read → `apply f` → write, and **`apply f` is a safepoint**: it can relocate the `Ref` cell (and the
read value, if it is a pointer). So the `modify` leaf **roots the `Ref` cell across `apply f` and
reloads it before the write** ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §3) — writing
through a `Ref` pointer captured *before* `apply f` would target a stale cell. The checked field API
would *panic* on such a stale pointer, but `run_effect`'s core **must not rely on that panic**: every
native leaf obeys the per-function rooting contract. `modify` is small but is the **first place a real
program trips the safepoint rule**, so its rooting is fixed here, not left to the implementer.

### 5. `Console.log` — `String` construction + an injectable output sink

> **Correction (2026-07-02):** the runtime should **not** own a `Console.log` primitive. `Console.log`
> is a JS-derived name and shape; a native-backend core runtime knowing it directly is a layering
> smell. Revised realisation: the runtime provides **one generic IO leaf, `stdio_write_line`** (the
> eventual `extern "C"` `purvasm_stdio_write_line`), framed as a **boot-parity / smoke-test scaffold
> for the FFI-absent period — not a permanent first-class runtime feature**. `Effect.Console.log` is
> instead a **`ulib` shadow** over a `Purvasm.Stdio.writeLine`-style API that calls this leaf. When
> user-defined (C) native **FFI** lands, stdio drops to an ordinary **library** leaf and the runtime
> keeps only the FFI-call mechanism + value conversion + effect-thunk forcing — it forgets individual
> leaf names. `Effect.Ref` (§4) remains the *core* runtime bring-up vehicle; the write-line leaf is
> merely the FFI-absence foothold. The `Str` ABI and the output sink below are unaffected (they stay in
> the runtime); only the leaf's **name and status** change — read "`Console.log`" below as the generic
> `stdio_write_line` leaf, with `Effect.Console.log` as its `ulib` shadow.

The canonical demo needs a `String` and an output boundary:

- **`String` construction and `Str` ABI**: add `new_str(bytes)` building a `Str` object and a reader —
  the first use of the `Str` kind. The representation is fixed here so `Console.log` and every future
  string primitive share it: payload `[len: raw][utf8 bytes: raw]` (ADR-0064 §2), where `len` is the
  **byte length** (raw word 0), and the bytes occupy the following words **in ascending string-index
  order in memory** — byte `i` of the string at payload byte offset `i`, so a `char*` / ptr+len reader
  (the future C/LLVM boundary) sees the bytes directly, with **no word-level byte-order dependence** —
  the trailing partial word **zero-filled**, so `size_words = 1 + ceil(len / 8)`. `new_str` **asserts
  valid UTF-8** and *owns* that invariant — every reader may then assume valid UTF-8 with no re-check. The **empty string** is a
  valid `Str` (`len = 0`, `size_words = 1` — the `len` word alone satisfies the `size >= 1` header
  invariant; strings are not arrays, so the empty-*array* singleton deferral does not apply). Bytes are
  raw (never scanned by GC, §2).
- **Output sink**: `Console.log`'s effect appends to an **injectable sink**, *not* hard-coded
  `stdout`, so tests and the differential assert output deterministically. In v1 the sink is
  **runtime-context state**; because the `CodeFn` context is `&mut Heap`, v1 co-locates the sink on
  the `Heap` (which already *is* the universal runtime context threaded to every leaf). This is a
  labelled layering bleed — the sink is safe-shell IO state, not GC-island state
  ([0063](0063-runtime-implementation-language-rust.md)); when the scheduler/IO driver lands it moves
  to a dedicated runtime/`Capability` context and the leaf `CodeFn` context widens past `Heap`.
  Production wires the sink to real `stdout`; tests read the captured lines.
- **Failure, normalisation, comparison unit**: a **sink-write failure is a fatal abort** — v1 has no
  effect-error channel, and a leaf cannot return an error into pure `Effect Unit`. `Console.log`
  appends **one entry per call**: the argument `String`'s UTF-8 bytes as a **single logical line**
  (the newline is the sink's line separator, matching `console.log`'s line semantics — the runtime does
  not embed a trailing `\n` in the stored entry). The **differential compares this normalised line
  sequence** against `boot`'s `Ffi.host` output line-for-line, *not* raw byte streams, so newline
  convention and buffering never enter the comparison. An invalid-UTF-8 argument cannot occur (the
  `new_str` invariant above), so it is a runtime bug, not a handled case.

### 6. Soundness contract and validation

- **Effect-soundness** ([0023](0023-effect-runtime-oracle.md)): effects run in **program order**, each
  `Perform` **exactly once** (never dropped/duplicated). In v1 this falls out of strict `apply` +
  structural `bindE`, with no optimiser to break it.
- **Forced-GC coverage** ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)): an effect program
  sized to collect mid-run must still return the same value and emit the same output — a `Ref` or a
  thunk env surviving a collection is the rooting contract under real effects.
- **Differential** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §7): leaves are
  held at parity with `boot`'s `Ffi.host` — value-observable `Ref` programs by return value, `log`
  programs by captured output.

### 7. Interaction with GER / impurification (ADR-0034)

The thunk model here is the **un-optimised** `Effect` representation and the **semantic ground truth**.
When the optimiser's GER / impurification lands, `bindE eff1 (\a -> rest)` — semantically
`\_ -> rest (eff1 unit) unit` — collapses to `let a = <perform eff1> in rest`, erasing the thunk
plumbing (`\_ ->`, the `unit` applications, the `bindE` closure). This does **not** obsolete this
record, for three reasons:

- **The native leaves are invariant.** Impurification erases the guest-side monadic *plumbing*, not a
  leaf's IO. `perform_log`, the `Ref` ops — the effectful runtime functions — are unchanged;
  impurification only changes whether guest code reaches them by **forcing a thunk**
  (`apply(thunk, unit)`) or by a **direct call**. The runtime exposes one set of native leaves for
  both call shapes.
- **The thunk representation is a required fallback, not merely a slow path.** Impurification is
  *partial*: it can only erase effects whose sequencing is statically visible (a `bind` position it can
  see). A first-class `Effect a` that **escapes** — stored in a data structure, passed polymorphically,
  returned from a higher-order function — cannot be impurified and **stays a thunk**. So
  `Effect a = closure` (§1) is needed regardless; impurification is a fast path *over* it for the
  visible cases — the "where does the effect fire" force/saturation problem of
  [0034](0034-effect-analysis-impurification.md).
- **This is the oracle reference.** The un-optimised thunk semantics *define* the effect sequence
  (program order, each `Perform` once — [0023](0023-effect-runtime-oracle.md)) that impurified code
  must reproduce; the §6 differential is what proves the optimiser preserved it.

What impurification *does* change is the **effect calling convention at the ABI**: an impurified `main`
is invoked directly rather than `apply(main, unit)` (its `Unit` argument is erased with the plumbing),
and a leaf in a visible `bind` position is called directly rather than through its thunk. Those are
codegen/ABI decisions for the **impurification-on-native increment** (a future record extending
[0034](0034-effect-analysis-impurification.md) to the native backend), not changes to the leaves or the
thunk representation this record fixes. v1 has no optimiser, so v1 runs the thunk path — and keeps it as
the escaping-effect fallback thereafter.

## Consequences

- A `main :: Effect Unit` runs end-to-end: `run_effect` + the `Ref` leaves give value-observable
  effects immediately; `Console.log` adds the stdout demo once `Str` + the sink land.
- The thunk representation survives the optimiser: impurification is a codegen fast path over the same
  native leaves, and escaping/first-class effects keep the thunk form — so this record is not throwaway
  when GER lands (see §7).
- No new object kind and no monad logic in the runtime — the runtime stays a forcer of thunks; the
  `Effect` monad remains the compiler's structural guest code, so the runtime and the oracle share one
  definition of effect sequencing.
- Leaves as Rust `CodeFn`s keep the C-ABI decision with codegen; only *behaviour* parity is committed
  now.
- The output sink is a **labelled v1 layering compromise** (IO state on the GC-island `Heap`),
  scheduled to move to the safe-shell driver with the scheduler.

## Alternatives considered

- **A dedicated `Effect`/thunk `Kind`.** A distinct object form for suspended effects. Rejected: an
  `Effect` is representationally an ordinary closure (ADR-0023), and a new kind would need GC-scan and
  `apply` support for no semantic gain; forcing is already `apply(thunk, unit)`.
- **`extern "C"` leaves now.** The eventual ABI ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3/§6),
  but there is no codegen caller yet, so a C-ABI boundary would be exercised only by hand-written
  tests — premature. Rust `CodeFn`s match the code-table stand-in and defer the boundary to codegen.
- **Hard-coded `stdout` for `Console.log`.** Simplest, but not assertable in unit tests nor
  byte-comparable in the differential without capturing a real process's output. An injectable sink is
  testable and still wires to `stdout` in production.
- **Lead with `Console.log` (stdout-first).** Iconic but needs `String` + an output boundary and is
  awkward to assert; `Effect.Ref` is value-observable and leads, exactly as ADR-0023 chose for the
  oracle.
- **Implement `bindE`/`pureE` as runtime primitives.** Faster than structural guest terms, but it
  forks effect sequencing away from the compiler/oracle definition and re-introduces the
  reflection/impurification hazards ADR-0023 keeps in the optimiser. The monad stays guest code.
