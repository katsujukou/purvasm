# 0074. `Effect.Exception` as a throw-only ulib shadow

- Status: ~~Proposed~~ **Accepted** _(2026-07-04: accepted by the maintainer, with the review revision folded in)_
- Date: 2026-07-04

## Abstract

`Effect.Exception` as a throw-only `ulib` shadow: pure-PS `Error`, `throwException` = stderr + `exit 1` (fatal, unrecoverable), `catchException` passes through; LLVM runtime completes the `writeErrLineImpl`/`exitImpl` leaves under a write-before-exit contract (stderr direct+flush; exit drains the stdout sink); real unwinding deferred

> **Revision (2026-07-04, pre-acceptance review):** four findings folded in. **(1)** "no runtime
> change" was inaccurate for the LLVM backend: the two capability leaves the shadow composes
> exist in boot's `Ffi.host` but not yet as runtime `pvf_*` symbols — §5 now puts that **leaf
> completion in scope** (no *new* capability name, but new `pvf_*` exports). **(2)** §5 pins the
> **write-before-exit contract**: the stderr leaf is a direct write + flush (never the drained
> stdout capture sink), and the exit leaf **drains the stdout sink before terminating** — else
> `throwException`'s message (or the program's own captured output) would be lost. **(3)** §3
> states explicitly that no catchable exception is introduced, so ADR-0034's effect-analysis/DBE
> premises are unchanged. **(4)** §2's `unsafeCoerce` is confined to a local `unsafeUnreachable`
> per the project's `unsafe` convention.

## Context

The `transformer` example (a `ReaderT`/`StateT`/`ExceptT`-over-`Effect` monad stack) fails at
startup on every non-JS backend with `unbound foreign: Effect.Exception.error` /
`…throwException`. Its `catchError` is the pure `ExceptT` instance; the only `Effect.Exception`
use is the driver's escape hatch `either (describe >>> Exn.throw) pure` — and on the example's
actual input the throw never even fires. It still crashes: `Effect.Exception.throw` is a
top-level strict CAF (`throwException <<< error`), so building the global table forces both
foreign values at init ([0072](0072-anf-to-llvm-lowering.md) §3 eager `Gcaf` init; the VM/OCaml
paths fail the same way).

This is a known frontier, not a new one: [0048](0048-ulib-tools-test-upstream-suite-execution.md)
recorded that boot lacks `Effect.Exception`/`throwException`, blocking every `Test.Assert`-based
upstream suite from running natively (its Phase-2 deferral).

The registry `exceptions` 6.1.0 exports `Error` (foreign data), `catchException`, `error`,
`errorWithCause`, `errorWithName`, `message`, `name`, `stack`, `throw`, `throwException`, `try` —
10 JS foreigns, of which only `throwException`/`catchException` are genuinely effectful; the rest
construct/inspect an `Error` value.

Two facts shape the native answer:

- **True `catchException` needs runtime unwinding that v1 does not have.** The runtime's only
  non-local exit is the Rust panic, and [0071](0071-codegen-runtime-c-abi.md) §7 deliberately
  *aborts* at the FFI boundary (a panic never unwinds through LLVM frames — that is UB). A real
  catchable guest exception is a runtime/codegen feature (an unwind scope or an Either-encoding
  of every `Effect`), not a leaf.
- **Native leaves do not throw.** The leaf convention composes failure as values
  (`Maybe`/`Either` in PureScript — [0045](0045-native-cli-run-interpreter-io-leaves.md) §4), so
  unlike Node — where host APIs throw and `try`/`catchException` have real work — the only
  exception source a native program has is its *own* `throwException`.

## Decision

Add a `ulib/exceptions/` shadow of `Effect.Exception` ([0065](0065-ulib-one-directory-per-registry-package.md);
export list identical to upstream, all 10 foreigns dropped), with **throw = fatal abort** and
**no catching**, over existing capabilities only — no *new* capability or leaf name, no resolver
change (the [0068](0068-purvasm-stdio-and-effect-console-ulib-shadow.md) pattern: no core resolver
learns a JS name). The LLVM runtime does not yet export the two capability leaves the shadow
composes; completing them (as `pvf_*` symbols) is part of this record's scope — §5.

### 1. `Error` is a pure PureScript value

`data Error = MkError { name :: String, message :: String, cause :: Maybe Error }`, constructor
unexported (`Error` stays abstract, as upstream's foreign data). `error m` = name `"Error"`;
`errorWithName`/`errorWithCause` fill the obvious fields; `message`/`name` project; `stack` is
`const Nothing` (there is no native stack trace; upstream's type is already `Maybe String`); the
`Show` instance renders `name <> ": " <> message` (upstream shows the JS stack when present — a
documented cosmetic divergence).

### 2. `throwException` is a fatal, unrecoverable abort

```purescript
throwException e = do
  writeErrLine (renderError e)   -- Purvasm.Stdio, ADR-0068
  exit 1                          -- Purvasm.System.Process, ADR-0056
  unsafeUnreachable              -- local, unexported (CLAUDE.md `unsafe` convention):
                                 -- the one place the never-returning `exit` meets the type
```

Message to stderr, exit code 1 — matching Node's uncaught-exception observable behaviour (stderr
+ exit 1) minus the stack trace. `throw = throwException <<< error` stays upstream's one-liner.
The `unsafeCoerce` that conjures the unreachable `a` after `exit` is confined to a module-local
`unsafeUnreachable :: Effect a` (never exported; the public surface stays upstream's).

### 3. `catchException` passes the action through; `try` follows

`catchException _ action = action`, and `try` keeps upstream's pure definition (so
`try a = Right <$> a`). This is **coherent, not a silent lie**, because §2 makes every throw
fatal: no exception ever reaches a handler, so a handler that never runs and a handler that
cannot run are observationally identical *up to the abort itself* — and the abort is loud
(stderr + exit 1) at the throw site. The one behavioural divergence from JS is therefore always
visible: a program that on Node throws, catches, and recovers will on native abort at the throw.
It can never silently produce a wrong value.

**Effect-analysis premises unchanged ([0034](0034-effect-analysis-impurification.md)).** This
shadow introduces **no catchable exception**: `throwException` is process termination composed
from ordinary effectful leaves, and `catchException` observes nothing — so the analysis's
standing model (no catch primitive; the pure-exception-reification leak arises only *under* a
`try`/`catch` the platform lacks; DBE's partial-correctness relaxation) is untouched. The
"revisit may-throw once a catch primitive exists" trigger [0034](0034-effect-analysis-impurification.md)
reserved fires with the future real-unwinding record (below), not with this one.

### 4. `ulib.json`

`dependencies`: `purvasm-stdio`, `purvasm-system` (both in-repo, resolved in-repo-first per
[0047](0047-ulib-package-manifest-extra-dependencies.md)), plus `maybe`/`either` as imports
require. No `test` block: the upstream `exceptions` suite exercises catch/recover semantics this
shadow deliberately does not have; it would need an `xfail` of most of the suite, which is
miscategorisation by [0048](0048-ulib-tools-test-upstream-suite-execution.md)'s own rule. The
shadow's behaviour is covered by the examples multi-backend sweep (`examples/run-examples.sh`)
and, later, by the real-unwinding record's tests.

### 5. LLVM runtime leaf completion, and the write-before-exit contract

Boot's `Ffi.host` already carries `Purvasm.Stdio.writeErrLineImpl` ([0068](0068-purvasm-stdio-and-effect-console-ulib-shadow.md))
and `Purvasm.System.Process.exitImpl` ([0056](0056-purvasm-system-host-leaves.md)); the Rust
runtime exports neither (`leaf.rs` has `pvf_Purvasm_2eStdio_2ewriteLineImpl` only). So "runs on
all backends" requires completing the two **existing** leaves on the LLVM runtime —
`pvf_Purvasm_2eStdio_2ewriteErrLineImpl` and `pvf_Purvasm_2eSystem_2eProcess_2eexitImpl`
([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) naming) — as part of this
record. No new capability *name* enters any resolver; the runtime catches up to boot's surface.

Their semantics carry a **write-before-exit contract**, pinned here because the v1 runtime's
stdout is a *captured sink drained at exit* (`pv_drain_output`, [0067](0067-v1-effect-execution-and-native-leaves.md)
§5) and a naïve port would silently lose output:

- **`writeErrLineImpl` writes stderr directly and flushes per call** — it must **never** ride the
  drained capture sink, else `exit` skips the drain and `throwException`'s message vanishes.
  (stderr is not part of the differential's captured-stdout comparison, so a direct write also
  keeps the [0067](0067-v1-effect-execution-and-native-leaves.md) §5 capture model intact.)
- **`exitImpl` drains the stdout sink (the `pv_drain_output` step) before terminating** with the
  given code — else a program's own already-captured `Console.log` output is lost on any exit
  path, `throwException` included. Exit is the one leaf allowed to run the drain mid-program,
  precisely because nothing runs after it.

### Deferred: real catchable exceptions

A future record owns catchable native exceptions — either a runtime unwind scope
(`pv_try`/landing-pad design through the [0071](0071-codegen-runtime-c-abi.md) boundary, with
shadow-stack frame restoration) or a compiler Either-encoding of throwing `Effect`s. When it
lands, this shadow's §2/§3 are replaced by real leaves and this record is superseded. Until then,
`Test.Assert`-style suites (which only *throw* on failure — the [0048](0048-ulib-tools-test-upstream-suite-execution.md)
Phase-2 blocker) are already served by throw-only.

## Consequences

- `transformer` runs on all backends and the examples sweep goes 7/7 (with the `record-studio`
  ulib patch); any program whose exception use is throw-on-fatal — including `Test.Assert`
  upstream suites, unblocking part of [0048](0048-ulib-tools-test-upstream-suite-execution.md)
  Phase 2 — becomes runnable natively.
- The divergence surface is exactly one behaviour, and it is loud: catch-and-recover programs
  abort at the throw instead of recovering. Documented here; superseded by the real-unwinding
  record.
- No new capability name in any resolver: pure ulib + two existing capability packages, with the
  LLVM runtime completing those packages' two missing `pvf_*` leaves under the write-before-exit
  contract (§5). The JS/stock-purs build keeps upstream's real exceptions (the shadow applies
  only on the native overlay).

## Alternatives considered

- **Native `Effect.Exception.*` leaves in boot/runtime.** Re-introduces JS names into core
  resolvers — the exact layering [0067](0067-v1-effect-execution-and-native-leaves.md)/[0068](0068-purvasm-stdio-and-effect-console-ulib-shadow.md)
  removed — and still cannot implement catching. Rejected.
- **`catchException` crashes when called** (instead of passing through). Louder, but it turns
  every *innocent* `try` around a non-throwing action (the common native case — leaves do not
  throw) into a crash, breaking programs that would run correctly. Pass-through fails only where
  behaviour would genuinely diverge. Rejected.
- **Implement real unwinding now.** The honest fix, but it is a runtime/codegen feature crossing
  the [0071](0071-codegen-runtime-c-abi.md) boundary design (panic ≠ guest exception; shadow-stack
  and trampoline state must be restored at the catch) — far beyond what the examples/suites need
  today. Deferred to its own record.
- **`throwException` via `Partial._crashWith`** (the existing pure crash leaf). One dependency
  fewer, but it is a *pure* stuck/panic: the message formats as a crash, the exit path is the
  runtime's abort (not exit code 1), and it wrongly classifies an `Effect` as a pure partial
  function. The stdio+exit composition matches Node's observable contract better. Rejected.

> **Refinement (2026-07-04, maintainer suggestion during implementation):** §2's `unsafeCoerce`d
> `unsafeUnreachable` is superseded by typing the capability honestly:
> `Purvasm.System.Process.exit : Int -> Effect Void` (it never returns — an [0056] surface
> refinement), so `throwException`'s tail is `absurd <$> exit 1` — **total, no `unsafeCoerce`
> anywhere**, the never-returns invariant carried by the type system rather than a confined
> unsafe. Callers wanting `Effect Unit` `void` the result (the CLI's two `exit 1` sites).
>
> **Progress (2026-07-04):** Implemented. The runtime exports
> `pvf_Purvasm_2eStdio_2ewriteErrLineImpl` (direct stderr write + flush per call, never the sink)
> and `pvf_Purvasm_2eSystem_2eProcess_2eexitImpl` (drains the stdout sink via `pv_drain_output`,
> then terminates) — the §5 contract, unit-tested (`writeErrLine` bypasses the sink) and
> e2e-tested (`llvm_run` of *write-then-`exit 0`* emits the written line; llvm-only, since the
> oracle's `exitImpl` would terminate the test runner). `ulib/exceptions/Effect.Exception.purs`
> ships the shadow (§1–§4 as specified, `unsafeUnreachable` local); `verify` reports it
> interface-faithful. The examples sweep is **7/7 across VM / OCaml-native / LLVM-native / JS**
> (`transformer` green; its XFAIL entry removed from `examples/run-examples.sh`).
