# Design Decisions

This directory records significant architectural decisions for this project, as lightweight
[ADRs](https://adr.github.io/) (Architecture Decision Records).

Each record captures **one** decision: the context that forced it, the
decision itself, its consequences, and the alternatives that were rejected
and why. A record's original text is never deleted-and-replaced — history is
preserved in place (see [Maintaining records](#maintaining-records)). A
genuinely *reversed* decision is retired by a new record that supersedes it,
not by rewriting the old one.

## Format

```plain
# <NNNN>. <Title>

- Status: Proposed | Accepted | Superseded by <NNNN>
- Date: YYYY-MM-DD

## Context
## Decision
## Consequences
## Alternatives considered
```

## Maintaining records

When a record drifts from the implementation, **do not delete and replace the original
text.** Keep the original readable as history and mark the change in place:

- **Correction / progress addendum** — strike the obsolete text with `~~…~~` and append a
  dated note explaining the change, e.g.
  `> **Correction (YYYY-MM-DD):** …` or `> **Progress (YYYY-MM-DD):** …`.
- **Status promotion** — keep the old status struck through and add the new one with a dated
  rationale, e.g.
  `- Status: ~~Proposed~~ **Accepted** _(YYYY-MM-DD: promoted — implemented in …)_`.
- **Reversal** — a decision that is genuinely overturned (not merely refined) is retired by a
  new record that supersedes it (`Status: Superseded by <NNNN>`), not by rewriting it.
- **The index below is the exception**: it is edited by **direct overwrite** (no strikethrough),
  since it is a derived table that must always show each record's current effective status.

Permanent records here are written in **English**. (Ephemeral working notes may be in any
language and are kept out of version control.)

## Index

| # | Title | Status |
| - | - | - |
| [0001](0001-phase-1-host-language-ocaml.md) | Implement the phase-1 host in OCaml; reject a PureScript-on-V8 seed | Accepted |
| [0002](0002-cesk-execution-model.md) | Start phase 1 with a CESK machine over a minimal strict core | Accepted |
| [0003](0003-stack-based-bytecode.md) | PURVASM bytecode is a stack machine | Accepted |
| [0004](0004-recursion-letrec-fix.md) | Recursion via `letrec` and store-backpatching | Accepted |
| [0005](0005-mutual-recursion-binding-groups.md) | Mutual recursion via recursive binding groups | Accepted |
| [0006](0006-string-utf8-char-int.md) | String as a UTF-8 byte sequence; Char as Int | Accepted |
| [0007](0007-monomorphic-primitives.md) | Monomorphic primitives; type classes stay dictionary-passing | Accepted |
| [0008](0008-number-ieee754-double.md) | Number as an IEEE 754 double (host float) | Accepted |
| [0009](0009-array-immutable-host-backed.md) | Array as an immutable, host-backed vector | Accepted |
| [0010](0010-record-row-polymorphic.md) | Record as an unordered field map | Accepted |
| [0011](0011-adt-pattern-matching.md) | Algebraic data types and `case` pattern matching | Accepted |
| [0012](0012-array-record-binders.md) | Array and record binders | Accepted |
| [0013](0013-case-guards.md) | Guards in `case` alternatives | Accepted |
| [0014](0014-corefn-ingestion.md) | CoreFn ingestion: decode `corefn.json` to a faithful `Corefn` AST | Accepted |
| [0015](0015-corefn-lowering.md) | CoreFn lowering: `Corefn` → `Cesk.Ast` | Accepted |
| [0016](0016-cross-module-linking.md) | Cross-module linking | Accepted |
| [0017](0017-primitive-ffi.md) | Primitive FFI: foreign leaves as eta-expanded primops | Accepted |
| [0018](0018-newtype-erasure.md) | Honour `IsNewtype`: erase newtype wrappers in lowering | Accepted |
| [0019](0019-mutable-array-building.md) | Mutable array building primitives (revising ADR-0009) | Accepted |
| [0020](0020-structural-ffi-guest-code.md) | Structural / higher-order FFI as guest code over first-order primitives | Accepted |
| [0021](0021-reachability-dce.md) | Link only the entry's reachable closure (dead-binding elimination) | Accepted |
| [0022](0022-native-foreign-rung.md) | Native foreign rung: opaque host-provided functions | Accepted |
| [0023](0023-effect-runtime-oracle.md) | Effect runtime at the CESK oracle: thunks, IO leaves, no reflection | Accepted |
| [0024](0024-by-need-recursive-bindings.md) | By-need recursive bindings (refining ADR-0004) | Accepted |
| [0025](0025-lower-ir-anf.md) | Lower IR: ANF with eval/apply, verified by round-trip against the oracle | Accepted |
| [0026](0026-benchmark-harness.md) | Benchmark harness: deterministic step/alloc cost on the oracle | Accepted |
| [0027](0027-dictelim.md) | DictElim: collapse statically-known type-class dispatch | Accepted |
| [0028](0028-copyprop-inline.md) | Copy-propagation and small-callee inlining | Accepted |
| [0029](0029-general-inlining.md) | General inlining (α-renaming) and value folding | Rejected |
| [0030](0030-bytecode-vm-slice1.md) | PURVASM bytecode VM (slice 1): instruction set, ANF codegen, stack interpreter | Accepted |
| [0031](0031-decision-tree-matching.md) | Decision-tree pattern compilation in the VM | Accepted |
| [0032](0032-vm-native-foreign-effect.md) | PURVASM bytecode VM (slice 2): native foreign rung and Effect | Accepted |
| [0033](0033-separate-compilation.md) | Separate compilation: per-module artifacts (`.pvmo`/`.pvmi`), linker, and the `purvm` executable | Accepted |
| [0034](0034-effect-analysis-impurification.md) | Effect-aware optimisation: structural effect analysis, the force/saturation soundness model, and impurification (GER) | Accepted |
| [0035](0035-native-backend-ocaml5-concurrency.md) | Native backend via OCaml 5 codegen; an M:N effect-handler concurrency runtime | Accepted |
| [0036](0036-anf-to-ocaml-value-representation.md) | ANF → OCaml codegen: value representation and calling convention | Accepted |
| [0037](0037-self-hosting-purescript.md) | Self-hosting: reimplement purvasm in PureScript on the boot native backend | Accepted |
| [0038](0038-base-package-and-ulib-patches.md) | A minimal native base (`Purvasm.*`) and a `ulib` of registry-package patches | Accepted |
| [0039](0039-ulib-st-array-and-st-uncurried.md) | `ulib` `Data.Array.ST` over the fixed-length primitive; `ST.Uncurried` structural | Accepted |
| [0040](0040-ulib-testing-strategy.md) | `ulib` testing: upstream suites by representation-seam fidelity; `purs`-side interface verify | Accepted |
| [0041](0041-int-number-conversion-primops.md) | `Int`↔`Number` conversion: a cross-representation primop pair in the `Purvasm.*` ABI | Accepted |
| [0042](0042-data-number-math-native-leaves.md) | `Data.Number` math family as JS-faithful native leaves (not primops, not `ulib`) | Accepted |
| [0043](0043-ulib-tools-build-verify-test.md) | `ulib-tools`: a PureScript CLI to build, interface-verify, and test the `ulib` patches | Accepted |
| [0044](0044-foreign-object-over-data-map.md) | `Foreign.Object a` as a `ulib` newtype over `Data.Map String a` | Accepted |
| [0045](0045-native-cli-run-interpreter-io-leaves.md) | A purvasm-native CLI entry: swap the `Run` interpreter to native IO leaves; never shadow `node-*` | Accepted |
| [0046](0046-argonaut-core-pure-purescript-ulib.md) | JSON in pure PureScript: a backend-agnostic shared parse/print core, with `argonaut-core` as a `ulib` adapter over it | Accepted |
| [0047](0047-ulib-package-manifest-extra-dependencies.md) | `ulib` package manifest: per-patch extra dependencies (in-repo or registry), with validation | Accepted |
| [0048](0048-ulib-tools-test-upstream-suite-execution.md) | `ulib-tools test`: per-package upstream-suite execution, JS-fidelity first; native/bespoke deferred | Accepted |
| [0049](0049-eliminate-superlinear-bytecode-construction.md) | Eliminate super-linear construction in bytecode lowering, match compilation, and link ordering | Accepted |
| [0050](0050-build-streaming-incremental-reuse.md) | Level-2 `build`: per-module streaming orchestration and incremental artifact reuse | Proposed |
| [0051](0051-flatten-json-serialization.md) | Flatten JSON serialization to a single join (the native serialize bottleneck) | Accepted |
| [0052](0052-native-unsafesetbyte-in-place.md) | Native `Purvasm.String.unsafeSetByte` in-place mutation — eliminate the O(output²) string building behind `blit`/`joinWith` | Accepted |
| [0053](0053-cheap-json-objects-corefn-decode.md) | Association-array JSON objects on the CoreFn decode path — remove the `Foreign.Object`/`Data.Map` build that dominates `loadClosure` | Rejected |
| [0054](0054-byte-oriented-json-parser.md) | Byte-oriented `Json.Core.Parser`: scan UTF-8 bytes directly, eliminating the code-point-array parse floor | Accepted |
| [0055](0055-ulib-auto-resolution-relative-to-binary.md) | Resolve `ulib` from `PURVASM_LIB` in the environment (set by the launcher) — no user flag, no binary self-location | Accepted |
| [0056](0056-purvasm-system-host-leaves.md) | Two host-capability packages — `purvasm-system` (env, process) and `purvasm-fs` (file IO) — to retire the `Purvasm.CLI.Native.*` leaf names | Accepted |
| [0057](0057-purescript-ci.md) | PureScript-CI: a standalone `spago` build/test + `purs-tidy` gate parallel to OCaml-CI, under `nix develop`, with its own `ps-ci-gate` | Accepted |
| [0058](0058-ulib-data-array-st-partial-shadow.md) | `ulib` `Data.Array.ST.Partial` shadow over the reified `STArray` (turns the `arrays` suite green) | Accepted |
| [0059](0059-native-abi-value-representation.md) | Native ABI: uniform tagged-word value representation, inline-tagged header, eval/apply two-entry calling convention, and the GC seam (collector policy deferred to wall 2) | Accepted |
| [0060](0060-native-codegen-llvm-owned-runtime.md) | Native codegen via LLVM, with an owned runtime (GC/scheduler/fibers); roots via `gc.statepoint`, `musttail` TCE, heap-continuation fibers, per-module ANF→LLVM | Accepted |
| [0061](0061-capability-local-shared-immutable-gc.md) | Capability-local copying heaps + a shared-immutable heap (no global STW); partition invariant via copy-on-send, shadow-stack roots first, concurrent black-hole for shared CAFs | Accepted |
| [0062](0062-mn-work-stealing-scheduler-fibers.md) | M:N work-stealing scheduler; heap-continuation fibers bound to a Capability (balance at fork); reduction-count preemption (= GC safepoints); io_uring/Waker async I/O; `Ref`/`AVar` | Accepted |
| [0063](0063-runtime-implementation-language-rust.md) | Runtime in Rust: a small `unsafe` GC island (`Value`/`TaggedWord`/`HeapPtr`, explicit field APIs — no `Gc<T>`→`&T`) under a safe scheduler/driver shell; Miri for the island only | Accepted |
| [0064](0064-v1-single-capability-native-abi-codegen-contract.md) | v1 first cut: single-capability native runtime — concrete ABI (63-bit low-tagged immediates → unboxed Int/Char/Bool), `apply`-N + shadow-stack codegen contract, Cheney local GC, boot-parity oracle; all cross-capability deferred to v2 | Accepted |
| [0065](0065-ulib-one-directory-per-registry-package.md) | `ulib` directory = one registry package: split `foldable-traversable`, testing `unfoldable` on its own | Proposed |
| [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) | v1 precise rooting: a `Heap`-owned shadow-stack (`Vec<Value>`) root API with explicit reload-after-safepoint, per-function rooting (self-rooting constructors; `apply` roots over-apply leftovers; `CodeFn`s root their own live values), and `alloc` collects-and-retries (fixed heap, OOM on still-full); the pre-codegen realisation of ADR-0064 §4 | Accepted |
| [0067](0067-v1-effect-execution-and-native-leaves.md) | v1 `Effect` execution: `Effect a` is an ordinary arity-1 closure (thunk), the monad stays compiler-structural, `run_effect` = `apply(main, unit)`, native leaves are Rust `CodeFn`s (C-ABI deferred), lead with value-observable `Effect.Ref` then a generic `stdio_write_line` leaf (adds `new_str` + an injectable output sink, v1 on `Heap`; a boot-parity/smoke-test scaffold until native FFI — `Effect.Console.log` is a `ulib` shadow, not a runtime primitive); behaviour-parity with boot | Accepted |
| [0068](0068-purvasm-stdio-and-effect-console-ulib-shadow.md) | `purvasm-stdio` package (`Purvasm.Stdio.writeLine`/`writeErrLine`, stdout/stderr line leaves) + `Effect.Console` as a `ulib` shadow over it; **removes** the `Effect.Console.log`/`error` special leaves from boot's resolver so no core resolver knows a JS console name (extends the ADR-0067 reframing to boot); no-op timers/grouping natively | Accepted |
| [0069](0069-v1-dynamic-record-operations.md) | v1 dynamic Record ops on the ADR-0059/0064 hash-id layout (`[label_ids → RawIds][values → Array]`, sorted FNV-1a-64 ids, parallel values): add `Kind::RawIds` + `new_record` + `get`/`insert`/`set`/`delete`/`modify` (immutable copy-on-update, self-rooting), dynamic keys hashed at runtime; static/literal labels collision-checked at link (ADR-0059), computed keys carry a documented negligible residual | Accepted |
| [0070](0070-v1-byneed-recursive-caf-force.md) | v1 by-need recursive CAFs: the `ByNeed` cell `[state][result]` as a 3-state (`Unforced`/`Building`/`Forced`) memoising suspension, `force` = black-hole-then-`apply(thunk,unit)`-then-memoise (cell rooted across the safepoint), `apply` auto-forces a `ByNeed` callee, `Grec` groups over a shared env; builds on the ADR-0069 record store (a field store does not force), unblocks the recursive `Monad Effect` dictionary (ADR-0067 §2 prerequisite) | Accepted |
| [0071](0071-codegen-runtime-c-abi.md) | Native runtime C ABI: exported context, allocation, root, apply, effect, record, by-need, and primitive helper surface for LLVM-generated code | Accepted |
| [0072](0072-anf-to-llvm-lowering.md) | ANF → LLVM lowering: per-module textual IR, lambda-lifting, rooting emission, and the native differential | Accepted |

## Scope
