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
| [0065](0065-ulib-one-directory-per-registry-package.md) | `ulib` directory = one registry package: split `foldable-traversable`, testing `unfoldable` on its own | Accepted |
| [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) | v1 precise rooting: a shadow-stack root API and GC-on-allocation | Accepted |
| [0067](0067-v1-effect-execution-and-native-leaves.md) | v1 Effect execution | Accepted |
| [0068](0068-purvasm-stdio-and-effect-console-ulib-shadow.md) | `purvasm-stdio` and the `Effect.Console` ulib shadow | Accepted |
| [0069](0069-v1-dynamic-record-operations.md) | v1 dynamic Record operations | Accepted |
| [0070](0070-v1-byneed-recursive-caf-force.md) | v1 by-need recursive CAFs | Accepted |
| [0071](0071-codegen-runtime-c-abi.md) | The codegen↔runtime C-ABI | Accepted |
| [0072](0072-anf-to-llvm-lowering.md) | ANF → LLVM lowering | Accepted |
| [0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) | ulib-shipped native foreign, resolved at link time | Accepted |
| [0074](0074-effect-exception-throw-only-ulib-shadow.md) | `Effect.Exception` as a throw-only ulib shadow | Accepted |
| [0075](0075-cross-backend-wall-clock-benchmark-harness.md) | Cross-backend wall-clock benchmarks | Accepted |
| [0076](0076-direct-known-arity-calls-musttail.md) | Direct known-arity calls and `musttail` TCE on the LLVM backend | Accepted |
| [0077](0077-cross-module-direct-calls-pmi-arity.md) | Cross-module direct calls over the `.pmi` surface | Accepted |
| [0078](0078-rust-foreign-bindgen-over-c-abi.md) | Idiomatic Rust native foreigns as a bindgen-style layer over the `pv_*` C ABI | Accepted |
| [0079](0079-ctx-header-abi-inline-rooting-fast-paths.md) | The context header becomes ABI | Accepted |
| [0080](0080-foreign-signature-reconstruction-cst.md) | Level-2: foreign signatures reconstructed from source, via the embedded CST parser | Accepted |
| [0081](0081-purvasm-regex-pure-ps-es-engine.md) | `purvasm-regex`: a pure-PureScript regex engine | Accepted |
| [0082](0082-native-codegen-port-to-level-2.md) | Porting the native (LLVM) code generator to the Level-2 compiler | Accepted |
| [0083](0083-match-compilation-to-anf-middle-end.md) | Match compilation to a shared ANF middle-end | Accepted (implemented) |
| [0084](0084-binary-pmo-pmi-and-cross-module-summary.md) | The cross-module optimiser summary — an `--opt`-only `.pmi` extension | Accepted |
| [0085](0085-native-build-orchestration-inmemory-summary-env.md) | Native build orchestration over an in-memory summary env | Accepted |
| [0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) | The optimiser/codegen seam over a backend-neutral ANF module | Accepted |
| [0087](0087-backend-neutral-build-driver-compileraction.md) | A backend-neutral build driver over an injected `CompilerAction` | Accepted |
| [0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md) | The VM backend lowers the optimiser seam like native, released from boot byte-identity | Accepted |
| [0089](0089-nbe-general-inliner.md) | The NbE general inliner on the optimiser seam | Accepted |
| [0090](0090-foreign-signatures-threaded-through-build-env.md) | Foreign signatures threaded through the build env | Accepted |
| [0091](0091-user-native-ffi-c-sibling-rust-dir.md) | User-defined native FFI: C siblings vs. one Rust dir | Accepted |
| [0092](0092-no-undeclared-native-leaves-from-intrinsics.md) | Structural intrinsics introduce no undeclared native leaves | Accepted |
| [0093](0093-dictionary-specialization.md) | Dictionary specialization on the optimiser seam | Accepted |
| [0094](0094-foreign-provider-pure-router-structural-folding-to-library-bodies.md) | Foreign provider is a pure router; structural folding moves to library bodies | Accepted |
| [0095](0095-effect-analysis-on-the-optimizer-seam.md) | Effect analysis on the optimiser seam: purity facts unlock dead-drop of pure neutral calls | Accepted |
| [0096](0096-memory-effect-summaries-pure-call-sink.md) | Memory-effect summaries: `mayTouchMutable` through call summaries; single-use pure clean calls sink | Accepted |
| [0097](0097-materialised-size-audit-multi-use-marks.md) | The materialised-size audit: multi-use re-materialising marks are gated on transitive size | Accepted |
| [0098](0098-effect-instance-dict-visibility-grouped-projection.md) | Effect instance-dictionary visibility: folding method projections off recursive dictionary CAFs (the `bindE`/`pureE` exposure GER needs) | Accepted |
| [0099](0099-generalized-effect-reflection-cperform.md) | Generalized effect reflection and impurification: an explicit `CPerform` run-marker and canonical `Effect`/`ST` lowering | Accepted |
| [0101](0101-ulib-tools-unicode-gen-command.md) | `ulib-tools unicode-gen`: fetch, pin, and generate the Unicode simple-case-mapping table, replacing `gen/gen_casemap.py` | Accepted |
| [0102](0102-runtime-v1-1-performance-baseline-for-dynamic-apply.md) | Runtime v1.1 performance baseline for dynamic apply | Accepted |

## Scope
