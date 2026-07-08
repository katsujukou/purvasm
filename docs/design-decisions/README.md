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
| [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) | v1 precise rooting: a `Heap`-owned shadow-stack (`Vec<Value>`) root API with explicit reload-after-safepoint, per-function rooting (self-rooting constructors; `apply` roots over-apply leftovers; `CodeFn`s root their own live values), and `alloc` collects-and-retries (fixed heap, OOM on still-full); the pre-codegen realisation of ADR-0064 §4 | Accepted |
| [0067](0067-v1-effect-execution-and-native-leaves.md) | v1 `Effect` execution: `Effect a` is an ordinary arity-1 closure (thunk), the monad stays compiler-structural, `run_effect` = `apply(main, unit)`, native leaves are Rust `CodeFn`s (C-ABI deferred), lead with value-observable `Effect.Ref` then a generic `stdio_write_line` leaf (adds `new_str` + an injectable output sink, v1 on `Heap`; a boot-parity/smoke-test scaffold until native FFI — `Effect.Console.log` is a `ulib` shadow, not a runtime primitive); behaviour-parity with boot | Accepted |
| [0068](0068-purvasm-stdio-and-effect-console-ulib-shadow.md) | `purvasm-stdio` package (`Purvasm.Stdio.writeLine`/`writeErrLine`, stdout/stderr line leaves) + `Effect.Console` as a `ulib` shadow over it; **removes** the `Effect.Console.log`/`error` special leaves from boot's resolver so no core resolver knows a JS console name (extends the ADR-0067 reframing to boot); no-op timers/grouping natively | Accepted |
| [0069](0069-v1-dynamic-record-operations.md) | v1 dynamic Record ops on the ADR-0059/0064 hash-id layout (`[label_ids → RawIds][values → Array]`, sorted FNV-1a-64 ids, parallel values): add `Kind::RawIds` + `new_record` + `get`/`insert`/`set`/`delete`/`modify` (immutable copy-on-update, self-rooting), dynamic keys hashed at runtime; static/literal labels collision-checked at link (ADR-0059), computed keys carry a documented negligible residual | Accepted |
| [0070](0070-v1-byneed-recursive-caf-force.md) | v1 by-need recursive CAFs: the `ByNeed` cell `[state][result]` as a 3-state (`Unforced`/`Building`/`Forced`) memoising suspension, `force` = black-hole-then-`apply(thunk,unit)`-then-memoise (cell rooted across the safepoint), `apply` auto-forces a `ByNeed` callee, `Grec` groups over a shared env; builds on the ADR-0069 record store (a field store does not force), unblocks the recursive `Monad Effect` dictionary (ADR-0067 §2 prerequisite) | Accepted |
| [0071](0071-codegen-runtime-c-abi.md) | Native runtime C ABI: exported context, allocation, root, apply, effect, record, by-need, and primitive helper surface for LLVM-generated code | Accepted |
| [0072](0072-anf-to-llvm-lowering.md) | ANF → LLVM lowering: per-module textual IR, lambda-lifting, rooting emission, and the native differential | Accepted |
| [0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) | `ulib`-shipped native foreign for the native backend: `.c` over the `pv_*` C-ABI (opaque `PVWord` + `ctx`-taking scalar accessors + a rooting contract), resolved by link-time `pvf_*` symbols; PS-first with arity/effect from the `foreign import` signature | Accepted |
| [0074](0074-effect-exception-throw-only-ulib-shadow.md) | `Effect.Exception` as a throw-only `ulib` shadow: pure-PS `Error`, `throwException` = stderr + `exit 1` (fatal, unrecoverable), `catchException` passes through; LLVM runtime completes the `writeErrLineImpl`/`exitImpl` leaves under a write-before-exit contract (stderr direct+flush; exit drains the stdout sink); real unwinding deferred | Accepted |
| [0075](0075-cross-backend-wall-clock-benchmark-harness.md) | Cross-backend wall-clock benchmarks (wall 4): input-size log sweeps, whole-process min-of-k timing, per-series scaling exponent + ratio-to-`js` baseline; `benchmarks/` PS corpus fed by argv (`purvm run` forwards guest argv); the [0026] steps/allocs harness stays the optimiser regression gate | Accepted |
| [0076](0076-direct-known-arity-calls-musttail.md) | Direct known-arity calls + `musttail` on the LLVM backend: every lifted function gets a direct entry (generic `AbiCodeFn` becomes a tail-calling wrapper); module-local saturated calls go direct, direct→direct tail edges `musttail` (pop-frame-before-tail); cross-module stays generic pending `.pmi` publication | Accepted |
| [0077](0077-cross-module-direct-calls-pmi-arity.md) | Cross-module direct calls: consume the `.pmi` export surface; recursive function members publish as `Erecfn of int` instead of `Efn` (recursiveness — the force-cell vs sentinel env fact — becomes interface-visible; carried via `group.recursive`) with a `format_version` 2→3 lockstep bump; top-level `$d` entries go external, `Gfun` calls skip even the `$root` load; rep publication stays deferred | Accepted |
| [0078](0078-rust-foreign-bindgen-over-c-abi.md) | Idiomatic Rust native foreigns as a bindgen-style layer over the `pv_*` C ABI: same `pvf_*` symbols, `purvasm-sys`/safe-wrapper/attribute-macro crates speaking only `purvasm.h` (never runtime internals), rooting enforced by frame-lifetime-branded `!Send` handles, single bundled staticlib link with driver-side exactly-one-provider validation + `nm`-class symbol audit; tagged `source` schema (`c` \| `rust-crate`) | Accepted |
| [0079](0079-ctx-header-abi-inline-rooting-fast-paths.md) | The context header becomes ABI: a versioned `pv_ctx_header` (roots base/len/cap + pending-tail flag) at offset 0 of `pv_ctx`, declared once in `purvasm.h` (runtime + 0078 sys layer + codegen all derive from it, `pv_abi_check` aborts on mismatch); codegen inlines the root/get/frame/pop/settle fast paths as IR — the measured remaining ~15–25 extern crossings per hot iteration — with slow paths unchanged and interoperable per site | Accepted |
| [0080](0080-foreign-signature-reconstruction-cst.md) | Level-2 foreign signatures reconstructed from source via the embedded CST parser (`language-cst-parser`): a source channel for foreign-bearing modules only, arity/effect-leaf interpretation as the pinned contract (validated by a boot-registry consistency differential), shapes flow to the native-codegen port + diagnostics; the silent `arity 1` default dies; `.pmi` publication deferred to the binary-artifact record | Accepted |
| [0081](0081-purvasm-regex-pure-ps-es-engine.md) | `purvasm-regex`: an ES-flavour regex engine in pure PureScript (demand = the 0080 CST Lexer's 14 patterns → exact feature floor incl. negative lookahead + 5 Unicode general categories; out-of-floor = loud construction error), `Data.String.Regex` as a `ulib` shadow (`Regex` = unexported ADT, instance ABI preserved); C/PCRE2/libregexp rejected (ES semantics + LLVM-only vs all-backend shadow requirement) | Accepted |
| [0082](0082-native-codegen-port-to-level-2.md) | Port the native LLVM backend to Level-2 PureScript (wall 3), gate split by layer: backend (`codegen_llvm` ~2465 loc) + required lowering (`DictElim`) = faithful `.ll`-byte-identical transcription vs frozen boot; optimiser (`Simplify`/`Dbe`/`EffectAnalysis` + **caller-homed higher-order specialization** + a new **NbE general inliner**) rebuilt, gated on behavioural `--opt ≡ --no-opt ≡ oracle` over **Level-2's own VM/LLVM/JS legs** *plus* a third **size/time-assert** gate the behavioural+bench gates are blind to; `--no-opt` flag on both compilers; two parallel tracks joined at native self-host. Blow-up defences pinned (reducer `Lazy` sharing, conservative inline gate + dedicated-`-O0`-TU codegen backstop). Cross-module inputs (foreign shapes + spec/inline bodies) ride the **ADR-0084** `.pmi` pruned-module summary — a prerequisite for the optimiser track (not the backend track); unboxing is link-time. Lowers the shared ADR-0083 `dtree` | Accepted |
| [0083](0083-match-compilation-to-anf-middle-end.md) | Replace the **three** per-backend matchers (bytecode = ADR-0031 decision tree; `codegen_ml` = record-binder hybrid; LLVM = CPS cascade, no tree) with ONE shared Maranget builder — a middle-end `CCase → dtree` intermediate (`Middle_end.Match_compile`, **not** a new `cexpr` node; `CCase` + optimiser passes untouched) that each backend lowers at its `CCase` site, in **both boot and Level-2**. Bytecode is a faithful split → byte-identical (golden unchanged, **no re-baseline needed**); native cascades → the tree, checked by the 4-way differential. One-time boot-freeze exception; keeps `codegen_ml` as the last insurance leg (retired once native self-host subsumes it); defers ADR-0082 (native port then just lowers the tree) | Accepted (implemented) |
| [0085](0085-native-build-orchestration-inmemory-summary-env.md) | **Native build orchestration**: replace the shortcut driver (`surfaceOf` via a redundant bytecode `compileModule` + monolithic `nativeSplit`, CoreFn→ANF lowered twice/module, no summary thread) with a **per-module compile unit** (`translExpr → normalize → DictElim(env) → (optimize) seam → emit .ll` + a module summary) driven by `build` as a **topological fold threading an in-memory dependency-summary `env`** — the clean pattern boot's *bytecode* build already uses (compile-unit + orchestrator), plus the summary thread it lacks. **DictElim is required lowering** (runs `--no-opt` too, module-level, env-consuming) so its cross-module **dictionary machinery rides the in-memory env — always**, split by *lifetime* from the optimiser's **persisted `--opt`-only `.pmi` `Summary`** (ADR-0084, `Nothing`-omitted, byte-identical): same type at the seam, orthogonal persistence. Kills the double-lowering; the `(optimize)` slot after DictElim is the optimiser track's seam (`Simplify` moves there from its per-binding stopgap). Env dict-machinery = accessor `φ` + instance `{φ→impl}`. Unblocks cross-module DictElim (both tracks were stalled on the missing summary) | Accepted |
| [0084](0084-binary-pmo-pmi-and-cross-module-summary.md) | The **cross-module optimiser summary** ADR-0082 §4 names as its prerequisite — an **`--opt`-only `.pmi` extension**, **not** a binary rewrite of the `.pmi` core (that stays byte-identical to boot; core binarisation is post-boot/future). Publishes each module's **per-module-optimised, pruned `M.Module`** (purs-wasm `summarize` — inline candidates ∪ impure/mem-eff bodies ∪ spec-callees ∪ dict machinery ∪ small; drops large-multi-use-pure ≈7/8; opaque refs to dropped helpers backed by **link-only exports**) + foreign `(arity,vsat,ret_vsat)` shapes as data (ADR-0080). Leaves the `.pmi` `hash` field untouched (export surface, same in both modes) and defers precise fingerprint invalidation — `--opt` always recompiles; the summary is a plain **additive JSON field** (appended after `hash`, absent under `--no-opt`, `version:3` unbumped), **not** a binary envelope. Pins self-pollution (`M` never reads its own summary) and a mode-keyed cache (`--opt`/`--no-opt` never cross-serve). **Unboxing out of scope** (link-time). Gates the optimiser cross-module track, **not** the backend `.ll` track — a real dependency but not an ADR-0082 acceptance blocker | Accepted |

## Scope
