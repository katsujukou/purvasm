# 0080. Level-2: foreign signatures reconstructed from source, via the embedded CST parser

- Status: ~~Proposed~~ **Accepted** _(2026-07-06: accepted by the maintainer)_
- Date: 2026-07-05

## Abstract

Level-2 foreign signatures reconstructed from source via the embedded CST parser (`language-cst-parser`): a source channel for foreign-bearing modules only, arity/effect-leaf interpretation as the pinned contract (validated by a boot-registry consistency differential), shapes flow to the native-codegen port + diagnostics; the silent `arity 1` default dies; `.pmi` publication deferred to the binary-artifact record

## Context

A foreign's **calling shape** — its (semantic) arity, and whether it is an effectful leaf — steers three
different consumers: the native backend's `AForeign` lowering builds a closure of the leaf's **physical
closure arity**, which it *derives* from the shape (a nullary `Effect a` leaf is semantic arity 0 but
physical arity 1 — it *is* the effect thunk; the derivation is `Backend.LLVM.Driver.leafClosureArity`, and
must stay backend-side — see §2)
([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §3); the effect-placement
analysis ([0034](0034-effect-analysis-impurification.md)'s `foreign_arity`/`effectful_leaf` inputs) decides
*where* an effect fires, the named minefield of that record; and diagnostics want to say what a
stuck foreign *is*. Today that knowledge has exactly one home: **boot's hand-maintained `Ffi.host`
registry**, with a silent `arity 1` default for unknown keys — every ulib-shipped `.c`
([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md)) and Rust crate
([0078](0078-rust-foreign-bindgen-over-c-abi.md)) foreign needs a boot-side entry to be compiled
correctly, a double-entry the FFI records have been paying since.

The Level-2 compiler cannot inherit that home. Its provider ladder deliberately **omits the
native rung** (the bytecode path resolves hosts at run time), so it carries no shape metadata at
all — and the agreed next arc, porting the native backend to Level 2, needs exactly that
metadata, in PureScript, while **Level 1 is frozen** (no further boot investment except what
Level 2+ requires).

The signature exists in precisely one stable place, and the reason is structural, not just
formats: **the dominant FFI idiom keeps the `foreign import` private and exports a safe
wrapper** — the very shape this project's own conventions mandate (`unsafeXX` stays
unexported). `purs` externs carry only a module's *exported* declarations, so the frequent
`fromNumberImpl`-style private foreign is simply absent from them (confirmed empirically in
purs-wasm's ADR-0016, the prior art for this record: decoding `Data.Int.externs.cbor` lists
`fromNumber` but not `fromNumberImpl`). CoreFn is untyped — `corefn.json` carries a module's
`foreign` *names*, nothing more. The `.purs` source's `foreign import f :: T` declaration is
the single artifact that has the private foreigns' types, and the workspace already locks
`language-cst-parser` (0.14.1), a production PureScript-in-PureScript parser.

One scoping note to preempt confusion: the [0034](0034-effect-analysis-impurification.md)-era pin "purity
needs no externs/CST" concerned the *purity analysis* — leaf bits plus interface propagation.
This record reconstructs *signatures*, a fact that never had another source; nothing there is
revisited.

## Decision

### 1. Per-module, by provenance — reconstruct where source exists, declared where it doesn't

Foreign-signature reconstruction is **purely syntactic and closes within a single module**, so
— like separate compilation itself ([0033](0033-separate-compilation.md)) — it is a per-module
operation, never a sweep over the loaded closure (closure-level processing is what separate
compilation is meant to avoid). A module with a non-empty CoreFn `foreign` list gets its
shapes from its **provenance**:

- a **non-`ulib` module** — the app's own `src/`, or a `.spago` registry package — is
  reconstructed from its source, located via spago's **`cache-db.json`** (it sits beside the
  corefn and maps every linked module to its source path — monorepo-correct, zero
  configuration, and carries per-source `[timestamp, hash]` for a future staleness check;
  adopted from purs-wasm ADR-0016, which also rejected sourcemap paths and a `--src` flag). The
  consumer *has* these sources, so reconstructing on the spot is free of any distribution cost;
- a **`ulib`-overlaid module** ships **no source** (a `ulib` is distributed as `corefn` + `.c`
  + `ulib.json`, ADR-0038/0073) — so its shapes are **declared in `ulib.json`'s `foreignSigs`
  map**, mandatory for every foreign the overlaid corefn retains, and the consumer reads them
  from the aggregated manifest. Requiring the consumer to reconstruct here would force `ulib`
  to also ship source, which it must not.

The declared `foreignSigs` are not an un-checked hand mirror (the thing this record set out to
kill). `ulib-tools build` — which *does* have the `ulib` source — **reconstructs each patch
module and requires the declared `foreignSigs` to match exactly** (same keys, same
`(arity, vsat, ret_vsat)`; a missing, extra, or divergent entry is a hard build error). So the
declaration is validated against the source once, at publish time, and the consumer trusts the
validated manifest — anti-drift preserved, source distribution avoided. (This is purs-wasm
ADR-0016's `ulib > source` precedence, resolved for a source-less `ulib`.)

Modules without foreigns are never touched: the cost is bounded by the foreign frontier (dozens
of modules), and each module is handled independently.

A foreign-bearing module whose source cannot be found or parsed is a **hard diagnostic**, not a
silent fallback — the silent-`arity 1` failure mode is the thing being retired. This
deliberately differs from 0016, which degrades to an all-opaque signature with arity taken
from the call site: purs-wasm's missing fact is marshal *kinds*, for which opaque is a usable
degraded value; ours is **arity and the effect bit**, for which no safe degraded value exists
(a call-site arity is unsound at partial-application sites, and a wrong effect bit silently
*moves* an effect — the [0034](0034-effect-analysis-impurification.md) minefield).

### 2. The type-to-shape interpretation is the contract

The shape is the [0034](0034-effect-analysis-impurification.md) **dual per-value summary**
`(arity, vsat, ret_vsat)` — the pair the effect analysis actually consumes — not a single
`effectful` bit (which conflates the two effect modes and mis-locates the [0034](0034-effect-analysis-impurification.md)
I1 force point). From a declaration's type, after peeling `forall`s, parens, constraints, and
kind annotations:

- count the top-level `->` arrows;
- classify the **return head** constructor (qualifier ignored), by these families — grounded
  in boot's [0039](0039-ulib-st-array-and-st-uncurried.md) adapters, since purvasm has **no
  packed-uncurried representation** (`mkFnN = identity`; an `FnN`/`EffectFnN` value *is* an
  N-ary curried closure):

  | return head | `arity` | `vsat` | `ret_vsat` | why |
  |---|---|---|---|---|
  | `Effect r` / `ST r t` | arrows | `false` | **`true`** | a perform leaf; its saturated result is a thunk forced later |
  | `EffectFn{N}` / `STFn{N}` | arrows + `N` | **`true`** | `false` | its N-arg saturation **runs** the effect (boot `mkSTFnN f = \x… -> run_eff (f x…)`) |
  | `Fn{N}` | arrows + `N` | `false` | `false` | pure uncurried; `mkFnN = identity`, `runFnN = saturated apply` |
  | anything else | arrows | `false` | `false` | opaque — words in, words out |

  So a bare `foreign import x :: EffectFn2 A B C` is `(2, vsat, ¬ret_vsat)`, and
  `runEffectFn2 :: EffectFn2 … -> A -> B -> Effect C` is `(3, ¬vsat, ret_vsat)` — different
  effect modes a single bit could not tell apart.

One CST reality, learned from 0016: the CST is **not desugared** — a type synonym or infix
type operator is not expanded. Arity from arrows is safe regardless (`->` is a dedicated CST
node), but a synonym can hide an effect head (`type Eff = Effect`, or aliasing an `EffectFnN`
— rare, and this project controls its ulib sources). Such a head is classified opaque (both
bits false) rather than guessed effectful.

The interpretation is validated by a **consistency differential**: for every key both boot's
registry and the reconstruction know, the shapes must agree — a standing tooling test, so the two
sources cannot drift while boot remains in the build path. Two normalisations are applied before the
compare, because the two sides speak different dialects. (i) **Arity is semantic vs physical.** Boot's
registry carries the **physical** `foreign_arity` (the closure arity), while the reconstruction carries
the **semantic** arrow count — and they *differ* for a nullary `Effect` leaf (`argvImpl`: reconstruction
`arity 0, retVsat`, boot `foreign_arity 1`). So the differential maps the reconstructed shape through the
same physical-arity conversion the backend uses — `if retVsat then max arity 1 else arity` — before
comparing to boot's arity; it does **not** compare the raw reconstructed arity, which would spuriously
disagree. (This conversion is applied only *at the comparison* and in the backend lowering; it is **not**
folded into the reconstructed `arity`, which stays semantic for ADR-0034's effect analysis.) (ii) **The
effect bit.** Boot's registry carries a single effectful bit, which is `ret_vsat` (it provides no
uncurried `EffectFn` leaf, so `vsat` is always `false` on that side); the differential normalises
accordingly.

### 3. Where the shapes flow

The per-module resolution yields a `qualified key → { arity, vsat, retVsat }` shape (§1) — the
`ForeignSig` a single module contributes — consumed by:

- the **native-codegen port** (the successor arc): `AForeign` closure arity and the
  effect-placement inputs come from the module's shapes — the port never grows a hand registry;
- **diagnostics**: an unbound or mis-provided foreign is reported with its declared signature.

The Level-2 provider ladder is otherwise unchanged: the hand-written rungs keep supplying
*semantics* (intrinsic eta-expansions, structural guest terms); what stops being hand-maintained
is the *shape metadata*.

### 4. Publication is deferred to the binary-artifact record

The map stays in-memory per build. Publishing foreign shapes in the `.pmi` (so dependents need
not re-parse, and the hash cascade guards FFI-shape changes like it guards
[0077](0077-cross-module-direct-calls-pmi-arity.md)'s call facts) is pinned as a **rider on the
planned binary `.pmo`/`.pmi` format record** — one more `format_version` bump, absorbed by the
same lockstep migration instead of two.

## Consequences

- The native-backend port becomes self-contained in Level 2: its one missing input class now
  has a PureScript-native source, and boot's registry stops being the bottleneck new foreigns
  must pass through twice.
- A silent-miscompile class dies: an unregistered foreign today defaults to arity 1 and can
  misplace an effect; after this record the same situation is a build-time diagnostic naming
  the declaration.
- Build cost: CST-parsing the foreign frontier only; the parser is a locked registry package,
  not a new trust surface.
- The signature remains a *claim* about the `.c`/`.js`/Rust implementation — exactly the trust
  the registry encoded, now written where the author already writes it.
- A stale source tree (source newer than the compiled corefn) can skew signatures;
  `cache-db.json` already records `[timestamp, hash]` per source, so surfacing the skew is a
  cheap future check — deferred exactly as 0016 deferred it (spago's build-then-compile flow
  makes it a non-issue in practice).

## Alternatives considered

- **Keep the hand registry (status quo), port it to Level 2.** Perpetuates double-entry and the
  silent default, and makes the native port carry a table that drifts from the sources it
  mirrors. Rejected — this is the failure mode the record exists to end.
- **Read `purs` externs (`externs.cbor`).** The decisive defect is structural: externs carry
  **only exported declarations**, and the dominant idiom (mandated by this repo's own
  conventions) keeps the foreign private behind a safe wrapper — precisely the keys that would
  be missing (0016's empirical finding). purs-wasm still merges externs *in* (it already had a
  decoder, and externs types come desugared); we have no decoder, and buying one — an
  undocumented, version-coupled CBOR format — for a tier that cannot cover the main case is
  backwards. Rejected outright; source is primary here, not a gap-filler.
- **Declare shapes in `ulib.json` manifests — adopted for `ulib`, rejected as the *only*
  source.** A `ulib` distributes no source, so a `ulib`-overlaid module *must* declare its shapes
  in the manifest (§1) — the manifest is where the `ulib` author already declares that foreign's
  provider, and it is validated against the source at `ulib-tools build`, so it is not an
  un-checked drifting mirror. But it is not primary: non-`ulib` foreigns (the app's `src/`, the
  registry `*Impl` internals) have no manifest and are reconstructed from the source the consumer
  holds. The two sources partition cleanly by provenance rather than competing.
- **A single closure-level reconstruction sweep** (reconstruct every foreign-bearing module of
  the loaded program in one pass). Simpler to write, but couples a syntactic, module-local fact
  to the whole closure — the coupling separate compilation ([0033](0033-separate-compilation.md))
  exists to avoid, and the wrong shape for §4's per-`.pmi` publication. Rejected for per-module
  resolution.
- **Wait for a typed CoreFn.** Does not exist in `purs` 0.15; not a plan.

> **Progress (2026-07-06, interim):** Implemented and validated on the bytecode/Node paths;
> the native-path validation is blocked on a provider decision recorded below.
>
> - **Pure pass** (`Purvasm.Compiler.ForeignSig`): 81/81 unit tests, the §2 table pinned as
>   tests including the documented limitations (a synonym hiding `Effect`; `EffectFnN`
>   opacity). One §2 clarification as implemented: under a clean parse the interpretation is
>   *total* (`Type Void` has no error nodes), so the per-declaration diagnostic reduces to
>   the module-level parse diagnostic; an unrecognised return head is pure-without-complaint
>   for ordinary constructors (`Int`) — the synonym caveat stands as documented.
> - **Driver** (`Purvasm.CLI.ForeignSigs`): `cache-db.json` + provenance-following. One
>   correction to §1's assumption: the staged ulib did *not* yet ship patch sources —
>   `ulib-tools build` now stages each patch as `<Module>/module.purs` beside its corefn.
>   The channel runs on every `build` (147 signatures on the fib-bench closure smoke); the
>   hard diagnostics were observed firing in anger (a stale overlay without `module.purs`
>   produced a clean, named error).
> - **Consistency differential** (`tools/foreign-sigs-diff.sh`, boot `foreign-sig-dump` — the
>   frozen-boot exemption §2 anticipated): **its first run caught an undocumented boot
>   convention** — a zero-arrow effectful leaf is registered as *the thunk itself* (arity 1,
>   e.g. `argvImpl`), while an n-arrow leaf is registered at its arrow count and returns the
>   thunk. The reconstruction's canonical shape stays (arrows, effect-bit); the differential
>   normalises boot's encoding, now documented where it is compared. Result: 24 shared keys
>   (compiler closure) / 19 (fib closure), **0 disagreements**.
> - **Byte-identity**: the artifact stream is untouched by the channel — Node-hosted Level-2
>   self-compile against a regenerated boot reference: **516/516 byte-identical** (the
>   closure grew from 488 with the `language-cst-parser` graph).
> - **Native-path blocker, honestly:** reconstruction reaches `PureScript.CST.Lexer`, whose
>   lexing is built on top-level `Data.String.Regex` CAFs (Unicode property classes +
>   lookahead). The Regex module sat in earlier closures but was never *reached*; now it is,
>   and it has no native provider, so `purvm native` of the Level-2 CLI correctly refuses
>   (`unbound variable Data.String.Regex._match`). Decision pending with the maintainer:
>   recommended path is a `Data.String.Regex` ulib patch over a **Rust-crate provider**
>   ([0078](0078-rust-foreign-bindgen-over-c-abi.md) §5; `fancy-regex` — lookaround rules out
>   the plain `regex` crate), which also keeps "JS-derived library primitives live in ulib,
>   not the runtime". Native self-compile identity re-runs once the provider lands.

> **Progress (2026-07-06, native path closed):** the `Data.String.Regex` blocker is resolved
> **not** by the tabled Rust-crate provider but by the pure-PureScript regex engine
> ([0081](0081-purvasm-regex-pure-ps-es-engine.md)) behind the `ulib/strings` shadow — the
> `ulib`-shadow doctrine, no ABI/runtime/foreign additions. Two further frontier modules the
> CST parser reached were closed the same way (small `ulib` shadows, pure-PS, all backends):
> `Data.Lazy` (upstream verbatim, its foreign trio re-homed to an `Effect.Ref`-memoised thunk
> — the "computed at most once" contract preserved, load-bearing for the parser's
> `TokenStream` backtracking) and its demanded `Effect.Unsafe` (`unsafePerformEffect` = apply
> the `Effect` closure to `unit`, the structural-`Effect` analog of the upstream JS move).
>
> Result: the Level-2 CLI **builds and self-compiles natively**, **528/528 byte-identical to
> boot** (the closure grew 488→528 with the `language-cst-parser` + engine graph), and the
> node-vs-native `foreign-sigs` dumps over the whole corpus are byte-identical (the §3 gate).
>
> **Channel wiring, revised:** running the source channel on *every* build (as first shipped)
> cost minutes on the native backend (CST-lexing the frontier through a pure-PS engine under
> the v1 call tax; the bytecode path never consumes the shapes). It is now **opt-in**
> (`build --check-foreign-sigs`); the standing checks remain the `foreign-sigs` subcommand and
> `tools/foreign-sigs-diff.sh`. The permanent fix is §4's `.pmi` shape publication — which the
> **native-codegen port will need regardless** (it consumes the shapes and must not re-parse
> the frontier per build). So §4 is upgraded from "deferred nicety" to a **prerequisite of the
> port**, riding the binary-artifact format bump.
>
> **Revision (2026-07-06): the single `effectful` bit became the [0034](0034-effect-analysis-impurification.md)
> dual `(arity, vsat, ret_vsat)` — §2 rewritten above.** The maintainer flagged that a single
> bit cannot represent an uncurried effectful value: a bare `foreign import x :: EffectFn2 A B C`
> is `arity 2, vsat` (its saturation *performs*), whereas `runEffectFn2 :: … -> Effect c` is
> `arity 3, ret_vsat` (its saturation returns a thunk) — treating the former as `ret_vsat` would
> mis-locate the [0034](0034-effect-analysis-impurification.md) I1 force point (a soundness bug,
> not just a wrong arity). The interpreter now classifies the return head into the §2 family
> table; crucially the `EffectFn`/`STFn` = `vsat` rule is **not a new convention** but the one
> boot's [0039](0039-ulib-st-array-and-st-uncurried.md) `mkSTFnN f = \x… -> run_eff (f x…)` already
> implements (the uncurried value runs on saturation). Both the pure-pass unit suite and the
> `foreign-sig-dump`/differential were moved to the triple (24 keys, 0 disagreements; boot's
> single bit maps to `ret_vsat`, `vsat=false`). The families remain intrinsic-resolved today
> (so the shapes stay inert until the native port consumes them), and the differential is still
> blind to them (boot returns `None` for intrinsic keys) — but the interpretation is now correct
> for the day an uncurried effectful foreign reaches the native `AForeign` rung.
