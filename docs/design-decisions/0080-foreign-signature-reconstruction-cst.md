# 0080. Level-2: foreign signatures reconstructed from source, via the embedded CST parser

- Status: ~~Proposed~~ **Accepted** _(2026-07-06: accepted by the maintainer)_
- Date: 2026-07-05

## Context

A foreign's **calling shape** — its arity, and whether it is an effectful leaf — steers three
different consumers: the native backend's `AForeign` lowering builds a closure of that arity
([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §3); the effect-placement
analysis ([0034](0034-effect-optimization.md)'s `foreign_arity`/`effectful_leaf` inputs) decides
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

One scoping note to preempt confusion: the [0034](0034-effect-optimization.md)-era pin "purity
needs no externs/CST" concerned the *purity analysis* — leaf bits plus interface propagation.
This record reconstructs *signatures*, a fact that never had another source; nothing there is
revisited.

## Decision

### 1. A source channel, for foreign-bearing modules only

The Level-2 compiler gains a second input channel beside `corefn.json`: for a module whose
CoreFn `foreign` list is non-empty, the driver parses its `.purs` (via `language-cst-parser`)
and extracts the `foreign import` declarations' types. Source location follows **the corefn's
provenance**:

- a workspace/registry module resolves through spago's **`cache-db.json`** (it sits beside the
  corefn in the output dir and maps every linked module to its source path — monorepo-correct
  with zero configuration, and it carries per-source `[timestamp, hash]` for a future
  staleness check; adopted from purs-wasm ADR-0016, which also rejected sourcemap paths and a
  `--src` root flag in its favour);
- a **ulib-patched module's corefn came from the patch**, so its signatures parse from the
  patch `.purs` (which the staged ulib already ships), never from the upstream source it
  shadows — provenance-following is this project's analog of 0016's `ulib > …` precedence.

Modules without foreigns are never parsed: the channel's cost is bounded by the foreign
frontier (dozens of modules, not the workspace).

A foreign-bearing module whose source cannot be found or parsed is a **hard diagnostic**, not a
silent fallback — the silent-`arity 1` failure mode is the thing being retired. This
deliberately differs from 0016, which degrades to an all-opaque signature with arity taken
from the call site: purs-wasm's missing fact is marshal *kinds*, for which opaque is a usable
degraded value; ours is **arity and the effect bit**, for which no safe degraded value exists
(a call-site arity is unsound at partial-application sites, and a wrong effect bit silently
*moves* an effect — the [0034](0034-effect-optimization.md) minefield).

### 2. The type-to-shape interpretation is the contract

From a declaration's type, after peeling `forall`s and (disallowed on foreigns anyway)
constraints:

- **arity** = the number of top-level `->` arrows;
- **effectful leaf** = the return position after those arrows is `Effect _` — the shape whose
  final application must not be assumed pure (the [0034](0034-effect-optimization.md)
  saturation rule);
- everything else about the type (records, type variables, `FnN`/`EffectFnN`) is *opaque* to
  this record — words in, words out, exactly the representation-opacity line the FFI records
  draw. A shape the interpreter does not understand is a diagnostic, not a guess.

One CST reality, learned from 0016: the CST is **not desugared** — a type synonym or infix
type operator is not expanded. Arity is safe regardless (`->` is a dedicated CST node, always
structurally visible), but a synonym can hide the `Effect` return (`type Eff = Effect` — rare,
and this project controls its ulib sources). Such a return position is treated as
*not recognisably effectful* and reported by the §2 diagnostic rather than guessed pure.

The interpretation is validated by a **consistency differential**: for every key both boot's
registry and the reconstruction know, the (arity, effect-bit) pair must agree — a standing
tooling test, so the two sources cannot drift while boot remains in the build path.

### 3. Where the shapes flow

Reconstruction yields a per-build `ForeignSig` map (`qualified key → { arity, effectful }`)
consumed by:

- the **native-codegen port** (the successor arc): `AForeign` closure arity and the
  effect-placement inputs come from this map — the port never grows a hand registry;
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
- **Declare shapes in `ulib.json` manifests.** Moves the truth away from the source into
  another hand-maintained mirror; workspace (non-ulib) foreigns would still need a channel.
  Rejected as primary (the manifest stays what it is: a *provider* map).
- **Wait for a typed CoreFn.** Does not exist in `purs` 0.15; not a plan.
