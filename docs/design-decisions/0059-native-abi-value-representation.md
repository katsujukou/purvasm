# 0059. Native ABI: value representation, calling convention, and the GC seam

- Status: ~~Proposed~~ **Accepted** _(2026-06-30: accepted by the maintainer)_
- Date: 2026-06-30

> **Revision (2026-06-30):** the sections below were refined after a pre-implementation review (no code
> depends on this record; impact is closed to this ADR and the still-`Proposed`
> [0060](0060-native-codegen-llvm-owned-runtime.md)). Substantive changes: 64-bit label ids + a
> link-time collision check; **per-constructor** nullary→`i31` (subsuming enum-like types) and the
> determinism invariant; `Char` → `i31` code point (entails revisiting
> [0006](0006-string-utf8-char-int.md)/[0015](0015-corefn-lowering.md)'s `Char`→`Int` collapse); an
> **arity-aware** generic entry; the write barrier scoped to *pointer* stores; the representation
> analysis sited on ANF; and `externs` noted as available rather than assumed absent.

## Context

The bootstrap plan ([0001](0001-phase-1-host-language-ocaml.md), [0037](0037-self-hosting-purescript.md))
makes `boot`'s OCaml a throwaway *means*: the durable artifact is `purvasm-ps`, and the project's
stated end is a **bespoke native runtime we own** — a language-neutral value ABI, GC, scheduler, and
effect handlers shared by every phase ([0001](0001-phase-1-host-language-ocaml.md)'s standing
two-level-memory constraint). Today the native path leans *entirely* on OCaml's runtime
([0035](0035-native-backend-ocaml5-concurrency.md), [0036](0036-anf-to-ocaml-value-representation.md)):
guest values are emitted as the OCaml-host `Rt.value`, garbage is collected by OCaml's GC, and there
is no C-FFI. That is the prototyping shortcut [0001](0001-phase-1-host-language-ocaml.md) reserved, not
the destination.

The maintainer's roadmap names four walls, in rough sequence:

1. **Native ABI** — define the owned native value/calling/memory ABI. Opens the C-function FFI road and
   lets the OCaml host be discarded.
2. **A native backend + runtime for Level-2** — emit native code from `.pmi`/`.pmo` and run it on an
   owned runtime. The codegen *how* (LLVM / C / other) and the **parallel runtime architecture** are
   undecided.
3. **Bootstrap that toolchain natively with `boot`** — Level-1 builds Level-2 into a *native-emitting*
   Level-3 compiler.
4. **A JS / ES / Native 3-way benchmark** drives optimisation from there.

This ADR settles **wall 1**: the *target-neutral* native ABI — value representation, heap layout, the
calling convention, and the **GC seam** (the interface, not the collector). It is the prerequisite for
walls 2–4. It deliberately defers the codegen target and the continuation/fiber representation (they
depend on wall 2), the GC *policy* (a dedicated wall-2 GC ADR), and two optimisations called out below.

**Why the ABI is where it matters.** A self-compile profile ([sidenotes/0008](sidenotes/0008-self-compile-profiling.md))
shows that with the build-speed regressions already fixed ([0049](0049-eliminate-superlinear-bytecode-construction.md)/
[0051](0051-flatten-json-serialization.md)/[0052](0052-native-unsafesetbyte-in-place.md)/[0054](0054-byte-oriented-json-parser.md)
took the 242-module cold self-compile from the README's stale ">10 min" to **~6.8 s**), roughly **60 % of
native self-time is runtime-representation overhead** — generic apply, scalar coercions (boxing), and
GC/allocation — not algorithmic compile work. The ABI is exactly where that ceiling is set, and where
`purvasm`'s competitiveness against `purs` (JS) and `purs-backend-es` (ES) will be decided.

**Inspiration and the load-bearing difference.** The representation is heavily inspired by `purs-wasm`
(its `0001` value representation, `0004` uniform-`eqref` calling convention, `0013` `Int`/`Number`
unboxing, and its per-module rep analysis). But `purs-wasm` targets **Wasm GC**: the engine provides
the garbage collector, typed `struct`/`array` layout, checked casts (`ref.cast`/RTT), and typed calls
(`call_ref`). `purvasm` takes the other road — a **native runtime with a self-owned, capability-local
GC** ([sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md)) — so **each of those engine
services becomes our responsibility**. `purs-wasm`'s *data shapes* port almost verbatim; the *engine
services behind them* must be re-homed. That re-homing is the substance of this ADR.

Inherited constraints the ABI must respect:

- **Type-erased CoreFn** — no per-position types; the representation must work without them
  (`purs-wasm` 0004, the same reason `purvasm`'s machine discriminates by tag/label/shape only).
- **`Int` is 32-bit wrapping** ([0006](0006-string-utf8-char-int.md)); strict CBV
  ([0002](0002-cesk-execution-model.md), no thunks except by-need CAFs / `Effect`).
- **Separate compilation** ([0033](0033-separate-compilation.md)) — module-local, `.pmo`/`.pmi`; no
  whole-program pass may be assumed.
- **The differential-oracle discipline** — native results must match the CESK oracle and the bytecode
  VM observably (values and `Effect` order). Keeping the native representation *tag-isomorphic* to the
  VM/`.pvm`/oracle value model keeps that differential cheap.

## Decision

Adopt a native, target-neutral ABI built on a **uniform tagged word**, an **inline-tagged heap header**,
an **eval/apply two-entry calling convention**, and a **GC seam** that fixes the collector's interface
while leaving its policy to wall 2. The C-FFI boundary is specified here as the rung that retires the
OCaml host.

### 1. Value representation — uniform tagged word

Every value is one machine word that is **either an `i31` immediate or a pointer to a heap block**,
distinguished by a low tag bit (the native realisation of `purs-wasm`'s `eqref`; "`i31ref`" is borrowed
as a *concept* — an OCaml-style tagged immediate the GC recognises as non-traced — not the Wasm type).

- **Nullary constructors are `i31` immediates** — the decision is **per-constructor** (OCaml-style), not
  per-type: within any ADT, a constructor with no fields is an `i31` immediate carrying its constructor
  index, while a field-carrying constructor is a heap block; a match first tests immediate-vs-pointer,
  then reads the `i31` value or the heap tag. This subsumes enum-like types (`Boolean` = 0/1, `Unit`,
  `Ordering`, user enums — all `i31`) *and* makes `Nothing`/`Nil`/`Leaf` allocation-free (better than
  `purs-wasm`'s shared heap globals). Disambiguation is by match/arithmetic **context** — the machine
  never consults a type ([0011](0011-adt-pattern-matching.md)), and well-typing forbids mixing, exactly
  as an OCaml immediate `1` is `true`/`()`/`1` by context.
- **Determinism invariant (what keeps this separately compilable):** a value's representation is a
  *deterministic function of the constructor's own arity*, which every module reads from the CoreFn
  `IsConstructor` meta at each use site — so all modules agree without a whole-program pass and **without
  needing `externs`**. (Where `externs` are loaded — see *Deferred* — they additionally enable
  *type-directed* field unboxing, but the core representation does not depend on them.) This is the
  essential contrast with §3's calling convention, which is body-determined and so needs `.pmi`.
- **`Int`** defaults to a boxed `$Int` block (`i32` payload), unboxed to a **raw `i32`** only where the
  **representation analysis** — a dataflow pass over ANF ([0060](0060-native-codegen-llvm-owned-runtime.md)
  §4), *not* a type query (CoreFn is erased); conservative, so an unprovable slot stays boxed and
  correctness never depends on it — proves a slot stays monomorphic. `i31` is **not** used for `Int`:
  31 bits cannot hold a 32-bit `Int` ([0006](0006-string-utf8-char-int.md)).
- **`Char`** is **never heap-allocated** — a `Char` slot is a raw value when monomorphic and an **`i31`
  immediate when boxed** (code point ≤ 0x10FFFF, 21 bits — always fits `i31`, so a `Char` at a boxed
  boundary avoids the `$Int` heap box that `Int` needs there: `Array Char`, `Maybe Char`, and polymorphic
  positions cost no per-`Char` allocation). Code-point (not UTF-16 code-unit) semantics is the principled
  choice once `String` is UTF-8 ([0006](0006-string-utf8-char-int.md)), matching `purs-wasm`'s code-point
  `Data.String.CodeUnits`. This is realised as a **representation annotation** the analysis carries on ANF
  slots (a distinct `Char` rep whose *boxed* form is `i31`, not `$Int`), seeded by keeping `Char`
  *literals* distinct in the IR and by `externs`-typed boundaries. It is **not** a heavy revert of
  [0006](0006-string-utf8-char-int.md)/[0015](0015-corefn-lowering.md): the oracle and VM may keep
  treating a `Char` value as its code-point `Int` (they do no unboxing), the bytecode is unchanged (a
  `Char` literal still pushes its code point) so the `.pmo` byte-identity is unaffected, and **only the
  native representation analysis consults the `Char` annotation**.
- **`Number`** defaults to a boxed `$Num` block (`f64`), unboxed to a raw `f64` under the same analysis.
- **`String`** is a heap block of **packed UTF-8 bytes** + length (an improvement over `purs-wasm`'s
  one-byte-per-`i32`-lane array). The payload is non-pointer, so the GC skips it. (Interacts with the
  in-place byte builder under a moving collector — see §4.)
- **`Array`** is a heap block of mutable tagged-word elements.
- **`Record`** is a uniform label-map: **64-bit FNV-1a hashed label ids**, sorted, with a parallel value
  array. The hash is a pure function of the name (no whole-program label numbering), which is what keeps
  records separately compilable. The id is a side-array value (not a tagged word), so it carries **no
  `i31` width constraint** — 64 bits makes a collision astronomically unlikely (~10⁻⁸ even at 10⁶
  distinct labels), so the check below is **insurance** against a *silent wrong-field-read* across module
  boundaries (a correctness hazard the differential oracle would not catch, since it compares only
  observed values). To run it the linker needs the name↔id mapping for the whole closure, so **`.pmi`
  carries an interface-time label-name table** — link-time metadata only; the *runtime* image uses the
  64-bit ids alone and never the names. The linker checks the merged closure's label set and
  **hard-errors on any collision, always** (not debug-only: the failure mode is a silent miscompile, the
  cost is link-time and interface-size only, and this matches `purs-wasm`, which keeps the same symbol
  table in its interface file and hard-errors at link). **Type-class dictionaries use this same record
  representation**, newtype-erased ([0018](0018-newtype-erasure.md)).
- **A field-carrying constructor** is a heap block: a `tag` (`i32`) plus its fields. The **initial cut
  stores fields as a homogeneous tagged-word array** ([0011](0011-adt-pattern-matching.md)'s `VData`
  shape) — every field is a self-describing tagged word, so GC scanning needs no per-object layout table.
  (Unboxed scalar data fields are deferred — see *Deferred*.)
- **Closure** is, after lambda-lifting, a flat pair **(code pointer, env-array pointer)**; free variables
  are read positionally from the env. Mutual recursion uses a **shared env block** knot-tied by
  back-patching the env array: function members are `(static code ptr, shared env)` — a pure back-patch,
  no forcing — while non-function value members of a heterogeneous group (the load-bearing case: the
  `Effect` monad dictionary, whose curried `ap monadEffect` forces a sibling *during* construction) use
  a **by-need cell** ([0024](0024-by-need-recursive-bindings.md)): forced on first dereference,
  black-holed during the force.
- **`Ref`/`STRef`** is a heap block holding one mutable tagged word.

### 2. Heap block header — inline tagged

Each heap block carries an **inline header** `{ size, GC color, kind }` (OCaml/Grain style). `kind` is a
small closed set — ADT / record / array / string / closure / foreign / `$Int` / `$Num` / `Ref` /
by-need-forwarding — that tells the GC the scan strategy. Because the initial cut keeps all heterogeneous
heap fields as tagged words, **the GC scans by checking each field's tag bit; no layout bitmap is needed**
(the reason OCaml's collector is simple). An ADT's constructor index lives in the header (or is
pointer-tagged, below).

Nullary constructors are already `i31` immediates (§1), so they carry no header and a match on them never
dereferences. The remaining optional optimisation is, for **field-carrying** constructors, to
**pointer-tag the constructor index into the spare bits of the block pointer** (GHC-style), so a match
that needs only the tag branches **without dereferencing** the block. This fits the tagged-word scheme
directly and attacks the dispatch cost the profile surfaced; it is an optimisation and may be deferred
past the first cut (the header tag suffices for correctness).

### 3. Calling convention — eval/apply, two entries

Use **eval/apply** (not push/enter), with the curried/uncurried distinction erased from the IR
([0025](0025-lower-ir-anf.md)). Each known-arity (lambda-lifted) function emits **two entries** — the
native realisation of [0036](0036-anf-to-ocaml-value-representation.md)'s hybrid:

- a **fixed boxed generic entry** — an **arity-aware apply-`N`** (`(closure, arg₀ … argₙ₋₁) → result`,
  all tagged-word) that applies all available arguments at once and builds a PAP only on genuine
  under-application, through which generic apply, partial application, and over-application flow
  uniformly. It is *not* a single-argument curried entry — a one-arg-at-a-time chain is exactly the
  `app_589` cost ([sidenotes/0008](sidenotes/0008-self-compile-profiling.md), 16.8 %) this ABI exists to
  cut (`purs-wasm`'s `$Code` is single-arg only because Wasm curries at that level); and
- a **direct known-arity entry** — `fn_x a b c …` — for statically resolved saturated calls, which may
  use unboxed parameter/result reps.

Because any function reachable through generic apply must present the boxed generic entry, **unboxed
ABIs are confined to the direct entry**. Native tail calls (`musttail` / trampoline) for the TCE the
eval/apply machine assumes ([0030](0030-bytecode-vm-slice1.md)) depend on the codegen target and are
deferred to wall 2.

**Cross-module boundary — boxed for now, but explicitly an open empirical question.** The initial cut
makes every cross-module call boxed (`purs-wasm`'s current behaviour: a per-module worker over-exports
all top-level functions, pinning all top-level signatures to the boxed ABI, so only module-internal
locals and lifted supercombinators unbox). `purs-wasm`'s benchmarks show little loss from this **only
because its corpus has no cross-module calls yet**; the cost in large real applications is unmeasured and
*not* dismissed. The ABI therefore **reserves the hook** to recover it later: per-export **rep
signatures published in `.pmi`**, so a cross-module *direct* call can use the callee's unboxed entry.
That stays sound and separately compilable because reps are **callee-determined** (chosen from the
callee's own body) and **callers coerce at the call site** — dependency-directed callee→caller, not a
whole-program pass — with the boxed generic entry unchanged. Turning it on activates
[0033](0033-separate-compilation.md)'s currently-dormant `.pmi`-hash recompilation cascade. This is a
follow-up ADR **gated on the wall-4 measurement**, not part of this cut.

### 4. GC seam (interface here; policy deferred to wall 2)

This ADR fixes the collector's *interface* so the representation is collector-neutral
([0001](0001-phase-1-host-language-ocaml.md)), realising [0002](0002-cesk-execution-model.md)'s
predicted "roots interface, added later":

- **Header GC color bits**, and a **forwarding `kind`** so a moving/copying collector can relocate.
- A **precise-roots interface** — **stack maps or a shadow stack** at safepoints (the mechanism is the
  codegen target's choice, [0060](0060-native-codegen-llvm-owned-runtime.md) §2) — which must distinguish
  **tagged-word roots from raw unboxed `i32`/`f64` locals** (the unboxing of §1 is exactly what makes this
  non-trivial).
- A **write-barrier hook at every mutating *pointer* store** — `Ref` set, `Array` set (needed only
  because elements are tagged words; an unboxed-`i32` array needs none), the **knot-tying env
  back-patch**, and the **by-need cell fill**. A barrier tracks *inter-region pointer creation*, so it
  does **not** apply to the in-place `unsafeSetByte` byte builder, which writes **non-pointer bytes** —
  that builder needs *move-safety / pinning* (below), not a barrier; a per-byte barrier would re-impose
  the O(n) cost [0052](0052-native-unsafesetbyte-in-place.md) removed. The Wasm engine tracked pointer
  stores invisibly; natively the codegen must emit them.
- A **heap-ownership color bit** (shared-immutable vs capability-local) plus the **partition invariant**:
  a shared-immutable object must not reference a capability-local one, so a capability can collect its
  local heap independently. Escape to another capability (via `AVar`/fork) promotes by transitive
  freeze/copy.
- **Moving-GC safety:** no raw interior pointer is held across a safepoint (re-load the base — `EnvField
  i`, record arrays, ADT fields), and [0052](0052-native-unsafesetbyte-in-place.md)'s "linear
  unsafe-build protocol" gains the rule **no GC move mid-build** (safepoint-free, pinned, or via a
  handle).

The **collector policy** — capability-local copying + shared-immutable heaps, and the **concurrent
black-hole** that shared-CAF lazy forcing across capabilities requires (the multicore hazard
[0036](0036-anf-to-ocaml-value-representation.md) flagged when noting OCaml `lazy` is single-domain) —
is a wall-2 GC ADR ([sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md) is its prior-art
survey).

### 5. C-FFI boundary

The native foreign rung is the ABI surface that **retires the OCaml host**. A native leaf is a
first-order host function over the native value representation; `Purvasm.*` primitives lower to native
operations or C calls, and effectful leaves are provided by `purvasm-system` / `purvasm-fs`
([0056](0056-purvasm-system-host-leaves.md)). The boundary marshals the native tagged word to/from the
C ABI and fixes ownership of C-allocated memory versus GC-managed blocks. This rung is **greenfield** —
`purs-wasm`'s `externref`/host-import surface is Wasm/JS-specific and is not inherited; the detailed C
calling convention may spill into a follow-up.

### Deferred (explicit)

- **Codegen target (LLVM / C / other) and the continuation/fiber representation** — wall 2; the
  continuation representation depends on the target (engine effect handlers vs a hand-built stack switch).
- **GC policy** — capability-local copying + shared-immutable + the concurrent black-hole — a wall-2 GC
  ADR.
- **Unboxed scalar data fields** (`purs-wasm` front-B) — the field *types* are available from `externs`
  (portable into Level-2 from `purs-wasm`'s externs modules; Level-3 needs a CBOR-decode FFI leaf from
  `boot`), so the remaining blocker is **GC layout descriptors** the collector consults for objects with
  non-self-describing fields, *not* type information; a follow-up once descriptors exist.
- **Unboxed primitive arrays** (`Array Int` / `Array Number` as raw `i32`/`f64` arrays — a distinct kind
  from the tagged-word `Array`) — common in numeric and byte-buffer code; a follow-up.
- **Cross-module boundary unboxing** (`.pmi` rep publication; activates the [0033](0033-separate-compilation.md)
  cascade) — a follow-up **gated on measurement** (§3).
- **`Char` representation annotation** — the only lowering change for §1's `Char`-as-`i31` is keeping
  `Char` *literals* distinct in the IR (a `LitChar` variant the oracle/VM evaluate as the code-point
  `Int`) plus a `Char` point in the rep lattice; [0006](0006-string-utf8-char-int.md)'s value-level
  `Char` = `Int` and the existing `.pmo` byte-identity are unaffected. A light touch on
  [0015](0015-corefn-lowering.md)'s lowering, not a type-system revert.
- **Tail-call mechanism** — coupled to the codegen target (wall 2).

## Consequences

- An **owned, portable value ABI** ([0001](0001-phase-1-host-language-ocaml.md)) that survives into the
  self-host and the bespoke runtime, opens C-FFI, and lets the OCaml host be discarded
  ([0056](0056-purvasm-system-host-leaves.md)).
- The **high-value unboxing is all module-local** — arithmetic-flow reps, per-constructor nullary→`i31`
  immediates, and `Char`-as-`i31` — so it needs no `.pmi` change, derives deterministically from CoreFn
  constructor arity (no `externs` required), and stays separately compilable; it directly attacks the
  coercion/allocation share of the [sidenotes/0008](sidenotes/0008-self-compile-profiling.md) profile.
- The representation stays **tag-isomorphic to the VM/`.pvm`/oracle** value model, so the
  differential-oracle discipline remains cheap (an inline tagged header maps onto the existing
  `VData{tag;fields}` / `Switch_ctor` world; a tagless info-table scheme would open a representation gap).
- **New obligations the Wasm engine used to discharge**, now ours and codegen-pervasive: a **write
  barrier at every mutating *pointer* store**, **precise roots** (stack maps or a shadow stack) that
  separate tagged-word from raw-unboxed roots, the **shared/local partition invariant**, and **moving-GC
  safety** for the in-place byte builder and all interior access. These are the real cost of (a).
- **Two safety nets are lost going off Wasm GC** and must be compensated by the ABI's own discipline:
  the engine's **checked `ref.cast`** (recovered as a header-`kind` check) and the engine's **typed
  `call_ref` trap** — an arity/signature mismatch that *traps* under Wasm becomes UB / a silent wrong
  value natively (the exact gap [0033](0033-separate-compilation.md)/[0034](0034-effect-analysis-impurification.md)
  already cite), so the eval/apply arity discipline must be rigorous because the target will not catch
  the bug.
- The **cross-module boundary stays boxed** until measured; the hook to unbox it is reserved but not
  built, so no premature `.pmi`-cascade churn is incurred.
- The **concurrent runtime hazards** (parallel black-hole, scheduler interleaving the sequential oracle
  cannot validate) are surfaced and explicitly pushed to wall 2, so wall-1 stays sequentially
  oracle-checkable.

## Alternatives considered

- **Target Wasm GC** (lean on the engine collector, like `purs-wasm`). Rejected: a single
  engine-managed heap is incompatible with the owned **capability-local + shared-immutable** runtime
  ([sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md)), and the project's goal is to
  *own* the runtime ([0001](0001-phase-1-host-language-ocaml.md)). Choosing (a) is what makes this ADR
  necessary.
- **Port `purs-wasm`'s representation wholesale, engine reliance included.** Not available — the GC,
  typed-struct layout, checked casts, and typed calls do not exist natively; re-homing them *is* this
  ADR.
- **`i31` for `Int`.** Rejected — 31 bits cannot hold a 32-bit `Int` ([0006](0006-string-utf8-char-int.md));
  same as `purs-wasm`.
- **Tagless / info-table header (STG).** Rejected for the bootstrap: per-scan info-pointer indirection,
  an info-table codegen subsystem, and a representation gap against the tag-based VM/`.pvm`/oracle that
  would make the differential expensive — while strict semantics removes STG's entry-code-per-object
  payoff. The inline tagged header is chosen; only **pointer-tagging** is borrowed.
- **Whole-program representation analysis** (`purs-wasm`'s pre-per-module form). Rejected — it breaks the
  module-local / separately compiled discipline ([0033](0033-separate-compilation.md)). Reps are
  callee-determined and `.pmi`-published only if/when cross-module boundary unboxing lands.
- **Unbox the cross-module boundary now.** Deferred, **not dismissed** — `purs-wasm`'s apparent
  no-gain is an artifact of a corpus with no cross-module calls; the real cost awaits the wall-4
  measurement, and the `.pmi`-cascade cost argues against committing before then.
- **Unbox scalar data fields now.** Deferred — without the engine's typed subtypes it requires
  GC-consulted per-constructor layout descriptors, a larger change than the bootstrap needs.
