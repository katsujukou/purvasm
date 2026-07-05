# 0079. The context header becomes ABI: codegen-inline fast paths for rooting and the trampoline flag

- Status: ~~Proposed~~ **Accepted** _(2026-07-05: accepted by the maintainer after review round 3)_
- Date: 2026-07-05

> **Revision (2026-07-05, maintainer review round 1 — from the 0078 side):** collision map
> confirmed as absorbed by §5 (the two contact surfaces are `purvasm.h` and the
> `runtime/src/{gc,abi}.rs` rooting core; commit-order coordination only), and
> `purvasm-foreign` semantics survive unchanged (entries remain the provider API). Two
> additions folded: §1 now requires **compile-time layout static assertions**
> (`size_of` + per-field `offset_of!`) on both Rust mirrors — a mirror mismatch is layout,
> which Miri cannot detect — and §5 records that `PV_CTX_HEADER_VERSION`/`pv_abi_check`
> unify with (and implement) 0078 §5's previously unspecified driver-side ABI check.
>
> **Revision (2026-07-05, review round 2):** two P1s + one P2 folded. (1) The "handle =
> index, unchanged" claim saw only the release build — the **debug** runtime packs a slot
> generation into the handle (`abi_root`) and keeps `root_gens` bookkeeping; §2 now pins a
> **mode-switched contract** on the existing `--debug` axis: release = inline fast paths +
> bare-index handles (entries interoperate), debug = entry-calls-only emission (pre-0079
> IR, full generation net; inline and packed handles never coexist). (2) An entry-stub-only
> `pv_abi_check` cannot catch a stale separately-compiled module `.o`; §1 now requires a
> **per-object link-time version symbol** (`pv_ctx_abi_v<N>`, defined by the runtime only
> for its own version) with the runtime check demoted to backstop. (3) §1 now states
> normatively that **foreign providers must not touch `pv_ctx_header`** (the foreign
> surface stays the `pv_*` entries; two privileged consumers only), and pins the C
> spelling: `PVContext` remains opaque, prefix-castability is a documented guarantee, not
> an exposed struct member.

## Context

The v1 `llvm/ml` gap now has a measured anatomy. After
[0076](0076-direct-known-arity-calls-musttail.md) (module-local direct calls, 1.4–1.7×),
[0077](0077-cross-module-direct-calls-pmi-arity.md) (cross-module direct calls, ≈1.0×),
`clang -O2` (≈1.0×), inline scalar primops ([0072](0072-anf-to-llvm-lowering.md) §7, ≈1.0×),
the first [0072](0072-anf-to-llvm-lowering.md) §6 rooting slice (need-driven `eval_atoms` +
uncaptured globals, 1.06–1.16×) and the inline `force` immediate check (1.01–1.05×), the gap
stands at ≈3–16× and the hot-loop IR tells one story: **a `fib` iteration still crosses the
runtime `extern` boundary ~30 times**, and the survivors are overwhelmingly the *rooting
choreography*, not work:

- `pv_root` / `pv_get` — root a produced word, reload it after a possible safepoint;
- `pv_frame` / `pv_pop_frame` — open/close the activation's shadow-stack frame;
- `pv_settle` — in the common case, only *test* that no generic tail is stashed.

Each is a call into the staticlib (opaque to LLVM — no LTO), under a `catch_unwind` guard
frame ([0071](0071-codegen-runtime-c-abi.md) §7), to execute **a handful of loads and
stores**: the runtime's shadow stack is `roots: Vec<TaggedWord>` with handle = index,
frame mark = length, `get` = index load, `pop` = truncate; the trampoline stash
([0071](0071-codegen-runtime-c-abi.md) §4) is an `Option` whose common case is `None`.
Three consecutive ≈1.0× levers are the proof by exhaustion: what remains is the *boundary
itself*, and no optimisation on either side of it can see across.

[0071](0071-codegen-runtime-c-abi.md) §5 made these entries opaque **deliberately** — the
right call while the representation was in motion. This record revisits exactly that line,
minimally: expose the few words of runtime state these five operations touch, so codegen can
emit their fast paths as plain IR, and keep everything else — heap layout, allocation,
`apply`/`force`/`tailcall`, object headers — as opaque as today.

Timing constraint worth naming: [0078](0078-rust-foreign-bindgen-over-c-abi.md) is building
the Rust FFI layer with `include/purvasm.h` as its source of truth. Whatever this record
exposes must land **in that same header, once**, so the bindgen layer regenerates against it
in one coordinated step instead of chasing codegen-private layout knowledge.

## Decision

### 1. A versioned `pv_ctx_header` at offset 0 of the context, declared in `purvasm.h`

The context pointer (`pv_ctx` / `*mut Heap`) begins with a `#[repr(C)]` header:

```c
/* purvasm.h — the codegen/FFI ABI header (ADR-0079). Offset 0 of pv_ctx. */
typedef struct pv_ctx_header {
  uint64_t *roots_base;   /* the shadow stack's storage (moves only on growth)   */
  uint64_t  roots_len;    /* one past the top root = the next handle = the mark  */
  uint64_t  roots_cap;    /* fast-path bound; len == cap → slow-path pv_root     */
  uint64_t  pending_tail; /* 0 = no stashed generic tail (pv_settle fast path)   */
} pv_ctx_header;

#define PV_CTX_HEADER_VERSION 1
```

`include/purvasm.h` is the **single truth**: the runtime's Rust definition, the
[0078](0078-rust-foreign-bindgen-over-c-abi.md) sys layer, and codegen's emitted offsets all
derive from this declaration. The generated entry stub calls a new
`pv_abi_check(uint32_t version)` once at startup and the runtime aborts loudly on a
mismatch — a stale object compiled against another header version cannot run quietly.

**The C spelling, pinned** (review open question): `PVContext` **stays opaque** in the
foreign API — the header does *not* re-declare it as a struct with a leading member.
`pv_ctx_header` is a standalone struct plus one normative guarantee: *a `PVContext*` points
to storage whose first bytes are a `pv_ctx_header`*, and exactly **two consumers** may rely
on that prefix — generated code (raw `getelementptr` on the context word) and the runtime /
sys `#[repr(C)]` mirrors. **Foreign providers must not read or write `pv_ctx_header`**: the
supported foreign surface remains the `pv_*` entries
([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §2), unchanged — this record widens the
*generated-code* ABI, not the foreign-author API, and the header section is fenced and
labelled accordingly.

Three nets, at three times, because they catch different drifts:

- **Compile time — layout static assertions on every Rust mirror.** A mirror mismatch is
  layout, not aliasing, so **Miri cannot see it** (a silently re-ordered field would pass
  every dynamic check that happens to read consistent garbage). Both the runtime's
  `#[repr(C)]` definition and the [0078](0078-rust-foreign-bindgen-over-c-abi.md) sys
  layer's carry `const` assertions pinning `size_of::<PvCtxHeader>()` and each field's
  `offset_of!` to the header's documented values — a drift fails the build before anything
  runs. (Review finding from the 0078 implementation experience.)
- **Link time — a per-object version symbol.** An entry-stub-only runtime check cannot see
  a *stale separately-compiled module object* whose inlined offsets predate the header: the
  fresh entry object would pass while the old `.o` corrupts state. So **every generated
  object that emits header-offset fast paths references
  `pv_ctx_abi_v<PV_CTX_HEADER_VERSION>`** (an external byte the runtime staticlib defines
  only for its own version) — linking any stale object fails loudly with an undefined
  versioned symbol, per object, at zero runtime cost.
- **Run time — `pv_abi_check`** in the entry stub, kept as the belt-and-braces backstop for
  flows the linker does not mediate (and as the mechanism the
  [0078](0078-rust-foreign-bindgen-over-c-abi.md) §5 driver check consumes).

### 2. The runtime keeps ownership; the storage becomes header-managed

`roots` moves from `Vec<TaggedWord>` to a raw region owned through the header fields (the
same discipline as the heap semi-spaces: a stable raw base, no whole-buffer `&mut`
re-borrows, growth only inside the slow-path entry which updates `roots_base`/`roots_cap`).
Every Rust-side root operation reads/writes the *same* header fields the generated IR does;
`pending_tail`'s `Option` gains the header flag word maintained at its two mutation points.

**Handle representation, stated precisely** (review [P1] — the first draft's "index; length,
unchanged" saw only the release build): the ABI handle is the bare index **in release**;
the **debug** runtime packs a slot generation into the high 32 bits
(`abi_root`/`abi_get`, the use-after-pop net) and maintains `root_gens` bookkeeping an
inline root would neither produce nor update. So the contract is **mode-switched, aligned
with the existing `--debug` axis** (which already selects the debug staticlib):

- **Release** — codegen emits the §3 inline fast paths; handle = bare index on both sides,
  so inline operations and the (release) entries interoperate freely per call site, and
  foreign providers calling the entries see exactly today's handles.
- **Debug** (`purvm native --debug`) — codegen emits **entry calls only**, exactly the
  pre-0079 IR: every root goes through the guarded, generation-checked entries, preserving
  the full debugging net (a suspected rooting bug reruns under `--debug` unchanged).
  Inline fast paths and packed handles never coexist in one binary, by construction.

Nothing forces a big-bang migration of codegen sites (a release binary may mix inline and
entry sites — same representation), and the entries remain the API for foreign `.c`/Rust
providers, whose calls are not hot.

The invariant the IR must uphold (and naturally does, since its stores precede any call):
the header fields are current at every runtime entry — a collection observes exactly the
roots the generated code has pushed.

### 3. The inline fast paths codegen emits

| operation | fast path (IR) | slow path |
|---|---|---|
| `frame` | load `roots_len` | — (total) |
| `root` | load `len`/`cap`; if `len < cap`: store word at `base[len]`, store `len+1`, handle = `len` | `len == cap` → call `pv_root` (grows, updates base/cap) |
| `get` | load `base`, load `base[handle]` | — (total) |
| `pop_frame` | store mark to `roots_len` | — (total) |
| `settle` | load `pending_tail`; `0` → pass value through | `≠ 0` → call `pv_settle` |

`base` is reloaded from the header per operation (it moves on growth); between safepoints
these loads are ordinary IR that `-O2` can CSE — which is the point: for the first time the
rooting choreography is visible to the optimiser, and the `-O2`/inline-primop groundwork
already in tree starts paying.

### 4. What does not change

Heap layout, object headers, allocation, `apply`/`force_if_byneed`/`tailcall`/`run_effect`,
the leaf ABI, `guard()` on every remaining entry, the write-barrier hooks, and the
[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4 rooting discipline
itself (root-on-create, reload-after-safepoint, pop-before-`musttail`) — this record changes
*who executes* five operations, not *when they happen*.

### 5. Sequencing with ADR-0078

One coordinated landing: the header gains `pv_ctx_header` + `pv_abi_check`, the runtime
restructures its storage behind it (Miri island discipline re-verified), the
[0078](0078-rust-foreign-bindgen-over-c-abi.md) layer regenerates once (its leaf-test suite
drives the restructured rooting core through the rlib and doubles as the regression
detector), then codegen switches its emission. Until the codegen step lands, the header
change is inert — the entries still work — so the runtime/FFI work is not blocked
mid-flight.

A unification falls out (spotted in the 0078 review): `PV_CTX_HEADER_VERSION` +
`pv_abi_check` **become the mechanism** for the driver-side ABI check
[0078](0078-rust-foreign-bindgen-over-c-abi.md) §5 pinned but left unspecified — the bundle
build verifies the ABI's own version constant instead of a bespoke version string.

## Consequences

- The ~15–25 rooting/trampoline crossings per hot iteration become loads/stores; this is the
  remaining majority of the measured boundary count, and the first change that lets the
  optimiser see a whole generated body. Success metric: the paired
  [0075](0075-cross-backend-wall-clock-benchmark-harness.md) table in this record's Progress
  note (the honest expectation, given the lever history: this is the largest single movement
  available, but the `pv_apply`-shaped remainder — PAP CAFs, Effect chains — is optimizer
  territory and stays).
- A new, small, versioned ABI surface: four header fields + one check entry, declared once in
  `purvasm.h` for all three consumers. The cost is real (layout is now contract), bounded
  (nothing else is exposed), and guarded (version check aborts, Miri gates the runtime side,
  the forced-GC differentials gate the codegen side).
- Runtime-side work touches the rooting hot core; it must re-establish the
  [Miri island discipline](0064-v1-single-capability-native-abi-codegen-contract.md) the heap
  buffer already follows.

## Alternatives considered

- **Status quo + more static root elision** ([0072](0072-anf-to-llvm-lowering.md) §6 slices
  2a/2b: raw env entries for let-bound immediates, frame elision for root-free bodies).
  Cheap and compatible — and worth doing *after* this record, where they compound — but each
  is another measured ≈1.0–1.05× against a boundary that stays; they cannot reach the calls
  that remain legitimately needed.
- **Cross-language LTO** (bitcode staticlib + `-flto` link) instead of any ABI exposure.
  Attacks the same wall without a contract change, but couples the Rust and clang LLVM
  versions (fragile even under nix pins, worse on ld64), leaves the `catch_unwind` guard
  frames in place, and makes the differential's failure modes toolchain-shaped. Rejected for
  v1; re-evaluable later as a *complement*.
- **`static inline` C accessors in `purvasm.h` only** (no codegen change). Serves the FFI
  audience but not the hot path: codegen emits `.ll`, not C, so it would need a
  compile-a-shim-to-bitcode-and-`alwaysinline` pipeline — the same layout exposure plus build
  complexity, minus the direct emission. The header *declaration* from §1 is kept; the
  accessor form can be added for FFI ergonomics independently.
- **Shrink `guard()` instead** (drop `catch_unwind` from the pure entries). Removes the guard
  cost but keeps the call and the optimisation barrier — the measured problem. (The scalar
  prim entries are already unguarded and were still worth inlining away.)
- **Expose more while we're at it** (allocation bump pointer, object headers). Each is a
  larger contract with its own failure modes (safepoint discipline around an inline
  allocator is a different design); this record deliberately exposes only the five
  operations the measurements convict.
