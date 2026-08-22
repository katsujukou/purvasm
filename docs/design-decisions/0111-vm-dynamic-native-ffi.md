# 0111. VM dynamic native FFI: load the provider, apply through the runtime

- Status: ~~Proposed~~ **Accepted** _(2026-08-16: accepted by the maintainer after three review
  rounds — §1.1's retention/export of both the `pv_*` API and the runtime's `pvf_*` leaves, §3's
  array promotion, §4's `host-runtime` provider class and scoped exactly-one, §5's pre-initialiser
  version failure, and §6's trusted loader API)_
- Date: 2026-08-16

> **Revision (2026-08-16, review round 1):** §1.1 is new — the VM's link must *retain* and *export*
> the whole foreign API (a static archive plus dead-strip does neither by itself). §2 scopes
> `pv_apply` to carrier-held runtime closures only. §3 replaces the elementwise array copy with
> **promotion**, because VM arrays are identity-bearing. §4's eager check is scoped to
> manifest-declared workspace keys, so it does not reintroduce
> [0091](0091-user-native-ffi-c-sibling-rust-dir.md) §1's dead-reference false positive. §5 is
> rewritten: `dlopen` runs initialisers before returning, so the version check must be a resolution
> failure (`PV_FOREIGN_ABI_VERSION` + an undefined host-symbol reference + `RTLD_NOW`), not a
> post-load inspection. §6 is new — the trusted loader API, which never exposes a code address as a
> guest value.
>
> **Revision (2026-08-16, review round 2):** §3 now *fixes* the promoted-array representation (a
> shared indirection cell) and spells out the migration against an ABI with no blank-array
> constructor, including the empty-array case; it also records that an effect thunk's result is a
> carrier `Unit` on the success path. §6 pins `ModuleHandle` to a range-checked loader-table index —
> `foreign import data` is opaque to the type system, not to the GC — and adopts
> `RTLD_NOW | RTLD_LOCAL`. §1.1's retained/exported list explicitly includes `pv_foreign_abi_v<N>`.
>
> **Revision (2026-08-16, review round 3):** the **runtime's own `pvf_*` leaves** are now a provider
> class. §1.1 retains and exports them (derived by `nm`, not hand-listed); §4 introduces the
> `host-runtime` pseudo-provider and extends exactly-one — including the named runtime-shadow
> collision — across it; §2 asks each provider separately; §6 registers the host as provider zero via
> a self-handle. Without this the VM could not resolve `show`, stdio, FS or `argv`: they live in the
> staticlib, are referenced by no VM code, and were being dead-stripped and never searched.

## Context

[0110](0110-owned-vm-purescript-native.md) starts an owned VM — a PureScript interpreter compiled to
a native executable — and names user FFI as its driver. This record designs that mechanism.

The constraint that shapes everything: **one authoring surface**. A `foreign import` is backed by a
`.c` sibling or one Rust crate ([0091](0091-user-native-ffi-c-sibling-rust-dir.md)), written against
[`runtime/include/purvasm.h`](../../runtime/include/purvasm.h) ([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §2),
exported as `pvf_<escapeIdent(key)>` ([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §3).
The VM must run *that* provider — not a second one written against a VM-specific ABI. A user who
writes their FFI once should be able to run it on both backends.

Four facts about the existing ABI decide most of the design:

1. **`pv_make_closure(ctx, code_addr, arity, env)` takes a real code address** (`runtime/src/abi.rs`,
   [0071](0071-codegen-runtime-c-abi.md) §3). A `dlsym`ed `pvf_*` can therefore become an ordinary
   purvasm closure value, and `pv_apply` can call it — so the VM needs no calling convention of its
   own *for leaf values*. Its own closures remain its own business
   ([0110](0110-owned-vm-purescript-native.md) §1.1).
2. **The VM runs on the runtime.** Its heap and GC are the runtime's, so a value it
   hands a leaf is an ordinary runtime value and the leaf's rooting contract
   ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §3) behaves exactly as it does natively.
   There is one heap and one `pv_*` implementation — no shim, no second semantics.
3. **The ABI has typed accessors but no introspection.** `purvasm.h` exposes constructors, typed
   readers (`pv_int_payload`, `pv_number_bits`, `pv_str_len`/`pv_str_copy`, `pv_array_len`,
   `pv_read_field`, `pv_record_get`, `pv_closure_env`), `pv_apply`, `pv_force_if_byneed` and the
   rooting frame — and deliberately *nothing* that answers "what kind is this word?"
   ([0069](0069-v1-dynamic-record-operations.md) representation opacity). A returned value therefore cannot
   be *walked* into a VM value.
4. **Records do not cross the FFI boundary today, on either backend.** `pv_new_record` /
   `pv_record_get` take label *ids* (sorted FNV-1a-64, [0069](0069-v1-dynamic-record-operations.md)) that
   only codegen mints; `purvasm.h` exposes no way for a provider to obtain one, and
   `crates/purvasm-foreign` ([0078](0078-rust-foreign-bindgen-over-c-abi.md)) exposes no record API
   at all. This is a pre-existing gap in the *native* FFI surface, not something the VM introduces.

## Decision

### 1. The provider is the same source, built as a shared object

For the VM, a provider is built as a **shared object** whose `pv_*` references are left **undefined**
and resolved at `dlopen` time against the VM executable — which already contains the runtime. One
runtime instance, one heap, the real `pv_*`.

- C: the same `.c`, the same `-DPVF_MODULE=<escapeIdent(M)>`, `clang -shared`
  (+ `-undefined dynamic_lookup` on darwin); the VM executable links with its dynamic symbol table
  exported (`-Wl,--export-dynamic` on ELF) so `pv_*` stay visible to a loaded module.
- Rust: the same `purvasm-foreign` crate, built `crate-type = ["cdylib"]`, *without* folding the
  runtime in — the bundle-staticlib dance of [0091](0091-user-native-ffi-c-sibling-rust-dir.md)
  §Addendum exists to avoid two `libstd`s in one *static* link and does not apply here.
- `purvasm.h` is unchanged for the author; `PVF_EXPORT` works verbatim. The only build-side
  difference is the output kind.

A throwaway spike confirmed the loading contract on darwin: a shared object whose `pv_*` were
undefined resolved them back into the host executable at `dlopen`, and the host serviced them
re-entrantly.

#### 1.1 The host must actually *contain* and *export* what a provider resolves against

Resolving `pv_*` against the VM executable only works if those symbols are in it. Two link-time
facts break that by default, and neither is fixed by `--export-dynamic` (which controls *export* of
symbols that survive, not *survival*):

- the runtime is a **static archive**, and an archive member is pulled in only if something already
  references it — the VM does not itself call, say, `pv_new_record`;
- the native link **dead-strips** (`--gc-sections` / `-dead_strip`, [0072](0072-anf-to-llvm-lowering.md)),
  so even a member that was pulled in is dropped when unreferenced.

A provider calling an API the VM happens not to use would then fail to load. So the VM's link must
pin **retention and export explicitly**.

The same two facts hit a second, larger symbol set — **the runtime's own `pvf_*` leaves**. The
runtime staticlib *is* a provider class ([0091](0091-user-native-ffi-c-sibling-rust-dir.md) §1's
"runtime"): `runtime/src/leaf.rs` defines `pvf_Data_2eShow_2eshowIntImpl`,
`pvf_Purvasm_2eStdio_2ewriteLineImpl`, the FS / `getenv` / `argv` / `Purvasm.Number` leaves and the
rest — around thirty symbols that a *guest* program references and the **VM itself never calls**.
Under the default link they are exactly what archive-extraction and dead-strip discard, and §2 would
not have looked for them anyway. A VM that dropped them could not run `show`, could not print, and
could not read a file: the ordinary corpus, not an exotic case.

So the retained-and-exported set has two parts, and only the first is hand-listed:

- **the foreign-author API** — the `pv_*` block of `purvasm.h` (§3's `pv_adt_tag` included), **plus
  §5's `pv_foreign_abi_v<N>`**, which is not an API function but is referenced by every module and so
  must be retained and exported on exactly the same footing;
- **every `pvf_*` the runtime staticlib defines** — enumerated the way
  [0091](0091-user-native-ffi-c-sibling-rust-dir.md) §4 already enumerates it, `nm` over the staticlib
  (`llvm-nm` preferred, per 0091 §Addendum). The set is therefore *derived*, never hand-maintained: a
  leaf added to `leaf.rs` is retained by the next build with no edit here.

From that union the VM's link inputs follow:

- an **anchor object** referencing every entry (so archive members are pulled in and dead-strip sees a
  root), a **forced-undefined list** (`-Wl,-u,…`, `-Wl,--undefined=…`), and an **export allowlist** so
  exactly that union — and not the runtime's internals — is dynamically visible;
- **both platforms pinned**: ELF (`--undefined` + `--export-dynamic`/version script, retained under
  `--gc-sections`) and Mach-O (`-u _pv_*` + `-exported_symbols_list`, retained under `-dead_strip`);
- **gated twice**: a provider fixture that calls at least one function from *each* API group —
  rooting, scalar accessors, constructors, field access, apply/force — is built and loaded; and a
  guest program that calls a *runtime* leaf from each family (`show`, a write, an FS read, `argv`)
  runs on the VM. A symbol the retention rules dropped fails one of those by name, rather than in a
  user's project.

This is why §Alternatives keeps "ship the runtime as a shared library" on the table: it removes this
whole class of link-retention work at the cost of a non-self-contained distribution.

> **Correction (2026-08-17, as implemented):** three departures, one of them a defect this record's
> own wording invited.
>
> - **The export allowlist is not optional, and "export dynamically" is not it.** An earlier
>   implementation read this section's `--export-dynamic` as *make the symbols visible* and let the
>   platform default stand on Mach-O. Measured, that exported **635** symbols — every `pv_g_*`
>   generated global, the Rust runtime's internals, and, decisively, the VM's own
>   `pvf_Purvasm_2eVM_2eLoader_2e*` leaves. A guest could then declare `foreign import` on
>   `Purvasm.VM.Loader.resolveImpl`, resolve it through the ordinary frontier, and hold the trusted
>   loader — exactly what §6 exists to prevent. The allowlist is therefore an explicit
>   `-exported_symbols_list` (Mach-O) / version script (ELF) naming the retained set and nothing else.
>   Measured after: **56** exported, 0 loader leaves, 0 `pv_g_*`, with runtime leaves still resolving.
> - **The `pv_*` API is derived from `purvasm.h`, not hand-listed.** This section said hand-listed,
>   with a fixture as the net. That is one-directional — it cannot see an API the header gained and
>   the list did not — and the hand-list had already acquired `pv_abi_check`, which the header files
>   under GENERATED-CODE ABI and a provider must never call. Parsing the header's author region is
>   both simpler and exact (verified against an independent extraction: 28 names, identical).
> - **No anchor object.** `-Wl,-u,<sym>` alone pulls the archive member in *and* roots it against
>   dead-strip, which is what the anchor was for. ~~Measured on Mach-O only; the same claim for
>   ELF + `--gc-sections` is **owed a measurement** before it is relied on, and until then this is a
>   Mach-O-verified mechanism, not a cross-platform one.~~ **Measured on ELF (2026-08-18) — see below.**

> **Progress (2026-08-18): the ELF measurement, and what it changed.** `tools/elf-export-probe.sh`,
> run in CI on clang/LLD 21.1.7 + GNU ld 2.44, varies only how the host exposes its symbols and
> reports retention and export *separately* (an executable may legitimately contain a symbol it does
> not export, so one column cannot answer both):
>
> | mode | retained | exported | leaked | dlopen |
> | --- | --- | --- | --- | --- |
> | none | yes | no | no | fail |
> | version script | yes | no | no | fail |
> | `--export-dynamic` + version script | yes | yes | no | ok |
> | `--dynamic-list` | yes | yes | no | ok |
>
> - **Retention carries over; the owed claim is settled.** `RETAINED` is `yes` in the `none` row —
>   `-Wl,-u` alone, no export flag anywhere, an archive member nothing references, under
>   `--gc-sections`. The anchor object stays deleted, and this is now a cross-platform mechanism
>   rather than a Mach-O-verified one.
> - **Export did NOT carry over, and the implementation was wrong.** An ELF executable populates
>   `.dynsym` with nothing by default, and a version script only *filters* what is exported. The VM
>   linked that way exported **0** symbols and could load no provider at all — a failure invisible on
>   Mach-O, where `-exported_symbols_list` does both jobs. The ELF branch is now `--dynamic-list`,
>   which states the wanted thing directly (put exactly these in `.dynsym`) in one flag, with the
>   leak check confirming it does not widen the set. This record's prose said
>   "`--export-dynamic`/version script"; both work, and the one-flag form is what ships.
>
> Two defects surfaced only because the ELF host was broken, and both are fixed:
>
> - **The loader misdiagnosed the failure.** An unresolved `pv_foreign_abi_v<N>` has two causes — a
>   provider built against another ABI (§5), and a host that does not export the symbol at all — and
>   every platform reports both as a missing symbol. The VM blamed the *provider*, sending the reader
>   to rebuild something that was already correct. The loader now decides in the order the evidence
>   is trustworthy: **first** it asks whether this host exports its own stamp, through `dlsym` on the
>   `host-runtime` handle — a fact about the binary rather than a reading of a message — and only
>   then reads a version, and only out of the platform's undefined-symbol field (`undefined symbol:`
>   / `flat namespace '…'`), validated as a canonical `pv_foreign_abi_v<N>` with nothing adjacent.
>   Searching the whole `dlerror()` string was not enough: it contains the provider's **path**, so a
>   file named `pv_foreign_abi_v99-bad.so` could forge either verdict for an unrelated missing
>   symbol. `tools/vm-loader-e2e.sh`'s `spoofed-path` leg is that case, built under exactly that
>   filename, and requires the plain refusal naming the symbol that is actually missing.
> - **The stale-module gate passed vacuously.** With nothing loadable, the stale module is refused
>   too, and the negative leg reported OK for a reason unrelated to the version. It now carries the
>   positive control's verdict and reports `INCONCLUSIVE` (a failure) without it — the same rule
>   §5.3 of [0109](0109-native-leaf-direct-lowering.md) applies to a noise floor: a negative result
>   is only readable when the positive one holds.
>
> **The gate is green on both platforms (2026-08-18).** With the ELF branch on `--dynamic-list`, the
> Linux run reports `allowlist 57, exported 57` and *exactly the allowlist, nothing else* — no
> surplus at all, not even the linker-defined symbols an executable usually carries in `.dynsym`,
> because `--dynamic-list` adds only the names it is given. macOS reports the same 57, for the same
> reason: the set is one derivation (28 header entries + the stamp + 28 runtime leaves), and only
> the file format differs. All six checks pass on each platform — the export set, API coverage, the
> load, the version's positive and negative legs, and the spoofed path — so §1.1 is measured end to
> end rather than on Mach-O alone. The `--export-dynamic` spelling this record's prose gives is a
> mechanism that works (the probe says so) but is not the one that ships.
>
> Also implemented, and not in this record: `--host-foreign-api` is **refused together with
> `--rust-ffi`**. That mode links the *bundle* (the runtime rlib folded with the app crate,
> ADR-0078 §5), whose `pvf_*` cannot be told apart by origin — so every app leaf would be retained and
> exported as though it were the runtime's, silently joining `host-runtime`'s provider set and
> defeating §4's exactly-one check. Refusing beats guessing until provenance can be separated.

### 2. Resolution and firing: delegate to the runtime's `apply`, at the carrier only

A `ForeignRef key arity` instruction ([0110](0110-owned-vm-purescript-native.md) §4) resolves to a
value the VM builds once and caches:

1. `pvf_<escapeIdent(key)>` — the same mangling the LLVM backend emits (`Backend.LLVM.Mangle`'s
   `mangleForeign`);
2. `dlsym` it **in each provider separately** — the `host-runtime` pseudo-provider (the VM executable
   itself, §6) *and* every loaded module — and require exactly one to define it (§4);
3. `pv_make_closure(addr, arity, unit)` — a real closure value, held in the VM's foreign carrier
   ([0110](0110-owned-vm-purescript-native.md) §3).

Step 2 searches the host because the runtime staticlib linked into the VM is itself a provider class
(§1.1): `Data.Show.showIntImpl`, `Purvasm.Stdio.writeLineImpl`, the FS and `argv` leaves and the rest
are satisfied there, with no module loaded at all. A program that uses only runtime leaves therefore
needs no `--ffi` argument and no manifest — which is most of the existing corpus.

Firing is `pv_apply(closure, args)`. **This is the calling convention for carrier-held runtime
closures only** — a resolved leaf and any closure a leaf returned. The interpreter's own eval/apply
([0110](0110-owned-vm-purescript-native.md) §1.1) is untouched: a VM closure is a code block plus an
environment, is applied by the VM, and never reaches `pv_apply`. What delegation buys is that
*within* that carrier, over- and under-application, effect thunks, and by-need forcing behave exactly
as they do natively, because they *are* the native paths. The VM contributes argument conversion (§3)
and nothing else.

`arity` comes from the image, never from the module ([0110](0110-owned-vm-purescript-native.md) §4):
it is the compiler's `leafClosureArity` over the reconstructed foreign shape
([0080](0080-foreign-signature-reconstruction-cst.md)), the same number native codegen already uses.

### 3. The boundary: convert immutables in, promote arrays, carry results out

This boundary is the **only** place the VM knows the native representation
([0110](0110-owned-vm-purescript-native.md) §3): the bytecode format stays free of it, and every
native-ABI encoding the conversion needs is *computed here* from what the format already carries.

**Going in (`toPv`)** — total over what a first-order leaf can take. `Int`, `Number`, `Boolean`,
`String` and `Unit` are converted with the corresponding constructors; a value already in the carrier
passes through unchanged; a by-need cell is forced first. A **data value**
converts with `pv_new_adt`, whose tag is derived from the constructor *name* the bytecode carries by
the native backend's own `ctorTag` (`fnv1a64(name).lo & 0x7fffffff`,
`Backend.LLVM.Mangle`) — a pure function, so the two sides agree by construction and no tag table is
stored anywhere. A VM closure, an under-applied constructor, or a record is **not** converted — each
is a named boundary error (`foreign boundary: <what> crossed <key>`), the shape boot's `Vm.Foreign`
already uses.

**Arrays are promoted, never converted.** A VM array is identity-bearing — `SetArray` mutates it in
place, `Ref`/`STRef` *are* one-element arrays, and no bytecode distinguishes a finished array from
one still under construction ([0110](0110-owned-vm-purescript-native.md) §3). An elementwise copy
would therefore be a correctness bug, not a cost: a leaf's write would land on a copy, and two VM
bindings holding the same array would stop agreeing.

**The representation, fixed here.** Promotion must be visible to every alias, so the forwarding state
lives in the array *value*, not at a binding — switching one `VArray (Array Value)` binding to a
carrier would leave every other reference, including one inside a data value, pointing at the old
storage. A VM array is therefore an indirection cell from creation:

    VArray ArrayCell
    ArrayCell = Ref (Local (Array Value) | Promoted LeafValue)

Every alias shares the `Ref`, so one write to it promotes them all at once. `Local` is the ordinary
case and costs one dereference per array operation; `Promoted` routes the same operations to
`pv_read_field` / `pv_write_field` (with `toPv` on the written value).

**The migration, given that `pv_new_array` wants a finished vector.** The ABI has no blank-array
constructor — `pv_new_array(elems, n)` takes `n ≥ 1` already-built words, and `pv_empty_array()`
covers `n = 0` — so the "allocate, forward, then fill" order is spelled out:

1. `n = 0` → `pv_empty_array()`; write `Promoted` and stop. (That constructor returns the canonical
   empty array, so two VM empty arrays promote to *one* object. Harmless: an empty array has no slot
   to write and purvasm has no value-identity primitive, so the merge is unobservable.)
2. `n ≥ 1` → build the runtime array **blank**: `pv_new_array` over `n` copies of `pv_unit()`.
3. **Write `Promoted carrier` into the cell now**, before any element is migrated. A cycle — an array
   reachable from itself, directly or through a data value — then terminates on finding the cell
   already `Promoted`, exactly as a copying collector's forwarding pointer does.
4. Migrate element `i` by the same boundary rules (recursively promoting nested arrays) and store it
   with `pv_write_field(carrier, i, …)`.

Rooting across that loop needs no shadow-stack work: step 3 puts the carrier in an ordinary
PureScript field, so it is traced — and *updated* — by the GC like any other value, and every element
migration re-reads it from the cell. This is a property of the VM being a purvasm program rather than
a foreign leaf: **the VM never holds a runtime word outside a traced field**, so the rooting contract
of [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §3 is the compiler's problem, not the
interpreter's.

Promotion is one-way and permanent, so **every alias — VM-side and leaf-side — observes one
object**. The cost is one dereference per array operation always, plus the accessor path for arrays
that have crossed; an array that never meets a leaf pays nothing more.

Because a `Ref` is a one-element array, this is also the whole of `Ref` support: nothing special is
required for it.

**Coming out (`ofPv`)** — there is no conversion. The result enters the VM as the **opaque carrier**,
for two independent reasons: fact 3 above makes a walk inexpressible, and decoding an identity-bearing
result into a fresh VM value would break the same invariant promotion exists to protect.

**Decoding happens at the use site.** The bytecode is generated from a well-typed program, so every
site that *eliminates* a value already knows the shape it demands. Each such site, on meeting the
carrier, uses the matching typed accessor: a scalar primop reads the payload; `Proj` reads a field;
an array length/index uses `pv_array_len`/`pv_read_field`; `Call` uses `pv_apply`; a force uses
`pv_force_if_byneed`. Nothing branches on representation, so [0069](0069-v1-dynamic-record-operations.md)'s
opacity is preserved: the VM only ever asks a question the program's type already answered.

One accessor is missing for that to be complete: **`SwitchCtor` on a leaf-returned data value needs
the constructor tag**, and no `purvasm.h` call reads it. This record proposes adding

    uint32_t pv_adt_tag(PVContext *ctx, PVWord adt);

to the runtime and the header — a shape-checked typed accessor in the same family as `pv_array_len`,
additive, costing the native path nothing. The dispatch is then `pv_adt_tag v` against each arm's
`ctorTag name`, the same derivation `toPv` uses. Without it a leaf may not *return* a data value,
which rules out `Maybe`/`Either`-returning leaves; that is too common to accept.

**Effect leaves** need no special case. A `retVsat` leaf's saturated result is an effect thunk — a
runtime closure in the carrier — and the VM runs an effect by applying it to unit, i.e. `pv_apply`.
Note what that returns: a *runtime* `Unit`, hence a carrier. For an ordinary `main :: Effect Unit`
ending in a native leaf (`Console.log "x"`), the program's final value is therefore a carrier on the
**success** path — which is why [0110](0110-owned-vm-purescript-native.md) §5's terminal demand for an
`Effect` entry is *run and discard*, not an inspection. Nothing here decodes a `Unit`, and nothing
needs to.

**The aliasing gate.** The invariant above is exactly the kind that a passing unit test does not
establish, so it gets a dedicated one: bind the same array to two VM names (and once inside a data
value), hand it to a leaf that writes an element, and observe the write from *every* alias — plus the
`Ref` form of the same test (`Ref.write` from the guest, read back through a leaf, and the reverse).
A cyclic array is in the same fixture, for step 2 above.

**Records are out of scope, and the reason is upstream** (fact 4). A record crossing the boundary is
a named error in both directions. Making records crossable requires a *native* ABI addition (a
supported way for a provider to obtain a label id) plus a DX-layer API in
`crates/purvasm-foreign`; that is its own record, and the VM should not invent a private answer to it.

### 4. Which providers exist, and the exactly-one-provider invariant

**The provider classes.** [0091](0091-user-native-ffi-c-sibling-rust-dir.md) §1 has four (runtime,
ulib, app-C, app-Rust); the VM has the same population reached two ways:

- **`host-runtime`** — the VM executable itself, always present, satisfying every `pvf_*` the runtime
  staticlib defines (§1.1). It is registered in the loader table as provider zero (§6), so it is not
  a special case in the resolver: it is a handle like any other.
- **loaded modules** — a ulib provider built as a shared object, or the app's own C sibling / Rust
  crate, each named explicitly (below).

Loading a shared object executes arbitrary native code, so *that* part is **explicit and opt-in**:
the VM loads only what it is told to load — `--ffi <path>`, repeatable — plus, when present, a
**build-emitted manifest** beside the image naming the providers the build already resolved (the
native link computes exactly this map, [0091](0091-user-native-ffi-c-sibling-rust-dir.md) §1). No
implicit discovery from the working directory or the environment. `host-runtime` needs no opt-in: it
is the binary the user already chose to run.

**The eager check is scoped, for [0091](0091-user-native-ffi-c-sibling-rust-dir.md) §1's reason.**
An earlier draft checked every `ForeignRef` in the image at startup. That reintroduces exactly the
false positive 0091 §1 was written to avoid: the referenced-key set **over-approximates** what a run
needs. There, a `.ll` textually names a `@pvf_` for every eta it lowers, including dead ones that
dead-strip removes; here, an image reaches a `ForeignRef` inside a *reachable definition* whose
branch may never execute, and the VM has no dead-strip and no liveness result with which to tell the
difference. A blanket zero-provider check would reject programs that run fine.

So the scoping is 0091's, transplanted:

- **eagerly**, over the keys the **manifest declares as workspace-provided** — the classes the user
  authors, where the build knows a leaf is referenced *iff* it is used, and where a missing `.so` is
  the likely error: **zero providers** → `no native provider for M.foo`, named before the program
  runs; **more than one** → an error naming the conflicting providers, the VM's form of
  [0091](0091-user-native-ffi-c-sibling-rust-dir.md) §4's exactly-one invariant;
- **lazily**, for every other key: resolution happens when the `ForeignRef` executes, and an
  unprovided key is a named error there (`unbound native foreign: M.foo`, boot's wording). A dead
  reference is never resolved, so it never fails.

With no manifest (a bare `--ffi` invocation), everything is lazy. The eager check is an *added*
diagnostic over declared workspace keys, never the mechanism the run depends on.

**Exactly-one spans `host-runtime` too, and that collision has a name.** Because step 2 asks each
provider separately, a module that exports a key the runtime already defines is *detected* rather
than silently preferred — the failure 0091 §Addendum calls the "runtime-shadow" case, where archive
member selection or load order would otherwise pick a winner quietly. A user provider exporting, say,
`pvf_Data_2eShow_2eshowIntImpl` is therefore the named error `M.foo provided by both host-runtime and
<module>`, at whichever point that key is resolved (eagerly for a declared workspace key, at first use
otherwise). A duplicate is never resolved by precedence: the VM has no shadowing rule, deliberately,
because "which `show` am I running?" is not a question a user should have to ask.

A module is never `dlclose`d: a closure built in §2 holds a code address into it. That is the
loader's contract, and §6 gives it a place to live.

### 5. ABI version: refuse the load, do not inspect after it

A stale module must fail by name — and it must fail **before it can run any code**. An earlier draft
had the loader read a version symbol *after* `dlopen`, which does not achieve that: `dlopen` runs the
module's initialisers (`init_array` / `+load` / a Rust `ctor`) before it returns, so a stale module
would already have called into a `pv_*` surface it disagrees with. The check must therefore be the
loader's *resolution* step, not a post-load inspection.

Three corrections, all forced by that:

- **Its own version counter.** `PV_CTX_HEADER_VERSION` versions the `pv_ctx_header` *layout*
  ([0079](0079-ctx-header-abi-inline-rooting-fast-paths.md)), which is generated-code ABI and
  explicitly *not* the foreign-author surface. The thing being versioned here is the foreign API, so
  it gets **`PV_FOREIGN_ABI_VERSION`**, bumped when that surface changes incompatibly (an additive
  call such as §3's `pv_adt_tag` does not bump it).
- **A reference, not a stamp.** The module emits an **undefined reference to a host symbol whose name
  carries the version** — `pv_foreign_abi_v<N>`, exported by the VM per §1.1's allowlist. Loading with
  **`RTLD_NOW`** (with `RTLD_LOCAL`, §6) resolves every reference before initialisers run, so a module built against a
  different N fails at load with that symbol named, and no module code executes. The loader turns that
  into a diagnostic naming the module path and the expected version rather than passing the raw
  `dlerror` through.
- **Both languages emit it, and unconditionally.** C gets the reference from `purvasm.h`, so the
  author writes nothing and no build flag selects it; Rust gets it from `purvasm-foreign`
  ([0078](0078-rust-foreign-bindgen-over-c-abi.md)), which never compiles the header — a `cdylib`
  would otherwise carry no stamp at all. One constant, two emitters, one symbol name. Emitting it
  unconditionally means a **statically** linked provider gets the same protection from the runtime
  staticlib's definition, exactly as [0079](0079-ctx-header-abi-inline-rooting-fast-paths.md)'s
  `pv_ctx_abi_v<N>` protects generated code — the dynamic path is then not a special case but the
  same contract, enforced by a different linker.

> **Progress (2026-08-17, as implemented):** the contract is live — `PV_FOREIGN_ABI_VERSION` and the
> reference to `pv_foreign_abi_v<N>` are in `runtime/include/purvasm.h`, the symbol is defined by
> `runtime/src/abi.rs`, and `tools/vm-loader-e2e.sh` holds the refusal to its terms. Four things the
> record left open, decided here:
>
> - **The version is written once, and every other spelling is derived from it.** The header pastes
>   the symbol name from the `#define` (so a bump renames the reference by editing one number), and
>   the VM's link derives the symbol it must export by *reading that `#define`*
>   (`Purvasm.CLI.NativeLink.foreignAbiStamp`) rather than naming it. A hand-written list would fail
>   in the one direction that matters: a host still exporting `…_v1` while every provider references
>   `…_v2` refuses every provider, and the two facts would have lived in different files. The
>   `#[no_mangle]` name in Rust cannot be computed, so *that* paste is instead pinned by a
>   `const _: () = assert!(…)` against the mirrored constant — a compile error rather than a
>   `dlopen` failure at a user's machine. The version is handled as a **token**, never a number:
>   the header pastes it, so `01` makes every provider reference `pv_foreign_abi_v01`, and a host
>   that parsed-and-reprinted it would export `…_v1` and refuse them all. Only a canonical decimal
>   is accepted (a sign cannot be part of an identifier at all), and it is pasted verbatim — which
>   also stops an ABI version from being bounded by the host language's integer range.
> - **Rust emits it per leaf, not once per crate.** `#[used]` keeps a static in its object file, but
>   the linker may still drop an object nothing references, and a `cdylib` carrying no reference is
>   exactly the module the loader must refuse. So `#[pv_foreign]` places the reference beside each
>   exported wrapper, where the symbol that must survive already lives. Verified on a real `cdylib`:
>   `pvf_…` defined, `pv_foreign_abi_v1` undefined, as with a C provider's object.
> - **The loader names the failure.** The platform reports a stale module as an ordinary missing
>   symbol, which reads as a host that forgot to export something; `Loader.c` recognises the shape
>   and reports "built against a different foreign ABI — this VM provides
>   `PV_FOREIGN_ABI_VERSION=<N>`" instead.
> - **The statically linked half is weaker than the dynamic half, and is not claimed.** The
>   reference protects a static link because the archive member must be found to resolve it; whether
>   dead-strip could ever discard the referring datum *before* resolution is not something this
>   change measured. The dynamic path — the one this record exists for — is measured, on Mach-O:
>   the bumped module is refused, and its initialiser marker never fires.

### 6. The loader's own API: privileged, and never a raw address

The VM is PureScript, so `dlopen`/`dlsym` reach it as leaves — and the obvious shape of those leaves
is the wrong one. `dlsym : Handle -> String -> Effect Int` would put a **code address into a guest
scalar**, where ordinary arithmetic can reach it and `pv_make_closure` would happily believe whatever
it is handed. The privileged surface is therefore drawn so that a code pointer is *never* a guest
value:

- `load : String -> Effect ModuleHandle` — `dlopen` with **`RTLD_NOW | RTLD_LOCAL`**, returning an
  **opaque** handle. `RTLD_NOW` is what makes §5's version check a pre-initialiser failure;
  `RTLD_LOCAL` keeps a provider's symbols out of the global namespace, so one provider can neither
  see nor interpose on another's, and a per-handle `dlsym` becomes the honest basis for §4's
  exactly-one determination (each module is asked separately, rather than the loader observing
  whichever definition won a global merge).
- ~~`resolve : ModuleHandle -> String -> Int -> Effect (Maybe LeafValue)`~~ (see the Correction below) — mangle the key
  (`pvf_<escapeIdent(key)>`), `dlsym` it in *that* module, and, **without the address ever becoming a
  purvasm value**, wrap it with `pv_make_closure` at the given arity. The result is a carrier-held
  closure or `Nothing`; the address exists only inside the leaf.

**`ModuleHandle`'s representation is pinned, because `foreign import data` does not pin it.** An
opaque type is opaque to the *type system*; at run time the value is whatever the leaf returned, and
the GC will trace it as a purvasm value wherever the VM stores it — in an ADT field, in an array. A
raw `dlopen` handle there is a non-value word masquerading as a tagged one: at best a
misinterpretation, at worst a traced bogus pointer. So the handle is a **non-negative `Int` index
into a loader-owned table** (held on the native side), which is a legitimate immediate and traces
correctly. No generation counter is needed — nothing is ever unloaded, so an index never goes stale —
but the index is **range-checked in release builds too**, and the safe API exposes no way to
construct one: `ModuleHandle`'s only introductions are `load` and the startup `host-runtime` entry
below. `LeafValue`, by contrast, needs no such care: it *is* a real purvasm closure built by
`pv_make_closure`.

**Provider zero is the host.** The table's first entry is `host-runtime` — the VM executable itself,
obtained once at startup as a self-handle (`dlopen(NULL, RTLD_NOW)`), which is exactly how the
runtime's own `pvf_*` leaves (§1.1) become reachable through the *same* `resolve`. Nothing about the
resolver knows the host is special: it is a `ModuleHandle` with a fixed index and the diagnostic name
`host-runtime`, which is why §4's exactly-one check can name it as a colliding provider like any
other.

The loader-owned table is also what gives "which providers exist" a single answer, for the §4 checks
and for diagnostics (each entry carries its path, or the `host-runtime` name). This is the whole
trusted surface: everything else in §1–§5 is ordinary purvasm code above it.

> **Correction (2026-08-17, as implemented):** the surface is three operations, not two, and
> `resolve` is **pure**:
>
> - `hostRuntime : Effect ModuleHandle` is its own effectful constructor rather than a table entry
>   this record assumed into existence. Establishing provider zero can fail (`dlopen(NULL)`), and
>   folded into `resolve` that failure would surface later wearing the one disguise a resolver must
>   never wear — "that symbol is absent" (§4 rests on `Nothing` meaning exactly *this provider does
>   not define it*).
> - `resolve : ModuleHandle -> String -> Arity -> Maybe LeafValue` drops the `Effect`. Every failure
>   that is not "the provider does not define this key" has moved into its arguments' constructors:
>   the table exists *because* a `ModuleHandle` exists, and the arity is in range *because* an
>   `Arity` exists. What is left is a question with a stable answer, so purity is honest and
>   `Nothing` regains a single meaning.
> - `Arity` is new, and is the reason the above holds: the number reaches `pv_make_closure` as a
>   `uint32_t`, where a negative value becomes an enormous arity and the closure is then called with
>   garbage. `Instruction` admits any `Int` (the stream is compiler-generated today, but that is a
>   convention, not a guarantee), so the constraint belongs in a type — checked once, at the image
>   reader and at the machine, not re-argued at the boundary.
>
> Two implementation obligations this record did not state, both live in `Loader.c`: a string that
> names a file or a symbol is **never truncated** (a shortened path opens a *different* file; a
> shortened symbol can resolve a *different* leaf, then call it at the wrong arity), and a path
> containing an interior NUL is refused by name rather than silently shortened by `dlopen`. `dlsym`
> failure is detected through `dlerror`, not a NULL address, since a symbol's value may legitimately
> be NULL.

### 7. Staging and gates

> **Progress (2026-08-17):** the VM now compiles and runs natively
> ([0110](0110-owned-vm-purescript-native.md) §2's Progress note), which is what makes any of the
> below reachable. It also fixes *where* the work happens: a natively compiled purvasm program cannot
> spawn a subprocess yet (no `purvasm-process`), so both halves of this record's build side — invoking
> `clang` for the program's own link (§1.1's retention/export flags) and compiling a provider to a
> shared object (§1) — run under the **node-hosted Level-2 CLI** until that gap closes. The *loading*
> side needs no subprocess and is unaffected.

1. The trusted loader (§6) with its `host-runtime` entry, the §1.1 retention/export pins, and the §5
   version contract — with the API-coverage provider fixture, before anything calls a leaf.
   **Done (2026-08-17)**, on the terms of §5's and §1.1's Progress notes. `tools/vm-loader-e2e.sh` is
   the gate: it builds the VM with `--host-foreign-api`, then loads a provider referencing every
   `pv_*` the header declares, and a stale one. Two things the slice needed that this list did not
   name — the VM takes `--ffi <path>` (explicit opt-in, §4: nothing is discovered), and the
   API-coverage fixture is **exhaustive** rather than one-entry-per-group, since the retained set is
   derived from the header and a listed-but-untested entry would be untested retention. Confirmed
   able to fail: the same fixture against a VM built *without* `--host-foreign-api` fails to load,
   naming the first symbol the link dropped. **Green on macOS and Linux (2026-08-18)**, the
   latter having first exposed — and then settled — §1.1's ELF question; the gate runs in CI
   (`vm-loader-ci.yaml`), so the platform difference cannot silently reappear.
2. Resolution against `host-runtime` alone, and firing, and `toPv` for **scalars and strings** only:
   the corpus's runtime leaves (`show`, stdio, FS, `argv`) run on the VM with **no module loaded**,
   which is both the first useful milestone and the cheapest test of §1.1's retention. Arrays are a
   named boundary error at this point, so the identity invariant is never violated even transiently.
   Loaded modules join in the next slice, at which point exactly-one spans both classes.
   **Done (2026-08-18)**: a guest program resolves `Data.Show.showIntImpl` and
   `Purvasm.Stdio.writeLineImpl` through `host-runtime`, applies the first to `42`, hands the
   resulting **carrier** straight to the second without decoding it, runs the effect thunk, and
   prints `42` — with no `--ffi` and no manifest. `tools/vm-loader-e2e.sh`'s `runtime-leaves` leg is
   that program, and it also asserts the entry still reports the leaf's result as a carrier rather
   than a string, which is the observable form of §3's "coming out" rule.

   Three things the record did not say, found while building it:

   - **Going in is not a conversion for scalars and strings; it is a change of type.** The VM is a
     purvasm program compiled by the same backend, so its `Int`/`Number`/`Boolean`/`String` already
     *are* runtime values of the representation a leaf expects (fact 2's "one heap, one `pv_*`").
     Rebuilding them through `pv_new_str` would copy a value into itself. So `Purvasm.VM.Foreign`
     holds one unexported `unsafeAsForeign` and no C conversion code at all — the sibling `.c` exists
     only for `pv_apply`.
   - **There is no `Unit` arm, because purvasm's run marker is `LInt 0`.** §3 lists `Unit` among the
     converted scalars, but no such value reaches the boundary: `CPerform t` lowers to `t` applied to
     `AtomLit (LInt 0)` in the bytecode backend *and* in the LLVM backend
     (`Backend.LLVM.Emit`), so running an effect passes an `Int` on both paths. The VM therefore
     passes exactly what a compiled program passes, and `pv_unit()` is not involved on this path.
   - **The VM's own leaves are trusted surface too.** `Purvasm.VM.Foreign.applyImpl` calls an
     arbitrary runtime closure, so exporting it would hand a guest the same reach §6 withholds for
     the loader. The export-set gate's trusted-surface check now covers the whole `Purvasm.VM.*`
     namespace rather than the loader alone.

   Three more, from review:

   - **The resolution cache is keyed by name but validated by arity.** Caching on the key alone let a
     second `ForeignRef` for the same key at a *different* arity reuse the first closure without a
     check — and a leaf indexes its argument vector by the arity its closure was built with, so a
     malformed image could reach a native read past the end of the arguments. The arity is now
     validated on every reference, before the cache is consulted, and the cache records the arity it
     was built with: a disagreement is refused as a corrupt image (the compiler derives one arity per
     key from the PureScript type, §4(a)), never re-resolved.
   - **A carrier remembers where it came from.** §3 promises errors naming the leaf that demanded a
     crossing, but by the time a value reaches the boundary the resolving instruction is long gone,
     so `VCarrier` gained a diagnostic `origin` (ADR-0110 §3's note). Nothing dispatches on it and it
     does not make the value less opaque; a result inherits the origin of the call that produced it,
     so an effect thunk returned by `writeLineImpl` and run later still names `writeLineImpl`.
   - **`Boolean` is the one supported arm with no native coverage.** `String`, `Int`, `Number` and the
     carrier pass-through are each read by a real leaf in the gate — `floatBitsHi 1.0` must print
     1072693248, which a wrong `Number` representation could not produce by chance — but **no runtime
     leaf takes a `Boolean`** (checked: nothing in `runtime/src/leaf.rs` calls `pv_bool_payload`), so
     nothing reads that arm across the boundary yet. It is closed by a loaded-module fixture in slice
     3, the first slice that can call one, and is flagged as untested where the code is.

   One constraint the staging did not anticipate: **a stuck run cannot be caught in-process.**
   purvasm's `Effect.Exception` is a throw-only shadow ([0074](0074-effect-exception-throw-only-ulib-shadow.md)),
   so `try` around `runBlock` does not come back — the process writes the diagnostic and exits. Every
   negative gate here is therefore a separate run observed by its exit status and stderr
   (`--self-test <name>`), not an assertion inside the VM's own entry.
3. Array promotion (§3) and the aliasing/cycle gate.
   **Done (2026-08-18)**, together with the loaded-module half slice 2 deferred here. The gate now
   runs 14 assertions, of which these are new: a leaf writes element 0 of an array the guest owns and
   **all three aliases see it** (two bindings and a field of a data value); the VM's own `SetArray` on
   an already-crossed array is **read back by a leaf** off the same object; an empty array and an
   array containing *itself* both promote, the latter terminating only because the cell is forwarded
   before any element migrates; a loaded module's leaf runs (which also closes slice 2's owed
   `Boolean` arm — `Test.Loader.describeBoolImpl` reads one); and a module exporting
   `pvf_Data_2eShow_2eshowIntImpl` is refused as `provided by both host-runtime and <path>` rather
   than one definition winning by load order.

   Three findings worth carrying forward — two surfaced by failing gates, the third by review:

   - **A `foreign import` of arity 0 is not a value.** `emptyArrayImpl :: ForeignValue` reached the VM
     as an arity-0 *closure* (`leafClosureArity` over a non-function type), so a leaf handed it got a
     `Closure` where it expected an `Array`. The empty case is now `blankArrayImpl 0`, and the rule
     is general: the VM's own privileged imports must take at least one argument.
   - **The provider set is fixed when the run environment is built.** Registering providers one at a
     time left exactly-one dependent on *when* a module joined: resolve `showIntImpl` against
     `host-runtime`, then add a module that also defines it, then mention the key again, and the
     answer comes from the cache with the collision never seen — and the carrier already handed out
     keeps working, so clearing the cache on registration would not have closed it either. The
     providers are now taken by `newEnv` and cannot be extended, which makes that ordering
     **unrepresentable** rather than checked. It costs nothing: loading is explicit (§4), so every
     provider is known before a program starts. `Env` is opaque for the same reason: as a record type
     synonym it stayed a record at every call site, so `env { providers = … }` could rebuild the set
     while SHARING the cache’s `Ref` — the same ordering, reachable from ordinary safe PureScript.
     (Both halves came from review rather than from a failing gate: the harness never registers a
     provider late, so no gate could have found either.)
   - **Applying a carrier that is not a function aborts in the runtime, not in the VM.** §2 delegates
     arity dispatch deliberately, so the VM hands `pv_apply` whatever the bytecode said was a
     function; an ill-typed image therefore fails as a Rust panic (`expected a pointer value, got an
     immediate`) instead of a VM `stuck`. That is the price of the delegation and it is the right
     trade for well-typed input — but it means a *malformed* image gets a worse diagnostic than a
     stuck guest does, which is worth knowing before the image reader admits images the VM did not
     build itself.
4. Effect leaves and the carrier-aware elimination sites.
   **Done (2026-08-19).** Effect leaves needed nothing, as §3 predicted: an effect thunk is a carrier
   and the VM runs it by applying it to the run marker, which slice 2 already did. The work was all
   on the *consuming* side, and it is deliberately invisible in the instruction set — every site
   below meets a carrier where it used to meet a VM value and **demands** the shape it already
   required, rather than asking what it has (there is nothing to ask; §3's opacity):

   - scalar primops (`Prim`), through a per-operand `Demand` table, so each arm's pattern is
     unchanged and the FFI is not visible in the arithmetic;
   - `JumpUnless`, a `Guarded` clause's condition, and `SwitchLit` — which demand a `Boolean`, a
     `Boolean`, and the discriminating literal's kind;
   - `IndexArray` / `LengthArray` / `SetArray` / `ProjArray` / `SwitchLen`, through one array
     entrance (below);
   - `force`, which now routes a carrier through `pv_force_if_byneed`: a leaf may hand back one of
     the runtime's own by-need cells, and the VM cannot tell by looking. This is the same rule the VM
     applies to its own thunks, extended to what crossed.

   The accessors check rather than convert: a VM `Int` and a runtime `Int` are the same word, so
   `intOf` calls `pv_int_payload` for its **shape check** and returns the value unchanged. A
   mis-shaped carrier therefore aborts in the runtime, exactly where a mis-shaped leaf argument does.

   Three corrections from review, each of which changed the work:

   - **`SetArray` is an elimination site too, and by a different route.** An array a leaf *returned*
     is a carrier from birth — it never had a VM cell, so no promotion ever happened to it — yet the
     identity invariant is the same. All array operations now go through `Purvasm.VM.Array.asCell`,
     which hands such a carrier a cell that is `Promoted` from creation: no copy, no new object, and
     writes land on the leaf's own array. The gate exercises both entrances against each other (the
     VM writes, the leaf reads it back).
   - **A branching site is an elimination site too, and the gate has to say so.** `Guarded` shipped
     undecoded: a guard whose condition a leaf supplied read as `guard: non-boolean condition`, a
     correct program refused. It was missed because the first gate exercised only the *value* sites
     (arithmetic, the array operations) — coverage that was incidental rather than structural. The
     gate now drives every carrier-aware control site once, each printing a line that names it and
     each wrong branch printing a `WRONG` line, and the whole arrangement was checked by reverting
     the fix and confirming the gate fails.
   - **This does not make a carrier printable.** An earlier note here implied the runtime-leaf gate's
     `<value>` would become a real rendering; it does not, and cannot: the VM's `describe` has no type
     to demand with, and a value that came from a leaf carries no tag to ask about. Rendering one at
     the terminal is [0110](0110-owned-vm-purescript-native.md) §5's **typed terminal demand**, which
     is a separate mechanism. What slice 4 makes observable is a carrier consumed by a *typed
     eliminator* — `show` over a decoded `Int`, an `IndexArray` over a leaf's array — which is what
     the gate asserts.
5. `pv_adt_tag` and data-returning leaves.
   **Done (2026-08-19).** `pv_adt_tag` is in the runtime, the header and `purvasm-sys`; `toPv` builds a
   data value with `pv_new_adt`; `SwitchCtor` dispatches on a leaf's ADT by tag and `Proj` reads its
   fields. A leaf can now return a `Maybe`, which §3 named as the thing its absence ruled out.

   The tag derivation moved rather than being copied. `ctorTag` (with `fnv1a64` and `utf8Bytes`
   beneath it) now lives in the `abi` package, which already existed for exactly this reason — the
   LLVM backend mints the tag when it emits an ADT and the VM mints the same number at the boundary,
   and `Purvasm.Abi.Mangle`'s own preamble warns that a copy "would drift, and the failure mode is
   silent". The compiler keeps its old module names as re-exports, and its 568 tests (which pin these
   encodings as goldens) pass unchanged.

   Two facts about the representation that the record did not state, both found by a crash:

   - **A nullary constructor has no heap object.** Codegen emits it as the immediate whose payload
     *is* the tag (ADR-0064 §1), so `pv_new_adt` — which is `arity >= 1` — never applies to it, and
     neither does a heap read. `pv_adt_tag` therefore answers for **both** representations, which
     is not a weakening of §3's opacity but the same shape `pv_array_len` already has (an immediate
     sentinel for the empty array, a heap object otherwise): one question, one answer, whichever form
     the value takes. Without that arm a leaf could return `Just x` but not `Nothing` — not a coherent
     surface, since the VM cannot ask which one it is holding and so cannot avoid the bad call.
   - **An `Adt`'s payload is `[tag] ++ fields`.** Field `i` is payload word `i + 1`, so reading a data
     value's field with the array accessor returns the raw TAG — a number that is not a value word at
     all, which the runtime rejects downstream as `pointer is not a live object`. That is what the
     first run of the gate produced. There is now an `adtField` accessor beside `readField`: one per
     layout, rather than one accessor and a convention to remember.

   `pv_adt_tag` is **additive**, so it does not bump `PV_FOREIGN_ABI_VERSION` (§5): a provider built
   before it existed references nothing new, and one built after it fails against an older runtime by
   the symbol's own absence.

   Four more from review, two of them defects in the ABI rather than in the VM:

   - **The foreign API could not build a nullary constructor at all.** `pv_new_adt` always allocated,
     so a provider asking for `Nothing` got a zero-field heap object: the right tag, and a value that
     misses every *native* `case Nothing`, because a generated `case` splits on representation before
     comparing tags (`Emit.purs`). The owned VM would have accepted it — `pv_adt_tag` reads either
     form — so this was "write once, run on both backends" failing on the backend the VM is not. The
     fixture's hand-encoded `pv_int(tag)` is what had hidden the gap.

     The fix is **`pv_new_nullary_adt`, a new symbol**, not a new meaning for `pv_new_adt`. Teaching
     the existing entry to answer for `n == 0` would have changed what a v1 symbol does: a provider
     built against the new header and loaded by an older runtime would have resolved it, been
     accepted, and silently misbehaved — which is the failure §5's version contract exists to
     prevent, and which a version stamp cannot catch if the stamp does not move. A new name is
     refused by an older runtime as an undefined symbol, so the change stays **additive** and
     `PV_FOREIGN_ABI_VERSION` does not move.

     `pv_new_adt` therefore **keeps** its v1 behaviour for `n == 0` — a zero-field heap object,
     non-canonical and documented as such — rather than being taught to refuse it. Refusing is itself
     a behaviour change to a v1 symbol: a provider built before the nullary entry existed passes the
     version check and then faults inside a call that used to return, which is the same class of
     silent-contract break, just louder. The safe Rust layer's `Ctx::new_adt` picks the correct entry
     for the author, so a leaf still writes `new_adt(tag, &[])` and never learns that the two differ.
   - **The safe Rust layer could receive a data value and do nothing with it.** `pv_adt_tag` reached
     `purvasm-sys` but not `Ctx`, whose raw context and word are private — so a `#[pv_foreign]` leaf
     could be handed a `Maybe` and had no way to inspect it, leaving §Context's "one authoring
     surface" true for C and false for the crate the DX layer exists to be. `Ctx` now has `adt_tag`
     and `adt_field` (the latter carrying the `+ 1` so a leaf never encodes the layout), with a
     round-trip test through the safe layer for both constructor shapes.
   - **The safe layer's field accessor had the same two holes as the VM's.** `i + 1` wraps at
     `u64::MAX` in a release profile (overflow checks off) and lands on slot 0 — the raw tag — and the
     accessor read a field without checking the value was an ADT at all, so an `Array` would have
     answered with its element `i + 1` instead of faulting, against `Ctx`'s stated contract that a
     shape error is a runtime fault. Both are closed (`checked_add`, and `adt_tag` first as the shape
     check), in the Rust layer and in the VM's own C accessor.
   - **A negative `Proj` index reached the tag slot.** A VM-local data value is refused by
     `Data.Array.index`, but a carrier had no such guard, and `adtField`'s `+ 1` turns `-1` into slot
     0 — the raw tag, the one word the separate accessor exists to keep out of value positions. It is
     refused on both sides now, and the gate asserts the two representations produce the *same*
     diagnostic.
   - **The outbound gate could not see a wrong tag.** The fixture answered "Just or else Nothing", so
     a broken nullary tag would have been reported as a correct `Nothing`. It has three outcomes now,
     and the third is a `WRONG` line the gate fails on.

   One limit worth stating rather than discovering: **`pv_adt_tag`'s shape check reaches only the
   pointer case.** A heap non-ADT aborts, but a nullary constructor is an immediate and therefore
   indistinguishable from an `Int`, a `Boolean` or `Unit` — the representation genuinely does not
   separate them. So this accessor, unlike the scalar ones, rests on the caller being a site whose
   TYPE already established that the value is an ADT, which a compiler-emitted `SwitchCtor` is.
6. Manifest emission from the build; the scoped eager diagnostics (§4).
   **Done (2026-08-20).** `purvasm build` writes `<outdir>/foreign-manifest` — the workspace-provided
   keys, one per line under a version banner — as a *projection* of the provider map the link already
   enforced, so the two cannot disagree about which keys the workspace provides. The VM takes
   `--manifest <path>` and checks each declared key has exactly one provider **before the program
   runs**; everything else stays lazy.

   Five things worth recording, the last two found in review:

   - **Existence is a different question from resolution, and the manifest needs only the first.**
     `Loader.resolve` builds a closure (`pv_make_closure` allocates), so asking every provider that
     way would allocate one per candidate and keep the winner. The new `Loader.declares` answers
     "does this provider define this key" with `dlsym` alone — which is also **why a manifest can
     carry keys without arities**, the arity being a fact only the image holds (ADR-0110 §4(a)). The
     exactly-one check in the resolver now uses it too, so exactly one closure is built per key.
   - **A manifest belongs to the image, not to the host.** The VM's own link emits one — its loader
     and boundary modules are workspace modules with C siblings — and that manifest names the VM's
     *trusted* leaves, which §6 deliberately does not export. Feeding it back to the VM would fail by
     design. Nothing does that, but the shape of the mistake is worth naming before an image reader
     makes manifests discoverable beside images.
   - **An unrecognised manifest is refused, not treated as empty.** A gate that silently becomes a
     no-op is worse than no gate, because the build still reports having emitted one.
   - **The key must survive the round trip exactly, so the escape has an exact inverse now.** The
     writer first reused `demangleKey`, a best-effort decoder for *diagnostics* that knows `_2e` and
     `_5f`. A key like `App.foo'` links as `pvf_App_2efoo_27`, would have been written as
     `App.foo_27`, and the reader would have re-mangled it to `pvf_App_2efoo_5f27` — a missing
     provider reported for a key the link had just resolved, which is precisely the claim that the
     manifest is a *projection* of the enforced provider map. `Purvasm.Abi.Mangle` now carries
     `unescapeIdent`, an exact inverse that also refuses what escaping could not have produced (`_61`
     for an alphanumeric), since accepting it would give one key two spellings and cost the encoding
     the injectivity ADR-0072 §2 chose it for. The writer FAILS on a symbol it cannot recover rather
     than approximating; only the diagnostic keeps a fallback.
   - **The reader accepts only the shape the writer emits.** Filtering blank lines before finding the
     banner would have accepted a leading one and silently dropped an empty key — and a key silently
     dropped is a key not checked.

   The format is line-oriented rather than JSON: the consumer is the VM, which would otherwise need a
   JSON parser to read a banner and some keys. Writer and reader hold the banner independently, so
   the gate feeds the VM the banner the *build* just wrote — a drift between them fails there rather
   than in a user's project.

   `--manifest` is a flag rather than discovery-beside-the-image because there is no image yet
   ([0110](0110-owned-vm-purescript-native.md)'s slice 2). When the reader lands, the manifest is
   found next to the image and the flag becomes the override.

Gates:

- **the write-once differential** — one `.c` leaf and one Rust leaf, each built twice from the same
  source: linked into a native binary, and loaded by the VM. The same program must produce the same
  result both ways. This is what turns "write once, run on both backends" into a checked claim;
- **the API-coverage load test** (§1.1): a provider calling at least one entry from each API group
  loads and runs, so a retention/export list that forgot a symbol fails here — **landed** in
  `tools/vm-loader-e2e.sh`, covering every entry rather than one per group, and asserting the load
  only (a leaf cannot be *called* until slice 2, and `RTLD_NOW` already binds every reference);
- **the runtime-leaf test** (§1.1 / §2): a guest program exercising a runtime leaf from each family —
  `show`, a line write, an FS read, `argv` — runs on the VM with no `--ffi` and no manifest. This is
  the gate for the `pvf_*` half of the retained set, which nothing in the VM's own code references —
  **landed** in `tools/vm-loader-e2e.sh`'s `runtime-leaves` leg with `show` + stdio, the two that
  together exercise a pure leaf, a carrier passed on undecoded, and an effect thunk run; FS and
  `argv` join when the image reader makes a fixture program cheaper to write than a hand-assembled
  block;
- **the runtime-shadow test** (§4): a loaded module exporting a key the runtime already defines fails
  with `provided by both host-runtime and <module>`, rather than one of them silently winning —
  **landed** (`Shadow.c`, built with `-DPVF_MODULE=Data_2eShow` so the symbol really is the
  runtime's own);
- **the stale-module test** (§5): a module built against a bumped `PV_FOREIGN_ABI_VERSION` fails to
  load, and a marker in its initialiser proves no module code ran — **landed** in
  `tools/vm-loader-e2e.sh`, with the marker's *positive* control alongside it (the same source built
  against the shipped header loads and does print the marker, so its absence in the stale leg is
  evidence rather than an assumption) and **gated on that control**: without it the leg reports
  `INCONCLUSIVE`, since a host that can load nothing refuses the stale module for the wrong reason;
- **the export-set test** (§1.1, the negative direction — added 2026-08-18): the generated allowlist
  and the executable's actual dynamic export set must match exactly, in both directions, with a
  re-exported `pvf_Purvasm_2eVM_2eLoader_2e*` or `pv_g_*` named as a trusted-surface failure. Every
  other gate here is positive, so none of them can see the regression that hands a guest the loader
  §6 exists to keep out of reach. It is also what caught the ELF export failure above, by number
  (`allowlist 57, exported 0`) rather than as a downstream load error;
- **the aliasing gate** (§3): shared arrays, a `Ref`, and a cyclic array observed through every alias
  after a leaf writes — plus an empty array and an array promoted while another promotion is in
  flight, the two cases the migration procedure's steps 1 and 3 exist for — **landed** in
  `tools/vm-loader-e2e.sh`'s `aliasing` and `cyclic-empty` legs, covering three aliases (two bindings
  and a data field), both directions of the write, the empty array and the self-referential one. A
  `Ref` is not exercised separately: it *is* a one-element array, so the same cell and the same
  `pv_write_field` path carry it — noted rather than silently skipped;
- **the effect-termination regression** ([0110](0110-owned-vm-purescript-native.md) §5):
  `main = Console.log "x"` writes and exits 0, with the carrier `Unit` from the final `pv_apply`
  discarded rather than inspected;
- **the handle-representation test** (§6): a `ModuleHandle` stored in an ADT field and an array
  survives a GC cycle and still resolves — the check that the handle is a legitimate traced value and
  not a raw pointer in disguise;
- boundary unit tests over every conversion arm *and* every unsupported arm (closure, partial
  constructor, record), since those errors are the boundary's contract;
- a resolution test in both scopes: a declared workspace key with no provider fails before the
  program runs; an undeclared key on a branch that never executes does **not**.

## Consequences

- A purvasm program with user FFI runs on the VM, on the same provider the native build links. The
  two backends stop disagreeing about what a program may contain.
- The runtime gains two entries — the `pv_adt_tag` accessor and the `pv_new_nullary_adt` constructor,
  the latter because a nullary constructor is an immediate and the field-carrying entry could not
  answer for it without changing what a v1 symbol does. The foreign surface also gains its own
  version constant (`PV_FOREIGN_ABI_VERSION`) and the host-symbol reference that enforces it, emitted
  by both `purvasm.h` and `purvasm-foreign`. All additive: no existing provider changes, and no
  existing symbol changes meaning — `pv_new_adt` with no fields keeps its (wrong, non-canonical) v1
  behaviour rather than becoming an error, since refusing it would be exactly the silent-then-loud
  break the version contract exists to prevent.
- The VM's own link acquires platform-specific retention and export rules (§1.1) over two sets: the
  hand-listed `pv_*` foreign API (plus the version symbol), and the runtime's `pvf_*` leaves, derived
  by `nm`. The first is a maintained artifact — adding a `pv_*` to the header without adding it there
  yields a provider that fails to load — which is why both coverage fixtures are gates.
- `host-runtime` makes the common case free: a program using only runtime leaves runs on the VM with
  no `--ffi`, no manifest, and no shared object anywhere. Dynamic loading is what *extends* the
  provider set, not what makes it exist.
- The VM executes arbitrary native code from a path the user names. That is the same trust boundary
  as linking a `.c` sibling into a native binary, but it moves from build time to run time, so the
  opt-in must stay explicit — and §6 keeps the mechanism (module handles, code addresses) out of
  guest reach so that boundary has exactly one door.
- Leaf calls are not free: scalars and strings convert cheaply, data values allocate per call, and an
  array **changes representation permanently** the first time it crosses — after which VM-side access
  to it costs an indirection. That is the price of the identity invariant, and it is charged only to
  arrays that actually meet a leaf. If it ever dominates, the escape hatch is
  [0110](0110-owned-vm-purescript-native.md)'s recorded alternative (VM values *are* runtime values),
  which deletes the boundary entirely.
- Array promotion is the one piece of the design with no counterpart in boot's VM or the native
  backend, so it carries its own gate (§7) rather than riding on the differential.
- The native-ABI encodings now have two consumers (the LLVM backend and the VM's boundary) in
  different packages: `mangleForeign` and `ctorTag`, both currently inside
  `Backend.LLVM.Mangle`. They must have **one** derivation — extracted into a shared package rather
  than copied — or they will drift, and the failure modes are silent: a "no provider" for a key that
  exists, and a `case` that takes the default arm on a value that matched.
- Record-typed FFI stays unsupported, and this record makes the reason explicit rather than letting
  it look like a VM limitation.

## Alternatives considered

- **Reimplement `pv_*` over VM values (a shim).** What a non-runtime host would be forced into: the
  VM implements the ~30-call surface itself over its own values. Rejected — two implementations of
  the value semantics, and rooting bugs in a provider become invisible on the VM while remaining
  fatal natively, which is exactly backwards for a component used as an oracle.
- **A VM-specific dynamic-module ABI** (no ctx, no rooting, first-order data only). Simpler to
  implement, and rejected on the record's premise: it forces users to author their FFI twice.
- **Link providers statically into the VM binary.** Rejected: the VM would need rebuilding per
  program, which is the boot situation this work exists to leave.
- **Ship the runtime as a shared library** and have both the VM and every provider link against it.
  This deletes §1.1 entirely — no anchor, no forced-undefined list, no export allowlist, no
  host-symbol resolution — because the provider's `pv_*` bind to the library directly, and one
  instance still serves both. Rejected *for now* on distribution: the project wants a self-contained
  executable (the runtime staticlib is located and linked at build time today, and `dist/` bundling is
  an open want), and a shared runtime turns that into an rpath/versioning problem for every shipped
  binary. It stays the named fallback if the per-platform retention rules prove brittle, and it is
  the cleaner answer the moment a shared runtime is wanted for other reasons.
- **Pass a vtable at module init** instead of resolving undefined `pv_*` against the host. More
  explicit and portable, and it versions cleanly — but it requires `purvasm.h` to redirect every
  `pv_*` through a pointer under a compile flag, complicating the one header the FFI author reads.
  Rejected while `dynamic_lookup`/`--export-dynamic` is proven; it is the fallback if a target
  platform cannot resolve host symbols.
- **Forbid arrays at the boundary instead of promoting them.** The other consistent answer to the
  identity problem, and what §7's slice 2 ships as an interim state. Rejected as the end state:
  `Array` arguments and results are among the most ordinary FFI shapes, and a permanent ban would
  make the VM's FFI a strict subset of the native one — which is the asymmetry this record exists to
  remove.
- **Copy arrays in and out (value-result) around each call.** Cheap, and wrong in a way that only
  shows up later: it handles a leaf that mutates *during* the call, but not one that retains the
  array, and it silently breaks aliasing between two VM bindings. Rejected.
- **Eager `ofPv` with a new `pv_kind` introspection call.** Would let a returned value be walked into
  a VM value. Rejected twice over: it copies identity-bearing values (breaking `Ref`), and a
  kind query is precisely the representation question [0069](0069-v1-dynamic-record-operations.md) forbids
  a consumer to ask — `pv_adt_tag` asks a *typed* question instead ("this is an ADT; which
  constructor?"), which is the same class as `pv_array_len`.
- **Take arity from the module** (an exported descriptor, or a companion symbol). Rejected: it
  duplicates a fact the PureScript type already states, adds an authoring burden the native path does
  not have, and lets the two drift — a wrong arity is a crash, not a diagnostic.
