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
- `resolve : ModuleHandle -> String -> Int -> Effect (Maybe LeafValue)` — mangle the key
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

### 7. Staging and gates

1. The trusted loader (§6) with its `host-runtime` entry, the §1.1 retention/export pins, and the §5
   version contract — with the API-coverage provider fixture, before anything calls a leaf.
2. Resolution against `host-runtime` alone, and firing, and `toPv` for **scalars and strings** only:
   the corpus's runtime leaves (`show`, stdio, FS, `argv`) run on the VM with **no module loaded**,
   which is both the first useful milestone and the cheapest test of §1.1's retention. Arrays are a
   named boundary error at this point, so the identity invariant is never violated even transiently.
   Loaded modules join in the next slice, at which point exactly-one spans both classes.
3. Array promotion (§3) and the aliasing/cycle gate.
4. Effect leaves and the carrier-aware elimination sites.
5. `pv_adt_tag` and data-returning leaves.
6. Manifest emission from the build; the scoped eager diagnostics (§4).

Gates:

- **the write-once differential** — one `.c` leaf and one Rust leaf, each built twice from the same
  source: linked into a native binary, and loaded by the VM. The same program must produce the same
  result both ways. This is what turns "write once, run on both backends" into a checked claim;
- **the API-coverage load test** (§1.1): a provider calling at least one entry from each API group
  loads and runs, so a retention/export list that forgot a symbol fails here;
- **the runtime-leaf test** (§1.1 / §2): a guest program exercising a runtime leaf from each family —
  `show`, a line write, an FS read, `argv` — runs on the VM with no `--ffi` and no manifest. This is
  the gate for the `pvf_*` half of the retained set, which nothing in the VM's own code references;
- **the runtime-shadow test** (§4): a loaded module exporting a key the runtime already defines fails
  with `provided by both host-runtime and <module>`, rather than one of them silently winning;
- **the stale-module test** (§5): a module built against a bumped `PV_FOREIGN_ABI_VERSION` fails to
  load, and a marker in its initialiser proves no module code ran;
- **the aliasing gate** (§3): shared arrays, a `Ref`, and a cyclic array observed through every alias
  after a leaf writes — plus an empty array and an array promoted while another promotion is in
  flight, the two cases the migration procedure's steps 1 and 3 exist for;
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
- The runtime gains one accessor (`pv_adt_tag`); the foreign surface gains its own version constant
  (`PV_FOREIGN_ABI_VERSION`) and the host-symbol reference that enforces it, emitted by both
  `purvasm.h` and `purvasm-foreign`. All additive; no existing provider changes.
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
