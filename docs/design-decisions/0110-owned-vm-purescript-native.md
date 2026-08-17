# 0110. The owned VM: a PureScript interpreter, natively compiled

- Status: ~~Proposed~~ **Accepted** _(2026-08-16: accepted by the maintainer after three review
  rounds — §1.1's two-layer apply, §3's identity invariant and shared array cell, §4's
  evolve-the-existing-`.pvm` scope with the tree-shaped `case`, and §5's typed terminal demand)_
- Date: 2026-08-16

> **Revision (2026-08-16, review round 1):** §1 gained §1.1, splitting guest-level eval/apply (the
> VM's) from runtime-level `apply` (delegated, carrier-only) — the earlier "the runtime owns `apply`
> and effect execution" contradicted a VM closure being a code block plus environment. §3's
> identity invariant now covers **VM arrays**, which are mutable (`SetArray`) and are what `Ref` is
> made of, so the boundary *promotes* them instead of copying. §4(a) drops the effect bit (no
> consumer; `leafClosureArity` already folds `retVsat` in). §5 pins the entry-result observation
> contract, since a carrier cannot be printed.
>
> **Revision (2026-08-16, review round 2):** §5's observation contract is now a **typed terminal
> demand** with two modes — an `Effect` entry runs and *discards*, because a `main` ending in a
> native leaf legitimately returns a carrier `Unit`; only a value entry can raise carrier-escape.
> §3 states that the forwarding lives in the array value (a shared cell), not at a binding, with the
> representation fixed in [0111](0111-vm-dynamic-native-ffi.md) §3. The Context's ownership sentence
> is corrected to host-level vs. guest-level, matching §1.1.

## Context

purvasm has exactly one interpreter: boot's OCaml VM (`boot/lib/vm/machine.ml`, driven by
`purvm run`). It carries two loads that nothing else carries:

- it is the **optimiser measurement field** — the deterministic steps/allocs harness
  ([0026](0026-benchmark-harness.md), the optimiser regression gate that
  [0075](0075-cross-backend-wall-clock-benchmark-harness.md) leaves in place) is how every MiddleEnd
  pass is judged, and [0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md) §0(a)
  routed `--opt` bytecode onto it, because the native path cannot yet compile the `Effect`-using
  corpus; and
- it is half of the **behavioural anchor** that replaced boot byte-identity
  ([0104](0104-retire-boot-byte-identity-gate.md) §2).

It also has a wall that the native path cleared long ago: **the VM cannot run user FFI.** Its
foreign frontier is a table compiled into the binary — the provider ladder ends at `Ffi.host`
(`boot/lib/ffi/ffi.ml`, `ladder = [intrinsic; structural_provider; native_provider]`, where the
native rung is *derived from* the host registry so "the two never drift apart"). A `foreign import`
the ladder does not know is left unbound by `Link.link` and is `stuck` when forced
(`boot/lib/vm/machine.ml`, `unbound native foreign: <key>`). Meanwhile a native build resolves the
same declaration to `pvf_<mangle(key)>` and satisfies it from a C sibling, a ulib `.c`, the runtime,
or one Rust crate, under a symbol audit and an exactly-one-provider invariant
([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) /
[0078](0078-rust-foreign-bindgen-over-c-abi.md) / [0091](0091-user-native-ffi-c-sibling-rust-dir.md)).
The two backends therefore disagree about what a purvasm program *is allowed to be*: on native, any
`foreign import` the user can provide; on the VM, only what the toolchain shipped.

That gap cannot be closed where it is. boot is **frozen** ([0104](0104-retire-boot-byte-identity-gate.md) §1).
The freeze deliberately leaves `Ffi.host` open to the Level-2-blocking exception class — that is
where [0103](0103-native-string-substrate-zero-copy-slices.md)'s parity leaves landed — but that
exception admits **leaves the project itself ships**, one table arm at a time. A general mechanism
for loading *user-supplied* native providers is a new subsystem inside boot's VM, in the language
the project is trying to stop investing in, and it is not a table entry.

The roadmap already anticipates the move. [0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md) §0
splits itself in two and defers its **(b)** scope — the decls + init image format, `lowerEntry`
owning reachability, the REPL substrate — explicitly to "**an owned VM interpreter (PS or Rust)**",
on the grounds that "changing the *executed* `.pvm` shape is impossible while boot's frozen `purvm`
is the only interpreter". This record opens that work, with user FFI as its driver rather than the
image format.

Two things make it tractable now rather than later:

- **A PureScript interpreter can be compiled natively today.** boot's LLVM path already compiles the
  Level-2 compiler itself to a native executable, and the Level-2 LLVM backend emits `.ll` from
  CoreFn for pure programs. The interpreter is an ordinary purvasm program, so the **host** level —
  its memory (heap, GC) and the application of *host* functions — is the Rust runtime's
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) /
  [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)), and none of it is re-implemented. The
  **guest** level — applying the bytecode program's closures, its partial applications, its thunks —
  is the interpreter's own job and always was; §1.1 draws that line precisely.
- **The loading contract is already proven, independent of host language.** A throwaway spike (not
  in-tree) built a shared object exporting a `PVCodeFn` whose `pv_*` calls were left *undefined*,
  `dlopen`ed it from a host executable, and had the module's `pv_*` references resolve back into the
  host — including a host implementation that re-entered its own managed runtime — on darwin with
  `clang -shared -undefined dynamic_lookup`. The mechanism is not the risk; **whose** interpreter we
  want to own is the decision.

## Decision

### 1. Charter

Build an interpreter for purvasm bytecode, written in PureScript and distributed as a native
executable. It owns: image loading, the instruction loop, VM values, the **guest-level eval/apply**
(§1.1), and the **foreign frontier — including dynamically loaded, user-supplied native providers**
([0111](0111-vm-dynamic-native-ffi.md)). It does not own: the heap, the garbage collector, or codegen.

#### 1.1 Two `apply`s, two layers

A VM closure is a *code block plus environment*, not a runtime closure, so the runtime's `apply`
cannot run it. There are two application layers and this record keeps them apart:

- **Guest-level (the VM's own).** Applying a VM closure, collecting arguments into a partial
  application, over-applying a saturated result, forcing a VM by-need cell, and running the guest's
  `Effect` — which in purvasm is applying a guest function to unit — are the *interpreter's*
  eval/apply ([0025](0025-lower-ir-anf.md), realised in the VM by
  [0030](0030-bytecode-vm-slice1.md)'s partial forms). An entry `main :: Effect _` is normally an ordinary VM
  closure, so **the first force of a program is VM-side**, not a `pv_run_effect`.
- **Runtime-level (delegated).** A value in the foreign carrier that is a *runtime* closure — a
  resolved leaf, or an effect thunk a leaf returned — is applied with `pv_apply`
  ([0111](0111-vm-dynamic-native-ffi.md) §2). The runtime owns arity dispatch, over-application, and
  by-need forcing *for those values only*.

The two meet where the interpreter's `Call` finds a carrier instead of a VM closure, and where a
VM-side effect run finds a carrier thunk. Nothing else crosses: the runtime never sees a VM closure,
and the VM never re-implements runtime application.

What the runtime *does* own unconditionally is memory: the VM allocates ordinary PureScript values,
so heap and GC are the runtime's with no VM involvement.

Non-goals, stated so they are not smuggled in later:

- **not** byte- or trace-identical to boot's VM (identity gates are retired,
  [0104](0104-retire-boot-byte-identity-gate.md)); parity is *behavioural*;
- **not** a fast VM. It is a measurement and portability instrument. Deterministic counts and
  correctness first; throughput is a later, separately-motivated track;
- **not** parallel/M:N — that is the runtime's v2 question
  ([0062](0062-mn-work-stealing-scheduler-fibers.md)), not the interpreter's;
- **not** a replacement for the native backend. A program that can be compiled should be compiled.

### 2. Host language, toolchain, distribution

PureScript, compiled to a native executable — by boot today (the path that already self-compiles the
Level-2 compiler), by the Level-2 native path as its `Effect` codegen lands. Its own host leaves come
from the existing ulib packages (`purvasm-fs`, `purvasm-system`, `purvasm-stdio`, `purvasm-base`);
it introduces no new privileged runtime surface except what
[0111](0111-vm-dynamic-native-ffi.md) needs for dynamic loading.

The interpreter lives in its own workspace package rather than inside `compiler/`: it consumes the
bytecode format, it does not produce it, and the compiler must not gain a dependency on it. Binary
naming during the transition (both `purvm`s exist) is an implementation detail, pinned when the
first slice ships.

> **Progress (2026-08-17):** §2's claim is measured, on **both** paths and with the same result
> (`result: 55`, `instructions: 134`, from a tail-recursive guest loop run through `vm/src/Main.purs`):
>
> - via **boot** — `purvm native --backend llvm --corefn-dir output --ulib ./purvasm_lib -m Main`,
>   a 929K executable;
> - via **Level-2** — the maintainer built and ran the same entry through `cli/index.node.js`.
>
> One constraint the record did not anticipate, and that
> [0111](0111-vm-dynamic-native-ffi.md) inherits: a natively compiled purvasm program **cannot spawn a
> subprocess** yet (there is no `purvasm-process`), so the Level-2 native path stops at emitting `.ll`
> and something else must invoke `clang` and the linker. Today that something is the **node-hosted**
> Level-2 CLI. Nothing about the VM depends on this — the *loading* side of 0111 needs no
> subprocess — but the *build* side (compiling a provider to a shared object) inherits the same
> node-hosted step until `purvasm-process` exists.

### 3. Value representation: a VM-owned ADT, converted at the FFI boundary

`Value` is an ordinary PureScript ADT owned by the VM — scalars, string, array, record, data tagged by
its **constructor name** (§4), closure (code block + captured environment), the partial forms
(under-applied closure / constructor), and a by-need cell — plus one carrier described below. It is
deliberately **not** the runtime's value representation.

This is a **principle, not a compromise**. The LLVM/native ABI
([0059](0059-native-abi-value-representation.md) / [0069](0069-v1-dynamic-record-operations.md)) is
one backend's representation among several possible ones; the VM is another target, and it owes that
backend nothing about how it lays values out — the relationship is wasm's value representation to
V8's native binary representation, not shadow to original. Two consequences follow and are binding on
the rest of this record: **no native-ABI encoding may leak into the bytecode format** (§4), and the
FFI boundary — the one place the VM deliberately meets the native representation — is where such
encodings are allowed to be known ([0111](0111-vm-dynamic-native-ffi.md)).

It is also what the interpreter needs on its own terms: a closure is a *code block plus environment*,
not a code address; a by-need cell answers to the VM's own thunk discipline; an under-applied
constructor exists only in the interpreter.

Two invariants that follow, and that [0111](0111-vm-dynamic-native-ffi.md) builds on:

- **A runtime value that crossed the boundary stays opaque.** `Value` carries a *foreign carrier*
  variant holding a runtime value as-is. It is not decoded on arrival — the ABI a native leaf is
  written against ([`runtime/include/purvasm.h`](../../runtime/include/purvasm.h)) exposes typed
  accessors but **no introspection**: given a word there is no supported way to ask "what kind is
  this?". Decoding therefore happens at the *use* site, where the bytecode already knows the shape it
  demands, or not at all.
- **Identity-bearing values are never copied — in either direction.** The class is larger than it
  looks: purvasm's `Array` is mutated in place (`NewArray`/`SetArray`, [0019](0019-mutable-array-building.md)),
  and `Ref`/`STRef` *are* one-element arrays written through `SetArray` (`Compiler.Ffi`'s `refNew` /
  `refWrite`). Nothing in the bytecode distinguishes a finished array from one still being built, so
  **every VM array is identity-bearing**, including array literals.

  Two consequences. A value that arrived from a leaf stays in the carrier for its whole life in the
  VM — it is never decoded into a VM value. And a VM array that crosses outward is **promoted, not
  copied**: its VM-side storage is replaced, in place and once, by the carrier, so every alias
  observes the same object afterwards and a leaf's write is visible to the VM (and vice versa). The
  promotion is a forwarding step the VM already knows how to do — it is the shape of the by-need
  cell's `Unbuilt → Built` transition — and it terminates on cyclic structure for the same reason a
  copying collector does: the forwarding is installed *before* the elements are migrated.

  For promotion to reach *every* alias, the forwarding state cannot live at a binding — it must be in
  the array value itself. So a VM array is, from creation, **an indirection cell** that every alias
  shares, holding either VM-local storage or the promoted carrier;
  [0111](0111-vm-dynamic-native-ffi.md) §3 fixes that representation and the migration procedure.
  This record owns the invariant, which is absolute: **the boundary copies immutable spines and
  promotes identity-bearing nodes; it never copies one.**

### 4. Bytecode: the existing `.pvm`, evolved — not a new format

The owned VM reads the **existing `.pvm` lineage**, kept recognisably itself: the same instruction
vocabulary, the same name-keyed environments, the same JSON-then-eventually-binary envelope question
left open. What [0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md) §0(b)
unblocks is the *ability* to change the executed shape at all; this record spends that budget on two
changes only, and explicitly declines the rest (indexed binders, a re-cut instruction set, the decls
+ init shape) as separable work with its own motivation.

**(a) A foreign reference carries its physical arity** — *required*, and that is all it carries. The
VM must know when a leaf saturates; unlike boot it cannot read that out of a compiled-in registry,
and it must not ask the loaded module. Arity is the *compiler's* knowledge (`ForeignSig`'s
reconstructed shape through `leafClosureArity`), exactly as native codegen already takes it; sourcing
it from the module would duplicate a fact the PureScript type already states and let the two drift.

No effect bit travels with it. An earlier draft added one; it has no consumer. `leafClosureArity`
already folds `retVsat` into the physical arity (a nullary `Effect a` leaf has closure arity 1), so
dispatch needs the arity alone — and FSR's actual shape is the `vsat`/`retVsat` pair
([0080](0080-foreign-signature-reconstruction-cst.md)), not a bit, so a single flag would have been
lossy as well as unused.

**(b) `case` dispatch keeps its tree shape** — *wanted, and cheap*. Today the decision tree is built
once, backend-agnostically, by `MiddleEnd.MatchCompile` ([0083](0083-match-compilation-to-anf-middle-end.md)),
and the bytecode lowering *linearises* it: each switch becomes `SwitchCtor (Array (String /\ Int))`
plus a default, whose `Int`s are back-patched **relative offsets** into a flat block. Every consumer
of a `.pvm` that wants the tree — the owned VM, another backend, an analysis — must therefore
reconstruct from offsets what the compiler already had. That is the format losing structure the
producer possessed, which is the wrong direction for an interchange artifact.

Restoring it is mechanical, and this record checked why: `MatchCompile`'s `DTree` is a **pure tree** —
a guard row carries its own fall-through subtree (`Dguard binds clauses ft`), and no node is shared —
and `compileTree` walks it emitting each node's region exactly once, using labels only to mark where a
subtree starts, plus one join label for the end of the whole `case`. So a switch whose arms and
default hold **nested code blocks** (the nesting the format already has for `Closure`/`MakeRec`) is
information-preserving in both directions and **cannot duplicate code**. The end-join disappears into
the structure: a `case` node yields a value and control resumes in the enclosing block — the same
move wasm makes with structured blocks instead of jumps.

Scope discipline for (b): it covers `case` dispatch (`SwitchCtor` / `SwitchLit` / `SwitchLen` and the
guard chain). `Jump`/`JumpUnless` elsewhere are the same *class* of question and are deliberately left
alone. If (b) turns out to entangle with the interpreter's control representation, it is droppable
without touching (a) — the two are independent.

**Not required, explicitly:** numeric constructor tags. An earlier draft of this record demanded them
so the FFI boundary could build a native ADT. That is unnecessary — the native tag is
`fnv1a64(name).lo & 0x7fffffff` (`Backend.LLVM.Mangle.ctorTag`), a pure function of the constructor
*name* the format already carries — and it would have been exactly the leak §3 forbids: one backend's
tag encoding baked into the shared format.

### 5. Correctness

The VM is a second implementation of purvasm's dynamic semantics, so it is gated against the ones
that exist:

- ~~**differential against boot's VM** over the existing program corpus~~ **the fixture-owned
  behavioural gate** over the existing program corpus — same result value, same
  observable output, on the same source compiled by the same Level-2 front half. **The observation
  contract is part of this gate, because a carrier is not printable.** A value that came from a leaf
  is opaque by construction (§3), and the VM has no introspection with which to render one — so the
  runner does not *inspect* the entry result at all. It applies a **typed terminal demand**, fixed by
  the entry's run mode, of which there are exactly two:

  - **`Effect` entry (the default).** The demand is *run and discard*. The final value is not
    observed, so it is irrelevant whether it is a VM `Unit` or a carrier — and it will frequently be
    a carrier, because a `main` whose last action is a native leaf (`Console.log "x"`) ends in
    `pv_apply` returning a runtime `Unit`. This is boot's runner behaviour too ("forces it to unit").
    Observation for these programs is their *output*, written through host leaves that take a carrier
    and pass it through.
  - **value entry (`--value`).** The demand is a VM value to print and compare. A carrier here is the
    named error `native value escaped as the program result`, and a fixture that wants to observe a
    leaf's result puts a VM-level observer in front of it (`show`, a comparison — elimination sites,
    which decode at the use site).

  An earlier draft made the carrier-escape error unconditional, which would have failed
  `main = Console.log "x"` on *success*; the regression test for that is named in §6's gates. If the
  two demands ever prove too coarse, the upgrade is to carry the entry's observation shape in the
  image — not needed to open the corpus.

  Instruction counts
  are compared only where the instruction sets correspond; the format change (§4) means they need not
  be equal, and this record does not pretend otherwise;
- **unit tests for the invariants the types cannot hold** — saturation/over-application, tail-call
  frame discipline, by-need forcing and black-holing, decision-tree dispatch on each discriminant
  kind, and every boundary-conversion arm once [0111](0111-vm-dynamic-native-ffi.md) lands;
- **the terminal-demand regression**: `main = Console.log "x"` writes its output and exits
  successfully, with the final `pv_apply` result discarded rather than inspected. It is listed
  separately because it is the case the first draft of the observation contract got wrong;
- ~~boot's VM stays the reference runner until the owned VM is green on both, and stays available as an
  oracle afterwards (the freeze keeps it stable, which is exactly what makes it a good oracle).~~

> **Correction (2026-08-17):** **boot is not an oracle for this VM, and identity with it is not a
> guard.** The record framed §5 as a differential against boot, which reads back the retired
> relationship: the LLVM backend has already passed the strict byte-identity gate against boot, and
> purvasm has entered the stage where it develops *without* being tied to it
> ([0104](0104-retire-boot-byte-identity-gate.md)). What must be guarded now is **semantic
> correctness**, which is a moving target that boot — frozen — cannot define.
>
> So the anchor is the **fixture-owned expected trace**: each fixture states what its program must
> print, and every runner is held to that. The owned VM joins
> `tools/l2-native-behavioural.sh` as a leg on those terms (§6's Progress note). boot's VM remains a
> leg of that gate for its own historical reasons; the owned VM is *not* compared to it, and a
> disagreement between the two is a question about which is right, not a failure of the new one.
>
> Agreement with the **LLVM backend** is worth watching — two implementations of the same semantics
> disagreeing is information — but it is a cross-check, not an authority: that backend is itself under
> active development, so it is not a definition of correct either. Where the two disagree, the fixture
> and the language's semantics decide.

### 6. Staging

1. `Value` + the instruction loop, over an in-memory program built by the test harness; differential
   and unit gates from §5, no image reading, no FFI.
2. The image reader in the VM, against today's `.pvm`; then §4(a) (foreign arity), then §4(b) (tree
   dispatch) — each a paired change to the Level-2 bytecode backend and the reader, version-stamped.
3. Dynamic native FFI ([0111](0111-vm-dynamic-native-ffi.md)).
4. Promote the owned VM to the reference runner for the optimiser measurement field; boot's VM stays
   as the oracle.

Each slice ships with its gates; none of them requires boot to change.

> **Progress (2026-08-17):** slice 1 is implemented — `Value`, the instruction loop, the primops, and
> the array cell, with 47 unit tests over the invariants §5 names. Two staging corrections, found
> while wiring the gate:
>
> - **The corpus differential cannot be part of slice 1.** It needs an image to run, so it cannot
>   precede slice 2's reader; and the fixtures are `Effect`-shaped, so their output goes through host
>   leaves, which is [0111](0111-vm-dynamic-native-ffi.md)'s `host-runtime` resolution. Slice 1's gate
>   is therefore the unit tests alone, and the differential lands with **slice 2 + 0111's slices 1–2**.
>   Nothing about the design changes; the order the record gave was wrong.
> - **The differential is a *third leg*, not an image comparison.** `tools/l2-native-behavioural.sh`
>   already holds boot's VM and the Level-2 native binaries to each fixture's **own** expected stdout
>   ([0104](0104-retire-boot-byte-identity-gate.md) §2's 2026-07-18 amendment, whose point is that a
>   bug the two legs *share* must still fail). The owned VM joins that gate the same way rather than
>   being compared to boot directly — strictly stronger, and it removes a transition this record had
>   worried about: the boot leg builds its own image with `purvm build` from CoreFn and never consumes
>   Level-2's `.pvm`, so §4's format changes cost the gate nothing. There are no paired artifacts and
>   no delinearizer; §Consequences' "one image per runner" note is thereby moot.

### 7. What this does not change

boot stays frozen and stays the bootstrap seed. The native backend, the runtime ABI, the `.pmi`
interface, and the optimiser seam are untouched by this record. Level-2's bytecode backend is
untouched until slice 2.

## Consequences

- The project gains an interpreter it can extend — the first extension being user FFI, which is the
  point. The VM's foreign frontier stops being a table in a frozen OCaml binary.
- A second implementation of purvasm's dynamic semantics now exists and must be kept honest; §5's
  differential is a standing cost, not a one-off gate.
- The executed bytecode format becomes changeable for the first time, unblocking
  [0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md)'s deferred **(b)** scope
  (decls + init, `lowerEntry`-owned reachability, and eventually a REPL substrate). This record spends
  that budget narrowly (§4) and leaves the rest of (b) to its own motivation.
- With §4(b), `.pvm` stops being a lossy rendering of the compiler's decision tree, so a consumer that
  is not the VM — another backend, an analysis, a disassembler — reads the tree instead of rebuilding
  it from relative offsets. Since a `.pvm` producer and consumer no longer share the linear-offset
  convention, the offset back-patcher in `Bytecode.Lower.Match` disappears for `case` dispatch.
- ~~boot's VM cannot read a `.pvm` carrying either §4 change, so from that point the differential in §5
  runs on *paired* artifacts (one image per runner from the same source), not one shared image. The
  version stamp is what makes the mismatch a loud failure rather than a misparse.~~
  > **Correction (2026-08-17):** the premise was wrong. boot's VM leg of the behavioural gate builds
  > its **own** image (`purvm build` from CoreFn) and never reads Level-2's `.pvm`, so §4's changes
  > cost it nothing and no paired artifacts arise — see §6's Progress note. The version stamp is still
  > worth having, now purely so a stale *owned* image fails loudly rather than being misparsed.
- The optimiser measurement field moves onto an artifact the project owns, so a measurement can be
  refined (new counters, allocation attribution, per-key profiles) instead of being limited to what
  frozen boot happens to count.
- Interpreter throughput becomes a real concern: a PureScript interpreter over a boxed `Value` ADT,
  itself running on the runtime's allocator, will be slower than boot's OCaml VM. Accepted for the
  measurement role (deterministic counts do not care), and a named future track.
- Retiring boot moves from "blocked" to "staged": once the owned VM is the reference runner and the
  Level-2 native path compiles the corpus, boot's remaining role is the cold-start seed.

## Alternatives considered

- **Extend boot's OCaml VM under a freeze exception.** Cheapest by far, and *feasible*: the spike in
  §Context proves an OCaml executable can host a `dlopen`ed provider and service its `pv_*` calls
  re-entrantly. Rejected on three counts. It invests a new subsystem in the component the project is
  retiring; the exception class it would claim is for shipped leaf-table arms, not for new
  mechanisms; and — decisively — every value crossing that boundary would need an OCaml↔runtime
  conversion written and maintained in OCaml, including a re-implementation of the runtime's `pv_*`
  surface over OCaml values. The PureScript VM gets the runtime side for free because it *runs on*
  the runtime.
- **Make VM values *be* runtime values.** The FFI boundary would then cost nothing — a leaf could be
  `dlsym`ed, wrapped with `pv_make_closure` (which takes a real code address,
  `runtime/src/abi.rs`), and applied like any other value, with exact native semantics. Rejected on
  §3's principle, not on cost: it would make the VM a shadow of one backend's ABI — the LLVM
  backend's — at the moment that backend is explicitly one of several possible ones. It also drives
  the whole interpreter through unsafe coercions and leaves no room for the interpreter-only values.
  The FFI boundary cost is the price of that independence, and it is paid in one place (§3, and
  [0111](0111-vm-dynamic-native-ffi.md) §3) rather than spread through the interpreter.
- **Write the owned VM in Rust, beside the runtime.** `dlopen` is trivial there and values are
  already runtime values. Rejected: it would put a third implementation of purvasm semantics in a
  third language, permanently outside the self-hosting story, and it would face the *same* boundary
  question in the opposite direction (Rust-side interpreter values vs. runtime values).
- **Drop the VM; make native the only execution path.** Rejected: it would delete the optimiser
  measurement field before the native path can compile the `Effect`-using corpus, delete the
  behavioural oracle [0104](0104-retire-boot-byte-identity-gate.md) leans on, and give up the
  run-without-AOT story that a REPL and a scripting mode need.
