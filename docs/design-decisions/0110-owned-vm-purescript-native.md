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

> **Pinned (2026-08-22):** `purvm` stays the frozen boot VM; the owned VM's executable is
> **`purvasm-vm`**; and a later `purvasm run` is the user-facing command that launches it. No
> provisional name (`purvm2`, `purvm-next`) reaches a public surface, and a harness distinguishes the
> two with `PURVM` and `PURVASM_VM` rather than by path.

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

> **Amendment (2026-08-18, as implemented):** the carrier variant carries a diagnostic **origin**
> beside the runtime value — the foreign key it came from — because
> [0111](0111-vm-dynamic-native-ffi.md) §3's boundary errors promise to name the leaf that demanded a
> crossing, and by the time a value reaches the boundary the instruction that resolved it is gone.
> Nothing dispatches on it, so the opacity below is unaffected: it is a label, not a decoding.

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
   as a **temporary calibration leg** — ~~the oracle~~ (§5's Correction: boot is not an oracle for this
   VM, so its remaining role is to agree during the changeover, not to define correctness).

> **Staging refinement (2026-08-22), forced by the scope correction in §Consequences:** slice 2 runs
> in five steps, and the third is not optional.
>
> - **A.** The reader, against today's `.pvm`, verified on programs with no foreign leaf.
> - **B.** §4(a) foreign arity — the paired backend/reader change, version-stamped. This is what lets
>   the owned VM run the corpus at all, since every benchmark entry is `main :: Effect Unit`.
> - **C.** The owned measurement leg in `benchmarks/run-benchmarks.sh`, **run alongside** boot's until
>   it agrees. While it runs, one Level-2 compilation emits both forms — the legacy image for
>   `purvm run --count` and the arity-carrying one for `purvasm-vm` — and because §4(a) adds metadata
>   without changing the instruction sequence, the two must agree on the **instruction count**, not
>   merely on output. That equality is the calibration; it is available exactly once, here.
> - **D.** §4(b) tree dispatch. Instruction counts change meaning at this point (a `case` stops being
>   a linear region), so the owned VM's counts become the baseline and boot's leg comes out.
>
>   > **Pinned before D (2026-08-24), on accepting B and C:**
>   >
>   > 1. Tree dispatch is a **new image version**. It does not redefine version 4 — an existing v4
>   >    image keeps meaning exactly what it means today, so a reader can tell the two apart by the
>   >    stamp rather than by inspecting a `case`.
>   > 2. Through the migration the three forms are distinct and named: **v3** boot's, **v4** the owned
>   >    VM's *linear* form, and the next version the owned VM's *tree* form.
>   > 3. After D the owned VM must still agree with boot on **output**. That parity does not lapse
>   >    because the encoding changed.
>   > 4. **Instruction-count parity does not continue.** It was B/C's calibration and it is kept as
>   >    evidence (the eight pairs above), not as a standing condition: tree dispatch changes the
>   >    instruction vocabulary, so equal counts would no longer mean the same programs ran.
>   > 5. The owned VM's counts are **re-taken after D** as the measurement field's new baseline. The
>   >    old numbers describe the old vocabulary and do not carry over.
>   > 6. D's completion must **write down the two dates the reader and the writer stop moving
>   >    together**: how long the reader keeps accepting the older versions, and when the emitter
>   >    stops producing v4. Leaving either implicit is how a format acquires a permanent tail.
> - **E.** The CLI wiring, which also owes a **manifest beside the bytecode image**: manifest emission
>   currently lives in the native linker (ADR-0111 §4), while `purvasm run` writes only `app.pvm`, so
>   the bytecode finalisation must emit the matching manifest for `--manifest` to become discovery.
>   ~~Not a blocker for B or C: the benchmark corpus uses runtime leaves only.~~ **Wrong, and only
>   measurement showed it (2026-08-24):** `bench-json-parse` reaches `Data.Number.isFinite`, which ulib
>   ships as a `.c` — a *workspace-provided* key, not a runtime one. A compiled program links it; a
>   hosted guest must be handed a provider for it (ADR-0111 §4). C therefore needs one piece of E
>   early, and the harness builds it itself until E packages ulib's native side properly.
>
> **B is not done when the reader and backend are.** It is done when C is green. A format bump that
> lands without the owned measurement leg leaves `run-benchmarks.sh` writing images its own runner
> cannot read.
>
> **Both closed 2026-08-24** — see the Corrections below for what C actually cost: §4(a) turned out
> to be leaf recognition, lowering, emission and reading rather than one field; the reader owed
> `Number` literals; and the VM owed its guest an argv.

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

> **Progress (2026-08-23): step A is done, and step B is wider than §4(a) reads.**
>
> - **A is complete.** The VM decodes today's `.pvm` and runs it: the three global shapes are three
>   evaluation strategies (closure, strict CAF at load, by-need forced on use), and the entry is the
>   typed terminal demand §5 describes. Its gate is `tools/vm-image-e2e.sh` — hand-written fixtures for
>   the format's edges (a matching arm *and* the default edge, since an off-by-one in the relative
>   offset base lands inside the wrong arm rather than failing; a stale version stamp refused by name),
>   plus a foreign-free program compiled by the Level-2 bytecode path and run end to end, so the
>   reader is not judged only against fixtures written by the person who wrote it.
>
> - **B's real scope, found while writing that gate.** The bytecode backend does not emit `ForeignRef`
>   *at all* today — the gap is not a missing arity field. `Purvasm.Compiler.Ffi.resolver` has an
>   intrinsic rung and a structural rung and nothing else, so a native key that neither answers stays
>   an ordinary `Load` of an unbound global. boot's VM satisfies that from its compiled-in registry;
>   the owned VM has no registry to satisfy it from, and must not acquire one. §4(a) is therefore four
>   things, not one: leaf **recognition** (FSR-derived arities), **lowering** (a foreign atom rather
>   than a variable), **emission** (`ForeignRef key arity`), and **reading**. The recognition side
>   already exists — `nativeLeafArities`, `leafClosureArity`, `resolveNativeForeigns` in the LLVM
>   backend — and is backend-neutral, so it moves to a shared module consumed by both backends rather
>   than being reimplemented for bytecode.
>
> - The step-A gate **asserts** that boundary instead of leaving it implicit: a program with a native
>   leaf must still fail as an unresolved `Load`. Step B flips that line, and its own gate must check
>   the leaf is emitted as `ForeignRef key arity` — not merely that the program ran, which a resolver
>   change could achieve by the wrong route.

> **Progress (2026-08-23), continued: step B is implemented, and step C's calibration holds.**
>
> - **Recognition is now shared, not copied.** `leafClosureArity`, `nativeLeafArities` and
>   `resolveNativeForeigns` moved out of the LLVM driver into `Purvasm.Compiler.NativeLeaf`, and both
>   backends read them. A second derivation over the same FSR would have been the drift source this
>   step exists to remove: two backends disagreeing about which keys are leaves fails as a link error
>   on one side and a stuck run on the other, which do not look like the same bug.
>
> - **The bytecode backend applies the lowering** it never had, so a native leaf reaches the image as
>   `ForeignRef` instead of an unbound `Load`. This does not move a decl's `deps` — the key migrates
>   from `fvExpr` to `cfExpr` and the union is the same set — and it does not move the instruction
>   count: the lowering is a one-for-one opcode substitution, confirmed by running one image both ways
>   (2,544,920 instructions with `fr`, 2,544,920 with the `ld` it replaced).
>
> - **The arity rides the image, not the `.pmo`.** `.pmo`/`.pmi` stay at format 3 — boot reads those,
>   and the `.pmi` is byte-identical to boot's by ADR-0088 §1 — while the *linked* image gains version
>   4, whose `ForeignRef` carries the leaf's physical closure arity. The arity is a link-time fact
>   (the linker compiles the FFI ladder's structural terms itself, and those reference leaves too), so
>   it is resolved once at finalisation from the whole closure's FSR shapes, which `BuildProducts` now
>   hands over. An image referencing a leaf those shapes do not describe is **refused, naming every
>   such key**, rather than written with a guessed arity.
>
> - **Both forms come out of one compilation** (`app.pvm` v3, `app.v4.pvm` v4) for as long as the two
>   runners coexist. The reader accepts both versions and refuses to read v4 syntax under a v3 stamp:
>   a stamp a reader overrides is not a stamp.
>
> - **Step C's equality holds where it can be measured, and C is BLOCKED on one missing piece.**
>   `benchmarks/run-benchmarks.sh` grew an owned leg beside boot's, opt-in through `PURVASM_VM` and
>   reported as SKIPPED rather than passing quietly when absent; it checks output *and* instruction
>   count, in `--opt` and `--no-opt` alike. On a program that takes no arguments the two runners agree
>   exactly — `Gate.GcChurn`: boot 2,544,920, owned 2,544,920, same output.
>
>   Every benchmark in the corpus, however, reads its input size from the guest's argv (ADR-0075 §4),
>   and at the time of writing **the owned VM had no notion of a guest argv**: it resolved
>   `Purvasm.System.Process.argvImpl` through `host-runtime`, whose leaf reported the *process's*
>   command line — so the guest saw `--image` where its first argument belongs and ran at its default
>   size. That is resolved by the 2026-08-24 Correction below; the leg still carries the `ARGV(mode)`
>   verdict, established by re-running at a doubled size rather than assumed, so a regression in the
>   injection is named for what it is instead of appearing as a bare count mismatch.
>
>   It is worth being precise about what that does and does not leave in doubt, because the argv gap
>   changes the *input*, not the agreement. Held to the same argv, the two runners agree exactly on a
>   corpus program: `bench-st-ref` at `--opt` counts **60,873** on the owned VM, and boot handed the
>   same three trailing arguments the VM's own command line gave its guest (`-- --image X --count`)
>   counts **60,873** too, on a program that exercises `ST`, arrays and string parsing. Boot's
>   ordinary run counts 60,581 — the whole 292 is `Int.fromString` walking `"--image"` instead of
>   finding no argument at all. The hand-written fixtures and `Gate.GcChurn` (2,544,920 both ways)
>   agree with no argument in play. So the calibration itself is demonstrated; what is missing is the
>   ability to *run it over the corpus unattended*.
>
>   A **second** reader gap surfaced with it and is closed: a `Number` literal is written as the
>   signed 64-bit decimal spelling of its IEEE-754 bit pattern (ADR-0038 §4), which step A's reader
>   refused. `Int.fromString` — which `Bench.Common.sizeArg` itself calls — reaches one, so no
>   benchmark could even be decoded. The inverse now lives in `Purvasm.Abi.Float64`, beside `Mangle`
>   and `Fnv1a64` and for the same reason: it is a fact about the artifact format that a writer and a
>   reader must not derive twice. It is pure PureScript — no `Math` leaf and no runtime API — and is
>   tested as a round trip *through the writer*, on the values where exact and approximate part
>   company (negative zero, subnormals, both infinities, both 64-bit extremes).
>
>   Writing that inverse turned up a **pre-existing optimiser bug**, unrelated to this record and
>   reported here only because it is what the code above is shaped around: an expression the
>   optimiser can constant-fold to **NaN** never reaches a fixpoint, so `Nbe` exhausts its rewrite
>   fuel on the binding and the build dies. `v = 0.0 / 0.0` reproduces it alone; `(1.0e308 * 10.0) -
>   (1.0e308 * 10.0)` and `inf / inf` too; ±Infinity folds correctly. The likely cause is a fixpoint
>   test comparing successive literals, since a NaN literal compares unequal to itself. `Float64`
>   avoids it by deriving both infinities and the NaN from its arguments, which leaves nothing to
>   fold — a workaround in *this* module, not a fix, and the bug is still there for the next caller.
>
>   The argv fix was a decision rather than an implementation detail, and it was taken by the
>   maintainer rather than in passing. See the Correction below.

> **Correction (2026-08-24): guest argv is runtime *context injection*, not a provider exception.**
> Accepted by the maintainer over two alternatives (a reserved VM-answered key set; deferring to step
> E). It closes step C's last blocker and leaves [0111](0111-vm-dynamic-native-ffi.md) §4 untouched.
>
> `argv` is not a question about *who provides* a leaf — it is the execution context a runner hands
> to the one provider there already is. So `Purvasm.System.Process.argvImpl` keeps `host-runtime` as
> its sole provider, exactly-one stands, and no key is reserved or given precedence anywhere.
>
> - The argv the guest observes lives on the **context** (`Heap`), not in a process global, so two
>   contexts hosting two guests cannot cross. It defaults to the process argv, so a compiled program
>   with no host above it is unchanged.
> - A host overrides it with `pv_runtime_set_guest_argv(ctx, argv)` before the guest runs, handing
>   over `[image path] ++ the guest's own arguments`. The VM's flags (`--image`, `--count`, `--ffi`,
>   `--manifest`) are never among them; the guest's start after an explicit `--`.
> - The strings are **copied out**. Retaining a `PVWord` would leave a heap pointer in runtime state
>   that the next collection invalidates and nothing traces.
> - The setter is **host-control, not foreign-author API**: declared in a separate
>   `runtime/include/purvasm_host.h`, absent from `purvasm.h`, absent from `purvasm-sys`, and
>   therefore absent from the export allowlist — so it is not in a `--host-foreign-api` executable's
>   dynamic exports, unbindable by a `dlopen`ed provider, and unnameable by a guest `ForeignRef`.
>   `PV_FOREIGN_ABI_VERSION` does not move: no provider can reference it, so no provider's
>   compatibility depends on it. The VM reaches it through one trusted sibling, `Purvasm/VM/Host.c`.
>
> Running the corpus turned up one more thing the record should carry, and it is **not** an argv
> problem: `bench-json-parse` needs `Data.Number.isFinite`, which ulib ships as a `.c` and a natively
> compiled program gets by *linking* it. A hosted guest has no such link, so it reaches that key the
> way [0111](0111-vm-dynamic-native-ffi.md) §4 already says a workspace-provided key is reached —
> through a module the runner loads. The VM side of that works today (§4's loaded-provider gate); what
> is missing is that nothing *packages* ulib's native side as a shared object, which is step E's
> manifest-beside-the-image work. The benchmark harness builds one itself in the meantime (eight keys,
> two files, the ordinary `--ffi` path) and says so where it does it.
>
> **Step C is green (2026-08-24).** All eight benchmarks agree with boot on output *and* instruction
> count, in `--opt` and `--no-opt` alike — the calibration this record said would be available
> exactly once, taken: `fib` 9,376,516/29,355,171 · `count-state` 193,341/347,393 · `effect-ref`
> 61,129/118,652 · `run-state-except` 3,390,879/4,200,677 · `st-ref` 61,120/91,566 ·
> `map-fold-array` 317,280/599,308 · `quicksort` 4,224,348/7,896,107 · `json-parse`
> 2,193,851/4,346,910. **B is therefore closed too**, on this record's own condition.
>
> One thing the run flags that is **not** step C's: `run-state-except` exceeds the ADR-0089 §7
> compile-*time* gate at ratio ≈ 4.3 against a 4.0 threshold, and it does so with the owned leg
> switched off. Measured directly, `--opt` takes ~6.0 s against `--no-opt`'s ~1.5 s, repeatably — a
> real optimiser-time property of that module, not noise, and against the 1.5–2.4 band the threshold
> was set over in 2026-07-11. Nothing here caused it: every cost this slice adds to a compile (a
> per-module ANF traversal, one extra 48 KB image write) falls on **both** modes, and adding a
> constant to both sides of a ratio above 1 moves it toward 1, not away.
>
> It is **not** blocking integration either: `--opt-effect` is a mode `.github/workflows/benchmarks.yaml`
> never passes, and that workflow is `workflow_dispatch` plus a weekly cron rather than push/PR — so
> the gate fires only when someone measures by hand. Tracked as its own question: which change took
> the ratio past 4.0, answered by bisection rather than by raising the threshold.
>
> Two more readings on 2026-08-24, on a machine that had been running heavy jobs for hours:
> `run-state-except` 4.291 then 4.558, and `effect-ref` 3.068 → 3.843 → **5.502**. So the two are not
> the same finding: `run-state-except` is over the threshold repeatably and under direct measurement
> (~6.0 s against ~1.5 s), while `effect-ref` swings by 1.8× between runs and is noise. The bisection
> above is owed for the first one only, and it wants a quiet machine.
>
> A **speed** observation, recorded so it is not rediscovered as a surprise: the owned VM is far
> slower than boot's, which is expected of an interpreter with no performance work done to it and is
> irrelevant to what step C measures (counts, not time) — but it decides the leg's cost. On
> `bench-st-ref` at `--opt` (61,120 instructions) five runs took 1.96 s against boot's 0.041 s, so
> roughly 45× on that one program, and the corpus leg runs each benchmark in both modes. That is why
> it is opt-in and why CI does not carry it. §4(b)'s tree dispatch is the first thing that should move
> the number; nothing here should be read as a measurement of the design.
>
> Verified: a fresh context reports the process argv; an override replaces it; two contexts keep
> theirs apart; an empty argv (the empty-array sentinel, not a heap object) is accepted; and the argv
> survives a collection that moves every string it was read from. The gates add `guest-argv` (the
> guest sees the whole `[image] ++ args` array, not just the element the corpus reads),
> `guest-argv-flags` (the VM's own flags never reach it), `host-control` (nothing named `pv_runtime_*`
> appears in the executable's dynamic exports — asserted on its own, since every other loader leg is
> positive and none would notice) and `host-control-reach`, which is what that absence is *for*: an
> image whose `ForeignRef` names the trusted setter exactly must be refused as `unbound native
> foreign`, since a guest able to reach it could rewrite the argv of the runner hosting it.

> **Progress (2026-08-24): step D — `case` keeps its tree shape (§4(b)).** Implemented against the
> terms pinned above, and this note discharges the last of them.
>
> - **The compiler keeps the tree and drops it on the way out.** `Lower.Match.compileTree` now emits a
>   switch whose arms and default are nested blocks, and a guard chain as one `Guarded` carrying its
>   clauses — no labels, no back-patching, no end-join. The offset form is produced by a new
>   `Bytecode.Linearise` for the two readers that predate it: the `.pmo` and boot's version-3 image.
>   The information now flows the way an interchange artifact should — the producer keeps the
>   structure it built, and one consumer that cannot use it gets a flattened copy.
>
> - **The flattening is byte-checked, not assumed.** Twenty-four images (eight benchmarks and four
>   other programs, each at `--opt` and `--no-opt`) were captured before the change and re-emitted
>   after: **21 identical**. Getting there caught one real difference — a tail call ends an
>   activation, so the old assembler emitted no fall-through jump after one, and neither may this.
>
>   The remaining **3 differ by construction, and only downward**: where an ANF-level `case` sat in
>   the tail of another's arm, the old lowering gave each its own join and control walked a chain of
>   `Jump 0`s between them. Inheriting the enclosing join collapses the chain. Output is identical on
>   boot in all three; the counts drop (`Gate.DictDispatch` −4, `bench-json-parse` −500,
>   `bench-run-state-except` −1,937). Reproducing the no-op chains would mean carrying a defect
>   forward for a checksum, and count parity is retired here by the pinned terms anyway.
>
> - **Version 5** is the owned VM's format: arities (§4(a)) *and* tree `case`s (§4(b)). It does not
>   redefine version 4, which is no longer produced — the calibration it existed for is taken and
>   recorded. The owned image's filename is now `app.owned.pvm`: the stamp inside says which format it
>   is, and a name repeating the number would need renaming at every bump.
>
> - **Answering the pinned question about dates.** The reader takes **version 5 only**, from this step.
>   Both retired versions are refused by *name* rather than by number — a version-3 image says it is
>   boot's and to run it with `purvm`; a version-4 one says what it was and that nothing produces it —
>   because a reader who has the wrong image in hand needs to know which runner to use. The emitter
>   stopped producing version 4 at this step, and still produces version 3, which is not scaffolding:
>   boot runs it, and the two runners remain held to the same **output**.
>
> - **The VM lost its migration scaffolding.** `SwitchCtorRel`/`SwitchLitRel`/`SwitchLenRel` and their
>   machine arms are gone, along with the version threading the reader needed while two shapes were
>   admissible. The tree arms and `Guarded` were already there and already gated.
>
> - **The corpus, after D.** Output parity holds 8/8 in both modes, and the owned VM's counts are
>   taken as the measurement field's new baseline (`--opt` / `--no-opt` / ratio):
>
>   | benchmark | owned `--opt` | owned `--no-opt` | ratio | boot's ratio |
>   |---|---:|---:|---:|---:|
>   | `fib` | 9,376,501 | 29,355,167 | 3.131 | 3.131 |
>   | `count-state` | 192,323 | 346,388 | 1.801 | 1.797 |
>   | `effect-ref` | 60,113 | 118,648 | 1.974 | 1.941 |
>   | `run-state-except` | 3,361,298 | 4,193,598 | 1.248 | 1.238 |
>   | `st-ref` | 60,104 | 91,562 | 1.523 | 1.498 |
>   | `map-fold-array` | 317,262 | 599,302 | 1.889 | 1.889 |
>   | `quicksort` | 4,188,747 | 7,861,518 | 1.877 | 1.869 |
>   | `json-parse` | 2,166,044 | 4,345,904 | 2.006 | 1.982 |
>
>   The counts sit just below boot's — the arm jumps the tree form does not need — and the *ratios*
>   move by under 2%, which is the reassuring part: the optimiser's measured effect did not change
>   when the vocabulary did. Behavioural gate ★ over the same change (7 fixtures, both modes, GC
>   stress, debug-ABI), which is an independent check of both halves: the tree form executes right and
>   `Linearise` flattens right.
>
> - **Gates.** The image gate's hand-written fixtures are version 5, so the nested decode is exercised
>   by a fixture and not only by a compiled program; the leaf-emission leg additionally refuses an
>   image whose switch arm carries an integer where a block belongs — a regression to the linear form
>   that would otherwise parse. `legacy-image` flipped from "still decodes" to "refused, naming
>   `purvm`". The benchmark leg compares **output** and now *reports* the owned VM's own counts as the
>   measurement field's new baseline.

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
  >
  > **Scope correction (2026-08-22):** the above holds for the behavioural gate and the examples
  > sweep, and **only** for them. It was written after checking those two, and it generalised one
  > step too far: `benchmarks/run-benchmarks.sh` — the optimiser measurement field
  > ([0088](0088-vm-backend-lowers-like-native-release-boot-byte-identity.md) §0(a)'s "`--opt`
  > bytecode routed onto boot's VM") — compiles with **Level-2's `purvasm run`** and then executes
  > that image with `purvm run --count`. boot reads a Level-2 `.pvm` there, so §4 **does** break it.
  >
  > Two consequences, and they are what §6's staging note now encodes: the §4(a) switchover and the
  > introduction of an owned measurement leg are **one migration, not two**; and a benchmark entry is
  > `main :: Effect Unit`, so it reaches a stdio leaf — which means the owned VM cannot run the corpus
  > at all until arity is in the image. "Reader first, format later" is therefore not available as a
  > way to stage this.
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
