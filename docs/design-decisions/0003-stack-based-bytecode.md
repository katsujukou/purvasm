# 0003. PURVASM bytecode is a stack machine

- Status: Proposed
- Date: 2026-06-16

## Context

The project name **purvasm** ("PURescript Virtual Abstract *Stack* Machine" /
"PURescript Vm ASseMbler") names a stack machine, but the bytecode shape —
stack-based vs register-based — had never actually been decided; an earlier note
calling it "register-based" was premature.

The one live concern about committing to a stack machine was whether
**stack-based bytecode is hard to compile to native code** in phase 2
(bytecode→native, see [0001](0001-phase-1-host-language-ocaml.md)). This record
settles the bytecode shape and records why that concern does not hold.

## Decision

The PURVASM bytecode VM is **stack-based**: an operand stack for intermediate
values, on top of the explicit continuation stack the CESK machine already has
(`Cont.t`, see [0002](0002-cesk-execution-model.md)). Together they *are* the
stack machine.

Because we own the bytecode format, the CoreFn→PURVASM lowering will maintain
invariants that keep phase-2 native compilation straightforward:

1. The operand-stack height is **statically known** at every program point.
2. The operand stack is **empty at basic-block / statement boundaries**, so
   control-flow merges need no operand-stack phi reconciliation.
3. Control flow is **structured**, lowered from CoreFn's already-tree-shaped IR.

Phase-2 native compilation does **not** translate stack operations literally.
It lifts bytecode back into an SSA-based IR by *abstract stack simulation* —
walking the code with a compile-time operand stack whose slots hold symbolic IR
values (`push` binds a temp; a consuming op emits an IR node) — yielding the same
SSA a register bytecode would. The operand stack is erased at compile time and
never appears in the generated native code.

## Consequences

- **Native compilation is unaffected by the stack choice.** The operand stack is
  a compile-time encoding; both stack and register bytecode normalize to the
  same SSA before codegen. Feasibility is proven at industrial scale: JVM
  bytecode and .NET CIL are both stack machines with top-tier native compilers
  (HotSpot/GraalVM, RyuJIT/NativeAOT).
- Lowering CoreFn (a tree) to stack bytecode is a postorder emit; recovering the
  tree/SSA for codegen is the inverse — both mechanical. Reverse-Polish gives
  def-use chains for free.
- Because we own the format, the invariants above are *guaranteed by
  construction* in our lowering, not verified after the fact as a JVM must.
- **Accepted trade-off:** stack bytecode interprets somewhat slower than register
  bytecode (fewer-instruction dispatch is the register VM's advantage, cf. Lua
  5). This is acceptable: the phase-1 interpreter prioritizes clarity, and
  phase-2 native performance is independent of the bytecode shape. Revisit only
  if interpreter throughput becomes a bottleneck before native/JIT lands.
- Synergy: the continuation stack (already built) is half the machine; the
  operand stack is the other half. The name matches the artifact.

## Alternatives considered

- **Register-based bytecode** (Lua 5 / Dalvik / V8 Ignition style). Faster to
  interpret and often denser per instruction. Rejected as the default because it
  buys no native-compilation advantage (both normalize to SSA), adds register
  allocation to the assembler, fits the existing CESK continuation stack less
  naturally, and diverges from the project's name and identity. May be
  reconsidered narrowly if and only if interpreter throughput proves limiting
  before native compilation exists.
- **Leave the shape open.** Rejected: the bytecode shape gates the design of the
  PURVASM assembler and the phase-1b lowering, so settling it now unblocks that
  work.
