# purvasm

Purvasm is an experimental abstract machine and bytecode format designed for interpreting and executing PureScript programs.

The name *purvasm* has dual origins:

* **PureScript VM Assembler** — referring to the bytecode assembler and compilation infrastructure.
* **PureScript Virtual Asynchronous Stack Machine** — referring to the runtime architecture designed to execute PureScript with a truly parallel execution model.

Although *purvasm* carries the name *virtual machine*, this is mostly for historical reasons. At present, our implementation targets native code generation.

## Roadmap

* Level 1

  * Implement the Purvasm bytecode compiler in OCaml
  * Implement native code generation in the OCaml boot compiler, now targeting LLVM and the owned runtime

* Level 2

  * Implement the Purvasm bytecode compiler in PureScript

* Level 3

  * Compile the Level 2 compiler using the bootstrapped Level 1 compiler

* Level 4

  * Compile the Level 2 compiler using the Level 3 compiler

## Current Status

The Level 1 compiler is now approaching completion. Earlier native execution lowered ANF to OCaml and
delegated the final native compilation step to the OCaml toolchain (`ocamlopt`). Purvasm now has its own
native ABI and an LLVM backend, with a Rust-owned runtime for heap manipulation, garbage collection, and
the C FFI boundary. See [ADR-0060](docs/design-decisions/0060-native-codegen-llvm-owned-runtime.md) for the
LLVM + owned-runtime direction, [ADR-0071](docs/design-decisions/0071-codegen-runtime-c-abi.md) for the
runtime C ABI, [ADR-0072](docs/design-decisions/0072-anf-to-llvm-lowering.md) for ANF-to-LLVM lowering, and
[ADR-0073](docs/design-decisions/0073-ulib-shipped-native-foreign-and-link-time-resolution.md) for
ulib-shipped native foreigns.

The native programs produced by the current Level 1 compiler still run on a single-capability runtime.
They do not yet provide true multicore parallel execution, and the current collector is a non-generational
copying collector. A full generational hybrid collector (copying + mark/sweep) and the M:N work-stealing
parallel runtime are v2 work; see [ADR-0064](docs/design-decisions/0064-v1-single-capability-native-abi-codegen-contract.md)
for the v1 runtime scope, [ADR-0061](docs/design-decisions/0061-capability-local-shared-immutable-gc.md) for
the planned GC design, and [ADR-0062](docs/design-decisions/0062-mn-work-stealing-scheduler-fibers.md) for
the scheduler/fiber runtime.

The immediate goals are:

* Compile the Level 2 compiler through the Level 1 compiler's OCaml-independent native path.
* Port the native components of the Level 1 compiler into the Level 2 compiler.

Together, these steps produce a native compiler written in PureScript.

Optimization is intentionally still modest. The current Level 1 compiler applies only a small set of
optimizations, and the Level 2-and-later compilers do not yet optimize at all. Once the Level 2 compiler can
compile PureScript applications to native code, optimization work will be guided by measured JS / ES /
Purvasm three-way comparisons rather than by guesswork. The examples differential gate described in
[ADR-0074](docs/design-decisions/0074-examples-multi-backend-differential-gate.md) is the first step toward
making those comparisons routine.

## Development Guide

This project requires multiple language toolchain: OCaml, PureScript and Rust.
All the requisites are listed in the flake.nix, so the quickest is to enter nix develop shell:

```sh
nix develop
```

### Build Level-1 compiler

```sh
cd boot
dune build
```

This emits the level-1 compiler in `boot/_build/default/bin/main.exe`

### Build level-2 compiler

```sh
spago build
```

Then you can use level-2 compiler through node:

```sh
node cli/index.node.js
```

Note that `cli/index.js`, which is the shell for level-3 native compiler, is **NOT** the entry module of level-2 compiler.
Currently, level-2 compiler emits the single bytecode `app.pvm`, which you can execute it with `purvm`:

```sh
boot/_build/default/bin/main.exe run output-pvm/app.pvm
```

### Build PS application to native executable with Level-1 compiler

```sh
boot/_build/default/bin/main.exe build --backend llvm --ulib ./purvasm_lib -m Main
```

Make sure that ulib packages are built.
To build the ulib, you can use ulib-tools:

```sh
node ulib-tools/index.js build --out ./purvasm_lib
```
