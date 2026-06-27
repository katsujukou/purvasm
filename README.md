# purvasm

Purvasm is an experimental abstract machine and bytecode designed for interpreting/executing PureScript.

The name purvasm comes from dual origin:

- *PureScript VM Assembler* -- so it's an bytecode **assembler** running on VM
- *PureScript Viratual Asynchronous Stack Machine* -- so it's a **runtime** which executes PureScript with genuine parallel execution model

purvasm は virtual machine の名を冠しているが、これは歴史的な背景によるもので、現在のところ我々はネイティブをターゲットにしている。

## Roadmap

- [x] Level 1:
  - Implement Purvasm bytecode compiler in OCaml
  - Implement native codegen in OCaml
- [x] Level 2:
  - Implement Purvasm bytecode compiler in PureScript
- [x] Level 3:
  - Compile Lv2 compiler with Lv1 booted compiler
- [ ] Level4: 
  - Compile Lv2 compiler with Lv3 compiler

Currently, the second-half of Level 3 is working: **we successfully compiled the purvasm compiler codebase written in PureScript** into the bytecode object files with the Lv1-native purvasm compiler.

The Lv2, PureScript version codebase, has no optimization pass, so the Lv3 compiler is horribly slow - it takes over ten minutes to compile 227 modules. So clealy our next goal is optimization.

While the Lv1 compiler is able to emit native binary, it completely relies on ocamlopt. So the runtime including GC and the concurrent execution model -- which is our central goal -- is not achieved by our own hand.
So after Lv2 optimization is done, out next goal is to implement the runtime system from scratch.
