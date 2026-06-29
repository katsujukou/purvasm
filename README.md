# purvasm

Purvasm is an experimental abstract machine and bytecode format designed for interpreting and executing PureScript programs.

The name *purvasm* has two origins:

* **PureScript VM Assembler** — referring to the bytecode assembler and compilation infrastructure.
* **PureScript Virtual Asynchronous Stack Machine** — referring to the runtime architecture designed to execute PureScript with a truly parallel execution model.

Although *purvasm* carries the name *virtual machine*, this is mostly for historical reasons. At present, our implementation targets native code generation.

## Roadmap

* [x] Level 1

  * Implement the Purvasm bytecode compiler in OCaml
  * Implement native code generation in OCaml
* [x] Level 2

  * Implement the Purvasm bytecode compiler in PureScript
* [x] Level 3

  * Compile the Level 2 compiler using the bootstrapped Level 1 compiler
* [ ] Level 4

  * Compile the Level 2 compiler using the Level 3 compiler

Currently, the latter half of Level 3 has been achieved: **we successfully compiled the Purvasm compiler codebase written in PureScript into bytecode object files using the Level 1 native Purvasm compiler.**

The Level 2 compiler, which is implemented in PureScript, currently has no optimization passes. As a result, the Level 3 compiler is extremely slow — compiling 227 modules takes more than ten minutes. Therefore, our next goal is to improve optimization.

Although the Level 1 compiler is capable of generating native binaries, it currently relies entirely on `ocamlopt` for code generation and runtime support. As a result, core components such as the runtime system, garbage collector, and concurrent execution model — which are central goals of this project — are not yet implemented by Purvasm itself.

After completing optimization of the Level 2 compiler, our next objective is to implement the runtime system from scratch.
