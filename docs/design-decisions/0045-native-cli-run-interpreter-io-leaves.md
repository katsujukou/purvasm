# 0045. A purvasm-native CLI entry: swap the `Run` interpreter to native IO leaves; never shadow `node-*`

- Status: Accepted
- Date: 2026-06-26

## Context

With the library foreigns resolved (`Data.Int`, `Data.Number` math, `Foreign.Object`,
`Data.Traversable`), the self-host CLI's native run hits `Node.FS.Constants.f_OK` — the **IO
boundary** ADR-0037 anticipated ("the first needs are file I/O, argv/stdout, process spawn").

`Purvasm.CLI` is built on the `Run` extensible-effect abstraction: the commands work in an effect
row `(FS + LOG + EXCEPT String + EFFECT + …)` and **all Node dependence is concentrated in one
interpreter**, `runNode` (`cli/src/Purvasm/CLI/Node.purs`), which discharges `FS`/`LOG`/`ENV` to
`Effect` via `node:fs` and `node:process`. The native run reaches `Node.*` only because
`Purvasm.CLI.Main` uses `runNode` (and `Process.argv`). So the fix is **not** to make `node-*`
work on purvasm — it is to provide a *different interpreter*.

Surveying what the commands actually use (so the native leaf set is minimal):

- **`FS`**: only `joinPath`, `mkdirP`, `readText`, `writeText` (no binary / `exists` exposed /
  `readDir` / `unlink` / `fileSize` / `resolvePath`).
- **argv**: `Process.argv` (in `Main`).
- **`ENV`**: defined as a capability but **no command uses it** (it is absent from every command's
  effect row); only `runNode` discharges it.
- **`LOG`**: `Log.info` ×4 via the terminal handler → `Console.log`/`Console.error`. `Console.log`
  is already a native leaf; `Console.error` (stderr) is not yet.
- **process spawn**: not needed *yet* — `build`/`compile` emit `.pvm`/`.pmo`/`.pmi` as *text*; no
  `ocamlopt` invocation. It is **planned** for the native-codegen phase (spawning `ocamlopt`), not
  out of scope.

The leaf set is therefore built up **incrementally**: this record adds the minimal effect subset to
reach `.pmi`/`.pmo`/`.pvm` output. The actions a command does not use *today* (binary FS,
`exists`, `readDir`, `resolvePath`, the `Process` spawn effect, `ENV`) are **planned, not
unsupported** — each lands when the phase that needs it arrives (notably native codegen).

## Decision

### 1. `node-*` packages are never `ulib`-shadowed (a standing non-goal)

`node-fs`/`node-process`/… are, by definition, Node-runtime packages. purvasm does not support
them; we never write a `ulib` shadow for `Node.*`. Reaching a `Node.*` foreign on the native
backend is a signal that a *Node interpreter* is being used where a native one is needed — fix the
interpreter, not the package.

### 2. A purvasm-native `Run` interpreter + a native entry module

Add, alongside `runNode`, a `runPurvasmNative` that discharges the **same** effect row using
native IO leaves instead of Node, and a native entry (e.g. `Purvasm.CLI.Main.Native`) that boot
compiles (`purvm native -m …`). The existing `Purvasm.CLI.Node`/`runNode`/`Purvasm.CLI.Main` stay
for the **dual-target** stock-`purs`/Node build (dev/test); only the entry and interpreter differ.
`ENV` is dropped from the native row (unused); `joinPath` is pure PureScript in the interpreter
(the separator is `/`), not a leaf.

> **Progress (2026-06-27):** the native interpreter's `dirname` (added when `Filesystem` promoted
> path handling, ADR-0043) follows the same pure `/`-separator convention as `joinPath`. This makes
> the native path ops **POSIX-only**: correct on the boot/posix target they run on today, and
> tolerated by Windows file APIs for paths we build ourselves, but not Windows-faithful (a
> `\`-separated argument would mis-split; `dirname` semantics differ). The Node interpreter
> (`runNode`, used by `cli`'s Node entry and `ulib-tools`) is already platform-correct via
> `node:path`. Windows fidelity for the native interpreter — delegating these ops to a
> platform-aware host path leaf — is **deferred to release**, not the bootstrap.

### 3. The first increment of the native IO leaf set, dual-target

The native IO leaf surface is grown **incrementally**; this record adds the subset for the
current `.pmi`/`.pmo`/`.pvm` output path. The leaves the native interpreter calls now — effectful
native leaves (ADR-0022/0023), each shipping a
`.js` (a `node:fs`/`node:process` implementation) so the CLI still builds and runs on stock
`purs`/Node, and resolving to a boot host leaf on purvasm (the `purvasm-base` dual-target pattern,
ADR-0038):

- `existsImpl :: String -> Effect Boolean` and `readTextImpl :: String -> Effect String` — the
  `ReadText` handler composes them into `Maybe String` (so `loadClosure`'s "skip absent module"
  and `Compile`'s "error on absent" both work).
- `writeTextImpl :: String -> String -> Effect Unit`.
- `mkdirRecImpl :: String -> Effect Unit` (mkdir -p).
- `argvImpl :: Effect (Array String)` — replaces `Process.argv`.
- `errorImpl :: String -> Effect Unit` (stderr) — for `Log` error/`Console.error`; `Console.log`
  (stdout) already exists.

That is ~6 small leaves. The remaining `Filesystem` actions (binary read/write, `exists`,
`readDir`, `unlink`, `fileSize`, `resolvePath`), the `Process` spawn effect, and `ENV` are
**planned next increments** — the native-codegen phase will need binary I/O and `Process` (to
invoke `ocamlopt`) — added as each is reached, with the same leaf + `.js` shape.

### 4. Leaf conventions: first-order primitives only

The native leaves traffic in **primitives** (`String`/`Boolean`/`Array`/`Unit`) — `Maybe` and
error handling are composed in the PureScript interpreter, never returned across the leaf boundary
(it is first-order, ADR-0020/0036). Effectful leaves follow the `Effect`-thunk model (ADR-0023):
the IO happens when the `Effect` is forced, and multi-argument leaves declare their real arity
(ADR-0034 I4) so saturation places the effect correctly. boot implements each leaf in `Ffi.host`
(oracle + VM) and re-implements it in the `codegen_ml` `Rt` prelude (native), the differential
enforcing parity. The leaf **names are an IO ABI surface** fixed by this record.

## Consequences

- The native CLI runs by interpreter substitution — the `Run` abstraction pays off exactly as
  designed; no `Node.*` ever resolves on purvasm.
- The native leaf surface grows by ~6 IO leaves (file read/write, mkdir, argv, stderr) — the
  "genuinely-native" leaves ADR-0038 §4 / ADR-0037 anticipated, beside `Console.log`/`showNumber`.
- Dual-target holds: the leaves' `.js` keep the CLI building and running on Node for dev/test; boot
  ignores the `.js` and binds the host leaf.
- `build`/`compile` become runnable natively end-to-end (read `corefn.json` → emit `.pmo`/`.pmi`/
  `.pvm`), the first time the self-host compiler *runs* as a native executable.
- the `Process` spawn effect (`ocamlopt`) and the richer `FS` surface (binary, `exists`, `readDir`,
  `resolvePath`, …) are **planned next increments** for the native-codegen phase — staged in as
  reached, not unsupported.

## Alternatives considered

- **`ulib`-shadow `node-fs`/`node-process` over native leaves.** Rejected as a category error:
  `node-*` are Node-runtime packages, not registry libraries we re-express; the `Run` interpreter
  is the designed seam. Shadowing `Node.*` would also drag the whole Node API surface in.
- **A bespoke native `main` bypassing `Run`.** Throws away the effect abstraction the CLI already
  has; the interpreter swap reuses all command logic unchanged.
- **Return `Maybe`/errors across the leaf boundary** (a leaf yielding `Just`/`Nothing` `VData`).
  Rejected: the foreign boundary is first-order (ADR-0020/0036); leaves stay primitive and the PS
  interpreter composes `Maybe`/`Either`.
- **Implement the full `Filesystem`/`ENV`/`Process` surface now.** Premature, not wrong: these are
  *planned* (native codegen will need binary I/O + spawn), but the minimal-FFI policy stages them in
  as each phase reaches them rather than up front. The current increment stops at `.pmi`/`.pmo`/
  `.pvm` output.
