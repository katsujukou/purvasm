# 0068. `purvasm-stdio`: a native stdout/stderr package, with `Effect.Console` as a `ulib` shadow over it

- Status: Accepted
- Date: 2026-07-02

## Context

[0067](0067-v1-effect-execution-and-native-leaves.md) §5 (Correction, 2026-07-02) settled that the
native **runtime** must not own a `Console.log` primitive — it exposes a generic `stdio_write_line`
leaf, and `Effect.Console.log` is realised as a **`ulib` shadow** over a `Purvasm.Stdio.writeLine`-style
API. This record builds that PureScript layer.

But the same JS-name special-casing the runtime just shed **still lives in the boot resolver**: today
`boot/lib/ffi/ffi.ml`'s `host` registry and `boot/lib/ocaml_backend/codegen_ml.ml`'s `foreign` both map
`Effect.Console.log` / `Effect.Console.error` **directly** to native leaves (`print_string … ;
print_newline`). So `Effect.Console.log` is a privileged native name on the boot/VM/native-OCaml paths.
The clean end state — matching the runtime — is that **no resolver knows a JS-derived console name**;
they know only a generic `Purvasm.Stdio.*` leaf, and `Effect.Console` is ordinary library code (a
`ulib` shadow) on top.

The mechanics are established (mirroring [0056](0056-purvasm-system-host-leaves.md)
`purvasm-system`/`purvasm-fs` and [0046](0046-argonaut-core-pure-purescript-ulib.md)
`purvasm-json`/`argonaut-core`):

- **Host-capability package**: a spago package of non-exported `…Impl :: … -> Effect X` `foreign
  import` leaves wrapped by a safe API, with a dual-target `.js` Node impl; boot binds each leaf key in
  its `host` registry.
- **Native leaf resolution keys on the fully-qualified `Impl` ident string** in three places:
  `ffi.ml`'s `host` (source of truth; `native_provider` derives the native rung from it; effectful keys
  are listed in the `effectful` list for the effect analysis), and `codegen_ml.ml`'s `foreign` (the
  standalone native backend re-implements the same leaves, held to `host` by the differential). The
  Level-2 `compiler/src/Purvasm/Compiler/Ffi.purs` **omits the native rung**, so it needs no change.
- **`ulib` shadow** ([0065](0065-ulib-one-directory-per-registry-package.md)): one directory per
  registry package, flat module files + a `ulib.json` (`dependencies` — in-repo-first resolved,
  [0047](0047-ulib-package-manifest-extra-dependencies.md); a `test` block — repo/ref/fidelity,
  [0048](0048-ulib-tools-test-upstream-suite-execution.md)); the shadow module keeps the upstream
  export list **exactly** and drops the `foreign import`s.

The registry `console` is **6.1.0** (`.spago/p/console-6.1.0`); its `Effect.Console` has **12 JS
`foreign import`s** (`log`/`info`/`debug`/`warn`/`error`/`time`/`timeLog`/`timeEnd`/`clear`/`group`/
`groupCollapsed`/`groupEnd`) plus 6 pure helpers (`logShow` … `grouped`); `Effect.Class.Console` has no
foreign.

## Decision

### 1. New package `packages/purvasm-stdio`

`Purvasm.Stdio` exposes two total leaves — line writers to the two standard streams:

```purescript
module Purvasm.Stdio (writeLine, writeErrLine) where
foreign import writeLineImpl    :: String -> Effect Unit   -- stdout, appends "\n"
foreign import writeErrLineImpl :: String -> Effect Unit   -- stderr, appends "\n"
writeLine    = writeLineImpl
writeErrLine = writeErrLineImpl
```

`spago.yaml`: `name: purvasm-stdio`, deps `effect, prelude`. Dual-target `.js`
(`process.stdout.write(s + "\n")` / `process.stderr.write(s + "\n")`). Two streams because Node's
`console.log`/`info`/`debug` write **stdout** and `console.warn`/`error` write **stderr**; the shadow
(§3) needs both to stay faithful. (`write`/`writeErr` without the newline are deferred until a consumer
needs them.)

### 2. Native leaf wiring — generic, no console name in the resolver

- **`ffi.ml`**: add `host` arms for `"Purvasm.Stdio.writeLineImpl"` (stdout: `print_string s;
  print_newline (); flush stdout`) and `"Purvasm.Stdio.writeErrLineImpl"` (stderr: `prerr_string`;
  `prerr_newline`; `flush stderr`), and add both keys to the `effectful` list. **Remove** the
  `"Effect.Console.log"` / `"Effect.Console.error"` arms and their `effectful` entries — the resolver no
  longer knows a console name.
- **`codegen_ml.ml`**: mirror — add the two `Purvasm.Stdio.*` `foreign` arms, remove the
  `Effect.Console.*` ones (kept at parity with `host` by the differential).
- **`compiler/.../Ffi.purs`**: no change (native rung omitted).
- **Runtime (future):** `Purvasm.Stdio.writeLineImpl` is the name that will resolve to the runtime's
  `stdio_write_line` leaf ([0067](0067-v1-effect-execution-and-native-leaves.md); eventual `extern "C"`
  `purvasm_stdio_write_line`) once native codegen wires foreigns to the runtime. `writeErrLineImpl` maps
  to a stderr sink the runtime adds then (it currently has one capture sink for the write-line
  scaffold). No `runtime/` change in this record.

### 3. `ulib/console/` — `Effect.Console` as a shadow over `Purvasm.Stdio`

A `ulib` shadow of `Effect.Console` (targeting console 6.1.0), export list identical to upstream, **no
`foreign import`** remaining:

- `log`, `info`, `debug` → `Purvasm.Stdio.writeLine` (stdout).
- `warn`, `error` → `Purvasm.Stdio.writeErrLine` (stderr) — matching Node's stream choice.
- `time`, `timeLog`, `timeEnd`, `group`, `groupCollapsed`, `groupEnd`, `clear` → **no-ops** (`pure
  unit`): there is no native console object; timers/grouping/clear have no native meaning. This is a
  documented behaviour change (they no longer touch a console), acceptable because they are
  presentation-only and value-unobservable.
- The 6 pure helpers (`logShow` etc.) are unchanged — they build on the now-PureScript foreigns.

`ulib/console/ulib.json`:

```json
{
  "dependencies": ["purvasm-stdio"],
  "test": { "repo": "https://github.com/purescript/purescript-console", "ref": "v6.1.0", "fidelity": "js" }
}
```

`purvasm-stdio` resolves **in-repo first** during `ulib-tools` staging
([0047](0047-ulib-package-manifest-extra-dependencies.md)); `verify-deps` guards a
`console → purvasm-stdio → console` cycle (there is none). Add `console` to the
`ulib-tools/prepare-release.sh` test gate.

### 4. Build / dependency wiring

Spago auto-discovers `packages/purvasm-stdio/spago.yaml`; any workspace package (and `ulib` patches, via
`ulib.json`) depends on it by bare name. No `pnpm-workspace.yaml` change (`packages/**` is not a JS
workspace member).

## Consequences

- The console name is gone from **every** purvasm resolver — boot, native-OCaml, and (future) the
  runtime — so the JS-derived `Console.log` is uniformly ordinary library code over a generic
  `Purvasm.Stdio` capability, exactly the [0067](0067-v1-effect-execution-and-native-leaves.md)
  reframing extended to the boot side.
- **Migration cost (flag for review):** any fixture / example / test that reaches `Effect.Console.log`
  on the boot/native path currently relies on the removed native leaf; after this change it resolves via
  the `ulib` console shadow, so it must be built with the `ulib` overlay applied. Fixtures that don't use
  the overlay need updating (point them at `Purvasm.Stdio` directly, or overlay `ulib/console`). The
  scope of that migration (how many fixtures now, vs. staged) is the main thing to settle in review.
- `purvasm-stdio` is a reusable native capability (a sibling of `purvasm-system`/`purvasm-fs`), not
  console-specific — future stdio needs (a raw `write`, reading stdin) extend it.
- On the JS / stock-`purs` build the shadow's `writeLine` routes through the `.js` Node impl, so the
  console suite runs at `js` fidelity ([0048](0048-ulib-tools-test-upstream-suite-execution.md)).

## Alternatives considered

- **Additive only: add `purvasm-stdio` + the shadow but keep boot's `Effect.Console.log`/`error` native
  leaves.** Smaller, no fixture migration — but it perpetuates the exact JS-name special-casing in the
  resolver that [0067](0067-v1-effect-execution-and-native-leaves.md) removed from the runtime, leaving
  two ways `Effect.Console.log` resolves (native leaf *and* shadow). Rejected as incoherent with the
  reframing; the whole point is that no core resolver knows `Console.log`.
- **`writeLine` only (route `warn`/`error` to stdout too).** One leaf, one sink; matches the runtime's
  current single capture sink. But it diverges from Node's stdout/stderr split that the `console` suite
  and real programs observe. Two leaves keep fidelity; the runtime's second sink is a small deferred
  add.
- **Put `writeLine` on `purvasm-system` instead of a new package.** `purvasm-system` is process/env
  (argv/exit/getenv); stdio is a distinct capability with its own future surface (stdin, raw write), so
  a dedicated package matches the one-capability-per-package shape of
  [0056](0056-purvasm-system-host-leaves.md).
- **Keep `Effect.Console` foreign and only shadow `log`.** A partial shadow still leaves JS-only
  foreigns (`time`/`group`/…) unbound on the native build. The shadow must drop **all** foreigns to
  keep `console`'s consumers compiling natively.
