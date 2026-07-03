# Coding Conventions

## Design Philosophy

- Preserve invariants through the type system whenever possible.

- Any expression that may, at runtime, produce values that do not conform to the type inferred by the PureScript compiler (e.g., through FFI) must be prefixed with `unsafe`.

- Expressions named `unsafeXX` according to the above convention should, wherever possible, not be exported directly. Instead, they should be wrapped in expressions whose safety is guaranteed by the type system, and only the safe API should be exported.

- Comments must follow these guidelines:
  - Comments in source code must be written in English.
  - Comments should explain **why** something is done, not **what** it does.
    Code should be written to be as self-descriptive as possible, making "what" comments unnecessary in principle.
    However, when an implementation must be made unusually complex due to performance optimizations, workarounds for bugs in external dependencies, or other unavoidable reasons, comments should explain the rationale behind such implementations.
  - Use the host language's **documentation-comment** form for the documentation of a *declaration* — a module's purpose (a preamble comment at the top of the file) and its public types, values, record fields, and variant constructors. In OCaml this is `(** ... *)` (in PureScript, `-- |`); place it in a valid documentation position (before the item, or trailing for a field/constructor). These feed generated docs (odoc) and editor hover, which ordinary comments do not.
    Reserve ordinary comments (`(* ... *)`) for implementation/"why" notes inside a definition; those are not interface documentation. (The OCaml dev build treats a misplaced documentation comment — warning 50 — as an error, so a `(**` comment must sit in a valid position.)

- Modules should, whenever practical, have a single responsibility.

  As a rule of thumb, modules exceeding roughly 100 lines should be reviewed to determine whether multiple responsibilities have accumulated.
  However, module length itself is not a problem; cohesion is more important than size.

  If multiple responsibilities have accumulated within a single module, consider splitting it according to the following guidelines:

  - Type definitions should be separated into an `XX.Types` module containing type definitions, type class instances, and definitions closely associated with those types (such as smart constructors or Argonaut-style codecs).

  - When a subsystem is naturally organized around an effect boundary, a dedicated `XX.Monad` module may be introduced to define the abstractions for that effect.

  - When implementations of an effect need to be substituted between production and test environments, consider further separating the effect's abstract interface (such as capabilities, type classes, or Run-style algebraic effects) from its concrete implementations (such as instance declarations or interpreters).

- Tests exist to verify invariants that the type system cannot guarantee on its own — type-class laws are a representative example. Conversely, a module whose safety is already adequately guaranteed by its types need not be force-fitted with unit tests.

- As a direct implcation of the above, unit tests should not be limited to representative inputs used in examples. Whenever practical, they should also cover edge cases, boundary conditions, invalid inputs, and other scenarios that are prone to subtle implementation errors.

- This project includes a number of examples and benchmarks for demonstration purposes. Examples are intended to illustrate typical usage and provide a quick sanity check of expected behaviour. Benchmarks are intended solely for performance evaluation.

## Technical

- Modules outside a subsystem should not depend on that subsystem's
  internal modules.

  For example, if `Foo.Internal` exists, only modules within the `Foo`
  subsystem should import it.

- Aggregator modules whose sole purpose is to re-export public APIs are permitted.
  Such modules should not contain implementation logic.

- All type class instances must be explicitly named.

- Test code must follow these guidelines:
  - Under the `{package}/test` directory, create `Unit` and `E2E` subdirectories, and place unit tests and end-to-end/integration tests in the corresponding directories.
  - By default, create one test module exposing a `spec` for each module under `src`, named using the pattern `Test.{target module name}`.

    - Example: The test module for `Foo.Bar` should be named `Test.Foo.Bar`.

    Within a test module, it is desirable to have at least one `describe` per publicly exported function.

    However, modules for which unit testing offers little value — and whose behaviour is judged to be sufficiently covered by end-to-end tests — are exempt from this guideline.
  - Define separate commands for unit tests and end-to-end tests in `package.json`, as shown below:

    ```json
    {
      "scripts": {
        "test:unit": "spago test",
        "test:e2e": "spago test -m Test.E2E.Foo"
      }
    }
    ```

    This example assumes that `test.main` in `spago.yaml` is set to `Test.Unit.Foo`.

## For Coding Agents

- Start by proposing the design as an ADR. When a coding agent creates a new ADR, it MUST set the ADR status to `Proposed`.

- Implementation must begin only after the human maintainer reviews the ADR and changes its status to `Accepted`.

- An ADR is considered accepted only if one of the following occurs:

  1. The human maintainer explicitly states "Accept" (or equivalent explicit approval), or
  2. The human maintainer directly edits the ADR and changes its status to `Accepted`.

- Do not interpret conversational feedback in prompts (e.g. "looks good", "sounds reasonable", "seems fine", etc.) as acceptance. Only the explicit signals above authorize implementation work.

- All git operations are performed by the human maintainer. Coding agents must not run `git commit`, `git push`, create pull requests, merge, or perform any other history-mutating git operation unless the maintainer explicitly instructs them to.

- When finishing a piece of work, always format the code before handing it off (e.g. `dune fmt` for OCaml sources). CI rejects unformatted code, so pushing unformatted changes breaks the build.
