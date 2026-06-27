# purvasm-json

A backend-agnostic, foreign-free PureScript JSON **parse / print core** for purvasm
(namespace `Json.Core.*`).

The lexer, recursive-descent parser, and structural serialiser live here once, parametrised over
node construction (a `Builder`) and elimination (an `Eliminator`) so any JSON representation can
ride them — `argonaut-core`'s `Json` (`Foreign.Object` + `Array`) and, later, `purescript-json`'s
`JObject` / `JArray` — without duplicating the grammar walk.

This is a third source category beside the `Purvasm.*` ABI base (`packages/purvasm-base`,
ADR-0038 §1) and the registry-patch `ulib`: original shared library code, not a primitive and not
a patch. It is dual-target (builds on stock `purs`).

See [ADR-0046](../../docs/design-decisions/0046-argonaut-core-pure-purescript-ulib.md) for the
design and rationale.

> **Status:** ADR-0046 is *Proposed*. This package is the agreed home (directory + manifest); the
> `Json.Core.*` implementation lands once the ADR is Accepted.
