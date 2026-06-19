# 0021. Link only the entry's reachable closure (dead-binding elimination)

- Status: Accepted
- Date: 2026-06-19

## Context

Linking (ADR-0016) loads an entry module's whole transitive *import* closure and
lowers **every** declaration of **every** module into one `Let`/`Letrec` chain,
which is then evaluated. Because the machine is strict, evaluating the chain
forces every top-level binding — including instance-dictionary CAFs for instances
the program never uses.

That eager forcing collides with incremental FFI coverage (ADR-0017, ADR-0020).
The `map (add 1) [1,2,3]` end-to-end program pulls `Data.Functor`, and through it
`Data.Function → Data.Ord → Data.Ordering → Data.Show`. The program never calls
`show`, yet linking forces `Data.Show`'s scalar instance CAFs (`showInt`,
`showNumber`, `showChar`, `showString`), whose fields reference the still
unimplemented foreign leaves `showStringImpl` et al. — so loading the program is
`stuck` on a leaf it would never actually evaluate. The same shape recurs for any
module dragged in by the import graph but unused by the entry: `Data.Semigroup`'s
`concatArray`, `Data.Ord`'s `ord*Impl`, and so on.

The import closure is the wrong unit. What the program actually needs is the set
of *declarations* reachable from the entry point — a strictly smaller set than
"every declaration of every imported module".

## Decision

Link only the entry point's **reachable binding closure**. Concretely:

- Lower each module's declarations to per-binding `(qualified key, term)` pairs,
  retaining their recursive-group structure (a `Rec` group stays a `Letrec`).
- Treat the resolver-provided foreign/builtin bindings as additional named
  bindings with their own free variables (a structural leaf such as `arrayMap`
  refers to nothing external; a future one might).
- Compute the set of keys transitively reachable from the entry key by following
  each reached binding's free variables (`free_vars`, already used for foreign
  resolution) across modules and into resolved foreign bindings.
- Emit `Let`/`Letrec` bindings only for reachable keys, preserving dependency
  (topological) order so a binding is introduced before its users. Unreachable
  bindings are dropped entirely and never forced.

This is the dead-code/tree-shaking step every real PureScript backend performs
(`purescript-backend-optimizer` does aggressive DCE); we apply it at the linker
rather than carry dead bindings into evaluation.

## Consequences

- The `Prelude` milestone programs (`answer`, `div`, `eq`, `both`, `doubled`)
  link and run against the *current* FFI bundle: only the leaves actually reached
  — `intAdd`, `intDiv`, `eqIntImpl`, `boolConj`/`boolNot`, `arrayMap` — need a
  resolution. `show*Impl`, `ord*Impl`, `concatArray` are unreachable and so are
  neither linked nor forced. FFI coverage can grow strictly with the programs we
  actually run, instead of with everything their import graph happens to touch.
- Eager initialisation is unchanged for the bindings that remain: this is a
  *which bindings* decision, not a *when to force* one. No new laziness, no
  thunks, no machine change — only `link` changes.
- Faithfulness: a JS backend runs every imported module's top level at import
  time, so DCE is observable only for bindings with load-time *effects*. purvasm
  models top-level bindings as pure values (the `Prelude` has no load-time
  effects), so dropping unreachable pure bindings is observationally safe. If a
  later stage admits genuine load-time effects, those modules' effectful
  initialisers must be treated as roots; revisit then.
- Diagnostics improve: a `stuck` on an unbound foreign now means the program
  *actually reaches* it, not that its import graph brushed against it.

## Alternatives considered

- **Lazy top-level CAF initialisation** — make each binding a memoised thunk
  forced on first reference, so unused dicts never force their leaves. Solves the
  same problem but changes evaluation semantics and needs machine support
  (thunks, black-holing for recursive groups), a larger and more invasive step
  than pruning dead bindings. Reachability DCE subsumes the motivating cases
  without touching the machine.
- **A "not implemented" stub rung** — have the ladder bind known-but-unimplemented
  leaves to a closure that is `stuck` only when applied. Keeps eager whole-closure
  linking but is a band-aid: it carries dead bindings into evaluation and must
  maintain a stub list. DCE removes the need entirely.
- **Implement the `show` batch now** — real `showInt`/`showNumber`/`showChar`/
  `showString` over a new string-primitive layer. Needed eventually for programs
  that *use* `show`, but irrelevant to programs that merely import a module that
  defines it; it should not be a prerequisite for `map`.
