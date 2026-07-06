# 0081. `purvasm-regex`: an ES-flavour regex engine in pure PureScript, with `Data.String.Regex` as a `ulib` shadow

- Status: ~~Proposed~~ **Accepted** _(2026-07-06: accepted by the maintainer)_
- Date: 2026-07-06

## Context

The [0080](0080-foreign-signature-reconstruction-cst.md) arc embeds `language-cst-parser` in the
Level-2 compiler, and its Lexer is a `Data.String.Regex` consumer — the demand (ADR-0038's
demand-driven rule) is concrete and bounded: **fourteen fixed patterns in one module**
(`PureScript.CST.Lexer`), all compiled as `unsafeRegex ("^(?:" <> p <> ")") unicode` and consumed
through `Regex.match` only. Their union fixes the feature floor exactly:

- character classes (ranges, negation), alternation, greedy quantifiers (`*`, `+`, `?`,
  `{m,n}`), capturing and non-capturing groups, `^`/`$`;
- **negative lookahead** (`(?!…)` — the block-comment and symbol patterns);
- **Unicode property escapes under the `u` flag, general categories only**: `\p{Lu}`, `\p{Ll}`,
  `\p{L}`, `\p{P}`, `\p{S}` — five categories, nothing more.

NOT demanded: lazy quantifiers, lookbehind, backreferences, named groups, `g`/`y` flags,
`replace`/`split`/`search`/`test`.

A C shadow fails twice. Semantically: `Data.String.Regex`'s contract is **ECMAScript** regex;
POSIX `regex.h` is a different language (no lookahead, no `\d`-class semantics), PCRE2 is
close-but-not-equal AND a system dependency (breaking the `.c`-retargets-anywhere property,
[0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §2); the only true ES engine
in C is QuickJS's libregexp — a ~15k-line vendoring. Structurally, and decisively: **a `.c`
foreign exists only on the LLVM backend**, while a `ulib` shadow must run on VM, OCaml-native,
LLVM, and JS alike (the differential and the examples sweep demand it) — a C engine would force a
third, OCaml host implementation with unattainable semantic alignment. (Rust is excluded by the
[0078](0078-rust-foreign-bindgen-over-c-abi.md) Policy: ulib ships no Rust.)

This is the [0046](0046-argonaut-core-pure-purescript-ulib.md) JSON situation again, and the same
answer applies: **a backend-agnostic pure-PureScript core, with the registry package as a thin
shadow over it.**

## Decision

### 1. `packages/purvasm-regex` — the engine, pure PureScript

An in-repo package (namespace `Regex.Core.*`, third source category per 0046): a pattern parser
producing a small regex AST, and a backtracking matcher over `Purvasm.String` primitives
(byte-oriented scanning where safe, code-point semantics where the `u` flag requires — the
[0054](0054-byte-oriented-json-parser.md) discipline). Scope v1 = exactly the feature floor above,
implemented completely; everything outside it is a **structured parse error at `regex`-construction
time** (`Either String Regex` — never a silently-wrong match).

Unicode general categories ship as a **generated pure-PS range-table module** (from
`UnicodeData.txt`, the five demanded categories; generation script checked in, output committed —
no build-time network). This is the bulk of the data cost and is shared by any future category.

### 2. `ulib` shadow of `Data.String.Regex` (+ `.Flags`, `.Unsafe`)

`Regex` becomes an ADT over the core (pattern AST + flags), constructors unexported — interface
compatible with the upstream `foreign import data Regex`. Instance declarations (e.g.
`Show Regex`) are preserved **exactly** (instance contexts are frozen ABI). v1 implements
`regex`, `unsafeRegex`, `source`, `flags`, `test`, `match`; `replace`/`replace'`/`search`/`split`
raise the structured unimplemented error until demanded (the ledger records them).

### 3. Gates

The [0040](0040-ulib-testing-strategy.md)/[0048](0048-ulib-tools-test-upstream-suite-execution.md)
mechanism as-is: the `strings` upstream suite with an **xfail ledger** for the out-of-floor
corners, plus a bespoke suite pinning the fourteen Lexer patterns against captured JS-`RegExp`
oracle outputs (the representation-divergent seam of 0040 — regex semantics — tested natively
under the differential). The CST-parser port itself is the end-to-end gate: its lexer output on
the compiler's own sources must match the JS run byte for byte.

> **Progress (2026-07-06):** core + shadow + consumer gate landed. `packages/purvasm-regex`
> (15/15 units, several expectations pinned against a live `node` `RegExp` oracle), the
> `ulib/strings` shadow (staged; the 0047 cycle it tripped was resolved by `Regex.Core.Utf8`
> over the four `purvasm-base` primitives — dual-mode, since the base's JS provider speaks
> UTF-16 units), and `examples/regex-demo` — sweep `OK(+js)`: byte-identical output on
> VM/OCaml/LLVM **and** against the real JS `RegExp`. The matcher is byte-oriented per §1
> (inline `cpAt` decode; no whole-input conversion) with an anchored fast path — measured:
> an anchored non-match is ~1 µs **independent of input length** (was O(|input|): 32 ms at
> 100k), so the 0080 Lexer's match-per-token usage stays linear overall.
>
> **Implemented / not implemented (the precise line as of this note):**
>
> - **Implemented (per §1, ES-semantic):** literals; `.` (non-LineTerminator); character
>   classes with ranges, negation, and `\p{…}` members; alternation (leftmost-first); greedy
>   `*` `+` `?` `{m}` `{m,}` `{m,n}`; capturing groups (ES `(`-order numbering) and `(?:…)`;
>   `^` `$` (input start/end); negative lookahead `(?!…)`; escapes `\n` `\r` `\t`, identity
>   escapes of the syntax characters (incl. `/` and `-`), and `\p{Lu} \p{Ll} \p{L} \p{P}
>   \p{S}`; flags `u` or none. API: `regex`/`unsafeRegex`/`source`/`flags`/`renderFlags`/
>   `parseFlags`/`test`/`match` (`exec` shape). Unicode tables: unidata 13.0.0 (newer-UCD
>   edge drift vs current JS engines is a known xfail class; regeneration is scripted).
> - **Rejected loudly at construction (`Left`, not wrong matches):** lazy quantifiers
>   (`*?` …); positive lookahead `(?=…)`; lookbehind and named groups (`(?<…`); class
>   escapes `\d \D \w \W \s \S` and boundaries `\b \B`; backreferences; hex/unicode escapes
>   (`\xHH`, `\uHHHH`, `\u{…}`), `\0`, `\cX`; `\p{…}` beyond the five categories (incl.
>   scripts and `\P{…}`); the flags `g` `i` `m` `s` `y`.
> - **Present but crash loudly if reached (undemanded, §2 ledger):** `replace`, `replace'`,
>   `search`, `split`.
> - **Known semantic notes:** matching is always code-point (`u`) semantic — purvasm strings
>   are code-point-native (ADR-0006), and the no-flag/`u` distinction of UTF-16 code-unit
>   matching is deliberately not reproduced; `^`/`$` never mean line boundaries (`m` is
>   rejected).
>
> **Progress (2026-07-06): §3 gate 1 — `strings` connected to the ledger machinery.**
> `ulib/strings/ulib.json` gains a `test` block at **`bespoke`** fidelity (not `js`: `strings`
> is the representation-divergent seam — ADR-0040 / ADR-0043 §5 — and the upstream `Test.Main`
> is one monolithic `Test.Assert` `Effect` that additionally exercises the undemanded
> `replace`/`replace'`/`search`/`split` and the `g`/`i`/`m`/`s` flags / named groups / `\w`,
> all outside the floor). The **xfail ledger** (seven entries) records that exact divergence
> surface, verified against the shadow's actual behaviour (each ledger corner is a construction
> `Left` or an unimplemented crash, never a wrong match). `ulib-tools test --package strings`
> now reports `skipped (bespoke, Phase 2)` — carrying the ledger — rather than *no test block*;
> its actual execution stays [0048](0048-ulib-tools-test-upstream-suite-execution.md) Phase-2
> deferred (needs `purvm` running a `Test.Assert` suite). The regex *behaviour* gate is already
> met independently: the `purvasm-regex` unit suite (node-oracle-pinned) plus `examples/regex-demo`
> in the four-backend sweep.
>
> **Progress (2026-07-06): §3 gate 2 — the full Lexer-pattern oracle suite landed.** All
> **17** demanding `PureScript.CST.Lexer` patterns (the ADR's "fourteen" was an undercount),
> each wrapped `^(?:…)` as the Lexer does, over a representative input set (53 cases:
> match / non-match / capture-exercising), pinned against a live `node` `RegExp` oracle in
> `packages/purvasm-regex/test` — including the two hardest: `symbol`
> (`(?!\p{P})\p{S}` — negative lookahead on a category) and the raw-string
> `""""{0,2}([^"]+"{1,2})*[^"]*"""`. 63/63 green (10 floor units + 53 oracle cases). The
> expectations and the pattern strings are generated by a committed `test/gen-oracle.mjs`
> (single source of truth — the committed block reproduces from it byte for byte, post-`purs-tidy`),
> so a UCD/engine change is re-pinned by one command. Together with gate 3 (the 528-module CST
> native byte-identity) this closes the regex-correctness surface; §3's only open item is the
> §3-gate-1 bespoke *execution*, which stays [0048](0048-ulib-tools-test-upstream-suite-execution.md)
> Phase-2 deferred.
>
> **Progress (2026-07-06): §3 gate 3 — the CST-parser e2e gate is MET, natively.** The
> `Data.String.Regex` shadow over this engine unblocks 0080's CST lexer end to end: the
> Level-2 CLI (which embeds `language-cst-parser`, hence this engine) **builds on the native
> LLVM backend** (previously blocked at `unbound Data.String.Regex._match`), self-compiles,
> and its **528/528 `.pmo`/`.pmi` are byte-identical to boot**. The equivalence seam is pinned
> directly on the real corpus: `foreign-sigs` dumped from the **node build (JS `RegExp`)** and
> from the **native build (this engine)** over the compiler's own 528-module closure are
> **byte-identical** — the strongest form of the §3 "match the JS run byte for byte" gate.
> A matcher `O(n²)` hot path the maintainer found and fixed cut the native channel run
> 23 min → 12.9 min; the residual is the v1 calling-convention tax (guest-stepped
> `Utf8.nextOffset`, `pv_apply` density), not the engine's logic — the node host runs the
> same engine in seconds. Rust-crate provider (the pre-engine fallback) is moot; the pure-PS
> engine is the shipped answer, zero ABI/runtime/foreign additions as §1 intended.

## Consequences

- Regex runs identically on all four backends with zero ABI/runtime/foreign additions; the 0080
  CST parser unblocks natively.
- A real engine to own (~parser + matcher + category tables); bounded by the explicit floor, and
  out-of-floor patterns fail loudly at construction, never subtly at match time.
- Backtracking performance is correct-first (0035); the Lexer's anchored-prefix patterns are
  shallow. If regex ever turns hot, an LLVM-only `.c` accelerator behind the same interface is a
  later, additive record.
- The category tables add committed generated code; regeneration is scripted and diffable.

## Alternatives considered

- **POSIX `regex.h` / PCRE2 / vendored libregexp `.c`** — rejected above: wrong semantics /
  system dependency / heavy vendoring, and all three serve only the LLVM backend, forcing a
  triple implementation across VM/OCaml/JS.
- **OCaml host leaves in boot (`Str`)** — same semantic mismatch, boot-only, and boot is frozen.
- **Full ES engine up front** (lazy quantifiers, lookbehind, named groups, full property
  escapes) — speculative beyond the demand; the floor + loud-unimplemented + ledger covers the
  gap honestly and grows on demand (0038).
- **Rewriting the Lexer to avoid regex** — a fork of upstream `language-cst-parser` in all but
  name; the ulib model patches behaviour-preserving, it does not maintain divergent copies.
