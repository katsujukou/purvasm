# 0014. CoreFn ingestion: decode `corefn.json` to a faithful `Corefn` AST

- Status: Accepted
- Date: 2026-06-18

## Context

The CESK core is now a complete executable spec for the constructs CoreFn emits
([0002](0002-cesk-execution-model.md)–[0013](0013-case-guards.md)), but nothing
yet *feeds* it real PureScript: programs are still hand-built core terms. The
CoreFn frontend closes that gap. It splits naturally into two decisions:

1. **Ingestion** — read `purs`'s `corefn.json` into an in-memory representation.
2. **Lowering** — translate that representation into `Cesk.Ast.term`, resolving
   names and the FFI-ident → primitive table.

This record settles **ingestion only**. Lowering — the part with the real
semantic choices (name resolution, the primitive table deferred by
[0007](0007-monomorphic-primitives.md) §3, constructor arity, `Char`→`Int`) — is
a separate record (ADR-0015 — to follow), because parsing JSON and assigning
meaning to it are genuinely different concerns (single responsibility).

`purs compile --codegen corefn` writes one `output/<Module>/corefn.json` per
module. It is a deeply nested, **internally tagged** JSON: every expression node
is an object with a `type` field (`"App"`, `"Var"`, `"Literal"`, …), every binder
an object with a `binderType` field, every literal a `{ "literalType", "value" }`
pair, each wrapped in an `annotation`.

**The shape we decode is taken from a verified reference, not reverse-engineered.**
`~/Projects/purescript-backend-wasm/compiler/src/PureScript/CoreFn.purs` is a
PureScript CoreFn AST "verified against compiler 0.15.16"; it is the source of
truth for the types this record settles. That definition is part of
**purescript-backend-wasm**, and follows the CoreFn model used by
**purescript-backend-optimizer** — i.e. we align with an established, in-use
definition rather than inventing one. (A local `purs` 0.15.16 dump was used only
to corroborate it, not to derive it.)

It is based on the CoreFn that **PureScript v0.15.16** emits at the time of
writing. Older compilers are expected to be compatible — CoreFn's shape changes
rarely — but the exact lowest supported version has not been established; pinning a
lower bound would need separate investigation.

## Decision

### Dependency: `yojson`, hand-written decoder, no ppx

1. **Add `yojson` to the toolchain** (`flake.nix` devShell + `dune-project`
   depends). It parses `corefn.json` into a generic JSON tree; `purs` (already in
   the devShell) generates that JSON. Parse target is **`Yojson.Safe`** (its
   number handling is a superset of `Basic`, so the decoder need not care which
   numeric subtype the parser picked for a `LitNumber`).

2. **No ppx deriver** (`ppx_deriving_yojson` / `ppx_yojson_conv`). CoreFn's
   encoding is **internally tagged** — `{ "type": "App", "abstraction": …,
   "argument": … }` — whereas every ppx deriver emits/expects a positional array
   (`["App", …]`) or an externally-tagged object (`{ "App": … }`). Matching
   CoreFn's convention through a deriver would need per-constructor and per-field
   attributes throughout *and* tag plumbing the derivers do not support. A focused
   **hand-written decoder** over the fixed set of nodes is clearer, has no
   attribute noise, and naturally drops `annotation` spans and splits `Qualified`
   names. Ingestion is therefore plain `yojson` + a `Corefn.Decode` module.

3. **The decoder is the validating boundary.** It reads untyped JSON and either
   produces a well-typed `Corefn` value or raises `Decode_error of string`
   (distinct from the machine's `stuck`). Because it validates shape, the produced
   AST conforms to its types and needs no `unsafe`-prefixed escape (per the
   `CLAUDE.md` convention); a malformed or unsupported node fails **loudly** at
   decode time, not later.

### A faithful `Corefn` AST (conforming to the reference)

Ingestion decodes into a dedicated `Corefn` subsystem whose types **conform to
the verified `CoreFn.purs`** above — same constructors, same field names — so the
IR is auditable against a known-good source. OCaml convention adapts only the
surface (module/constructor names Capitalized, type names lowercase `t`; `end`
becomes `end_`); the words are the reference's.

The exact constructors and field names are the reference's; **this record does
not reproduce them** — a copy here would only drift from the code, and the
reference is already the spec. The subsystem is split by concern, one module
each: `Corefn.Names` (`module_name`/`ident`/`proper_name`/`qualified`),
`Corefn.Ann` (`source_span`/`constructor_type`/`meta`/`ann`), `Corefn.Literal`
(the polymorphic `'a literal`, reused for expression and binder literals),
`Corefn.Expr` (`expr`/`bind`/`rec_binding`/`guard`/`case_alternative`/`binder`),
and `Corefn.Module` (`import` and the module record).

Two choices here are ours, not mere transcription:

- **Decode the full `Module` record** (`name`/`path`/`builtWith`/`imports`/
  `exports`/`reExports`/`foreignNames`/`decls`), including fields lowering ignores
  at first — faithful and cheap; multi-module concerns are ADR-0015's.
- **`case_alternative.result` stays an `Either`** of a guard list (`Left`) or a
  single expression (`Right`), mapping directly onto ADR-0013's
  `Guarded`/`Unconditional` at lowering.

The JSON's `literalType` tags map onto the constructors (`"IntLiteral" → LitInt`,
`"NumberLiteral" → LitNumber`, …); the node `type`/`binderType` tags map onto
`expr`/`binder` constructors. A `Var`'s `Qualified` keeps only the optional module
name — the binding-site `sourcePos` the JSON carries is dropped, exactly as the
reference notes. `Ann` (span + meta) is **retained**, following the reference:
`Meta` in particular (`IsConstructor`, `IsNewtype`, …) is information lowering may
use, and the span keeps a future door open for diagnostics. The JSON `annotation`
also carries **comments**; these are intentionally dropped (as the reference
does) — they have no bearing on execution and nothing needs them yet, and like
the span they can be re-added if a tool later wants them.

### Scope of this slice

- Decode **one module's `corefn.json`** into a `Corefn.t`, the full record above.
  Multi-module linking is a lowering concern (ADR-0015); ingestion records
  `imports`/`re_exports`/`foreign_names` as data.
- Support **every** `expr`/`binder`/`literal`/`bind` form in the reference — they
  all correspond to constructs the core already evaluates (or that lowering maps),
  so there is no reason to decode a partial set.

## Consequences

- **Conformance over guesswork.** The IR is a transcription of a verified
  reference, so "does this match CoreFn?" is answered by comparing to one file,
  not by trusting an empirical sample. Fidelity risk drops accordingly.
- **One small dependency.** `yojson` is the only new library; `purs` (generation)
  is already provisioned. No ppx, so no derive-attribute coupling and no syntax
  extension in the build.
- **Parsing and meaning are separated.** `Corefn` (this record) is a pure data
  mirror of CoreFn; the semantic mapping lives entirely in lowering (ADR-0015). A
  decoded `Corefn.t` can be checked structurally without running anything.
- **The IR reads like the compiler.** Faithful names make a reviewer's PureScript
  knowledge transfer directly, and make the lowering's faithfulness a local check.
- **`Ann`/`Meta` are kept**, so the door is open to meta-driven lowering (newtype
  erasure, constructor fast paths) and to source-mapped diagnostics later, at the
  cost of a little extra decode work — a cost the reference already pays.
- **Loud failure on the unsupported.** Anything outside the decoded shape (or a
  malformed file) is a `Decode_error`, never a silent partial parse — the same
  surface-problems-early stance the machine takes with `stuck`.
- **Sets up lowering.** With `decls` as `bind` (Rec/NonRec), `Constructor` with
  `field_names`, `Var` as `qualified`, and `case_alternative.result` as
  `Either (guard list) expr`, ADR-0015 has exactly what it needs to map to
  `Letrec`/`Let`, `Ctor(name, arity)`, resolved references, and 0013's
  `Guarded`/`Unconditional`.

## Alternatives considered

- **Reverse-engineer the AST from `purs` JSON dumps (empirical + experience).**
  Rejected in favour of conforming to the verified `CoreFn.purs` reference: a
  working, version-checked type definition is more reliable than inferring shapes
  from samples, and keeps the IR auditable against a single source. (Local dumps
  are still useful to *corroborate* the reference, not to replace it.)
- **ppx deriver (`ppx_deriving_yojson` / `ppx_yojson_conv`).** Rejected: CoreFn's
  internal-tag-plus-named-fields encoding matches no deriver's convention, so a
  deriver needs pervasive attributes and still cannot express the
  `type`/`binderType`/`literalType` discriminators cleanly. A hand decoder over a
  known, fixed set is less code and far clearer.
- **Decode straight into `Cesk.Ast.term`, skipping a `Corefn` IR.** Rejected: it
  fuses JSON shape and language semantics into one pass, so a decode bug and a
  lowering bug become indistinguishable and the real lowering decisions (name
  resolution, prim table) get buried in JSON plumbing. The faithful IR is a
  stable, inspectable seam.
- **Consume PureScript's externs instead of CoreFn.** Rejected for now:
  `corefn.json` is the type-erased, already-desugared program the machine runs;
  externs carry type/class information needed only for *optimised* dictionary
  layout ([0010](0010-record-row-polymorphic.md) alternatives), which is out of
  scope. Ingestion targets CoreFn alone.
- **Drop `Ann`/`Meta` to slim the decoder.** Rejected: the reference keeps them,
  `Meta` is plausibly useful to lowering, and the decode cost is small; omitting
  them would diverge from the source of truth for no real gain.
- **`Yojson.Basic` instead of `Safe`.** Acceptable and nearly equivalent here;
  `Safe` is chosen only so the decoder never has to care which numeric subtype the
  parser produced. Not a load-bearing choice.
