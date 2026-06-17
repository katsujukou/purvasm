# 0006. String as a UTF-8 byte sequence; Char as Int

- Status: Accepted
- Date: 2026-06-17

## Context

PureScript's `String` is, by specification, a sequence of **UTF-16 code units**,
and `Char` is a single UTF-16 code unit (BMP only). That choice is inherited
directly from the JavaScript runtime, where strings are UTF-16. purvasm no
longer targets JS — it owns its value representation across all three phases — so
there is no reason to carry the UTF-16 baggage (surrogate pairs, code units that
are not code points, `length` counting halves of astral characters).

This record fixes how purvasm represents strings and characters.

## Decision

1. **`String` is a UTF-8 byte sequence.** The runtime representation is the host
   OCaml immutable `string` (itself a byte sequence). Add `Value.VString of
   string` and `Ast.LString of string`. A string literal is stored as the UTF-8
   bytes of its source text, with no transcoding.

2. **`Char` is `Int` (a Unicode code point).** There is no separate `VChar` /
   `LChar`; a character is carried as `VInt` / `LInt` holding its code point.
   CoreFn's char literals lower to integer literals. Because CoreFn is already
   type-erased, the runtime never needs to tell `Char` from `Int`.

3. **String ordering is byte-wise.** `Eq` / `Ord` (and any comparison primitive)
   compare the underlying bytes directly. By the design of UTF-8, byte
   lexicographic order equals code-point order equals the natural string order,
   so no normalization or decoding is needed to compare.

4. **Character-indexed APIs are specified in code points, not bytes.** Operations
   like `length`, `charAt`, `codePointAt`, `take`/`drop` are defined to count
   **code points**; the UTF-8 ↔ code-point conversion lives inside those
   operations (the future `Data.String` FFI), never in the core value, which
   stays raw bytes. The phase-1a core — string literals, concatenation, and
   comparison — does not depend on this unit choice and so does not implement any
   of these yet.

### Surface / value additions (phase-1a scope)

- `Ast.lit`: add `LString of string`. (No `LChar`; char literals are `LInt`.)
- `Value.t`: add `VString of string`.
- `Prim`: make `Eq` / `Lt` work on two `VString`s (byte-wise), and add a string
  concatenation primitive (`Append`). These are the minimum that make a string a
  first-class, evaluable, comparable value; richer operations wait for the FFI.

## Consequences

- **Zero-cost literals and host reuse.** Source UTF-8 bytes become the value
  verbatim; the OCaml `string` *is* the representation. No transcoding layer.
- **Trivial ordering.** Byte order already is code-point order, so `Ord` needs no
  special logic — a direct consequence of choosing UTF-8 over UTF-16.
- **Divergence from `Data.String.CodeUnits`.** PureScript's code-unit API assumes
  UTF-16; under purvasm the "code unit" is a UTF-8 byte, which is not a useful
  character index. The `Data.String` FFI will therefore be implemented with
  **code-point** semantics (closer to `Data.String.CodePoints`); reconciling
  libraries written against CodeUnits is a frontend/FFI concern, flagged here and
  resolved when the FFI lands.
- **`Char` conveniences are identities.** `Data.Char.toCharCode` / `fromCharCode`
  and the `Enum`/`BoundedEnum` instances become identity or trivial, since `Char`
  *is* its code point.
- **A code-point index is O(n) over UTF-8.** Random character access requires
  scanning from the start (or an auxiliary index). Acceptable: string-heavy
  indexing is rare, and a rope / index structure can be added later behind the
  same `VString` interface if it ever matters.

## Alternatives considered

- **Mirror PureScript: UTF-16 code units.** Rejected. It re-imports the exact JS
  coupling we are now free of — surrogate pairs, code units that are not
  characters, astral `length` surprises — for the sole benefit of bit-for-bit
  `Data.String.CodeUnits` compatibility, which type-erased CoreFn does not need.
- **String as an array of code points (`int array`).** O(1) character indexing,
  but 4× memory, loses host-`string` reuse, and forces transcoding of every
  literal. UTF-8 bytes are compact and host-native; defer fast indexing to an
  optional later structure rather than paying for it everywhere.
- **A dedicated `VChar` value.** Type-safe in isolation, but CoreFn has already
  erased the `Char`/`Int` distinction, so a separate runtime tag buys nothing;
  folding `Char` into `Int` is simpler and loses no observable behavior.
