-- | The **first-order, byte-level** `String` primitives the higher-level `Data.String.*`
-- | code-point operations are built on. A `String` is a UTF-8 byte array; these expose
-- | it as bytes — the Rust `.as_bytes()` / `.bytes()` analog — so the code-point layer
-- | (`length` / `charAt` / `take` / …, written as PureScript that decodes UTF-8) can run
-- | standalone on purvasm and be specialized by the optimizer (ADR-0027 / ADR-0028).
-- |
-- | "Code unit = byte" lives here, deliberately NOT in `Data.String.CodeUnits` (which adopts
-- | code-point semantics on this backend; ADR-0006). The purvasm backend resolves the
-- | `foreign import`s to its byte-level `String` primitives; the accompanying `String.js` provides
-- | them for stock `purs` / `purs-backend-es` so a `purvasm-base`-using project also compiles and
-- | runs on the JS backends (there a "byte" is a UTF-16 code unit — the JS-side encoding — which is
-- | correct for JS-hosted code).
module Purvasm.String
  ( byteLength
  , byteAt
  , unsafeNew
  , unsafeSetByte
  , byteSlice
  , dropCodePoints
  , takeCodePoints
  , codePointLength
  , codePointAt
  , byteIndexOf
  , byteLastIndexOf
  , compareBytes
  , appendBulk
  , materialize
  ) where

-- | The UTF-8 byte length. On the purvasm backend: a byte-array length primitive.
foreign import byteLength :: String -> Int

-- | The byte (0-255) at index `i`, **unchecked** (out of range traps). On the purvasm backend: a
-- | byte-array index primitive.
foreign import byteAt :: String -> Int -> Int

-- | Allocate a `String` of `n` **zeroed** bytes, to be filled with `unsafeSetByte`. On the purvasm
-- | backend: a zeroed byte-array allocation primitive.
foreign import unsafeNew :: Int -> String

-- | Write byte `b` (0-255) at index `i`, **mutating the string in place** (O(1) on the native
-- | backend, ADR-0052), and return that same string, so a builder loop threads it — keeping the
-- | write live (not dead-code-eliminated) and ordered by the data dependency, without needing an
-- | effect. Unchecked (out of range traps).
-- |
-- | Contract (the *linear unsafe-build protocol* — the caller MUST uphold these; the native
-- | implementation mutates in place and relies on them, like `Purvasm.Array.unsafeSet`,
-- | ADR-0009/0019):
-- |
-- |   * the buffer MUST be a fresh allocation from `unsafeNew` (never a shared or literal `String`);
-- |   * it MUST be threaded **linearly** — pass the *returned* string to the next write; never reuse
-- |     a reference to the buffer taken before a write, and do not retain it after it is published as
-- |     an immutable `String`;
-- |   * when copying bytes between two strings, source and destination MUST be distinct (no `src = dst`
-- |     overlapping copy).
-- |
-- | Violating any of these mutates a value other callers observe (silent corruption). All current
-- | `ulib` builders honour it (`Data.String.Common.blit`, `Data.String.Internal.Utf8` `putCp`/copy,
-- | `Data.Semigroup` `append`, `Data.Int.byteString`, `Data.Show` `put1`..`put4`).
foreign import unsafeSetByte :: String -> Int -> Int -> String

-- | The `[from, to)` byte range as a String value (ADR-0103 §5). On the purvasm backend this is
-- | the bounded-retention slice builder — a zero-copy view when the backing is at most 4× the
-- | result, a compact copy otherwise — release-validated (in range, both endpoints on UTF-8
-- | code-point boundaries). On JS a "byte" is a UTF-16 code unit and this is a plain `slice`.
foreign import byteSlice :: Int -> Int -> String -> String

-- | Drop the first `k` Unicode code points (`k` clamped to `[0, length]`) — the lexer-cursor
-- | primitive: O(consumed), never O(remaining) (ADR-0103 §1/§3).
foreign import dropCodePoints :: Int -> String -> String

-- | Keep the first `k` Unicode code points (`k` clamped).
foreign import takeCodePoints :: Int -> String -> String

-- | The Unicode code-point count. O(1) when the value carries a known count or after the first
-- | memoised demand; a first demand walks only the value's own bytes (ADR-0103 §2).
foreign import codePointLength :: String -> Int

-- | The `i`-th Unicode code point, or `-1` when `i` is out of range.
foreign import codePointAt :: Int -> String -> Int

-- | The first byte offset `>= from` at which the needle's bytes occur in the hay, or `-1`
-- | (hay → needle → from; an empty needle matches at any in-range `from`). UTF-8 is
-- | self-synchronising, so a valid needle's match always lands on a code-point boundary.
foreign import byteIndexOf :: String -> String -> Int -> Int

-- | The last byte offset `<= from` at which the needle occurs, or `-1`.
foreign import byteLastIndexOf :: String -> String -> Int -> Int

-- | Byte-lexicographic order: `-1`/`0`/`1` (equals code-point order under UTF-8). Borrowed in
-- | place on the purvasm backend — no copy-out (ADR-0103 §4).
foreign import compareBytes :: String -> String -> Int

-- | Concatenation as one bulk operation (the `Semigroup String` implementation's primitive).
foreign import appendBulk :: String -> String -> String

-- | Sever ownership below the bounded-retention threshold: compact a view to its own packed
-- | bytes (identity on an already-packed String). Use at boundaries that must not retain a
-- | larger backing (ADR-0103 §1).
foreign import materialize :: String -> String
