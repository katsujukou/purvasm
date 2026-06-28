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
