-- | `purvasm-base` `Purvasm.Char` (ADR-0038 / ADR-0006): the `Char` ↔ code-point `Int` conversions
-- | the `Data.String.*` UTF-8 codec is written over. A `Char` is a Unicode **code point** on this
-- | backend (ADR-0006 — not a UTF-16 code unit), represented as an `i32`, so both directions are the
-- | **identity** on that `i32`; the purvasm backend resolves them to the identity (a no-op).
-- | Per ADR-0038, `Purvasm.*` is first-order only — no HOFs, no `Prelude`. All types are `Prim`
-- | (`Char` / `Int`).
-- |
-- | The accompanying `Char.js` provides them for stock `purs` / `purs-backend-es` (there a `Char` is
-- | a JS UTF-16 unit, so the JS impls use `charCodeAt` / `fromCharCode`).
module Purvasm.Char
  ( toCodePoint
  , fromCodePoint
  ) where

-- | The `Char`'s Unicode code point. On the purvasm backend: the identity.
foreign import toCodePoint :: Char -> Int

-- | The `Char` for a code point, **unchecked** (the caller guarantees a valid code point; the
-- | UTF-8 decoder only ever produces valid ones). On the purvasm backend: the identity.
foreign import fromCodePoint :: Int -> Char
