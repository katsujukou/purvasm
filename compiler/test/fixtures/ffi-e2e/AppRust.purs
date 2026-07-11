-- | app-Rust FFI fixture (ADR-0091 §Addendum): a `foreign import` whose provider is a `#[pv_foreign]`
-- | leaf in the sibling `rust-crate`, bundled with the runtime rlib and linked. `tools/ffi-e2e.sh`
-- | builds this with `--rust-ffi` and asserts the result (`addSeven 35 == 42`).
module AppRust where

foreign import addSeven :: Int -> Int

answer :: Int
answer = addSeven 35
