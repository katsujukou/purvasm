-- | A user-package native foreign written in Rust (ADR-0078): the native implementation is
-- | the `#[pv_foreign]` crate under `foreign/` (using the crates.io `rand` crate — the point
-- | of choosing Rust); `Lib.js` serves the stock JS backend (dual-target, ADR-0038). The
-- | build wires the crate in via this example's `ulib.json`
-- | (`purvm native --backend llvm --native-foreign examples/rust-ffi`).
-- |
-- | Randomness is a genuine effect, so the foreigns are `Effect`-typed: the Rust side marks
-- | them `effect` (the ADR-0067 thunk shape), the JS side is the usual curried thunk.
module Lib (rand, shuffle) where

import Effect (Effect)

-- | A random `Int` in `1..max`.
foreign import rand :: Int -> Effect Int

-- | A uniformly shuffled copy of the array.
foreign import shuffle :: Array Int -> Effect (Array Int)
