module Example.RustFFI.Main where

import Prelude

import Data.Array (sort)
import Effect (Effect)
import Effect.Console (log)
import Lib (rand, shuffle)

-- The output is PROPERTIES of the random results, not the results themselves, so it is
-- deterministic across runs and backends — the example stays diffable in the sweep even
-- though the JS and Rust generators can never be seeded identically.
main :: Effect Unit
main = do
  n <- rand 100
  log ("rand 100 is in 1..100     = " <> show (n >= 1 && n <= 100))
  xs <- shuffle [ 1, 2, 3, 4, 5, 6, 7 ]
  log ("shuffle preserves elements = " <> show (sort xs == [ 1, 2, 3, 4, 5, 6, 7 ]))
