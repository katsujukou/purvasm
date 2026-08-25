-- | What the guest sees as its own command line (ADR-0075 §4): element 0 the image, then the
-- | arguments the runner was asked to pass on. Printed joined and whole, because the benchmark corpus
-- | only ever reads element 1 — a gate that did the same would not notice the image path being wrong.
module VMGate.Argv where

import Prelude

import Data.String.Common (joinWith)
import Effect (Effect)
import Purvasm.Stdio (writeLine)
import Purvasm.System.Process (argv)

main :: Effect Unit
main = argv >>= (writeLine <<< joinWith "|")
