-- | Shared plumbing for the wall-clock benchmark corpus (ADR-0075 §4): every benchmark reads its
-- | input size from argv (element 0 is the executable/image/script on every backend — the
-- | drop-one convention), so one built artifact serves the whole size sweep.
module Bench.Common (sizeArg) where

import Prelude

import Data.Array (drop, head)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Purvasm.System.Process (argv)

-- | The benchmark input size: the first user argument, else a small smoke default.
sizeArg :: Effect Int
sizeArg = do
  as <- argv
  pure (fromMaybe 1000 (head (drop 1 as) >>= Int.fromString))
