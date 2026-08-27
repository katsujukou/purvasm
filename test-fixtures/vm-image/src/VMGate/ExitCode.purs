-- | A program that ends with a chosen status, for the launcher's exit-status contract: a runner must
-- | hand back what the program reported, not flatten every failure to 1.
module VMGate.ExitCode where

import Prelude

import Data.Array (drop, head)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Purvasm.System.Process (argv, exit)

main :: Effect Unit
main = do
  as <- argv
  void (exit (fromMaybe 0 (head (drop 1 as) >>= Int.fromString)))
