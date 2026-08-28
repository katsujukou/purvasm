-- | A program that fails, for the launcher gate: what a runner does with a program's *own* failure is
-- | a contract of its own — the command must not report success, and the program's output must reach
-- | the terminal on its way out.
module VMGate.Fails where

import Prelude

import Effect (Effect)
import Effect.Exception (error, throwException)

main :: Effect Unit
main = throwException (error "VMGate.Fails: failing on purpose")
