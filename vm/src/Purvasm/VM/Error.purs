-- | How the VM stops when the guest program reaches a state its semantics has no answer for.
-- |
-- | "Stuck" is the oracle's word (ADR-0016): not a VM defect but a program that asked for something
-- | ill-typed, out of range, or genuinely undefined — a non-function applied, a wrong-kind
-- | discriminant, a self-forcing recursive value. It aborts the run, exactly as boot's `Vm_error`
-- | does, so the two interpreters agree on *which* programs have a value.
module Purvasm.VM.Error
  ( stuck
  ) where

import Prelude

import Effect (Effect)
import Effect.Exception as Exception

-- | Abort the run with a diagnostic. The prefix names the component, since a stuck guest and a
-- | crashed host are very different bug reports.
stuck :: forall a. String -> Effect a
stuck message = Exception.throw ("purvasm vm: " <> message)
