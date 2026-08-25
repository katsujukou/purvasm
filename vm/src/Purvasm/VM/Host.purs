-- | The host-control surface: how the VM configures the runtime **for the guest it is about to run**
-- | ([ADR-0110](../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §4(a) Correction).
-- |
-- | Distinct from [Purvasm.VM.Loader](Loader.purs), which is about the *foreign frontier* — who
-- | provides a leaf. Nothing here provides anything; these set the execution context a leaf reads
-- | from, and the entries live in `purvasm_host.h` rather than `purvasm.h` precisely so a guest
-- | cannot reach them: they are absent from the export allowlist, so no `dlopen`ed provider can bind
-- | them and no guest `ForeignRef` can name them.
module Purvasm.VM.Host
  ( setGuestArgv
  ) where

import Prelude

import Effect (Effect)

-- | Give the guest its own argv (ADR-0075 §4): element 0 the image, then the arguments meant for the
-- | program. Must be called before the guest runs.
-- |
-- | Without it the guest reads the argv of the **process**, which is the VM's own command line — so a
-- | program taking its input from `argv` would see `--image` where its first argument belongs. That is
-- | not a provider question: `Purvasm.System.Process.argvImpl` still has exactly one provider,
-- | `host-runtime` (ADR-0111 §4 is untouched); what changes is the context that provider reads.
setGuestArgv :: Array String -> Effect Unit
setGuestArgv = setGuestArgvImpl

foreign import setGuestArgvImpl :: Array String -> Effect Unit
