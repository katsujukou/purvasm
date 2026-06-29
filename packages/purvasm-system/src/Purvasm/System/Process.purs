-- | Process information — the host-system rung (ADR-0022/0056), the OCaml `Sys.argv` / Node
-- | `process.argv` role. On purvasm, boot binds the leaf to a host leaf; the `.js` is the Node impl
-- | for the dual-target.
module Purvasm.System.Process
  ( argv
  ) where

import Effect (Effect)

-- | The process arguments, element 0 the executable. Not exported — wrapped by `argv`.
foreign import argvImpl :: Effect (Array String)

-- | The process argument vector. Element 0 is the executable (native `Sys.argv`); on Node the
-- | runtime (`"node"`) is dropped so element 0 is the script — so a consumer drops one element
-- | uniformly to get the user arguments.
argv :: Effect (Array String)
argv = argvImpl
