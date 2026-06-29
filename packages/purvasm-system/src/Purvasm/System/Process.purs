-- | Process information & control — the host-system rung (ADR-0022/0056), the OCaml `Sys`/`exit` /
-- | Node `process` role. On purvasm, boot binds each leaf to a host leaf; the `.js` is the Node impl
-- | for the dual-target.
module Purvasm.System.Process
  ( argv
  , exit
  ) where

import Prelude

import Effect (Effect)

-- | The process arguments, element 0 the executable. Not exported — wrapped by `argv`.
foreign import argvImpl :: Effect (Array String)

-- | Terminate the process with an exit code. Not exported — wrapped by `exit`.
foreign import exitImpl :: Int -> Effect Unit

-- | The process argument vector. Element 0 is the executable (native `Sys.argv`); on Node the
-- | runtime (`"node"`) is dropped so element 0 is the script — so a consumer drops one element
-- | uniformly to get the user arguments.
argv :: Effect (Array String)
argv = argvImpl

-- | Terminate the process with the given exit code (0 = success). Does not return.
exit :: Int -> Effect Unit
exit = exitImpl
