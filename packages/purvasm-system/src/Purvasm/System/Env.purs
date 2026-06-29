-- | Environment access — the host-system rung (ADR-0022/0056), the OCaml `Sys.getenv` / Node
-- | `process.env` role. The first-order leaf yields `""` for an unset variable (the boundary
-- | cannot carry `Maybe`, ADR-0020/0036); `lookupEnv` folds that to `Nothing` in PureScript.
-- | On purvasm, boot binds the leaf to a host leaf; the `.js` is the Node impl for the dual-target.
module Purvasm.System.Env
  ( lookupEnv
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | Read an environment variable; `""` when unset. Not exported — wrapped by `lookupEnv`.
foreign import getenvImpl :: String -> Effect String

-- | Read an environment variable; `Nothing` when it is unset (or empty).
lookupEnv :: String -> Effect (Maybe String)
lookupEnv name = do
  val <- getenvImpl name
  pure (if val == "" then Nothing else Just val)
