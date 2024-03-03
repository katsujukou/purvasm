module Purvasm.LCore.Foreign where

import Data.Maybe (Maybe(..))
import Purvasm.Global (GlobalEnv)
import Purvasm.LCore.Syntax (LCore)
import Purvasm.Types (GlobalName)

overrideForeign :: GlobalEnv -> GlobalName -> Maybe LCore
overrideForeign _ _ = Nothing