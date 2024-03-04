module Purvasm.NCore.Foreign where

import Data.Maybe (Maybe(..))
import Purvasm.Global (GlobalEnv)
import Purvasm.NCore.Syntax (NCore)
import Purvasm.Types (GlobalName)

overrideForeign :: GlobalEnv -> GlobalName -> Maybe NCore
overrideForeign _ _ = Nothing