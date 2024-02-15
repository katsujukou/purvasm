module Purvasm.ECore.SpecialGlobal where

import Prelude

import Purvasm.Global (GlobalName(..), mkGlobalName)
import Purvasm.Types (Ident(..), ModuleName(..))

glo_Data_Unit_unit :: GlobalName
glo_Data_Unit_unit = mkGlobalName (ModuleName "Data.Unit") (Ident "unit")

glo_Data_Boolean_otherwise :: GlobalName
glo_Data_Boolean_otherwise = mkGlobalName (ModuleName "Data.Boolean") (Ident "otherwise")