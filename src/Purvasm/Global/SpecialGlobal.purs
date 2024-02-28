module Purvasm.Global.SpecialGlobal where

import Purvasm.Types (GlobalName, mkGlobalName, Ident(..), ModuleName(..))

glo_Data_Unit_unit :: GlobalName
glo_Data_Unit_unit = mkGlobalName (ModuleName "Data.Unit") (Ident "unit")

glo_Prim_undefined :: GlobalName
glo_Prim_undefined = mkGlobalName (ModuleName "Prim") (Ident "undefined")

glo_Data_Boolean_otherwise :: GlobalName
glo_Data_Boolean_otherwise = mkGlobalName (ModuleName "Data.Boolean") (Ident "otherwise")

glo_Data_Semiring_intAdd :: GlobalName
glo_Data_Semiring_intAdd = mkGlobalName (ModuleName "Data.Semiring") (Ident "intAdd")

glo_Data_Semiring_intMul :: GlobalName
glo_Data_Semiring_intMul = mkGlobalName (ModuleName "Data.Semiring") (Ident "intMul")

glo_Data_Semiring_numAdd :: GlobalName
glo_Data_Semiring_numAdd = mkGlobalName (ModuleName "Data.Semiring") (Ident "numAdd")

glo_Data_Semiring_numMul :: GlobalName
glo_Data_Semiring_numMul = mkGlobalName (ModuleName "Data.Semiring") (Ident "numMul")

