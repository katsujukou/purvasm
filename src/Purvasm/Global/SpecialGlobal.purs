module Purvasm.Global.SpecialGlobal where

import Purvasm.Types (GlobalName, Ident(..), ModuleName(..))
import Purvasm.Types (mkGlobalName) as T

mkGlobalName :: String -> String -> GlobalName
mkGlobalName m i = T.mkGlobalName (ModuleName m) (Ident i)

_Data_Unit_unit :: GlobalName
_Data_Unit_unit = mkGlobalName "Data.Unit" "unit"

_Prim_undefined :: GlobalName
_Prim_undefined = mkGlobalName "Prim" "undefined"

_Data_Ord_ordInt :: GlobalName
_Data_Ord_ordInt = mkGlobalName "Data.Ord" "ordInt"

_Data_Ord_ordNum :: GlobalName
_Data_Ord_ordNum = mkGlobalName "Data.Ord" "ordNum"

_Data_Ordering_LT :: GlobalName
_Data_Ordering_LT = mkGlobalName "Data.Ordering" "LT"

_Data_Ordering_EQ :: GlobalName
_Data_Ordering_EQ = mkGlobalName "Data.Ordering" "EQ"

_Data_Ordering_GT :: GlobalName
_Data_Ordering_GT = mkGlobalName "Data.Ordering" "GT"

_Data_Boolean_otherwise :: GlobalName
_Data_Boolean_otherwise = mkGlobalName "Data.Boolean" "otherwise"

_Data_Semiring_intAdd :: GlobalName
_Data_Semiring_intAdd = mkGlobalName "Data.Semiring" "intAdd"

_Data_Semiring_intMul :: GlobalName
_Data_Semiring_intMul = mkGlobalName "Data.Semiring" "intMul"

_Data_Ring_intSub :: GlobalName
_Data_Ring_intSub = mkGlobalName "Data.Ring" "intSub"

_Data_EuclideanRing_intDiv :: GlobalName
_Data_EuclideanRing_intDiv = mkGlobalName "Data.EuclideanRing" "intDiv"

_Data_EuclideanRing_intMod :: GlobalName
_Data_EuclideanRing_intMod = mkGlobalName "Data.EuclideanRing" "intMod"

_Data_Semiring_numAdd :: GlobalName
_Data_Semiring_numAdd = mkGlobalName "Data.Semiring" "numAdd"

_Data_Semiring_numMul :: GlobalName
_Data_Semiring_numMul = mkGlobalName "Data.Semiring" "numMul"

