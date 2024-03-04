module Purvasm.NCore.Typeclass where

import Prelude

import Data.Maybe (Maybe(..))
import Purvasm.Global (GlobalEnv)
import Purvasm.Global as Global
import Purvasm.Global.SpecialGlobal as SpecialGlobal
import Purvasm.NCore.Env (VariableDesc(..))
import Purvasm.NCore.Syntax (NCore(..))
import Purvasm.NCore.Types (Var(..))
import Purvasm.Primitives (Primitive(..))
import Purvasm.Types (BlockTag(..), GlobalName)

overrideInstance :: GlobalEnv -> GlobalName -> GlobalName -> Maybe NCore
overrideInstance genv className ident =
  if ident == SpecialGlobal._Data_Eq_eqInt then mkEqImpl P_equ_i32
  else if ident == SpecialGlobal._Data_Eq_eqBoolean then mkEqImpl P_equ_i32
  else if ident == SpecialGlobal._Data_Eq_eqNumber then mkEqImpl P_equ_f64
  else if ident == SpecialGlobal._Data_Ord_ordInt then mkOrdImpl P_lt_i32 P_equ_i32 SpecialGlobal._Data_Eq_eqInt
  else if ident == SpecialGlobal._Data_Ord_ordBoolean then mkOrdImpl P_lt_i32 P_equ_i32 SpecialGlobal._Data_Eq_eqBoolean
  else if ident == SpecialGlobal._Data_Ord_ordNum then mkOrdImpl P_lt_f64 P_equ_f64 SpecialGlobal._Data_Eq_eqNumber
  else Nothing

  where

  mkEqImpl p_equ = Just do
    NCPrim (PMakeBlock $ TDict className)
      [ NCFunction 2
          (NCPrim p_equ [ NCVar VarUnknown (Var 1), NCVar VarUnknown (Var 0) ])
      ]

  mkOrdImpl p_lt p_equ eqImpl = do
    lt <- mkLT
    gt <- mkGT
    eq <- mkEQ
    pure $ NCPrim (PMakeBlock $ TDict className)
      [ NCFunction 2 $
          NCifthenelse
            (NCPrim p_lt [ NCVar VarUnknown (Var 1), NCVar VarUnknown (Var 0) ])
            (lt)
            ( NCifthenelse
                (NCPrim p_equ [ NCVar VarUnknown (Var 1), NCVar VarUnknown (Var 0) ])
                eq
                gt
            )
      , NCFunction 1 $ NCPrim (PGetGlobal eqImpl) []
      ]

  mkLT :: _ NCore
  mkLT = do
    desc <- Global.lookupConstructor SpecialGlobal._Data_Ordering_LT genv
    pure $ NCPrim (PMakeBlock $ TConstr desc.tag) []

  mkEQ :: _ NCore
  mkEQ = do
    desc <- Global.lookupConstructor SpecialGlobal._Data_Ordering_EQ genv
    pure $ NCPrim (PMakeBlock $ TConstr desc.tag) []

  mkGT :: _ NCore
  mkGT = do
    desc <- Global.lookupConstructor SpecialGlobal._Data_Ordering_GT genv
    pure $ NCPrim (PMakeBlock $ TConstr desc.tag) []
