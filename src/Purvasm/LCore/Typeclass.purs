module Purvasm.LCore.Typeclass where

import Prelude

import Data.Maybe (Maybe(..))
import Purvasm.Global (GlobalEnv)
import Purvasm.Global as Global
import Purvasm.Global.SpecialGlobal as SpecialGlobal
import Purvasm.LCore.Env (VariableDesc(..))
import Purvasm.LCore.Syntax (LCore(..))
import Purvasm.LCore.Types (Var(..))
import Purvasm.Primitives (Primitive(..))
import Purvasm.Types (BlockTag(..), GlobalName)

overrideInstance :: GlobalEnv -> GlobalName -> GlobalName -> Maybe LCore
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
    LCPrim (PMakeBlock $ TDict className)
      [ LCFunction 2
          (LCPrim p_equ [ LCVar VarUnknown (Var 1), LCVar VarUnknown (Var 0) ])
      ]

  mkOrdImpl p_lt p_equ eqImpl = do
    lt <- mkLT
    gt <- mkGT
    eq <- mkEQ
    pure $ LCPrim (PMakeBlock $ TDict className)
      [ LCFunction 2 $
          LCifthenelse
            (LCPrim p_lt [ LCVar VarUnknown (Var 1), LCVar VarUnknown (Var 0) ])
            (lt)
            ( LCifthenelse
                (LCPrim p_equ [ LCVar VarUnknown (Var 1), LCVar VarUnknown (Var 0) ])
                eq
                gt
            )
      , LCFunction 1 $ LCPrim (PGetGlobal eqImpl) []
      ]

  mkLT :: _ LCore
  mkLT = do
    desc <- Global.lookupConstructor SpecialGlobal._Data_Ordering_LT genv
    pure $ LCPrim (PMakeBlock $ TConstr desc.tag) []

  mkEQ :: _ LCore
  mkEQ = do
    desc <- Global.lookupConstructor SpecialGlobal._Data_Ordering_EQ genv
    pure $ LCPrim (PMakeBlock $ TConstr desc.tag) []

  mkGT :: _ LCore
  mkGT = do
    desc <- Global.lookupConstructor SpecialGlobal._Data_Ordering_GT genv
    pure $ LCPrim (PMakeBlock $ TConstr desc.tag) []
