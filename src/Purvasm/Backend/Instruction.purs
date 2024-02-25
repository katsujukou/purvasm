module Purvasm.Backend.Instruction where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Purvasm.Backend.Types (AtomicConstant, GlobalName, Label, Primitive, StructuredConstant)
import Purvasm.Types (ConstructorTag)

data Instruction
  = Stop
  -- Local variable
  | KQuote StructuredConstant
  | KAccess Int
  | KLet
  | KEndLet Int
  | KDummies Int -- used for letrec binding
  -- handle function call
  | KGrab
  | KPush
  | KApply
  | KClosure Label
  | KReturn
  | KPushMark
  | KTermApply
  | KStartFun
  -- Logical and comparison
  | KAnd
  | KOr
  | KXor
  | KNot
  | KEqu -- | Value equality for scalar integers,
  | KNeq -- | Referencial equality for heap-allocated value. 
  | KLt_i32
  | KGt_i32
  | KLe_i32
  | KGe_i32
  | KEquStruct -- structual equality of block value
  | KNeqStruct
  -- 32bit integer arithmetic
  | KInc_i32
  | KDec_i32
  | KNeg_i32
  | KAdd_i32
  | KSub_i32
  | KMul_i32
  | KDiv_i32
  | KMod_i32
  -- Double precision number arithmetic
  | KAdd_num
  | KSub_num
  | KMul_num
  | KDiv_num
  | KNeg_num
  -- jump and branch
  | KLabel Label
  | Kswitch (Array Label)
  | KBranchTagEqu ConstructorTag Label
  | KBranchEqu AtomicConstant Label -- branch if accumulator holds given scalar value
  | KBranchNeq AtomicConstant Label -- branch if accumulator does not hold given constant value
  | KBranchEquZ Label
  | KBranchNeqZ Label
  | KBranch Label -- branch always
  -- primitive operation
  | KGetGlobal GlobalName
  | KSetGlobal GlobalName
  | Kprim Primitive
  -- Effect-specific operation
  | KEffPerform
  | KEffTermPerform
  | KEffAsign Int
  | KMakeEff Int
  | KStartEff

derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

type CodeBlock = List Instruction
