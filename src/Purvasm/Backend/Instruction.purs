module Purvasm.Backend.Instruction where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Purvasm.Backend.Types (GlobalName, Label, Primitive, StructuredConstant)

data Instruction
  = Stop
  -- Local variable
  | KQuote StructuredConstant
  | KAccess Int
  | KLet
  | KEndLet Int
  -- handle function call
  | KGrab
  | KPush
  | KApply
  | KClosure Label
  | KReturn
  | KPushMark
  | KTermApply
  -- jump and branch
  | KLabel Label
  | Kbranch Label
  -- primitive operation
  | KGetGlobal GlobalName
  | KSetGlobal GlobalName
  | Kprim Primitive
  -- misc
  | KPerformEff
  | KStartFun
  | KStop

derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

type CodeBlock = List Instruction
