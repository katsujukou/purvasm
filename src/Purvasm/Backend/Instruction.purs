module Purvasm.Backend.Instruction where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Purvasm.Backend.Types (Label(..))
import Purvasm.MiddleEnd.Syntax as ME
import Purvasm.Types (StructuredConstant)

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
  | KPushMark
  | KTermApply
  | KClosure Label
  | KReturn
  -- jump and branch
  | KLabel Label
  | Kbranch Label
  -- primitive operation
  | KGetGlobal String String
  | Kprim ME.Primitive
  -- misc 
  | KStartFun

derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

type BackendCode = List Instruction
