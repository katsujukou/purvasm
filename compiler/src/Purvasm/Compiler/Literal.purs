module Purvasm.Compiler.Literal where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Literal
  = LInt Int
  | LBool Boolean
  | LNumber Number
  | LString String

derive instance Eq Literal
derive instance Generic Literal _
instance Show Literal where
  show = genericShow
