module Purvasm.Compiler.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data LogVerbosity = Quiet | Normal | Verbose

derive instance Eq LogVerbosity
derive instance Ord LogVerbosity
derive instance Generic LogVerbosity _
instance Show LogVerbosity where
  show = genericShow