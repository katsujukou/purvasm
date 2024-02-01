module Purvasm.Compiler where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Purvasm.Types (ModuleName(..))

data CompileStep
  = LoadEntryModule ModuleName
  | BuildBuildPlan
  | Finish

derive instance Generic CompileStep _
instance Show CompileStep where
  show = genericShow
