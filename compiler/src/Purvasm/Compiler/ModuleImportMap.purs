module Purvasm.Compiler.ModuleImportMap
  ( ModuleImportMap
  , delete
  , empty
  , module ReExports
  , modules
  , nodes
  ) where

import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import PureScript.CoreFn as CF
import PureScript.CoreFn.Json as CFJ
import Purvasm.Types (ModuleName)
import Data.HashMap (filter, member) as ReExports

type ModuleImportMap = HashMap ModuleName (CFJ.PartialModule CF.Ann)

empty :: ModuleImportMap
empty = HashMap.empty

nodes :: ModuleImportMap -> Array ModuleName
nodes m = HashMap.keys m

modules :: ModuleImportMap -> Array (CFJ.PartialModule CF.Ann)
modules m = HashMap.values m

delete :: ModuleName -> ModuleImportMap -> ModuleImportMap
delete mn = HashMap.delete mn