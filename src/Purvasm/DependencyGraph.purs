module Purvasm.DependencyGraph where

import Data.Graph (Graph)
import Purvasm.Types (ModuleName)

type ModuleGraph = Graph ModuleName
