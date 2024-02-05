module Purvasm.DependencyGraph
  ( ModuleGraph
  , module ReExports
  ) where

import Data.HashGraph (HashGraph)
import Data.HashGraph (addVertices, topsort) as ReExports
import Purvasm.Types (ModuleName)

type ModuleGraph = HashGraph ModuleName
