module Purvasm.Backend where

import Prelude

import Purvasm.Backend.PmoFile (PmoFile(..))
import Purvasm.MiddleEnd as ME
import Spago.Generated.BuildInfo (pursVersion)
import Spago.Generated.BuildInfo as Spago

compileProgram :: PmoHeader -> ME.Program -> PmoFile
compileProgram head (ME.Program lcProgram) = do
  PmoFile
    { head
    , datasec: []
    , textsec: []
    , refsec: []
    , symbols: []
    }