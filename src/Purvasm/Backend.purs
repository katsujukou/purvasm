module Purvasm.Backend where

import Purvasm.Backend.PmoFile (PmoFile(..), PmoHeader)
import Purvasm.MiddleEnd as ME

compileProgram :: PmoHeader -> ME.Program -> PmoFile
compileProgram head (ME.Program lcProgram) = do
  PmoFile
    { head
    , datasec: []
    , textsec: []
    , refsec: []
    , symbols: []
    }