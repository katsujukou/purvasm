module Purvasm.Backend where

import Purvasm.Backend.PmoFile (PmoFile(..), PmoHeader)
import Purvasm.LCore.Syntax as LCF

compileModule :: PmoHeader -> LCF.Module -> PmoFile
compileModule head (LCF.Module lcModule) = do
  PmoFile
    { head
    , datasec: []
    , textsec: []
    , refsec: []
    , symbols: []
    }