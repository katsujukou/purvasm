module Purvasm.Backend.Translate where

import Purvasm.Backend.Types (Ident(..), ModuleName(..))
import Purvasm.MiddleEnd.Types as ME
import Safe.Coerce (coerce)

translIdent :: ME.Ident -> Ident
translIdent = coerce

translModuleName :: ME.ModuleName -> ModuleName
translModuleName = coerce
