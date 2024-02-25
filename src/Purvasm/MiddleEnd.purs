module Purvasm.MiddleEnd
  ( module ReExports
  , module ECore
  , module LCore
  , translateIdent
  , translateModuleName
  ) where

import PureScript.CoreFn as CF
import Purvasm.ECore.Syntax (Binding(..), Expr(..), Literal(..), Module(..)) as ECore
import Purvasm.ECore.Translate (translateCoreFn) as ECore
import Purvasm.LCore.Syntax (Program(..), LCore(..)) as LCore
import Purvasm.LCore.Types (Var(..)) as LCore
import Purvasm.Primitives (Primitive(..)) as ReExports
import Purvasm.Types (Arity, Ident(..), ModuleName(..)) as ReExports
import Purvasm.Types (Ident, ModuleName)
import Safe.Coerce (coerce)

translateModuleName :: CF.ModuleName -> ModuleName
translateModuleName = coerce

translateIdent :: CF.Ident -> Ident
translateIdent = coerce

