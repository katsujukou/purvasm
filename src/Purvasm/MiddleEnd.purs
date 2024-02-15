module Purvasm.MiddleEnd
  ( module ReExports
  , module ECore
  , module ELambda
  , translateIdent
  , translateModuleName
  ) where

import PureScript.CoreFn as CF
import Purvasm.ECore.Syntax (AtomicLiteral(..), Binding(..), Expr(..), Literal(..), Module(..)) as ECore
import Purvasm.ECore.Translate (translateCoreFn) as ECore
import Purvasm.ELambda.Syntax (Program(..), ELambda(..)) as ELambda
import Purvasm.ELambda.Types (Var(..)) as ELambda
import Purvasm.Primitives (Primitive(..)) as ReExports
import Purvasm.Types (Arity, Ident(..), ModuleName(..)) as ReExports
import Purvasm.Types (Ident, ModuleName)
import Safe.Coerce (coerce)

translateModuleName :: CF.ModuleName -> ModuleName
translateModuleName = coerce

translateIdent :: CF.Ident -> Ident
translateIdent = coerce

