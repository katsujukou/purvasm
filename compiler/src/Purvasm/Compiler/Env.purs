module Purvasm.Compiler.Env where

import Prelude

import Purvasm.Global (GlobalEnv)

type CompileEnv =
  { global :: GlobalEnv
  }