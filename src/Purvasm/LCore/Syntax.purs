module Purvasm.LCore.Syntax where

newtype Module = Module
  { decls :: Array Unit
  }