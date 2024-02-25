module Purvasm.ECore.Transform where

import Prelude

import Purvasm.ECore.Syntax (Ann, Module)

transformModule :: Module Ann -> Module Ann
transformModule = identity
