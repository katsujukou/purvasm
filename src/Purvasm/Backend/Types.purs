module Purvasm.Backend.Types
  ( Label(..)
  , module ReExports
  ) where

import Prelude

import Purvasm.Primitives (Primitive(..)) as ReExports
import Purvasm.Types (Arity, AtomicConstant(..), GlobalName, mkGlobalName, Ident(..), ModuleName(..), StructuredConstant(..), BlockTag(..)) as ReExports

newtype Label = Label Int

instance Show Label where
  show (Label label) = "(Label " <> show label <> ")"
