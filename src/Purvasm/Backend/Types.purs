module Purvasm.Backend.Types where

import Prelude

import Data.Newtype (class Newtype)

newtype Label = Label Int

instance Show Label where
  show (Label label) = "(Label " <> show label <> ")"

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
instance Show ModuleName where
  show (ModuleName modname) = "(ModuleName " <> modname <> ")"

newtype Ident = Ident String

derive instance Eq Ident
derive instance Ord Ident
derive instance Newtype Ident _
instance Show Ident where
  show (Ident ident) = "(Ident " <> ident <> ")"
