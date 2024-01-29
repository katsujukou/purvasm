module Purvasm.Backend.ObjectFile where

import Prelude

import Data.Array as Array
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Backend.Instruction (BackendCode)
import Purvasm.Backend.Types (Ident, ModuleName)

type CodeBlock =
  { closures :: BackendCode
  , toplevel :: BackendCode
  }

newtype ObjectFile = ObjectFile
  { name :: ModuleName
  , phrases :: Array (Ident /\ CodeBlock)
  }

derive instance Newtype ObjectFile _
instance Show ObjectFile where
  show (ObjectFile obj) = "(ObjectFile " <> show obj <> ")"

writeCode :: ObjectFile -> Ident -> BackendCode -> BackendCode -> ObjectFile
writeCode (ObjectFile obj) ident toplevel closures = ObjectFile $
  obj { phrases = Array.cons (ident /\ { toplevel, closures }) obj.phrases }
