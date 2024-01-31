module Purvasm.Backend.ObjectFile where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Backend.Instruction (BackendCode)
import Purvasm.Backend.Types (Ident, ModuleName)

-- writeCode :: ObjectFile -> Ident -> BackendCode -> BackendCode -> ObjectFile
-- writeCode (ObjectFile obj) ident toplevel closures = ObjectFile $
--   obj { phrases = Array.cons (ident /\ { toplevel, closures }) obj.phrases }

type SymbolDesc =
  { name :: Ident
  , typ :: SymbolType
  , dataOfs :: Int
  , textOfs :: Int
  }

type SymbolsManager = ReaderT ()