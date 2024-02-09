module Purvasm.Backend.PmoFile where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Purvasm.Backend.Instruction (CodeBlock)
import Purvasm.Backend.Types (Ident, ModuleName, GlobalName)

-- The type of toplevel symbol.
data SymbolType
  = Value
  | Function Int -- arity
  | EffValue

derive instance Generic SymbolType _
instance Show SymbolType where
  show = genericShow

-- symbol descriptor
type SymbolDesc =
  { name :: Ident
  , typ :: SymbolType
  , dataOfs :: Int
  , textOfs :: Int
  }

type ObjectHeader =
  { version :: String
  , pursVersion :: String
  , name :: ModuleName
  }

newtype PmoFile = PmoFile
  { head :: ObjectHeader
  , symbols :: Array SymbolDesc
  , textsec :: Array CodeBlock
  , datasec :: Array CodeBlock
  , refsec :: Array GlobalName
  }

derive instance Newtype PmoFile _
instance Show PmoFile where
  show (PmoFile obj) = "(PmoFile " <> show obj <> ")"

moduleName :: PmoFile -> ModuleName
moduleName (PmoFile { head }) = head.name

newtype PmiFile = PmiFile
  {
  }