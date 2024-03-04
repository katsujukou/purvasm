module Purvasm.Backend.PmoFile where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Purvasm.Backend.Instruction (CodeBlock)
import Purvasm.Backend.Types (GlobalName, Ident, ModuleName)
import Purvasm.Types (RecordId)

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

type PmoHeader =
  { version :: String
  , pursVersion :: String
  , name :: ModuleName
  }

newtype PmoFile = PmoFile
  { head :: PmoHeader
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

data PType
  = PTUnknown
  | PTTypeclassInstance GlobalName
  | PTRecord RecordId

derive instance Generic PType _
instance Show PType where
  show = genericShow

newtype FuncSig = FuncSig
  { args :: Array PType
  , return :: PType
  }

instance Show FuncSig where
  show (FuncSig sig) = "(FuncSig " <> show sig <> ")"

newtype PmiFile = PmiFile
  { head :: PmoHeader
  , typeclasses :: HashMap Ident (Array Ident)
  , records :: HashMap RecordId (Array String)
  , functions :: HashMap Ident FuncSig
  }

instance Show PmiFile where
  show (PmiFile pmi) = "(PmiFile " <> show pmi <> ")"
