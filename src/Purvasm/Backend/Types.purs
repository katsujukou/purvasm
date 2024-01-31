module Purvasm.Backend.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Purvasm.MiddleEnd as ME
import Purvasm.Types (StructuredConstant)

data Instruction
  = Stop
  -- Local variable
  | KQuote StructuredConstant
  | KAccess Int
  | KLet
  | KEndLet Int
  -- handle function call
  | KGrab
  | KPush
  | KApply
  | KClosure Label
  | KReturn
  | KPushMark
  | KTermApply
  -- jump and branch
  | KLabel Label
  | Kbranch Label
  -- primitive operation
  | KGetGlobal ModuleName Ident
  | KSetGlobal ModuleName Ident
  | Kprim ME.Primitive
  -- misc
  | KPerformEff
  | KStartFun

derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

type CodeBlock = List Instruction

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

newtype ObjectFile = ObjectFile
  { head :: ObjectHeader
  , symbols :: Array SymbolDesc
  , textsec :: Array CodeBlock
  , datasec :: Array CodeBlock
  }

derive instance Newtype ObjectFile _
instance Show ObjectFile where
  show (ObjectFile obj) = "(ObjectFile " <> show obj <> ")"

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

