module Purvasm.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

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

type Arity = Int

newtype Global a = Global
  { mod :: ModuleName
  , name :: Ident
  , desc :: a
  }

instance Show a => Show (Global a) where
  show (Global desc) = "(Global " <> show desc <> ")"

type GlobalName = Global Unit

mkGlobalName :: ModuleName -> Ident -> GlobalName
mkGlobalName mod name = Global { mod, name, desc: unit }

data StructuredConstant
  = SCAtom AtomicConstant
  | SCBlock BlockTag (Array StructuredConstant)

derive instance Generic StructuredConstant _
instance Show StructuredConstant where
  show sc = genericShow sc

data BlockTag
  = TString
  | TNumber
  | TArray
  | TNumberArray
  | TClosure
  | TClosureGrp
  | TConstr Int

derive instance Generic BlockTag _
instance Show BlockTag where
  show = genericShow

data AtomicConstant
  = ACInt Int
  | ACNumber Number
  | ACBool Boolean
  | ACChar Char
  | ACString String

derive instance Generic AtomicConstant _
instance Show AtomicConstant where
  show = genericShow