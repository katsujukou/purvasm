module PureScript.ExternsFile.Names where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import PureScript.ExternsFile.Decoder.Class (class Decode, decoder)
import PureScript.ExternsFile.Decoder.Generic (genericDecoder)
import PureScript.ExternsFile.Decoder.Newtype (newtypeDecoder)
import PureScript.ExternsFile.SourcePos (SourcePos)

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
derive instance Eq ModuleName
derive instance Ord ModuleName
instance Show ModuleName where
  show (ModuleName mn) = "(ModuleName " <> mn <> ")"

instance Decode ModuleName where
  decoder = ModuleName <$> decoder

newtype ProperName = ProperName String

derive instance Newtype ProperName _
derive instance Eq ProperName
derive instance Ord ProperName
instance Show ProperName where
  show (ProperName pn) = "(ProperName " <> pn <> ")"

instance Decode ProperName where
  decoder = newtypeDecoder

newtype OpName = OpName String

derive instance Newtype OpName _
derive instance Eq OpName
derive instance Ord OpName
instance Show OpName where
  show (OpName on) = "(OpName " <> on <> ")"

instance Decode OpName where
  decoder = newtypeDecoder

newtype Ident = Ident String

derive instance Newtype Ident _
derive instance Eq Ident
derive instance Ord Ident
instance Show Ident where
  show (Ident id) = "(Ident " <> id <> ")"

instance Decode Ident where
  decoder = newtypeDecoder

data QualifiedBy
  = BySourcePos SourcePos
  | ByModuleName ModuleName

derive instance Eq QualifiedBy
derive instance Ord QualifiedBy
derive instance Generic QualifiedBy _

instance Show QualifiedBy where
  show = genericShow

instance Decode QualifiedBy where
  decoder = genericDecoder

data Qualified a = Qualified QualifiedBy a

derive instance Eq a => Eq (Qualified a)
derive instance Ord a => Ord (Qualified a)
derive instance Generic (Qualified a) _
instance Show a => Show (Qualified a) where
  show = genericShow

instance Decode a => Decode (Qualified a) where
  decoder = genericDecoder

data NameSource = UserNamed | CompilerNamed

derive instance Eq NameSource
derive instance Ord NameSource
derive instance Generic NameSource _

instance Show NameSource where
  show = genericShow

instance Decode NameSource where
  decoder = genericDecoder

