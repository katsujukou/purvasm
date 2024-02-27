module Purvasm.Types
  ( Arity
  , AtomicConstant(..)
  , BlockTag(..)
  , ConstructorTag
  , Global(..)
  , GlobalName
  , Ident(..)
  , ModuleName(..)
  , RecordId(..)
  , RecordSig(..)
  , RecordTypeDesc(..)
  , StaticRef(..)
  , StaticValue(..)
  , StructuredConstant(..)
  , class IsIdent
  , mkGlobal
  , offsetOfProp
  , parseModuleName
  , toIdent
  ) where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Data.String.Regex (test) as Re
import Data.String.Regex.Flags (unicode) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import PureScript.CoreFn as CF
import Safe.Coerce (coerce)

class IsIdent a where
  toIdent :: a -> Ident

instance IsIdent String where
  toIdent = Ident

instance IsIdent CF.Ident where
  toIdent = coerce

instance IsIdent CF.ProperName where
  toIdent = coerce

newtype ModuleName = ModuleName String

derive newtype instance Eq ModuleName
derive newtype instance Ord ModuleName
derive newtype instance Hashable ModuleName

instance Show ModuleName where
  show (ModuleName modname) = "(ModuleName " <> modname <> ")"

parseModuleName :: String -> Maybe ModuleName
parseModuleName str = split (Pattern ".") str
  # Array.all (Re.test regex)
  # if _ then Just (ModuleName str) else Nothing
  where
  regex = Re.unsafeRegex """^[A-Z][a-zA-Z0-9]*$""" Re.unicode

newtype Ident = Ident String

derive instance Eq Ident
derive instance Ord Ident
derive instance Newtype Ident _
derive newtype instance Hashable Ident
instance Show Ident where
  show (Ident ident) = "(Ident " <> ident <> ")"

type Arity = Int

type ConstructorTag = Int

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
  | TClass
  | TDict GlobalName
  | TRecord RecordId
  | TNumberArray
  | TClosure
  | TClosureGrp
  | TConstr Int

derive instance Generic BlockTag _
instance Show BlockTag where
  show = genericShow

data AtomicConstant
  = ACUnit
  | ACInt Int
  | ACNumber Number
  | ACBool Boolean
  | ACChar Char
  | ACString String

derive instance Eq AtomicConstant
derive instance Generic AtomicConstant _
instance Show AtomicConstant where
  show = genericShow

data RecordId = RecordId (Maybe ModuleName) RecordSig

derive instance Eq RecordId
derive instance Generic RecordId _
instance Hashable RecordId where
  hash (RecordId mn id) = hash { modname: mn, ident: id }

instance Show RecordId where
  show = genericShow

newtype RecordSig = RecordSig (Array String)

instance Show RecordSig where
  show (RecordSig sig) = "(RecordSig " <> show sig <> ")"

derive newtype instance Eq RecordSig
derive newtype instance Hashable RecordSig

offsetOfProp :: String -> RecordSig -> Maybe Int
offsetOfProp prop (RecordSig sig) = Array.findIndex (_ == prop) sig

data RecordTypeDesc = RTDict GlobalName | RTPlain

derive instance Generic RecordTypeDesc _
instance Show RecordTypeDesc where
  show = genericShow

newtype Global a = Global
  { modname :: ModuleName
  , ident :: Ident
  , it :: a
  }

derive instance Newtype (Global a) _
derive instance Eq a => Eq (Global a)
derive instance Ord a => Ord (Global a)
derive newtype instance Hashable a => Hashable (Global a)
instance Show GlobalName where
  show (Global { modname, ident }) = "(GlobalName { module: " <> show modname <> ", ident: " <> show ident <> " })"
else instance Show a => Show (Global a) where
  show (Global g) = "(Global " <> show g <> ")"

mkGlobal :: forall a. ModuleName -> Ident -> a -> Global a
mkGlobal modname ident it = Global { modname, ident, it }

type GlobalName = Global Unit

data StaticValue
  = SConst StructuredConstant
  | SRef StaticRef

derive instance Generic StaticValue _

instance Show StaticValue where
  show = genericShow

newtype StaticRef = StaticRef GlobalName

instance Show StaticRef where
  show (StaticRef ref) = "(StaticRef { refTo: " <> show ref <> " })"