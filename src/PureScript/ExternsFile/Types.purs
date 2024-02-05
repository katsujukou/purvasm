module PureScript.ExternsFile.Types where

import Prelude
import Prim hiding (Type, Constraint)

import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple.Flat (T3)
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.ExternsFile.Decoder.Class (class Decode, decoder)
import PureScript.ExternsFile.Decoder.Generic (genericDecoder)
import PureScript.ExternsFile.Decoder.Monad (Decoder(..), runDecoder)
import PureScript.ExternsFile.Decoder.Newtype (newtypeDecoder)
import PureScript.ExternsFile.Names (OpName, ProperName, Qualified)
import PureScript.ExternsFile.PSString (PSString, unsafeUTF16ToString)
import PureScript.ExternsFile.SourcePos (SourceAnn, SourcePos)

newtype SkolemScope = SkolemScope Int

derive instance Eq SkolemScope
derive instance Ord SkolemScope
derive instance Newtype SkolemScope _

instance Show SkolemScope where
  show (SkolemScope s) = "(SkolemScope " <> show s <> ")"

instance Decode SkolemScope where
  decoder = newtypeDecoder

data WildcardData
  = HoleWildcard String
  | UnnamedWildcard
  | IgnoredWildcard

derive instance Eq WildcardData
derive instance Ord WildcardData
derive instance Generic WildcardData _ 
instance Show WildcardData where
  show = genericShow

instance Decode WildcardData where
  decoder = genericDecoder

data TypeVarVisibility
  = TypeVarVisible
  | TypeVarInvisible

derive instance Eq TypeVarVisibility
derive instance Ord TypeVarVisibility
derive instance Generic TypeVarVisibility _ 
instance Show TypeVarVisibility where
  show = genericShow

instance Decode TypeVarVisibility where
  decoder = genericDecoder 
  
data ConstraintData
  = PartialConstraintData (Array (Array String)) Boolean

derive instance Eq ConstraintData
derive instance Ord ConstraintData
derive instance Generic ConstraintData _ 
instance Show ConstraintData where
  show = genericShow 

instance Decode ConstraintData where
  decoder = genericDecoder

data Constraint a = Constraint
  a
  (Qualified ProperName)
  (Array (Type a))
  (Array (Type a))
  (Maybe ConstraintData)

derive instance Functor Constraint
derive instance Foldable Constraint
derive instance Traversable Constraint
derive instance Generic (Constraint a) _
instance Show a => Show (Constraint a) where
  show = genericShow 

instance Decode a => Decode (Constraint a) where
  decoder = genericDecoder

type SourceType = Type SourceAnn

type SourceConstraint = Constraint SourceAnn

newtype Label = Label PSString

derive instance Newtype Label _ 
-- derive instance Generic Label _ 
derive newtype instance Eq Label 
derive newtype instance Ord Label 

instance Show Label where
  show (Label pss) = pss # unwrap >>> unsafeUTF16ToString

instance Decode Label where
  decoder = newtypeDecoder 


data Type a
  = TUnknown a Int
  | TypeVar a String
  | TypeLevelString a PSString
  | TypeLevelInt a Int
  | TypeWildcard a WildcardData
  | TypeConstructor a (Qualified ProperName)
  | TypeOp a (Qualified OpName)
  | TypeApp a (Type a) (Type a)
  | KindApp a (Type a) (Type a)
  | ForAll a TypeVarVisibility String (Maybe (Type a)) (Type a) (Maybe SkolemScope)
  | ConstrainedType a (Constraint a) (Type a)
  | Skolem a String (Maybe (Type a)) Int SkolemScope
  | REmpty a
  | RCons a Label (Type a) (Type a)
  | KindedType a (Type a) (Type a)
  | BinaryNoParensType a (Type a) (Type a) (Type a)
  | ParensInType a (Type a)
 
derive instance Functor Type
derive instance Foldable Type
derive instance Traversable Type
derive instance Generic (Type a) _ 
instance Show a => Show (Type a) where
  show typ = genericShow typ

instance Decode a => Decode (Type a) where
  decoder = Decoder \fgn -> runDecoder (genericDecoder @(Type a)) fgn

data Role
  = Nominal
  | Representational
  | Phantom

derive instance Eq Role 
derive instance Ord Role 
derive instance Generic Role _ 
instance Show Role where
  show = genericShow

instance Decode Role where
  decoder = genericDecoder

displayRole :: Role -> String
displayRole r = case r of
  Nominal -> "nominal"
  Representational -> "representational"
  Phantom -> "phantom"


data TypeKind
  = DataType
      DataDeclType
      (Array (T3 String (Maybe SourceType) Role))
      (Array (ProperName /\ Array SourceType))
  | TypeSynonym
  | ExternData (Array Role)
  | LocalTypeVariable
  | ScopedTypeVar

derive instance Generic TypeKind _
instance Show TypeKind where
  show = genericShow

instance Decode TypeKind where
  decoder = genericDecoder 

data DataDeclType
  = Data
  | Newtype

derive instance Eq DataDeclType
derive instance Ord DataDeclType
derive instance Generic DataDeclType _ 
instance Show DataDeclType where
  show = genericShow

instance Decode DataDeclType where 
  decoder = genericDecoder 

data FunctionalDependency = FunctionalDependency
  (Array Int) -- list of parameters determining type instance
  (Array Int) -- lift of parameters determined by determiners

derive instance Eq FunctionalDependency
derive instance Ord FunctionalDependency
derive instance Generic FunctionalDependency _
instance Show FunctionalDependency where
  show = genericShow

instance Decode FunctionalDependency where
  decoder = genericDecoder


newtype ChainId = ChainId (String /\ SourcePos)

derive instance Eq ChainId 
derive instance Ord ChainId 
derive instance Newtype ChainId _
instance Show ChainId where
  show (ChainId cid) = "(ChainId " <> show cid <> ")" 

instance Decode ChainId where
--   decoder = newtypeDecoder
  decoder = ChainId <$> decoder

mkChainId :: String -> SourcePos -> ChainId
mkChainId fileName startingSourcePos = ChainId (fileName /\ startingSourcePos)
