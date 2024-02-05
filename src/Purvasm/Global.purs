module Purvasm.Global where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import PureScript.ExternsFile (ExternsFile(..))
import Purvasm.Types (Arity, ConstructorTag, Ident, ModuleName)

newtype GlobalName = GlobalName { modname :: ModuleName, ident :: Ident }

derive instance Newtype GlobalName _
derive instance Eq GlobalName
derive instance Ord GlobalName
instance Show GlobalName where
  show (GlobalName g) = "(GlobalName " <> show g <> ")"

mkGlobalName :: ModuleName -> Ident -> GlobalName
mkGlobalName modname ident = GlobalName { modname, ident }

newtype GlobalEnv = GlobalEnv
  { valueDecls :: Map GlobalName ValueDesc
  , constructorDecls :: Map GlobalName ConstructorDesc
  , typeclassDecls :: Map GlobalName TypeclassDesc
  }

instance Show GlobalEnv where
  show (GlobalEnv env) = "(GlobalEnv " <> show env <> ")"

emptyEnv :: GlobalEnv
emptyEnv = GlobalEnv
  { valueDecls: Map.empty
  , constructorDecls: Map.empty
  , typeclassDecls: Map.empty
  }

derive instance Newtype GlobalEnv _

-- | Descriptor of values defined in global scope.
data ValueDesc
  = ValNormal
  | ValTypeclassMember GlobalName -- name of typeclass 
  | ValTypeclassInstance GlobalName -- name of Typeclass instance name
  | ValPrim PrimDesc

derive instance Generic ValueDesc _
instance Show ValueDesc where
  show = genericShow

type ConstructorDesc =
  { isNewtype :: Boolean
  , arity :: Arity
  , tag :: ConstructorTag
  }

type TypeclassDesc =
  { members :: Array Ident
  }

type PrimDesc = {}

applyExternsToEnv :: ExternsFile -> GlobalEnv -> GlobalEnv
applyExternsToEnv _ = identity