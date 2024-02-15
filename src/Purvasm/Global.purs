module Purvasm.Global where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashMap as HashMap
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.Traversable (foldl)
import Data.Tuple (uncurry)
import PureScript.CoreFn as CF
import PureScript.CoreFn.Analyser (typeclassInstanceOfExpr)
import PureScript.CoreFn.Analyser as Analyser
import Purvasm.Types (Arity, ConstructorTag, Ident(..), ModuleName(..))
import Safe.Coerce (coerce)

newtype GlobalName = GlobalName { modname :: ModuleName, ident :: Ident }

derive instance Newtype GlobalName _
derive instance Eq GlobalName
derive instance Ord GlobalName
derive newtype instance Hashable GlobalName
instance Show GlobalName where
  show (GlobalName g) = "(GlobalName " <> show g <> ")"

globalNameOfQualifiedVar :: CF.Qualified CF.Ident -> Maybe GlobalName
globalNameOfQualifiedVar (CF.Qualified (Just moduleName) ident) = Just $ mkGlobalName (coerce moduleName) (coerce ident)
globalNameOfQualifiedVar _ = Nothing

identPart :: GlobalName -> Ident
identPart (GlobalName { ident }) = ident

renameIdent :: GlobalName -> Ident -> GlobalName
renameIdent (GlobalName gn) ident = GlobalName $ gn { ident = ident }

identOfGlobalName :: GlobalName -> Ident
identOfGlobalName (GlobalName { ident }) = ident

mkGlobalName :: ModuleName -> Ident -> GlobalName
mkGlobalName modname ident = GlobalName { modname, ident }

lookupValue :: GlobalName -> GlobalEnv -> Maybe ValueDesc
lookupValue valueName (GlobalEnv genv) = HM.lookup valueName genv.valueDecls

lookupTypeclass :: GlobalName -> GlobalEnv -> Maybe TypeclassDesc
lookupTypeclass clsName (GlobalEnv genv) = HM.lookup clsName genv.typeclassDecls

lookupConstructor :: GlobalName -> GlobalEnv -> Maybe ConstructorDesc
lookupConstructor ctorName (GlobalEnv genv) = HM.lookup ctorName genv.constructorDecls

insertConstructor :: GlobalName -> ConstructorDesc -> GlobalEnv -> GlobalEnv
insertConstructor ctorName desc (GlobalEnv genv) = GlobalEnv $ genv
  { constructorDecls = genv.constructorDecls # HM.insert ctorName desc
  }

insertTypeclass :: GlobalName -> TypeclassDesc -> GlobalEnv -> GlobalEnv
insertTypeclass className desc (GlobalEnv genv) = GlobalEnv $ genv
  { typeclassDecls = genv.typeclassDecls # HM.insert className desc
  }

insertValue :: GlobalName -> ValueDesc -> GlobalEnv -> GlobalEnv
insertValue valueName desc (GlobalEnv genv) = GlobalEnv $ genv
  { valueDecls = genv.valueDecls # HM.insert valueName desc
  }

newtype GlobalEnv = GlobalEnv
  { valueDecls :: HashMap GlobalName ValueDesc
  , constructorDecls :: HashMap GlobalName ConstructorDesc
  , typeclassDecls :: HashMap GlobalName TypeclassDesc
  , typeclassInstanceDecls :: HashMap GlobalName TypeclassinstanceDesc
  }

instance Show GlobalEnv where
  show (GlobalEnv env) = "(GlobalEnv " <> show env <> ")"

emptyEnv :: GlobalEnv
emptyEnv = GlobalEnv
  { valueDecls: HashMap.empty
  , constructorDecls: HashMap.empty
  , typeclassDecls: HashMap.empty
  , typeclassInstanceDecls: HashMap.empty
  }

derive instance Newtype GlobalEnv _

-- | Descriptor of values defined in global scope.
data ValueDesc
  = ValNormal
  | ValTypeclass GlobalName
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
  , typ :: GlobalName
  }

type TypeclassDesc =
  { members :: Array Ident
  }

type TypeclassinstanceDesc =
  { class :: GlobalName
  }

type PrimDesc = {}

applyCorefnEnv :: CF.Module CF.Ann -> GlobalEnv -> GlobalEnv
applyCorefnEnv (CF.Module cfm@{ decls }) =
  flip (foldr (uncurry insertTypeclass')) classified.typeclassInstances
    >>> flip (foldr (uncurry insertTypeclassInstance)) classified.typeclassInstances
    >>> flip (foldr (uncurry insertConstructor')) classified.constructors
  where
  classified = Analyser.classify decls

  insertConstructor' :: CF.Ident -> CF.Expr CF.Ann -> GlobalEnv -> GlobalEnv
  insertConstructor' _ (CF.ExprConstructor _ typ' ctor args) genv@(GlobalEnv { constructorDecls })
    | typ <- qualify (coerce typ')
    , sameTypeDescs <- HM.filter (_.typ >>> (_ == typ)) constructorDecls
    , tag <- 1 + foldl (\t desc -> t `max` desc.tag) (-1) sameTypeDescs =
        let
          desc = { tag, arity: Array.length args, isNewtype: false, typ }
        in
          insertConstructor (qualify $ coerce ctor) desc genv
  insertConstructor' _ _ genv = genv

  insertTypeclass' :: CF.Ident -> (CF.Expr CF.Ann) -> GlobalEnv -> GlobalEnv
  insertTypeclass' _ (CF.ExprApp _ abs mems) genv
    | CF.ExprVar (CF.Ann { meta: Just CF.IsNewtype }) var <- abs
    , Just gloname <- globalNameOfQualifiedVar var
    , CF.ExprLit _ (CF.LitRecord memberProps) <- mems
    , Ident clsname <- identPart gloname
    , cls <- Str.replace (Str.Pattern "$Dict") (Str.Replacement "") clsname
    , typeclassName <- renameIdent gloname (Ident cls)
    , Nothing <- lookupTypeclass typeclassName genv =
        let
          typeclass =
            { members: memberProps <#> CF.propKey >>> coerce
            }
        in
          genv
            # insertTypeclass typeclassName typeclass
                >>> insertValue gloname (ValTypeclass typeclassName)
  insertTypeclass' _ _ genv = genv

  insertTypeclassInstance :: CF.Ident -> (CF.Expr CF.Ann) -> GlobalEnv -> GlobalEnv
  insertTypeclassInstance (CF.Ident ident) expr
    | Just clsInstance <- typeclassInstanceOfExpr expr
    , CF.Qualified (Just moduleName) (CF.Ident cls) <- clsInstance.typeclass =
        let
          typeclassName = mkGlobalName (coerce moduleName) (Ident cls)
        in
          insertValue (qualify (Ident ident)) (ValTypeclassInstance typeclassName)
  insertTypeclassInstance _ _ = identity

  qualify :: Ident -> GlobalName
  qualify = GlobalName <<< { modname: coerce cfm.name, ident: _ }
