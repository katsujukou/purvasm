module Purvasm.Global
  ( ConstructorDesc
  , GlobalEnv(..)
  , PrimDesc
  , TypeclassDesc
  , Value
  , ValueDesc(..)
  , ValueSource(..)
  , applyCorefnEnv
  , emptyEnv
  , externsEnv
  , globalNameOfQualifiedVar
  , identOfGlobalName
  , insertConstructor
  , insertTypeclass
  , lookupConstructor
  , lookupRecordType
  , lookupTypeclass
  , lookupTypeclassValue
  , lookupValue
  , module ReExports
  , renameIdent
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldr)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashSet (HashSet)
import Data.HashSet as HS
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.String.Regex as Re
import Data.Traversable (foldMap, foldl)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CoreFn as CF
import PureScript.CoreFn.Analyser (typeclassInstanceOfExpr)
import PureScript.CoreFn.Analyser as Analyser
import PureScript.CoreFn.Utils (typeClassConstructorRegex)
import PureScript.ExternsFile (ExternsDeclaration(..), ExternsFile(..)) as Ext
import PureScript.ExternsFile (identOfExternsDeclaration)
import PureScript.ExternsFile.Names (ProperName(..), Qualified(..), QualifiedBy(..)) as Ext
import PureScript.ExternsFile.Types (Constraint(..), Type(..)) as Ext
import Purvasm.Global.SpecialGlobal as SpecialGlobal
import Purvasm.Primitives (Primitive(..))
import Purvasm.Record (RecordSignature, mkRecordSignature)
import Purvasm.Types (Global(..), GlobalName, mkGlobal, mkGlobalName) as ReExports
import Purvasm.Types (class IsIdent, Arity, ConstructorTag, Global(..), GlobalName, Ident(..), ModuleName, RecordId(..), mkGlobalName, toIdent)
import Safe.Coerce (coerce)

globalNameOfQualifiedVar :: forall name. IsIdent name => CF.Qualified name -> Maybe GlobalName
globalNameOfQualifiedVar (CF.Qualified (Just moduleName) ident) = Just $ mkGlobalName (coerce moduleName) (toIdent ident)
globalNameOfQualifiedVar _ = Nothing

renameIdent :: GlobalName -> Ident -> GlobalName
renameIdent (Global gn) ident = Global $ gn { ident = ident }

identOfGlobalName :: GlobalName -> Ident
identOfGlobalName (Global { ident }) = ident

lookupValue :: GlobalName -> GlobalEnv -> Maybe Value
lookupValue valueName (GlobalEnv genv) = HM.lookup valueName genv.valueDecls

lookupTypeclass :: GlobalName -> GlobalEnv -> Maybe TypeclassDesc
lookupTypeclass clsName (GlobalEnv genv) = HM.lookup clsName genv.typeclassDecls

lookupTypeclassValue :: GlobalName -> GlobalEnv -> Maybe TypeclassDesc
lookupTypeclassValue name genv = lookupValue name genv >>= case _ of
  { desc: ValTypeclass cls } -> lookupTypeclass cls genv
  _ -> Nothing

lookupConstructor :: GlobalName -> GlobalEnv -> Maybe ConstructorDesc
lookupConstructor ctorName (GlobalEnv genv) = HM.lookup ctorName genv.constructorDecls

lookupRecordType :: Maybe ModuleName -> RecordSignature -> GlobalEnv -> Maybe RecordId
lookupRecordType mbModuleName sig (GlobalEnv genv) =
  let
    recordId = RecordId mbModuleName sig
  in
    if recordId `HS.member` genv.recordTypes then Just recordId
    else Nothing

insertConstructor :: GlobalName -> ConstructorDesc -> GlobalEnv -> GlobalEnv
insertConstructor ctorName desc (GlobalEnv genv) = GlobalEnv $ genv
  { constructorDecls = genv.constructorDecls # HM.insert ctorName desc
  }

insertTypeclass :: GlobalName -> TypeclassDesc -> GlobalEnv -> GlobalEnv
insertTypeclass className desc (GlobalEnv genv) = GlobalEnv $ genv
  { typeclassDecls = genv.typeclassDecls # HM.insert className desc
  }

insertValue :: GlobalName -> Value -> GlobalEnv -> GlobalEnv
insertValue valueName desc (GlobalEnv genv) = GlobalEnv $ genv
  { valueDecls = genv.valueDecls # HM.insert valueName desc
  }

newtype GlobalEnv = GlobalEnv
  { valueDecls :: HashMap GlobalName Value
  , constructorDecls :: HashMap GlobalName ConstructorDesc
  , typeclassDecls :: HashMap GlobalName TypeclassDesc
  , recordTypes :: HashSet RecordId
  }

instance Show GlobalEnv where
  show (GlobalEnv env) = "(GlobalEnv " <> show env <> ")"

emptyEnv :: GlobalEnv
emptyEnv = GlobalEnv
  { valueDecls: HM.empty
  , constructorDecls: HM.empty
  , typeclassDecls: HM.empty
  , recordTypes: HS.empty
  }

derive instance Newtype GlobalEnv _

type Value =
  { constraints :: Array (Maybe GlobalName)
  , desc :: ValueDesc
  , source :: Set ValueSource
  }

data ValueSource = Corefn | ExternsFile | PmiFile

derive instance Eq ValueSource
derive instance Ord ValueSource
derive instance Generic ValueSource _

instance Show ValueSource where
  show = genericShow

-- | Descriptor of values defined in global scope.
data ValueDesc
  = ValPlain
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
  { members :: Array (Ident /\ Maybe GlobalName)
  }

type PrimDesc =
  { prim :: Primitive
  , arity :: Int
  }

setConstraints :: ValueSource -> GlobalName -> Array (Maybe GlobalName) -> GlobalEnv -> GlobalEnv
setConstraints src ident constraints genv = insertValue ident value genv
  where
  value = case lookupValue ident genv of
    Nothing -> { desc: ValPlain, constraints: constraints, source: Set.singleton src }
    Just v -> v { constraints = constraints, source = Set.insert src v.source }

applyCorefnEnv :: CF.Module CF.Ann -> GlobalEnv -> GlobalEnv
applyCorefnEnv (CF.Module cfm@{ decls }) =
  flip (foldr (uncurry insertTypeclassInstance)) classified.typeclassInstances
    >>> flip (foldr (uncurry insertConstructor')) classified.constructors
    >>> flip (foldr (uncurry insertNewtypeConstructor)) classified.newtypeConstructors
    >>> flip (foldr insertTypeclassConstructor) (fst <$> classified.typeclassConstructors)
    >>> flip (foldr insertPrimValue) (cfm.foreign <> (fst <$> classified.plain))
    >>> flip (foldr (uncurry insertAnyValue)) classified.plain
  where
  moduleName :: ModuleName
  moduleName = coerce cfm.name

  classified = Analyser.classify decls

  insertConstructor' :: CF.Ident -> CF.Expr CF.Ann -> GlobalEnv -> GlobalEnv
  insertConstructor' (CF.Ident ident) exp genv@(GlobalEnv { constructorDecls }) = case exp of
    CF.ExprConstructor _ type_ ctor args
      | CF.Ident ctorname <- ctor
      , not (Re.test typeClassConstructorRegex ctorname) ->
          let
            globalIdent = mkGlobalName moduleName (Ident ident)
            CF.ProperName typ' = type_
            typ = mkGlobalName moduleName (Ident typ')
            tag = constructorDecls
              # HM.filter (_.typ >>> (_ == typ))
              # foldMap (_.tag >>> Array.singleton)
              # foldl max (-1)

            constr =
              { isNewtype: false
              , arity: Array.length args
              , typ
              , tag: tag + 1
              }
          in
            insertConstructor globalIdent constr genv
    _ -> genv

  insertNewtypeConstructor :: CF.Ident -> CF.Expr CF.Ann -> GlobalEnv -> GlobalEnv
  insertNewtypeConstructor (CF.Ident ident) _ genv =
    let
      globalIdent = mkGlobalName moduleName (Ident ident)
      constr =
        { isNewtype: true
        , arity: 1
        , typ: globalIdent -- not accurate
        , tag: 0
        }
    in
      insertConstructor globalIdent constr genv

  insertTypeclassConstructor :: CF.Ident -> GlobalEnv -> GlobalEnv
  insertTypeclassConstructor (CF.Ident ident) =
    let
      typeclassCtor = mkGlobalName moduleName $ Ident ident
      className = Ident $ Str.replace (Str.Pattern "$Dict") (Str.Replacement "") ident
    in
      insertValue typeclassCtor
        { desc: ValTypeclass $ mkGlobalName moduleName className
        , constraints: []
        , source: Set.singleton Corefn
        }

  insertTypeclassInstance :: CF.Ident -> (CF.Expr CF.Ann) -> GlobalEnv -> GlobalEnv
  insertTypeclassInstance (CF.Ident ident) expr genv =
    case typeclassInstanceOfExpr expr of
      Just instance_
        | Just className <- globalNameOfQualifiedVar instance_.typeclass ->
            let
              globalIdent = mkGlobalName moduleName (coerce ident)
              constraints = map (globalNameOfQualifiedVar =<< _) instance_.constraints
              genv1 = case lookupTypeclass className genv of
                Just _ -> genv
                Nothing -> genv
                  # insertTypeclass className
                      { members: ((_ /\ Nothing) <<< coerce <<< fst) <$> instance_.members
                      }
              genv2 = genv1
                # flip (foldr insertRecordIdsOfExpr) (snd <$> instance_.members)
            in
              case lookupValue globalIdent genv2 of
                Nothing -> genv2 # insertValue globalIdent
                  { desc: ValTypeclassInstance className
                  , constraints
                  , source: Set.singleton Corefn
                  }
                Just val -> genv2 # insertValue globalIdent
                  { desc: ValTypeclassInstance className
                  , constraints: Array.zipWith (<|>) val.constraints constraints
                  , source: Set.insert Corefn val.source
                  }
      _ -> unsafeCrashWith "insertTypeclassInstance: Impossible!"

  insertRecordIdsOfExpr :: CF.Expr CF.Ann -> GlobalEnv -> GlobalEnv
  insertRecordIdsOfExpr expr genv =
    Analyser.collectRecordSignatures expr
      # foldr (insertRecordId <<< RecordId Nothing <<< mkRecordSignature) genv

  insertPrimValue :: CF.Ident -> GlobalEnv -> GlobalEnv
  insertPrimValue (CF.Ident ident) genv =
    let
      gloname = mkGlobalName moduleName (Ident ident)
      prim = case gloname of
        _
          | gloname == SpecialGlobal._Data_Semiring_intAdd -> Just { prim: P_add_i32, arity: 2 }
          | gloname == SpecialGlobal._Data_Semiring_intMul -> Just { prim: P_mul_i32, arity: 2 }
          | gloname == SpecialGlobal._Data_Ring_intSub -> Just { prim: P_sub_i32, arity: 2 }
          | gloname == SpecialGlobal._Data_HeytingAlgebra_boolConj -> Just { prim: P_land, arity: 2 }
          | gloname == SpecialGlobal._Data_HeytingAlgebra_boolDisj -> Just { prim: P_lor, arity: 2 }
          | otherwise -> Nothing
    in
      case prim of
        Nothing -> genv
        Just desc -> genv # insertValue gloname
          { desc: ValPrim desc
          , constraints: []
          , source: Set.empty
          }

  insertAnyValue :: CF.Ident -> CF.Expr CF.Ann -> GlobalEnv -> GlobalEnv
  insertAnyValue (CF.Ident ident) expr genv =
    let
      globalIdent = mkGlobalName moduleName (Ident ident)
      mkRecordId = RecordId Nothing <<< mkRecordSignature
      recordIds = mkRecordId <$> Analyser.collectRecordSignatures expr
      genv' =
        if Array.null recordIds then genv
        else foldr insertRecordId genv recordIds
    in
      case lookupValue globalIdent genv' of
        Just val -> genv'
          # insertValue globalIdent
              ( val
                  { source = Set.insert Corefn val.source
                  }
              )
        Nothing -> genv'
          # insertValue globalIdent
              { desc: ValPlain
              , constraints: []
              , source: Set.singleton Corefn
              }

  insertRecordId :: RecordId -> GlobalEnv -> GlobalEnv
  insertRecordId recordId (GlobalEnv genv@{ recordTypes }) = GlobalEnv $
    genv { recordTypes = HS.insert recordId recordTypes }

-- insertRecordType recordSig (GlobalEnv genv'@{ recordTypes }) =
--   let
--     recordId = RecordId Nothing recordSig
--   in
--     GlobalEnv (genv' { recordTypes = HashSet.insert recordId recordTypes })

externsEnv :: Ext.ExternsFile -> GlobalEnv -> GlobalEnv
externsEnv (Ext.ExternsFile _ modname _ _ _ _ extDecls _) genv =
  genv
    # flip (foldr applyExternDeclEnv) extDecls

  where
  moduleName :: ModuleName
  moduleName = coerce modname

  applyExternDeclEnv :: Ext.ExternsDeclaration -> GlobalEnv -> GlobalEnv
  applyExternDeclEnv decl =
    addValue decl
      >>> applyConstraints decl

    where
    declIdent = identOfExternsDeclaration decl

    globalIdent = mkGlobalName moduleName declIdent

    addValue :: Ext.ExternsDeclaration -> GlobalEnv -> GlobalEnv
    addValue = case _ of
      Ext.EDInstance cls _ _ _ _ constrs _ _ _ _
        | Ext.Qualified (Ext.ByModuleName mn) (Ext.ProperName clsName) <- cls
        , classIdent <- mkGlobalName (coerce mn) (Ident clsName) ->
            let
              constraints = constrs
                # maybe
                    []
                    ( map case _ of
                        Ext.Constraint _ constraintClsName _ _ _
                          | Ext.Qualified (Ext.ByModuleName mn') (Ext.ProperName cls') <- constraintClsName ->
                              mkGlobalName (coerce mn') (Ident cls')
                        _ -> unsafeCrashWith "addValue: BySourcePos"
                    )
                # Array.reverse
            in
              insertTypeclassInstanceValue classIdent (Just <$> constraints)
      Ext.EDValue _ _ -> insertPlainValue globalIdent
      _ -> identity

    insertPlainValue :: GlobalName -> GlobalEnv -> GlobalEnv
    insertPlainValue name = insertValue name { desc: ValPlain, constraints: [], source: Set.singleton ExternsFile }

    insertTypeclassInstanceValue clsName constraints = insertValue globalIdent
      { desc: ValTypeclassInstance clsName
      , constraints
      , source: Set.singleton ExternsFile
      }

    applyConstraints :: Ext.ExternsDeclaration -> GlobalEnv -> GlobalEnv
    applyConstraints = case _ of
      Ext.EDValue _ type_
        | Ext.ForAll _ _ _ _ typ _ <- type_
        , constrs <- constraintsPart (stripForall typ)
        , not (Array.null constrs) -> setConstraints ExternsFile globalIdent constrs

      _ -> identity

  stripForall = case _ of
    Ext.ForAll _ _ _ _ t _ -> stripForall t
    t@_ -> t
  constraintsPart = go []
    where
    go constrs = case _ of
      Ext.ConstrainedType _ constr t
        | Ext.Constraint _ typeclass _ _ _ <- constr
        , Ext.Qualified (Ext.ByModuleName mn) (Ext.ProperName name) <- typeclass -> do
            let
              typeclassName = mkGlobalName (coerce mn) (Ident name)
            go (Array.snoc constrs (Just typeclassName)) t
      _ -> constrs

