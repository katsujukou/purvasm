module PureScript.CoreFn where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as SCU
import Data.Traversable (class Traversable)

newtype Ident = Ident String

derive newtype instance Eq Ident
derive newtype instance Ord Ident
derive instance Newtype Ident _
instance Show Ident where
  show (Ident ident) = "(Ident " <> show ident <> ")"

newtype ModuleName = ModuleName String

derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName
derive instance Newtype ModuleName _

instance Show ModuleName where
  show (ModuleName mn) = "(ModuleName " <> mn <> ")"

newtype ProperName = ProperName String

derive newtype instance Eq ProperName
derive newtype instance Ord ProperName

instance Show ProperName where
  show (ProperName pn) = "(ProperName " <> pn <> ")"

data Qualified a = Qualified (Maybe ModuleName) a

derive instance Generic (Qualified a) _
derive instance Eq a => Eq (Qualified a)
derive instance Ord a => Ord (Qualified a)
derive instance Functor Qualified

instance Show a => Show (Qualified a) where
  show = genericShow

unQualified :: forall a. Qualified a -> a
unQualified (Qualified _ a) = a

qualifiedModuleName :: forall a. Qualified a -> Maybe ModuleName
qualifiedModuleName (Qualified mn _) = mn

newtype Ann = Ann
  { meta :: Maybe Meta
  }

instance Show Ann where
  show (Ann ann) = "(Ann " <> show ann <> ")"

data Meta
  = IsConstructor ConstructorType (Array Ident)
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere
  | IsSyntheticApp
  -- The following items are not part of the original CoreFn's Meta
  | IsTypeclassMember (Qualified ProperName) Ident
  | IsRecursBindGrp (Array Ident)
  | IsRecursBindGrpMember

derive instance eqMeta :: Eq Meta
derive instance ordMeta :: Ord Meta
derive instance Generic Meta _
instance Show Meta where
  show = genericShow

data ConstructorType
  = ProductType
  | SumType

derive instance Eq ConstructorType
derive instance Ord ConstructorType
derive instance Generic ConstructorType _
instance Show ConstructorType where
  show = genericShow

newtype Module a = Module
  { name :: ModuleName
  , imports :: Array (Import a)
  , decls :: Array (Bind a)
  , foreign :: Array Ident
  }

instance Show a => Show (Module a) where
  show (Module m) = "(Module " <> show m <> ")"

moduleName :: forall a. Module a -> ModuleName
moduleName (Module mod) = mod.name

data Import a = Import a ModuleName

derive instance Functor Import
derive instance Generic (Import a) _
instance Show a => Show (Import a) where
  show = genericShow

importName :: forall a. Import a -> ModuleName
importName (Import _ name) = name

data ReExport = ReExport ModuleName Ident

derive instance Eq ReExport
derive instance Ord ReExport
derive instance Generic ReExport _
instance Show ReExport where
  show = genericShow

data Bind a
  = NonRec (Binding a)
  | Rec (Array (Binding a))

derive instance Functor Bind
derive instance Generic (Bind a) _
instance Show a => Show (Bind a) where
  show = genericShow

nonRecBinding :: forall a. Bind a -> Maybe (Binding a)
nonRecBinding = case _ of
  NonRec b -> Just b
  _ -> Nothing

isRec :: forall a. Bind a -> Boolean
isRec (Rec _) = true
isRec _ = false

data Binding a = Binding a Ident (Expr a)

derive instance Functor Binding
derive instance Generic (Binding a) _
instance Show a => Show (Binding a) where
  show = genericShow

bindingIdent :: forall a. Binding a -> Ident
bindingIdent (Binding _ ident _) = ident

bindingExpr :: forall a. Binding a -> Expr a
bindingExpr (Binding _ _ expr) = expr

data Expr a
  = ExprVar a (Qualified Ident)
  | ExprLit a (Literal (Expr a))
  | ExprConstructor a ProperName Ident (Array String)
  | ExprAccessor a (Expr a) String
  | ExprUpdate a (Expr a) (Array (Prop (Expr a)))
  | ExprAbs a Ident (Expr a)
  | ExprApp a (Expr a) (Expr a)
  | ExprCase a (Array (Expr a)) (Array (CaseAlternative a))
  | ExprLet a (Array (Bind a)) (Expr a)

derive instance Functor Expr
derive instance Generic (Expr a) _
instance Show a => Show (Expr a) where
  show expr = genericShow expr

data CaseAlternative a = CaseAlternative (Array (Binder a)) (CaseGuard a)

derive instance Functor CaseAlternative
derive instance Generic (CaseAlternative a) _
instance Show a => Show (CaseAlternative a) where
  show cas = genericShow cas

data CaseGuard a
  = Unconditional (Expr a)
  | Guarded (Array (Guard a))

derive instance Functor CaseGuard
derive instance Generic (CaseGuard a) _
instance Show a => Show (CaseGuard a) where
  show cg = genericShow cg

data Guard a = Guard (Expr a) (Expr a)

derive instance Functor Guard

derive instance Generic (Guard a) _
instance Show a => Show (Guard a) where
  show = genericShow

data Prop a = Prop String a

derive instance Functor Prop
derive instance Foldable Prop
derive instance Traversable Prop
derive instance Eq a => Eq (Prop a)
derive instance Generic (Prop a) _
instance Show a => Show (Prop a) where
  show = genericShow

propKey :: forall a. Prop a -> String
propKey (Prop k _) = k

propValue :: forall a. Prop a -> a
propValue (Prop _ a) = a

findProp :: forall a. String -> Array (Prop a) -> Maybe a
findProp prop = Array.findMap (\(Prop k v) -> if prop == k then Just v else Nothing)

data Literal a
  = LitInt Int
  | LitNumber Number
  | LitString String
  | LitChar Char
  | LitBoolean Boolean
  | LitArray (Array a)
  | LitRecord (Array (Prop a))

derive instance Eq a => Eq (Literal a)
derive instance Generic (Literal a) _

derive instance Functor Literal
derive instance Foldable Literal
derive instance Traversable Literal

instance Show a => Show (Literal a) where
  show = genericShow

data Binder a
  = BinderNull a
  | BinderVar a Ident
  | BinderNamed a Ident (Binder a)
  | BinderLit a (Literal (Binder a))
  | BinderConstructor a (Qualified ProperName) (Qualified Ident) (Array (Binder a))

derive instance Functor Binder
derive instance Generic (Binder a) _
instance Show a => Show (Binder a) where
  show b = genericShow b

emptyAnn :: Ann
emptyAnn = Ann
  { meta: Nothing
  }

exprAnn :: forall a. Expr a -> a
exprAnn = case _ of
  ExprVar a _ -> a
  ExprLit a _ -> a
  ExprConstructor a _ _ _ -> a
  ExprAccessor a _ _ -> a
  ExprUpdate a _ _ -> a
  ExprAbs a _ _ -> a
  ExprApp a _ _ -> a
  ExprCase a _ _ -> a
  ExprLet a _ _ -> a

litChildren :: forall a. Literal a -> Array a
litChildren = case _ of
  LitArray as -> as
  LitRecord ps -> propValue <$> ps
  _ -> []

isPrimModule :: ModuleName -> Boolean
isPrimModule (ModuleName name) = name == "Prim" || SCU.take 5 name == "Prim."