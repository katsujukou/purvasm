module Purvasm.ECore.Syntax where

import Prelude

import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Purvasm.Global (ConstructorDesc, GlobalName)
import Purvasm.Types (AtomicConstant, Ident, ModuleName, RecordId)

newtype Module a = Module
  { name :: ModuleName
  , decls :: Array (Binding a)
  }

instance Show a => Show (Module a) where
  show (Module m) = "(Module " <> show m <> ")"

data Binding a = Binding Ident (Expr a)

derive instance Generic (Binding a) _
instance Show a => Show (Binding a) where
  show = genericShow

data Expr a
  = ExprLit a Literal
  | ExprVar a Ident
  | ExprGlobal a GlobalName
  | ExprArray a (Array (Expr a))
  | ExprRecord a RecordId (Array (Prop (Expr a)))
  | ExprTypeclass a (Array (Ident /\ Maybe GlobalName))
  | ExprTypeclassInstance a GlobalName (Maybe (Array GlobalName))
  | ExprAbs a (Array Ident) (Expr a)
  | ExprApp a (Expr a) (Array (Expr a))
  | ExprLet a (Array (Tuple Ident (Expr a))) (Expr a)
  | ExprLetRec a (Array (Tuple Ident (Expr a))) (Expr a)
  -- Unlike the `ExprConstructor` in Corefn,
  -- `ExprConstruct` represents the constructed value, not the constructor itself.
  | ExprConstruct a ConstructorDesc (Array (Expr a))
  -- during transformation, accessors and updators should be eliminated
  -- because they will be splitted into Lookup and Get/SetField.
  | ExprAccess a (Expr a) String
  | ExprUpdate a (Expr a) (Array (Tuple String (Expr a)))
  | ExprLookup a (Expr a) String
  | ExprGetField a Int (Expr a)
  | ExprSetField a Int (Expr a) (Expr a)
  | ExprGetSize a (Expr a)
  -- Get constructor tag
  | ExprCase a (Array (Expr a)) (Array (CaseAlternative a))
  -- if-then-else
  | ExprIf a (Expr a) (Expr a) (Expr a)
  | ExprStaticFail a
  | ExprstaticHandle a (Expr a) (Expr a)
  -- indicate that given expression should be omitted 
  | ExprNone

derive instance Generic (Expr a) _
instance Show a => Show (Expr a) where
  show expr = genericShow expr

newtype CaseAlternative a = CaseAlternative
  { patterns :: Array Pattern
  , action :: Expr a
  }

derive instance Generic (CaseAlternative a) _
instance Show a => Show (CaseAlternative a) where
  show = genericShow

data Pattern
  = PatWildcard
  | PatVar Ident
  | PatLiteral AtomicConstant
  | PatArray (Array Pattern)
  | PatRecord (Array (Prop Pattern))
  | PatConstruct ConstructorDesc (Array Pattern)
  | PatAliase Ident Pattern

derive instance Generic Pattern _
instance Show Pattern where
  show pat = genericShow pat

data LiteralType
  = TArray
  | TRecord

data Literal
  = LitAtomic AtomicConstant
  | LitStruct StructuredLiteral

derive instance Generic Literal _
instance Show Literal where
  show l = genericShow l

data StructuredLiteral
  = LitArray (Array Literal)
  | LitRecord RecordId (Array (Prop Literal))
  | LitConstructor ConstructorDesc (Array Literal)

derive instance Generic StructuredLiteral _
instance Show StructuredLiteral where
  show sl = genericShow sl

data Prop a = Prop String a

derive instance Functor Prop
derive instance Foldable Prop
derive instance Traversable Prop
derive instance Generic (Prop a) _
instance Show a => Show (Prop a) where
  show = genericShow

propKey :: forall a. Prop a -> String
propKey (Prop k _) = k

propValue :: forall a. Prop a -> a
propValue (Prop _ v) = v

newtype Ann = Ann
  { meta :: Maybe Meta
  , context :: List Context
  -- , vars :: VariableTable
  }

instance Show Ann where
  show (Ann ann) = "(Ann " <> show ann <> ")"

data Meta = IsTypeclassDict GlobalName

derive instance Generic Meta _
instance Show Meta where
  show = genericShow

-- type VariableTable = HashMap Ident VarDesc

-- data VarDesc
--   = Unknown
--   | KnownLiteral Literal
--   | BoundTo VarDesc Occurrunce
--   | TypeclassDict GlobalName

-- derive instance Generic VarDesc _
-- instance Show VarDesc where
--   show = genericShow

data Context
  = ToplevelPhrase GlobalName
  | AppFunc
  | AppArg (Maybe GlobalName) Int
  | CtorArg GlobalName Int
  | RecordUpdateExpr
  | RecordUpdateUpdator String
  | CaseHead
  | CaseAction

derive instance Generic Context _
instance Show Context where
  show = genericShow

-- data FuncType = Anonymous | Var Ident | Global GlobalName

-- derive instance Generic FuncType _
-- instance Show FuncType where
--   show = genericShow

emptyAnn :: Ann
emptyAnn = Ann
  { meta: Nothing
  , context: L.Nil
  -- , vars: HM.empty
  }

addContext :: Context -> Ann -> Ann
addContext ctx (Ann ann) = Ann $ ann { context = L.Cons ctx ann.context }

setMeta :: Meta -> Ann -> Ann
setMeta meta (Ann ann) = Ann (ann { meta = Just meta })

exprAnn :: Expr Ann -> Ann
exprAnn = case _ of
  ExprLit a _ -> a
  ExprVar a _ -> a
  ExprGlobal a _ -> a
  ExprRecord a _ _ -> a
  ExprArray a _ -> a
  ExprTypeclass a _ -> a
  ExprTypeclassInstance a _ _ -> a
  ExprAbs a _ _ -> a
  ExprApp a _ _ -> a
  ExprLet a _ _ -> a
  ExprLetRec a _ _ -> a
  ExprConstruct a _ _ -> a
  ExprAccess a _ _ -> a
  ExprUpdate a _ _ -> a
  ExprLookup a _ _ -> a
  ExprGetField a _ _ -> a
  ExprSetField a _ _ _ -> a
  ExprGetSize a _ -> a
  ExprCase a _ _ -> a
  ExprIf a _ _ _ -> a
  ExprStaticFail a -> a
  ExprstaticHandle a _ _ -> a
  ExprNone -> emptyAnn