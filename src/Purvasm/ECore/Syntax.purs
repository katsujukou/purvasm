module Purvasm.ECore.Syntax where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Global (ConstructorDesc, GlobalName(..))
import Purvasm.Types (Arity, ConstructorTag, Ident, ModuleName)

newtype Module a = Module
  { name :: ModuleName
  , decls :: Array (Binding a)
  }

instance Show a => Show (Module a) where
  show (Module m) = "(Module " <> show m <> ")"

data Bind a
  = NonRec (Binding a)
  | Rec (Array (Binding a))

derive instance Generic (Bind a) _
instance Show a => Show (Bind a) where
  show = genericShow

data Binding a = Binding Ident (Expr a)

derive instance Generic (Binding a) _
instance Show a => Show (Binding a) where
  show = genericShow

data Expr a
  = ExprLit a Literal
  | ExprVar a Ident
  | ExprGlobal a GlobalName
  | ExprArray a (Array (Expr a))
  | ExprRecord a (Array (Prop (Expr a)))
  | ExprTypeclassInstance a GlobalName (Maybe (Array (Prop (Expr a))))
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
  -- Get constructor tag
  | ExprTag a (Expr a)
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
  | PatLiteral AtomicLiteral
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
  = LitAtomic AtomicLiteral
  | LitStruct StructuredLiteral

derive instance Generic Literal _
instance Show Literal where
  show l = genericShow l

data AtomicLiteral
  = LitInt Int
  | LitBoolean Boolean
  | LitChar Char
  | LitString String
  | LitNumber Number
  | LitUnit

derive instance Generic AtomicLiteral _
instance Show AtomicLiteral where
  show al = genericShow al

data StructuredLiteral
  = LitArray (Array Literal)
  | LitRecord (Array (Prop Literal))
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

newtype Ann = Ann
  { meta :: Maybe Meta
  -- , context :: List Context
  -- , vars :: VariableTable
  }

instance Show Ann where
  show (Ann ann) = "(Ann " <> show ann <> ")"

data Meta = IsTypeclasssDict GlobalName

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

-- type Occurrunce = List (Either Int String)

-- data Context
--   = FuncBody (Array (Ident /\ VarDesc))
--   | LetBinder (Array (Ident /\ VarDesc))
--   | LetBody (Array (Ident /\ VarDesc))
--   | AppFunc
--   | AppArg Int
--   | ConstructorArg Int
--   | ArrayElement
--   | RecordProp String
--   | CaseExpr
--   | CaseHead
--   | CaseAction (Array (Ident /\ VarDesc))

-- derive instance Generic Context _
-- instance Show Context where
--   show = genericShow

-- data FuncType = Anonymous | Var Ident | Global GlobalName

-- derive instance Generic FuncType _
-- instance Show FuncType where
--   show = genericShow

emptyAnn :: Ann
emptyAnn = Ann
  { meta: Nothing
  -- , context: L.Nil
  -- , vars: HM.empty
  }

-- addVar :: Ident -> VarDesc -> Ann -> Ann
-- addVar ident desc (Ann ann) = Ann (ann { vars = HM.insert ident desc ann.vars })

-- contextOfAnn :: Ann -> List Context
-- contextOfAnn (Ann { context }) = context

-- addContext :: Context -> Ann -> Ann
-- addContext ctx (Ann ann) = Ann (ann { context = L.Cons ctx ann.context })

-- setContext :: List Context -> Ann -> Ann
-- setContext ctx (Ann ann) = Ann (ann { context = ctx })