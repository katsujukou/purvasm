module Purvasm.LCore.Syntax where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Purvasm.NCore.Env (VariableDesc)
import Purvasm.NCore.Types (Var)
import Purvasm.Primitives (Primitive)
import Purvasm.Types (Arity, AtomicConstant, BlockTag, ConstructorTag, Ident, ModuleName, RecordId, GlobalName)

newtype Module = Module
  { name :: ModuleName
  , static :: Array (Ident /\ LowExpr)
  , decls :: Array (Ident /\ LowExpr)
  , foreigns :: Array Ident
  }

derive instance Newtype Module _
instance Show Module where
  show (Module m) = "(Module " <> show m <> ")"

data LowExpr
  = LEStatic StaticExpr
  | LEDynamic Expr
  | LEstaticfail
  | LEstatichandle

derive instance Generic LowExpr _
instance Show LowExpr where
  show exp = genericShow exp

newtype StaticRef = StaticRef GlobalName

derive instance Newtype StaticRef _
instance Show StaticRef where
  show (StaticRef ref) = "(StaticRef " <> show ref <> ")"

data StaticImm
  = ImmNil
  | ImmUnboxedInt Int
  | ImmRef StaticRef

derive instance Generic StaticImm _
instance Show StaticImm where
  show = genericShow

data StaticExpr
  = SEImm StaticImm
  | SEConstString String
  | SEConstNumber Number
  | SEConstNumberArray (Array Number)
  | SEConstArray (Array StaticExpr)
  | SEConstRecord RecordId (Array StaticImm)
  | SEConstBlock BlockTag (Array StaticImm)
  | SEFunction Arity LowExpr

derive instance Generic StaticExpr _
instance Show StaticExpr where
  show exp = genericShow exp

data Expr
  = LEInlinedStatic StaticExpr
  | LEStaticRef StaticRef
  | LEVar VariableDesc Var
  | LEPrim Primitive (Array LowExpr)
  | LEApp (LowExpr) (Array LowExpr)
  | LElet (Array LowExpr) LowExpr
  | LEletrec (Array LowExpr) LowExpr
  | LEifthenelse LowExpr LowExpr LowExpr
  | LEswitch LowExpr (Array (ConstructorTag /\ LowExpr))
  -- More accurate equality ?
  | LEconditional LowExpr (Array (AtomicConstant /\ LowExpr))

derive instance Generic Expr _
instance Show Expr where
  show exp = genericShow exp