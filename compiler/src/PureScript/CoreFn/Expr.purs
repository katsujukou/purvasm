-- | CoreFn expressions, binders, and binding groups (ADR-0014), transcribed
-- | faithfully from the verified [CoreFn.purs] reference. Constructor and field
-- | names follow the reference.    
module PureScript.CoreFn.Expr where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import PureScript.CoreFn.Ann (Ann)
import PureScript.CoreFn.Names (Ident, ProperName, Qualified)
import PureScript.CoreFn.Literal (Literal)

-- | A CoreFn expression.
data Expr
  = Literal Ann (Literal Expr)
  -- | `Constructor ann typeName constructorName fieldNames`
  | Constructor Ann ProperName ProperName (Array Ident)
  -- | `Accessor ann fieldName record`
  | Accessor Ann String Expr
  -- | `ObjectUpdate ann record copyFields updates`; `copyFields` is `Just` the
  -- | untouched labels for a monomorphic (closed-record) update, and `Nothing`
  -- | for a polymorphic (open-row) update.
  | ObjectUpdate Ann Expr (Maybe (Array String)) (Array (Tuple String Expr))
  -- | `Abs ann argument body`
  | Abs Ann Ident Expr
  -- | `App ann abstraction argument`
  | App Ann Expr Expr
  | Var Ann (Qualified Ident)
  -- | `Case ann scrutinees alternatives`
  | Case Ann (Array Expr) (Array CaseAlternative)
  -- | `Let ann bindings body`
  | Let Ann (Array Bind) Expr

derive instance genericExpr :: Generic Expr _
derive instance eqExpr :: Eq Expr
instance showExpr :: Show Expr where
  show e = genericShow e

-- | A binding group: a single non-recursive binding or a set of mutually
-- | recursive ones.
data Bind
  = NonRec Ann Ident Expr
  | Rec (Array RecBinding)

derive instance genericBind :: Generic Bind _
derive instance eqBind :: Eq Bind
instance showBind :: Show Bind where
  show b = genericShow b

-- | One binding within a recursive group.
type RecBinding = { ann :: Ann, ident :: Ident, expr :: Expr }

-- | A guarded result: the guard expression and the value it produces.
type Guard = { guard :: Expr, expression :: Expr }

-- | A `case` alternative: a row of binders and either a single unguarded
-- | result (`Right`) or a list of guarded results (`Left`).
type CaseAlternative =
  { binders :: Array Binder
  , result :: Either (Array Guard) Expr
  }

-- | A pattern binder.
data Binder
  = NullBinder Ann
  | LiteralBinder Ann (Literal Binder)
  | VarBinder Ann Ident
  -- | `ConstructorBinder ann typeName constructorName subBinders`
  | ConstructorBinder Ann (Qualified ProperName) (Qualified ProperName) (Array Binder)
  -- | `NamedBinder ann name binder` — an as-pattern (`name@binder`).
  | NamedBinder Ann Ident Binder

derive instance genericBinder :: Generic Binder _
derive instance eqBinder :: Eq Binder
instance showBinder :: Show Binder where
  show b = genericShow b

