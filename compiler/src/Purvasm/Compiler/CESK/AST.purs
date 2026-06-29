module Purvasm.Compiler.CESK.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Purvasm.Compiler.Binder (Binder)
import Purvasm.Compiler.Literal (Literal)
import Purvasm.Compiler.Primitive (PrimOp)

data Term
  = TmLit Literal
  | TmVar String
  | TmLam String Term
  | TmApp Term Term
  | TmLet String Term Term
  | TmLetrec (Array (String /\ Term)) Term
  | TmIf Term Term Term
  | TmPrim PrimOp (Array Term)
  | TmArray (Array Term)
  | TmRecord (Array { prop :: String, term :: Term })
  | TmAccessor Term String
  | TmUpdate Term (Array { prop :: String, term :: Term })
  | TmCtor String Int
  | TmForeign String
  | TmCase (Array Term) (Array Alternative)

type Alternative =
  { binders :: Array Binder
  , result :: Rhs
  }

data Rhs
  = Unconditional Term
  | Guarded (Array { guard :: Term, rhs :: Term })

derive instance Eq Term
derive instance Generic Term _
instance Show Term where
  show t = genericShow t

derive instance Eq Rhs
derive instance Generic Rhs _
instance Show Rhs where
  show r = genericShow r

