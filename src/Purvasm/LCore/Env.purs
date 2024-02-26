module Purvasm.LCore.Env where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.ECore.Syntax as ECF
import Purvasm.Global (GlobalEnv, GlobalName)
import Purvasm.LCore.Types (Occurrunce)
import Purvasm.Types (Ident, ModuleName, RecordId)

type TranslEnv =
  { moduleName :: ModuleName
  , globals :: GlobalEnv
  , locals :: LocalSymbolTable
  , static :: Array (Tuple Ident (ECF.Expr ECF.Ann))
  , fresh :: Int
  , isToplevel :: Boolean
  }

type Variable =
  { desc :: VariableDesc
  , occur :: Occurrunce
  }

data VariableDesc
  = VarUnknown
  | VarTypeclass GlobalName
  | VarRecord RecordId
  | VarGlobal GlobalName

derive instance Generic VariableDesc _
instance Show VariableDesc where
  show = genericShow

data LocalSymbolTable
  = Tnull
  | Tenv VariableDesc (HashMap Ident Occurrunce) LocalSymbolTable

derive instance Generic LocalSymbolTable _
instance Show LocalSymbolTable where
  show t = genericShow t

searchEnv :: Ident -> TranslEnv -> Maybe (Int /\ Variable)
searchEnv ident = _.locals >>> go 0
  where
  go i = case _ of
    Tnull -> Nothing
    Tenv desc vars env
      | Just occur <- HM.lookup ident vars -> Just $ i /\ { desc, occur }
      | otherwise -> go (i + 1) env

extendByNewVar :: Ident -> VariableDesc -> TranslEnv -> TranslEnv
extendByNewVar var desc tenv = tenv { locals = locals' }
  where
  locals' = Tenv desc (HM.singleton var L.Nil) tenv.locals