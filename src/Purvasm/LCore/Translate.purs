module Purvasm.LCore.Translate where

import Prelude

import Control.Monad.State (State, get, modify, modify_, put, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Global (GlobalName, identOfGlobalName, mkGlobalName)
import Purvasm.LCore.Syntax (Expr(..), LowExpr(..), Module(..), StaticExpr(..), StaticImm(..), StaticRef(..))
import Purvasm.NCore.Syntax as NC
import Purvasm.Types (AtomicConstant(..), BlockTag(..), GlobalName, Ident(..), ModuleName(..), StructuredConstant(..), toIdent)

type LowerEnv =
  { moduleName :: ModuleName
  , next :: Int
  , static :: Array (Ident /\ StaticExpr)
  }

type Lower a = State LowerEnv a

lower :: forall a. LowerEnv -> Lower a -> a /\ LowerEnv
lower lenv m = runState m lenv

pushStatic :: Ident -> StaticExpr -> Lower StaticRef
pushStatic ident lexp = do
  { moduleName } <- modify \env -> env { static = Array.cons (ident /\ lexp) env.static }
  pure $ StaticRef $ mkGlobalName moduleName ident

pushStaticAnon :: StaticExpr -> Lower StaticRef
pushStaticAnon lexp = do
  env <- get
  let
    ident = toIdent $ "$pvsm_static_" <> show env.next
    env' = env { static = Array.cons (ident /\ lexp) env.static, next = env.next + 1 }
  put env'
  pure $ StaticRef $ mkGlobalName env.moduleName ident

lowerModule :: NC.Module -> Module
lowerModule (NC.Module ncModule@{ name: moduleName, foreigns }) = do
  -- let
  --   decls /\ { static } = lower { moduleName, static: [], next: 0 } do
  --     traverse (lowerDeclaration moduleName) ncModule.decls

  Module
    { name: moduleName
    , decls: [] -- decls
    , static: []
    , foreigns
    }

-- lowerDeclaration :: ModuleName -> Ident /\ NC.NCore -> Lower (Ident /\ LowExpr)
-- lowerDeclaration moduleName (name /\ lambda) = (name /\ _) <$> go { isToplevel: true } lambda
--   where
--   globalName = mkGlobalName moduleName name

--   go { isToplevel } = case _ of
--     NC.NCConst cst -> case cst of
--       SCAtom acst -> case constImm acst of
--         Right imm
--           | isToplevel ->
--               LEDynamic <<< LEStaticRef <$> (pushStatic name (SEImm imm))
--           | otherwise -> pure $ LEStatic (SEImm imm)
--         Left seCst
--           | isToplevel ->
--               LEDynamic <<< LEStaticRef <$> (pushStatic name seCst)
--           | otherwise -> do
--               ref <- pushStaticAnon seCst
--               pure $ LEDynamic (LEStaticRef ref)
--       SCBlock tag items -> case tag of
--         _ -> unsafeCrashWith "Not implemented"
--     _ -> unsafeCrashWith "Not implemented"

constImm :: AtomicConstant -> Either StaticExpr StaticImm
constImm = case _ of
  ACInt i -> Right $ ImmUnboxedInt i
  ACBool b -> Right $ ImmUnboxedInt (if b then 1 else 0)
  ACChar ch -> Right $ ImmUnboxedInt (fromEnum ch)
  ACUnit -> Right $ ImmNil
  ACNumber num -> Left $ SEConstNumber num
  ACString str -> Left $ SEConstString str