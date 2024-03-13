module Purvasm.LCore.Translate where

import Prelude

import Control.Monad.State (State, get, modify, put, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Global (Global(..), GlobalEnv(..), GlobalName, mkGlobalName)
import Purvasm.Global as Global
import Purvasm.LCore.Syntax (LowExpr(..), Module(..), StaticExpr(..), StaticImm(..), StaticRef(..), exprStaticImm)
import Purvasm.NCore.Syntax (NCore(..))
import Purvasm.NCore.Syntax as NC
import Purvasm.Primitives (Primitive(..))
import Purvasm.Types (AtomicConstant(..), Ident, ModuleName, StructuredConstant(..), toIdent)

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

lowerModule :: GlobalEnv -> NC.Module -> Module
lowerModule genv (NC.Module ncModule@{ name: moduleName, foreigns }) = do
  -- let
  --   decls /\ { static } = lower { moduleName, static: [], next: 0 } do
  --     traverse (lowerDeclaration genv) ncModule.decls

  Module
    { name: moduleName
    , decls: []
    , static: []
    , foreigns
    }

lowerDeclaration :: GlobalEnv -> NC.Declaration -> Lower (Ident /\ LowExpr)
lowerDeclaration genv { name, lambda } = (name /\ _) <$> lowerExpr genv name lambda

lowerExpr :: GlobalEnv -> Ident -> NC.NCore -> Lower LowExpr
lowerExpr genv name = go { isToplevel: true }
  where
  go { isToplevel } = case _ of
    NC.NCNil -> pure $ LEStatic (SEImm ImmNil)
    NC.NCConst scst -> lowerConstant isToplevel scst
    NC.NCVar desc var -> pure $ LEVar desc var
    NC.NCPrim prim args -> do
      { moduleName, static } <- get
      case prim of
        PGetGlobal gloname
          | Global { modname } <- gloname
          , modname == moduleName -> do
              let
                _ = unsafePerformEffect do
                  logShow $ static
              unsafeCrashWith "Op!"
          | Just { static: true } <- Global.lookupValue gloname genv ->
              LENone <$ pushStatic name (SEImm (ImmRef $ StaticRef gloname))
        _ -> LEPrim prim <$> traverse goRec args
    NC.NCFunction ary body
      | isToplevel -> LENone <$ (SEFunction ary <$> goRec body >>= pushStatic name)
      | otherwise -> do
          trFunc <- SEFunction ary <$> goRec body
          ref <- pushStaticAnon trFunc
          pure $ LEStatic (SEImm $ ImmRef ref)
    NC.NClet binds body -> LElet <$> traverse goRec binds <*> goRec body
    NC.NCletrec binds body -> LEletrec <$> traverse goRec binds <*> goRec body
    NC.NCApply abs args -> LEApp <$> goRec abs <*> traverse goRec args
    NCSwitch head tbl -> LEswitch <$> goRec head <*> traverse (traverse goRec) tbl
    NC.NCConditional head tbl -> LEconditional <$> goRec head <*> traverse (traverse goRec) tbl
    NC.NCifthenelse cond ifSo notSo -> LEifthenelse <$> goRec cond <*> goRec ifSo <*> goRec notSo
    NC.NCStaticFail -> pure LEstaticfail
    NC.NCStaticHandle e1 e2 -> LEstatichandle <$> goRec e1 <*> goRec e2
    NC.NCNone -> pure LENone

  goRec exp = go { isToplevel: false } exp

  lowerConstant :: Boolean -> StructuredConstant -> Lower LowExpr
  lowerConstant isToplevel = case _ of
    SCAtom acst -> case constImm acst of
      Right immCst -> pure $ LEStatic (SEImm immCst)
      Left sexp
        | isToplevel -> pure $ LEStatic sexp
        | otherwise -> do
            ref <- pushStaticAnon sexp
            pure $ LEStatic (SEImm $ ImmRef ref)
    SCBlock tag elts -> do
      blkCst <- traverse (lowerConstant false) elts
      case traverse exprStaticImm blkCst of
        Nothing -> unsafeCrashWith "non-immediate static in block constant is impossible"
        Just immBlkCst
          | isToplevel -> pure $ LEStatic $ SEConstBlock tag immBlkCst
          | otherwise -> do
              ref <- pushStaticAnon (SEConstBlock tag immBlkCst)
              pure $ LEStatic (SEImm $ ImmRef ref)

constImm :: AtomicConstant -> Either StaticExpr StaticImm
constImm = case _ of
  ACInt i -> Right $ ImmUnboxedInt i
  ACBool b -> Right $ ImmUnboxedInt (if b then 1 else 0)
  ACChar ch -> Right $ ImmUnboxedInt (fromEnum ch)
  ACUnit -> Right $ ImmNil
  ACNumber num -> Left $ SEConstNumber num
  ACString str -> Left $ SEConstString str