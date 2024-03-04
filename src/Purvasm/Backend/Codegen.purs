module Purvasm.Backend.Codegen where

import Prelude
import Prim hiding (Function)

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, modify_)
import Control.Parallel (parSequence)
import Data.Array (length, (..))
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Backend.Instruction (CodeBlock, Instruction(..))
import Purvasm.Backend.PmoFile (PmoFile(..), SymbolDesc, SymbolType(..))
import Purvasm.Backend.Types (Arity, Ident, Label(..), ModuleName, Primitive(..), mkGlobalName)
import Purvasm.MiddleEnd (NCore(..), Var(..))
import Purvasm.MiddleEnd as ME
import Safe.Coerce (coerce)

type SymbolsManagerState =
  { tbl :: Map Ident SymbolDesc
  , dataNext :: Int
  , textNext :: Int
  }

type CodegenEnv =
  { moduleName :: ModuleName
  , symMngr :: AVar SymbolsManagerState
  }

type CodegenState =
  { stack :: List { label :: Label, arity :: Arity, function :: NCore }
  , nextLabel :: Int
  -- , code :: Array CodeBlock
  }

type Codegen a = ReaderT CodegenEnv (StateT CodegenState Aff) a

newLabel :: Codegen Label
newLabel = do
  st <- get
  modify_ (_ { nextLabel = st.nextLabel + 1 })
  pure (Label st.nextLabel)

addToCompile :: Arity -> NCore -> Codegen Label
addToCompile arity function = do
  label <- newLabel
  let newEntry = { label, arity, function }
  modify_ (\st -> st { stack = L.Cons newEntry st.stack })
  pure label

typeOfPhrase :: NCore -> SymbolType
typeOfPhrase = case _ of
  NCVar _ _ -> unsafeCrashWith "Impossible: toplevel phrase is NCVar."
  NCFunction arity _ -> Function arity
  _ -> Value

compileToplevelPhrase :: Ident -> NCore -> Codegen (CodeBlock /\ CodeBlock)
compileToplevelPhrase ident lambda = do
  { moduleName } <- ask
  let
    cont = L.singleton $ KSetGlobal (mkGlobalName moduleName ident)
    typ = typeOfPhrase lambda
  datasec <- compileExpr Nothing cont lambda
  textsec <- compileRest Nil
  pushNewSymbol ident typ (L.length textsec > 0)
  pure $ datasec /\ textsec
  where
  compileRest code = do
    get >>= \{ stack } -> case stack of
      Nil -> pure code
      ({ label, arity, function } : rest)
        | arity == 1 -> do
            modify_ (\st -> st { stack = rest })
            compiled <- compileExpr Nothing (KReturn : code) function
            compileRest (KLabel label : KStartFun : compiled)
        | otherwise -> do
            modify_ (\st -> st { stack = rest })
            compiled <- compileExpr Nothing (KReturn : code) function
            compileRest $
              KLabel label : KStartFun :
                (Array.foldr (\_ -> (KGrab : _)) compiled (1 .. (arity - 1)))

compileExpr :: Maybe Label -> CodeBlock -> NCore -> Codegen CodeBlock
compileExpr handler = go
  where
  go cont = case _ of
    NCConst cst -> pure (KQuote cst : cont)
    NCVar _ (Var n) -> pure (KAccess n : cont)
    NCApply body args -> case cont of
      KReturn : c' -> do
        cbody <- go (KTermApply : c') body
        compileExprList (KPush : cbody) args
      _ -> do
        cbody <- go (KApply : cont) body
        code <- compileExprList (KPush : cbody) args
        pure (KPushMark : code)
    NCFunction arity body
      | isTail cont -> do
          let grabN = flip (Array.foldr (\_ c -> KGrab : c)) (1 .. arity)
          grabN <$> go cont body
      | otherwise -> do
          label <- addToCompile arity body
          pure (KClosure label : cont)
    NClet args body -> do
      let
        c1 =
          if isTail cont then cont
          else KEndLet (length args) : cont
        compArgs cont' = Array.uncons >>> case _ of
          Nothing -> pure cont'
          Just { head, tail } -> do
            compiledArgs <- compArgs cont' tail
            go (KLet : compiledArgs) head
      c2 <- go c1 body
      compArgs c2 args
    NCPrim (PGetGlobal globalName) _ ->
      pure (KGetGlobal globalName : cont)
    NCPrim (PSetGlobal globalName) _ ->
      pure (KSetGlobal globalName : cont)
    NCPrim p exprList -> do
      compileExprList (Kprim p : cont) exprList
    _ -> unsafeCrashWith "Not implemented!"

  compileExprList :: CodeBlock -> Array NCore -> Codegen CodeBlock
  compileExprList cont = Array.uncons >>> case _ of
    Nothing -> pure cont
    Just { head, tail }
      | Array.null tail -> go cont head
      | otherwise -> do
          c' <- go cont head
          compileExprList (KPush : c') tail

compileModule :: ME.Program -> Aff PmoFile
compileModule (ME.Program m@{ decls }) = do
  symMngr <- AVar.new { tbl: Map.empty, dataNext: 0, textNext: 0 }
  let
    moduleName = translModuleName m.name
    env = { moduleName, symMngr }

  phrases <- parSequence
    ( decls <#> \({ name, lambda: bound }) -> do
        let
          toplevelSymbol = translIdent name
          initialState =
            { stack: L.Nil
            , nextLabel: 0
            }
        flip evalStateT initialState $
          runReaderT (compileToplevelPhrase toplevelSymbol bound) env
    )

  symbols <- AVar.take symMngr <#> _.tbl

  -- generate object code file
  pure $ PmoFile $
    { head:
        { name: moduleName
        , pursVersion: "0.15.14"
        , version: "0.1.0"
        }
    , symbols: Array.fromFoldable (Map.values symbols)
    , datasec: map fst phrases
    , textsec: map snd phrases # Array.filter ((_ > 0) <<< L.length)
    , refsec: []
    }

pushNewSymbol :: Ident -> SymbolType -> Boolean -> Codegen Unit
pushNewSymbol name typ hasText = do
  env <- ask
  liftAff do
    symMngr <- liftAff (AVar.take env.symMngr)
    let
      newEntry =
        { name
        , typ
        , dataOfs: symMngr.dataNext
        , textOfs: if hasText then symMngr.textNext else -1
        }
    env.symMngr # AVar.put
      ( symMngr
          { tbl = Map.insert name newEntry symMngr.tbl
          , dataNext = symMngr.dataNext + 1
          , textNext = symMngr.textNext + if hasText then 1 else 0
          }
      )

translIdent :: ME.Ident -> Ident
translIdent = coerce

translModuleName :: ME.ModuleName -> ModuleName
translModuleName = coerce

isTail :: CodeBlock -> Boolean
isTail = case _ of
  KReturn : _ -> true
  _ -> false