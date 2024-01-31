module Purvasm.Backend.Codegen where

import Prelude
import Prim hiding (Function)

import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.State (State, StateT, evalState, evalStateT, get, modify_, runState)
import Control.Parallel (parSequence)
import Data.Array (length, (..))
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Backend.Types (CodeBlock, Ident(..), Instruction(..), Label(..), ModuleName, ObjectFile(..), SymbolDesc, SymbolType(..))
import Purvasm.MiddleEnd (Arity, ELambda(..), Primitive(..), Var(..))
import Purvasm.MiddleEnd as ME
import Safe.Coerce (coerce)

type SymbolsManagerState =
  { tbl :: Array SymbolDesc
  , dataOfs :: Int
  , textOfs :: Int
  }

type CodegenEnv =
  { moduleName :: ModuleName
  , symMngr :: AVar SymbolsManagerState
  }

type CodegenState =
  { stack :: List { label :: Label, arity :: Arity, function :: ELambda }
  , nextLabel :: Int
  -- , code :: Array CodeBlock
  }

type Codegen a = ReaderT CodegenEnv (StateT CodegenState Aff) a

newLabel :: Codegen Label
newLabel = do
  st <- get
  modify_ (_ { nextLabel = st.nextLabel + 1 })
  pure (Label st.nextLabel)

addToCompile :: Arity -> ELambda -> Codegen Label
addToCompile arity function = do
  label <- newLabel
  let newEntry = { label, arity, function }
  modify_ (\st -> st { stack = L.Cons newEntry st.stack })
  pure label

typeOfPhrase :: ELambda -> SymbolType
typeOfPhrase = case _ of
  ELVar _ -> unsafeCrashWith "Impossible: toplevel phrase is ELVar."
  ELFunction arity _ -> Function arity
  _ -> Value

compileToplevelPhrase :: Ident -> ELambda -> Codegen { ident :: Ident, typ :: SymbolType, datasec :: CodeBlock, textsec :: CodeBlock }
compileToplevelPhrase ident lambda = do
  { moduleName } <- ask
  let
    cont = L.singleton $ KSetGlobal moduleName ident
    typ = typeOfPhrase lambda
  datasec <- compileExpr Nothing cont lambda
  textsec <- compileRest Nil
  pure { ident, typ, datasec, textsec }
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

compileExpr :: Maybe Label -> CodeBlock -> ELambda -> Codegen CodeBlock
compileExpr handler = go
  where
  go cont = case _ of
    ELConst cst -> pure (KQuote cst : cont)
    ELVar (Var n) -> pure (KAccess n : cont)
    ELApply body args -> case cont of
      KReturn : c' -> do
        cbody <- go (KTermApply : c') body
        compileExprList (KPush : cbody) args
      _ -> do
        cbody <- go (KApply : cont) body
        code <- compileExprList (KPush : cbody) args
        pure (KPushMark : code)
    ELFunction arity body
      | isTail cont -> do
          let grabN = flip (Array.foldr (\_ c -> KGrab : c)) (1 .. arity)
          grabN <$> go cont body
      | otherwise -> do
          label <- addToCompile arity body
          pure (KClosure label : cont)
    ELlet args body -> do
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
    ELPrim (PGetGlobal modname ident) _ ->
      pure (KGetGlobal (translModuleName modname) (translIdent ident) : cont)
    ELPrim (PSetGlobal modname ident) _ ->
      pure (KSetGlobal (translModuleName modname) (translIdent ident) : cont)
    ELPrim p exprList -> do
      compileExprList (Kprim p : cont) exprList
    _ -> unsafeCrashWith "Not implemented!"

  compileExprList :: CodeBlock -> Array ELambda -> Codegen CodeBlock
  compileExprList cont = Array.uncons >>> case _ of
    Nothing -> pure cont
    Just { head, tail }
      | Array.null tail -> go cont head
      | otherwise -> do
          c' <- go cont head
          compileExprList (KPush : c') tail

compileModule :: ME.Module -> Aff ObjectFile
compileModule (ME.Module m@{ decls }) = do
  symMngr <- AVar.new { tbl: [], dataOfs: 0, textOfs: 0 }
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
        { typ, datasec, textsec } <- flip evalStateT initialState $
          runReaderT (compileToplevelPhrase toplevelSymbol bound) env
        -- tbl <- AVar.take symTbl
        -- AVar.put (Array.snoc tbl {}) symTbl
        pure
          { symbol: toplevelSymbol
          , datasec
          , textsec
          }
    )

  -- generate object code file
  pure $ ObjectFile $
    { head:
        { name: moduleName
        , pursVersion: "0.15.14"
        , version: "0.1.0"
        }
    , symbols: []
    , datasec: []
    , textsec: []
    }

pushNewSymbol :: Ident -> SymbolType -> Codegen Unit
pushNewSymbol name typ = do
  env <- ask
  symMngr <- liftAff $ AVar.take env.symMngr
  pure unit

translIdent :: ME.Ident -> Ident
translIdent = coerce

translModuleName :: ME.ModuleName -> ModuleName
translModuleName = coerce

isTail :: CodeBlock -> Boolean
isTail = case _ of
  KReturn : _ -> true
  _ -> false