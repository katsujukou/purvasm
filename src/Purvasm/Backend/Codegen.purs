module Purvasm.Backend.Codegen where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (State, evalState, get, modify_, runState)
import Control.Parallel (parSequence)
import Data.Array (length, (..))
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Backend.Instruction (BackendCode, Instruction(..))
import Purvasm.Backend.ObjectFile (ObjectFile(..))
import Purvasm.Backend.Translate (translIdent, translModuleName)
import Purvasm.Backend.Types (Label(..), ModuleName)
import Purvasm.MiddleEnd.Syntax (ELambda(..), Primitive(..))
import Purvasm.MiddleEnd.Syntax as ME
import Purvasm.MiddleEnd.Types (Arity, Var(..))

type CodegenEnv =
  { moduleName :: ModuleName
  }

type CodegenState =
  { stack :: List { label :: Label, arity :: Arity, function :: ELambda }
  , nextLabel :: Int
  -- , code :: Array BackendCode
  }

type Codegen a = ReaderT CodegenEnv (State CodegenState) a

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

-- register :: BackendCode -> Codegen Unit
-- register code = do
--   modify_ (\st -> st { code = Array.cons code st.code })

-- compileModule :: Module -> CodegenState
-- compileModule (Module m@{ name: moduleName }) =
--   let
--     env = { moduleName }
--     initialState = { stack: Nil, nextLabel: 0, code: [] }
--   in
--     runState (runReaderT (tailRecM go (m.decls /\ [])) env) initialState
--   where
--   go (decls /\ compiled) = case Array.uncons decls of
--     Nothing -> do
--       { stack } <- get
--       case stack of
--         L.Nil -> pure (MonadRec.Done unit)
--         c : rest -> do
--           modify_ (\st -> st { stack = rest })
--           pure $ MonadRec.Loop [ c ]
--     Just { head: { lambda }, tail: rest } -> do
--       compileLambda Nil lambda
--       pure $ MonadRec.Loop rest

compileLambda :: ELambda -> Codegen { toplevel :: BackendCode, closures :: BackendCode }
compileLambda lambda = do
  toplevel <- compileExpr Nothing (L.singleton (Kbranch (Label 0))) lambda
  closures <- compileRest Nil
  pure { toplevel, closures }
  where
  compileRest code = do
    get >>= \{ stack } -> case stack of
      Nil -> pure code
      ({ label, arity, function } : rest)
        | arity == 1 -> do
            modify_ (\st -> st { stack = rest })
            compiled <- compileExpr Nothing (KReturn : code) function
            compileRest (KLabel label : KStartFun : compiled)
        | otherwise -> unsafeCrashWith "Oops!"

compileExpr :: Maybe Label -> BackendCode -> ELambda -> Codegen BackendCode
compileExpr handler = go
  where
  -- go :: (BackendCode -> BackendCode) -> BackendCode -> ELambda -> Codegen (MonadRec.Step _ BackendCode)
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
      pure (KGetGlobal modname ident : cont)
    ELPrim p exprList -> do
      compileExprList (Kprim p : cont) exprList
    _ -> unsafeCrashWith "Not implemented!"

  compileExprList :: BackendCode -> Array ELambda -> Codegen BackendCode
  compileExprList cont = Array.uncons >>> case _ of
    Nothing -> pure cont
    Just { head, tail }
      | Array.null tail -> go cont head
      | otherwise -> do
          c' <- go cont head
          compileExprList (KPush : c') tail

compileModule :: ME.Module -> Aff ObjectFile
compileModule (ME.Module m@{ decls }) = do
  let
    moduleName = translModuleName m.name
    env = { moduleName }
  phrases <- parSequence
    ( decls <#> \({ name, lambda }) -> do
        let
          initialState =
            { stack: L.Nil
            , nextLabel: 0
            }
          compiled = flip evalState initialState $
            runReaderT (compileLambda lambda) env
        pure $ (translIdent name) /\ compiled
    )
  pure (ObjectFile { name: moduleName, phrases })

isTail :: BackendCode -> Boolean
isTail = case _ of
  KReturn : _ -> true
  _ -> false