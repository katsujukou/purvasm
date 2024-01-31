module Main where

import Prelude

import Control.Monad.Rec.Class (tailRecM)
import Control.Monad.Rec.Class as MonadRec
import Control.Monad.State (StateT, execStateT, get, modify_)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (!!), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Backend.Codegen (compileModule)
import Purvasm.Backend.Instruction (Instruction(..))
import Purvasm.Backend.ObjectFile (ObjectFile(..), CodeBlock)
import Purvasm.Backend.Types as B
import Purvasm.MiddleEnd.Syntax (ELambda(..), Module(..), Primitive(..))
import Purvasm.MiddleEnd.Types (Ident(..), ModuleName(..), Var(..))
import Purvasm.Types (AtomicConstant(..), StructuredConstant(..))

program :: Module
program = Module
  { name: ModuleName "Sample"
  , decls:
      [ { name: Ident "add"
        , lambda:
            ELFunction 2
              ( ELPrim
                  (P_add_i32)
                  [ ELVar (Var 1)
                  , ELVar (Var 0)
                  ]
              )
        }
      , { name: Ident "inc"
        , lambda:
            ( ELApply
                (ELPrim (PGetGlobal (ModuleName "Sample") (Ident "add")) [])
                [ ELConst (SCAtom (ACInt 1))
                ]
            )
        }
      ]
  }

main :: Effect Unit
main = launchAff_ do
  Console.log "\x1b[1;32mpurvasm-v2 simulator\x1b[0m\n"
  objectCodeFile <- compileModule program
  logShow objectCodeFile
  sf <- execute objectCodeFile "add" Undefined
  Console.log "\n\x1b[1;34m[final state]\x1b[0m"
  Console.log (describe sf)

type Env = Array Value

type CodeAddr = Int
type Closure = { ofs :: CodeAddr, env :: List Value }

data Value
  = Const StructuredConstant
  | Ofs CodeAddr
  | Clos CodeAddr Env
  | Epsilon
  | Undefined

derive instance Generic Value _
instance Show Value where
  show v = genericShow v

type MachineState =
  { prg :: Map B.Ident CodeBlock -- object file
  , pgc :: CodeAddr -- program counter
  , acc :: Value -- accumulator
  , arg :: List Value -- arguments stack
  , env :: List Value -- environment stack
  , ret :: List Value -- return stack
  }

describe :: MachineState -> String
describe state = "[\x1b[33mACC\x1b[0m] = " <> show state.acc
  <> ("\n [\x1b[33mARG\x1b[0m] = " <> show (Array.fromFoldable state.arg))
  <> ("\n [\x1b[33mENV\x1b[0m] = " <> show state.env)
  <> ("\n [\x1b[33mRET\x1b[0m] = " <> show state.ret)
  <> ("\n [\x1b[33mPGC\x1b[0m] = " <> show state.pgc)
  <> "\n"

type Simulator a = StateT MachineState Aff a

execute :: ObjectFile -> Aff MachineState
execute (ObjectFile { phrases }) entryPoint ini = do
  let
    entry =
      { ident: B.Ident entryPoint
      , ofs: Toplevel
      , index: 0
      }
  execStateT eval (initialState (Map.fromFoldable phrases) ini entry)

  where
  initialState pg acc cp =
    { program: pg
    , acc
    , asp: 0
    , argStack: Nil
    , env: []
    , cp
    }

  eval :: StateT MachineState Aff Unit
  eval = tailRecM step unit

  step _ = do
    inst <- fetchCode
    case inst of
      KQuote sc -> do
        loadAcc (Const sc)
        continue
      -- KGetGlobal _ ident -> do
      --   pg <- get <#> _.program
      --   case Map.lookup ident pg of
      --     Nothing -> throwError (error $ "Unknown ident" <> show ident)
      --     Just code -> do
      --       loadAcc (CodePointer )
      KPush -> do
        acc >>= push
        continue
      KPushMark -> do
        push Epsilon
        continue
      _ -> pure $ MonadRec.Done unit

  loop = pure (MonadRec.Loop unit)

  continue = proceedIndex *> loop

  proceedIndex :: Simulator Unit
  proceedIndex = do
    modify_ \st -> st { cp = st.cp { index = st.cp.index + 1 } }

  acc :: Simulator Value
  acc = get >>= _.acc >>> pure

  loadAcc :: Value -> Simulator Unit
  loadAcc acc = modify_ \st -> st { acc = acc }

  push :: Value -> Simulator Unit
  push v = modify_ \st -> st { argStack = v : st.argStack }

  fetchCode :: Simulator Instruction
  fetchCode = do
    state@{ cp } <- get
    let
      mbInst =
        case Map.lookup cp.ident state.program of
          Nothing -> unsafeCrashWith "Unknown ident"
          Just { closures, toplevel }
            | Toplevel <- state.cp.ofs -> toplevel !! state.cp.index
            | otherwise -> closures !! state.cp.index
    case mbInst of
      Nothing -> unsafeCrashWith "Out of range!"
      Just inst -> pure inst