module Main where

import Prelude

import Control.Monad.Rec.Class (tailRecM)
import Control.Monad.Rec.Class as MonadRec
import Control.Monad.State (StateT, execStateT, get, modify_)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (!!))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
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
      [ { name: Ident "f"
        , lambda:
            ELFunction 1
              ( ELlet
                  [ ELFunction 1
                      ( ELPrim (P_add_i32) [ ELVar (Var 0), ELConst (SCAtom (ACInt 1)) ]
                      )
                  ]
                  ( ELApply
                      (ELVar (Var 0))
                      [ ELVar (Var 1)
                      ]
                  )
              )
        }
      , { name: Ident "main"
        , lambda:
            ELApply
              (ELPrim (PGetGlobal "Sample" "f") [])
              [ ELConst (SCAtom (ACInt 42)) ]
        }
      ]
  }

main :: Effect Unit
main = launchAff_ do
  Console.log "\x1b[1;32mpurvasm-v2 simulator\x1b[0m\n"
  objectCodeFile <- compileModule program
  logShow objectCodeFile
  sf <- execute objectCodeFile "main" Undefined
  Console.log "\n\x1b[1;34m[final state]\x1b[0m"
  Console.log (describe sf)

data Value
  = Const StructuredConstant
  | Ofs Int
  | Undefined

derive instance Generic Value _
instance Show Value where
  show = genericShow

type MachineState =
  { program :: Array (B.Ident /\ CodeBlock)
  , cp :: CodePointer
  , acc :: Value
  , argStack :: List Value
  , env :: List Value
  }

describe :: MachineState -> String
describe { cp, acc, argStack, env } =
  " [\x1b[33mENV\x1b[0m] = " <> show (Array.fromFoldable env)
    <> ("\n [\x1b[33mACC\x1b[0m] = " <> show acc)
    <> ("\n [\x1b[33mASP\x1b[0m] = " <> show (Array.fromFoldable argStack))
    <> ("\n [\x1b[33mPGC\x1b[0m] = " <> show cp)
    <> "\n"

data Offset = Toplevel | Closures

instance Show Offset where
  show Toplevel = "Toplevel"
  show _ = "Closures"

type CodePointer =
  { ident :: B.Ident
  , ofs :: Offset
  , index :: Int
  }

type Simulator a = StateT MachineState Aff a

execute :: ObjectFile -> String -> Value -> Aff MachineState
execute (ObjectFile { phrases }) entryPoint ini = do
  let
    entry =
      { ident: B.Ident entryPoint
      , ofs: Toplevel
      , index: 0
      }
  execStateT eval (initialState phrases ini entry)

  where
  initialState pg acc cp =
    { program: pg
    , acc
    , argStack: Nil
    , env: Nil
    , cp
    }

  eval :: StateT MachineState Aff Unit
  eval = tailRecM step unit

  step _ = do
    inst <- fetchCode
    case inst of
      KPushMark -> do
        proceedIndex
        pure $ MonadRec.Loop unit
      _ -> pure $ MonadRec.Done unit

  proceedIndex :: Simulator Unit
  proceedIndex = do
    modify_ \st -> st { cp = st.cp { index = st.cp.index + 1 } }

  fetchCode :: Simulator Instruction
  fetchCode = do
    state <- get
    let
      mbInst =
        case Array.findMap (\(ident /\ ph) -> if ident == state.cp.ident then Just ph else Nothing) state.program of
          Nothing -> unsafeCrashWith "Unknown ident"
          Just { closures, toplevel }
            | Toplevel <- state.cp.ofs -> toplevel !! state.cp.index
            | otherwise -> closures !! state.cp.index
    case mbInst of
      Nothing -> unsafeCrashWith "Out of range!"
      Just inst -> pure inst