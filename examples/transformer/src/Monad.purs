module Example.Transformer.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, get, put, runStateT)
import Data.Either (Either)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console

data Enthusiasm = Passionate | Mild | None

type Env = { enthusiasm :: Enthusiasm }

type State = { energy :: Int }

data Error = OutOfEnergy

newtype AppM a = AppM (ReaderT Env (StateT State (ExceptT Error Effect)) a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAsk Env AppM
derive newtype instance MonadState State AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadError Error AppM

runAppM :: forall a. Env -> State -> AppM a -> Effect (Either Error (Tuple a State))
runAppM env state (AppM m) = runExceptT (runStateT (runReaderT m env) state)

speak :: String -> AppM Unit
speak str = AppM do
  { enthusiasm } <- ask
  s0 <- get
  let
    msg /\ s1 = case enthusiasm of
      Passionate -> (str <> "!!") /\ (s0 { energy = s0.energy - 3 })
      Mild -> (str <> "!") /\ (s0 { energy = s0.energy - 1 })
      None -> (str <> ".") /\ s0

  Console.log msg
  when (s1.energy < 0) do
    put { energy: 0 }
    throwError OutOfEnergy

sleep :: AppM Unit
sleep = AppM do
  s0 <- get
  let s1 = s0 { energy = s0.energy + 5 }
  put s1