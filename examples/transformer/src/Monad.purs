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

type Env = { debug :: Boolean, enthusiasm :: Enthusiasm }

type State = { energy :: Int }

data Error = OutOfEnergy

newtype AppM a = AppM (ExceptT Error (ReaderT Env (StateT State Effect)) a)

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

runAppM :: forall a. Env -> State -> AppM a -> Effect (Tuple (Either Error a) State)
runAppM env state (AppM m) = runStateT (runReaderT (runExceptT m) env) state

consume :: AppM Unit
consume = AppM do
  s0 <- get
  { enthusiasm } <- ask
  let
    s1 = case enthusiasm of
      Passionate -> s0 { energy = s0.energy - 3 }
      Mild -> s0 { energy = s0.energy - 1 }
      None -> s0
  put s1
  when (s1.energy < 0) do
    Console.log "I've exhausted. Please let me rest..."
    throwError OutOfEnergy

speak :: String -> AppM Unit
speak str = do
  env@{ enthusiasm } <- ask
  s0 <- get
  when (env.debug) $ Console.logShow s0
  if s0.energy >= 0 then do
    Console.log $ str <> punctuaion enthusiasm
    consume
  else do
    Console.log "(Cannot speak more...)"
  where
  punctuaion = case _ of
    Passionate -> "!!"
    Mild -> "!"
    None -> "."

sleep :: AppM Unit
sleep = AppM do
  Console.log "I'm going to sleep for a while..."
  s0 <- get
  put (s0 { energy = s0.energy + 5 })