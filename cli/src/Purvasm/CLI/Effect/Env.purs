module Purvasm.CLI.Effect.Env
  ( Env(..)
  , ENV
  , _env
  , interpret
  , lookupEnv
  ) where

import Prelude

import Data.Maybe (Maybe)
import Run (Run, on, send)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Env a = LookupEnv String (Maybe String -> a)

derive instance functorEnv :: Functor Env

type ENV r = (env :: Env | r)

_env :: Proxy "env"
_env = Proxy

interpret :: forall r a. (Env ~> Run r) -> Run (ENV + r) a -> Run r a
interpret handler = Run.interpret (on _env handler send)

-- | Read an environment variable; `Nothing` if it is unset (or empty, per the interpreter).
lookupEnv :: forall r. String -> Run (ENV + r) (Maybe String)
lookupEnv name = Run.lift _env (LookupEnv name identity)
