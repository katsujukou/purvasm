module Example.Transformer.Main where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Either (either)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Exception as Exn
import Example.Transformer.Monad (AppM, Enthusiasm(..), Error(..), runAppM, sleep, speak)

run :: forall a. AppM a -> Effect a
run = runAppM env initialState >=> either (describe >>> Exn.throw) (fst >>> pure)
  where
  env = { enthusiasm: Passionate }
  initialState = { energy: 5 }
  describe = case _ of 
    OutOfEnergy -> "Out of energy!"

main :: Effect Unit
main = run do
  (do 
    speak "Hello"
    speak "I'm a Purvasm Native program"
    speak "Really cool app"
  ) `catchError` handleOutOfEnergy
  speak "I feel refreshed now, let's keep going!"
  where
  handleOutOfEnergy = case _ of
    OutOfEnergy -> do
      speak "I'm out of energy, need to sleep..."
      sleep