module Example.HelloWorld.Main where

import Prelude

import Effect (Effect)
import Purvasm.Stdio (writeLine)

main :: Effect Unit
main = do
  writeLine "Hello from Purvasm Native!"
