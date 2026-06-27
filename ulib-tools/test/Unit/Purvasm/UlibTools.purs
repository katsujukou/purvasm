module Test.Unit.Purvasm.UlibTools (main) where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Purvasm.UlibTools.Build as Build

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Build.spec
