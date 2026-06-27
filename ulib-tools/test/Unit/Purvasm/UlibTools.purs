module Test.Unit.Purvasm.UlibTools (main) where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Purvasm.UlibTools.Manifest as Manifest
import Test.Unit.Purvasm.UlibTools.Stage as Stage
import Test.Unit.Purvasm.UlibTools.Verify as Verify

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Manifest.spec
  Stage.spec
  Verify.spec
