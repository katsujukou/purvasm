module Test.Unit.Json.Core (main) where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Json.Core.Parser as Parser
import Test.Unit.Json.Core.Printer as Printer
import Test.Unit.Json.Core.Utf8 as Utf8

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Parser.spec
  Printer.spec
  Utf8.spec
