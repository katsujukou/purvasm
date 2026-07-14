module Purvasm.UlibTools.Options where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Either (Either)
import Purvasm.UlibTools.Build as Build
import Purvasm.UlibTools.Test as Test
import Purvasm.UlibTools.UnicodeGen as UnicodeGen
import Purvasm.UlibTools.Verify as Verify
import Purvasm.UlibTools.VerifyDeps as VerifyDeps

data Command
  = Build Build.Options
  | Verify Verify.Options
  | VerifyDeps VerifyDeps.Options
  | Test Test.Options
  | UnicodeGen UnicodeGen.Options

command :: ArgParser Command
command =
  ArgParser.choose "COMMAND"
    [ ArgParser.command [ "build" ]
        "Compile the ulib patches to corefn (supersedes install.sh)."
        ((Build <$> Build.options) <* ArgParser.flagHelp)
    , ArgParser.command [ "verify" ]
        "Check ulib patch interface-faithfulness against the registry module."
        ((Verify <$> Verify.options) <* ArgParser.flagHelp)
    , ArgParser.command [ "verify-deps" ]
        "Verify patch dependencies: resolvable (in-repo/registry) and acyclic."
        ((VerifyDeps <$> VerifyDeps.options) <* ArgParser.flagHelp)
    , ArgParser.command [ "test" ]
        "Run ulib behaviour tests by representation-seam fidelity."
        ((Test <$> Test.options) <* ArgParser.flagHelp)
    , ArgParser.command [ "unicode-gen" ]
        "Fetch, pin, and generate Data.String.Internal.CaseMap from UnicodeData.txt (ADR-0101)."
        ((UnicodeGen <$> UnicodeGen.options) <* ArgParser.flagHelp)
    ]
    <* ArgParser.flagHelp

parse :: Array String -> Either ArgParser.ArgError Command
parse =
  ArgParser.parseArgs "ulib-tools"
    "Build, verify, and test the ulib registry-package patches (ADR-0043)."
    command
