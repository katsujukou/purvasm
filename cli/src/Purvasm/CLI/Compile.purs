module Purvasm.CLI.Compile where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Fmt as Fmt
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Module (Module)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Ulib (corefnPathFor, requireUlibDir)
import Purvasm.Compiler.Bytecode.Artifact (interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Compile (compileModule)
import Run (Run, EFFECT)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { targetModule :: String
  , outDir :: FilePath
  , corefnDir :: FilePath
  , quiet :: Boolean
  }

options :: ArgParser.ArgParser Options
options = ArgParser.fromRecord
  { corefnDir:
      ArgParser.argument [ "--corefn-dir" ]
        "Path to the PureScript compiler's output directory.\n\
        \Defaults to './output'."
        # ArgParser.default "output"
  , outDir:
      ArgParser.argument [ "--outdir" ]
        "Path to the output directory the compiled artifacts are placed in.\n\
        \Defaults to './output-pvm'."
        # ArgParser.default "output-pvm"
  , targetModule:
      ArgParser.anyNotFlag "MODULE"
        "Name of the module to compile."
  , quiet:
      ArgParser.flag [ "--quiet" ]
        "Suppress any message output to stdout"
        # ArgParser.boolean
  }

-- | Compile a single module to its `.pmo`/`.pmi` objects (the counterpart of boot's
-- | `purvm compile`): read `<corefnDir>/<entryModule>/corefn.json`, decode it, run
-- | `compileModule`, and write the artifacts under `<outDir>/_build`. Artifact correctness is
-- | anchored by the ADR-0104 goldens (§4) and the behavioural/fixpoint gates (§2), not by boot
-- | byte-identity.
cmd :: forall r. Options -> Run (ENV + LOG + FS + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  info $ Fmt.fmt @"Compiling {modname}" { modname: opts.targetModule }
  -- Resolve the module through the `ulib` overlay first (the patched module wins, ADR-0055),
  -- falling back to `--corefn-dir`. A missing overlay is a hard error (ADR-0055 correction).
  ulibDir <- requireUlibDir
  corefnPath <- corefnPathFor ulibDir opts.corefnDir opts.targetModule
  src <- FS.readText corefnPath >>= maybe
    (throw $ Fmt.fmt @"Failed to read corefn.json at {corefnPath}" { corefnPath })
    pure
  case parseModule src of
    Left err -> throw err
    Right m -> do
      let artifact = compileModule m
      buildDir <- FS.joinPath [ opts.outDir, "_build" ]
      FS.mkdirP buildDir
      pmofilePath <- FS.joinPath [ buildDir, artifact.name <> ".pmo" ]
      pmifilePath <- FS.joinPath [ buildDir, artifact.name <> ".pmi" ]
      FS.writeText pmofilePath (moduleToString artifact)
      FS.writeText pmifilePath (interfaceToString (interfaceOf artifact))
      info "✓ Compile finished"

  where
  info log = do
    unless opts.quiet $ Log.info log

parseModule :: String -> Either String Module
parseModule src = do
  json <- jsonParser src
  lmap printJsonDecodeError (decodeModule json)
