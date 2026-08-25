-- | `census byneed`: run the ADR-0107 by-need demand-site census over a program's compilation
-- | closure and write the per-object TSV report.
-- |
-- | The census drives `Purvasm.Compiler.build` with the census `Backend` and the **CLI's own**
-- | `CompilerAction` (`Purvasm.CLI.Build.mkAction`) — only the two emission capabilities are
-- | overridden, so module loading, the `ulib` overlay, the FSR thread and the optimiser mode are the
-- | build's, not a re-implementation. That is the reproducibility property: an object appears in the
-- | report iff the compiler compiled it, and the report's object order is the build's emission order
-- | (`mod_<i>.ll` ↔ report row `i`), which is what lets the reconciliation pair census sites against
-- | emitted force chains per object.
module Purvasm.Census.ByNeed.Cmd
  ( Options
  , options
  , cmd
  ) where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Common (joinWith)
import Effect.Ref as Ref
import Fmt as Fmt
import Purvasm.CLI.Build as Build
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.ForeignSigs as ForeignSigs
import Purvasm.CLI.Ulib (requireUlibDir)
import Purvasm.Census.ByNeed.Backend (censusBackend)
import Purvasm.Census.ByNeed.Report (header)
import Purvasm.Compiler (build)
import Run (EFFECT, Run, liftEffect)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { corefnDir :: FilePath
  , outFile :: FilePath
  , workDir :: FilePath
  , entryModule :: String
  , entryName :: String
  , value :: Boolean
  , noOpt :: Boolean
  }

options :: ArgParser Options
options = fromRecord
  { corefnDir:
      ArgParser.argument [ "--corefn-dir" ]
        "Path to the PureScript compiler's output directory. Defaults to './output'."
        # ArgParser.default "output"
  , outFile:
      ArgParser.argument [ "--out" ]
        "Path of the TSV report to write. Defaults to './byneed-census.tsv'."
        # ArgParser.default "byneed-census.tsv"
  , workDir:
      ArgParser.argument [ "--workdir" ]
        "Scratch directory for the driver's hook outputs. Defaults to './.census-build'."
        # ArgParser.default ".census-build"
  , entryModule:
      ArgParser.argument [ "--entry" ]
        "Entry module of the program to census. Defaults to `Main`."
        # ArgParser.default "Main"
  , entryName:
      ArgParser.argument [ "--entry-name" ]
        "Entry binding within the entry module. Defaults to `main`."
        # ArgParser.default "main"
  , value:
      ArgParser.flag [ "--value" ]
        "Census the entry as a bare value rather than an `Effect` (matches `purvasm build --value`;\n\
        \a value entry forces its result in the entry stub, an effect entry does not)."
        # ArgParser.boolean
  , noOpt:
      ArgParser.flag [ "--no-opt" ]
        "Census the optimiser-free lowering (matches `purvasm build --no-opt`). The census must be\n\
        \taken in the SAME mode as the build it is reconciled against: the two modes have different\n\
        \demand-site populations."
        # ArgParser.boolean
  }

cmd :: forall r. Options -> Run (ENV + LOG + FS + PROC + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  Log.info $ Fmt.fmt @"By-need census from entry {mod}.{name} ({mode})"
    { mod: opts.entryModule, name: opts.entryName, mode: if opts.noOpt then "--no-opt" else "--opt" }
  ulibDir <- requireUlibDir
  FS.mkdirP opts.workDir
  modIdx <- liftEffect (Ref.new 0)
  irBuf <- liftEffect (Ref.new [])
  rows <- liftEffect (Ref.new [])
  fsEnv <- ForeignSigs.loadEnv { ulibDir, corefnDir: opts.corefnDir }
  let
    -- The build options the CLI action closes over. Only the fields `mkAction` reads matter here
    -- (`corefnDir` for loading, `optMaxIter` for the fixpoint bound, `emitIr` for the trace hooks);
    -- the link-only knobs are the CLI's own defaults, since the census never links.
    buildOptions :: Build.Options
    buildOptions =
      { corefnDir: opts.corefnDir
      , outDir: opts.workDir
      , entryModule: opts.entryModule
      , entryName: opts.entryName
      , value: opts.value
      , checkForeignSigs: false
      , noOpt: opts.noOpt
      , emitLlvm: true
      , emitIr: Nothing
      , optMaxIter: Build.optMaxIterCap
      , runtimeLib: Nothing
      , rustFfi: Nothing
      -- The census stops at `--emit-llvm`, and nothing loads a provider into what it does not link
      -- (ADR-0111 §1.1 is for a program that hosts `dlopen`ed modules — the VM).
      , hostForeignApi: false
      }

    recordRows label ir = liftEffect $ Ref.modify_
      (\rs -> rs <> map (\l -> label <> "\t" <> l) (Array.filter (_ /= "") (String.split (Pattern "\n") ir)))
      rows

    action = (Build.mkAction buildOptions ulibDir opts.workDir fsEnv modIdx irBuf)
      { emitFile = \artifact -> do
          i <- liftEffect (Ref.read modIdx)
          liftEffect (Ref.modify_ (_ + 1) modIdx)
          recordRows (show i) artifact.backendIR
          pure (show i)
      , emitEntry = \ir -> do
          recordRows "entry" ir
          pure "entry"
      }

    buildOpts =
      { entryModule: opts.entryModule
      , entryName: opts.entryName
      , isEffect: not opts.value
      , opt: not opts.noOpt
      }
  build (censusBackend (not opts.value)) action buildOpts >>= case _ of
    Left err -> throw (Build.renderBuildError err)
    Right products -> do
      out <- liftEffect (Ref.read rows)
      FS.writeText opts.outFile (joinWith "\n" (Array.cons ("#index\t" <> String.drop 1 header) out) <> "\n")
      Log.info $ Fmt.fmt @"✓ censused {n} object(s) (+ entry) → {out}"
        { n: show (Array.length products.modules), out: opts.outFile }
