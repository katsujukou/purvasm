-- | `census apply`: the ADR-0108 §2 static call census over a program's compilation closure.
-- |
-- | Same shape as `census byneed` and for the same reason: the run is driven by
-- | `Purvasm.Compiler.build` with the CLI's own `CompilerAction` (only the two emission
-- | capabilities overridden), so the module set, the `ulib` overlay, the FSR thread and the
-- | optimiser mode are the BUILD's. What differs is only the backend — here one that emits each
-- | object's classification events instead of its `.ll`, from the same emission.
module Purvasm.Census.Apply.Cmd
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
import Purvasm.Census.Apply.Backend (applyCensusBackend)
import Purvasm.Census.Apply.Report (header)
import Purvasm.Compiler (build)
import Purvasm.Compiler.Backend.LLVM.Abi (defaultHeapWords)
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
        "Path of the TSV report to write. Defaults to './apply-census.tsv'."
        # ArgParser.default "apply-census.tsv"
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
        "Census the entry as a bare value rather than an `Effect` (matches `purvasm build --value`)."
        # ArgParser.boolean
  , noOpt:
      ArgParser.flag [ "--no-opt" ]
        "Census the optimiser-free lowering (matches `purvasm build --no-opt`). The census must be\n\
        \taken in the SAME mode as the build it is reconciled against: dispatch populations differ\n\
        \by construction between the two modes."
        # ArgParser.boolean
  }

cmd :: forall r. Options -> Run (ENV + LOG + FS + PROC + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  Log.info $ Fmt.fmt @"Apply census from entry {mod}.{name} ({mode})"
    { mod: opts.entryModule, name: opts.entryName, mode: if opts.noOpt then "--no-opt" else "--opt" }
  ulibDir <- requireUlibDir
  FS.mkdirP opts.workDir
  modIdx <- liftEffect (Ref.new 0)
  irBuf <- liftEffect (Ref.new [])
  rows <- liftEffect (Ref.new [])
  fsEnv <- ForeignSigs.loadEnv { ulibDir, corefnDir: opts.corefnDir }
  let
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
      }

    recordRows label ir = liftEffect $ Ref.modify_
      (\rs -> rs <> map (\l -> label <> "\t" <> l) (Array.filter (_ /= "") (String.split (Pattern "\n") ir)))
      rows

    -- Object indices follow the build's own emission order (`emitFile` is called once per object,
    -- in order), so report row `i` pairs with the build's `mod_<i>.ll`.
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

    backendOpts =
      { isEffect: not opts.value
      , heapWords: defaultHeapWords
      , debug: false
      , profileApply: false
      -- the ADR-0107 lattice stays ON: the census must describe the emission that ships.
      , byNeed: true
      }
  build (applyCensusBackend backendOpts) action buildOpts >>= case _ of
    Left err -> throw (Build.renderBuildError err)
    Right products -> do
      out <- liftEffect (Ref.read rows)
      FS.writeText opts.outFile (joinWith "\n" (Array.cons ("#index\t" <> String.drop 1 header) out) <> "\n")
      Log.info $ Fmt.fmt @"✓ censused {n} object(s) (+ entry) → {out}"
        { n: show (Array.length products.modules), out: opts.outFile }
