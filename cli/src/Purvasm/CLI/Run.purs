-- | `purvasm run`: the bytecode/VM path (ADR-0088). Drives the backend-neutral `Purvasm.Compiler.build`
-- | driver with the **bytecode** `Backend` (`bytecodeBackend`) and a CLI `CompilerAction` over the `Run`
-- | stack: `loadModule` reads a module's `corefn.json` (through the `ulib` overlay, ADR-0055), `emitFile`
-- | writes each module's `.pmo`/`.pmi` under `<outDir>/_build`. Finalisation links the artifacts into a
-- | single runnable `app.pvm` (`Link.link`, the pure per-target step, ADR-0087 §4). The VM consumes the
-- | seam's **optimised** output — `--opt` runs the real optimiser (the measurement field, ADR-0088) — and
-- | the emitted bytecode stays in boot's runnable `Image` shape (ADR-0088 §0 scope (a)); the byte-identity
-- | gate to boot is released, but the `.pmi` is unchanged.
module Purvasm.CLI.Run where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Fmt as Fmt
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Purvasm.CLI.Compile (parseModule)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.EmitIr (irHooks)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.ForeignSigs as ForeignSigs
import Purvasm.CLI.Ulib (corefnPathFor, requireUlibDir)
import Purvasm.Compiler (BuildError(..), CompilerAction, LoadResult(..), build, loadClosure)
import Purvasm.Compiler.Backend.Bytecode (bytecodeBackend)
import Purvasm.Compiler.Bytecode.Artifact (ModuleArtifact, interfaceToString, moduleToString)
import Purvasm.Compiler.Bytecode.Image (imageToString)
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.Translate (nameKey)
import Purvasm.Compiler.Ffi as Ffi
import Purvasm.Compiler.Link (link)
import Purvasm.Compiler.Literal (Literal(..))
import Run (EFFECT, Run, liftEffect)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { corefnDir :: FilePath
  , outDir :: FilePath
  , entryModule :: String
  , checkForeignSigs :: Boolean
  , noOpt :: Boolean
  , emitIr :: Maybe String
  }

options :: ArgParser Options
options = fromRecord
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
  , entryModule:
      ArgParser.argument [ "--entry" ]
        "Name of the entry module which contains `main`,\n\
        \the entry point of the whole program. Defaults to `Main`."
        # ArgParser.default "Main"
  , checkForeignSigs:
      ArgParser.flag [ "--check-foreign-sigs" ]
        "Extra diagnostic: reconstruct every module's foreign signatures up front and\n\
        \log the resolved count (ADR-0080). The build itself already reconstructs them\n\
        \per module (ADR-0090); this is an eager whole-closure sweep for the standing\n\
        \`foreign-sigs` command and tools/foreign-sigs-diff.sh. Off by default:\n\
        \source-channel lexing is expensive on the native backend."
        # ArgParser.boolean
  , noOpt:
      ArgParser.flag [ "--no-opt" ]
        "Disable the optimiser (the DictElim + NbE-inliner fixpoint, ADR-0086/0089); keep only\n\
        \normalisation (dictionaries stay applied). `--opt` runs the real optimiser over the\n\
        \ANF — the VM is the optimiser's effect-measurement field (ADR-0088)."
        # ArgParser.boolean
  , emitIr:
      ArgParser.argument [ "--emit-ir" ]
        "Trace the named module's per-round optimiser ANF to `<module>.ir` under the build\n\
        \directory (ADR-0087 §3.1). A trace, not a stop — the build still completes."
        # ArgParser.optional
  }

-- | The heuristic cap on optimiser fixpoint rounds (ADR-0087 §3.1), mirrored from the native build.
optMaxIter :: Int
optMaxIter = 10

-- | Render a `BuildError` as the CLI's `EXCEPT String` message.
renderBuildError :: BuildError -> String
renderBuildError = case _ of
  EntryMissing name -> "entry module not found: " <> name
  LoadFailed e -> Fmt.fmt @"{name}: {detail}" { name: e.moduleName, detail: e.detail }
  ForeignSigFailed e -> Fmt.fmt @"{name}: {detail}" { name: e.moduleName, detail: e.detail }

-- | The CLI `CompilerAction` for the bytecode build over the `Run` stack: single-module CoreFn loading
-- | (through the `ulib` overlay), per-module `.pmo`/`.pmi` emission, and the shared `--emit-ir` trace
-- | hooks. `emitEntry` is inert — the VM entry is the link-time `mainTerm` the finalisation below supplies.
mkAction
  :: forall r
   . Options
  -> FilePath
  -> FilePath
  -> ForeignSigs.Env
  -> Ref (Array String)
  -> CompilerAction ModuleArtifact (Run (ENV + LOG + FS + EXCEPT String + EFFECT + r))
mkAction opts ulibDir buildDir fsEnv irBuf =
  { workdir: buildDir
  , maxOptimizeIter: optMaxIter
  , loadModule: \name -> do
      path <- corefnPathFor ulibDir opts.corefnDir name
      FS.readText path >>= case _ of
        Nothing -> pure Missing
        Just src -> case parseModule src of
          Left err -> pure (Failed { moduleName: name, detail: err })
          Right mod -> pure (Loaded { path, mod })
  -- ADR-0090 §2: reconstruct this module's foreign shapes (self-guarding — empty when foreign-free),
  -- mapping a reconstruction failure to the driver's `ForeignSigError` so the build halts as data.
  , foreignSigsOf: \mod ->
      lmap (\detail -> { moduleName: nameKey mod.name, detail })
        <$> ForeignSigs.moduleForeignSigsE fsEnv mod
  , emitFile: \artifact -> do
      let name = artifact.backendIR.name
      pmoPath <- FS.joinPath [ buildDir, name <> ".pmo" ]
      pmiPath <- FS.joinPath [ buildDir, name <> ".pmi" ]
      FS.writeText pmoPath (moduleToString artifact.backendIR)
      FS.writeText pmiPath (interfaceToString artifact.interface)
      Log.info $ Fmt.fmt @"  compiled {name}" { name }
      pure pmoPath
  , emitEntry: \_ -> pure "(vm entry is link-time)"
  , hooks: irHooks opts.emitIr buildDir irBuf
  }

-- | Compile every module reachable from the entry to its `.pmo`/`.pmi`, then link the closure into a
-- | single runnable `app.pvm` — the entry `<module>.main` is an `Effect`, forced by applying it to unit.
cmd :: forall r. Options -> Run (ENV + LOG + FS + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  Log.info $ Fmt.fmt @"Building from entry {entry}" { entry: opts.entryModule }
  ulibDir <- requireUlibDir
  Log.debug $ Fmt.fmt @"Overlaying patched ulib from {dir}" { dir: ulibDir }
  buildDir <- FS.joinPath [ opts.outDir, "_build" ]
  FS.mkdirP buildDir
  irBuf <- liftEffect (Ref.new [])
  -- The FSR static inputs, read once and closed into the action's `foreignSigsOf` capability (ADR-0090 §2)
  -- and reused by the `--check-foreign-sigs` diagnostic.
  fsEnv <- ForeignSigs.loadEnv { ulibDir, corefnDir: opts.corefnDir }
  let
    action = mkAction opts ulibDir buildDir fsEnv irBuf
    buildOpts =
      { entryModule: opts.entryModule
      , entryName: "main"
      , isEffect: true
      , opt: not opts.noOpt
      }
  -- Eager whole-closure FSR sweep as a diagnostic (the build's own per-module `foreignSigsOf` already
  -- consumes signatures, ADR-0090); opt-in via `--check-foreign-sigs`.
  when opts.checkForeignSigs do
    loadClosure action opts.entryModule >>= case _ of
      Left err -> throw (renderBuildError err)
      Right loaded -> do
        total <- foldM (\n m -> (n + _) <<< Map.size <$> ForeignSigs.moduleForeignSigs fsEnv m.mod) 0 loaded
        Log.debug $ Fmt.fmt @"foreign-sigs: {n} signatures resolved" { n: show total }
  build bytecodeBackend action buildOpts >>= case _ of
    Left err -> throw (renderBuildError err)
    Right products -> do
      let
        artifacts = map _.artifact.backendIR products.modules
        mainTerm = TmApp (TmVar (opts.entryModule <> ".main")) (TmLit (LInt 0))
        image = (link artifacts Ffi.resolver mainTerm) { isEffect = true }
      appPath <- FS.joinPath [ opts.outDir, "app.pvm" ]
      FS.writeText appPath (imageToString image)
      Log.info $ Fmt.fmt @"✓ Build finished → {app}" { app: appPath }
