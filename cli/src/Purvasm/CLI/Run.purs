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
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Tuple.Nested ((/\))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Fmt as Fmt
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Purvasm.CLI.Compile (parseModule)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Env as Env
import Purvasm.CLI.EmitIr (irHooks)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Purvasm.CLI.ForeignProvider as ForeignProvider
import Purvasm.CLI.ForeignSigs as ForeignSigs
import Purvasm.CLI.Ulib (corefnPathFor, requireUlibDir)
import Purvasm.Compiler (BuildError(..), CompilerAction, LoadResult(..), build, loadClosure)
import Purvasm.Compiler.Backend.Bytecode (bytecodeBackend)
import Purvasm.Compiler.Bytecode.Artifact (ModuleArtifact, interfaceToString, moduleToString)
import Purvasm.Compiler.Bytecode.Image (foreignRefKeys, imageToString, imageToStringWithArities)
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.Translate (nameKey)
import Purvasm.Compiler.Ffi as Ffi
import Purvasm.Compiler.Link (link)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.NativeLeaf (nativeLeafArities)
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
  -- | Everything after `--`, handed to the program as its own argv. The VM's flags are the VM's; a
  -- | guest's arguments are never guessed out of what is left over.
  , guestArgs :: Array String
  -- | Produce the artifacts and stop. What a build system or a test harness wants from this command
  -- | is the image, not the run.
  , buildOnly :: Boolean
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
      ArgParser.argument [ "--main", "-m" ]
        "Name of the module whose `main` is the program's entry point.\n\
        \Defaults to `Main`. A module with no `main` is an error."
        # ArgParser.default "Main"
  , checkForeignSigs:
      ArgParser.flag [ "--check-foreign-sigs" ]
        "Extra diagnostic: reconstruct every module's foreign signatures up front and\n\
        \log the resolved count. The build reconstructs them per module anyway; this is\n\
        \an eager whole-closure sweep. Off by default: reading the sources to do it is\n\
        \expensive on the native backend."
        # ArgParser.boolean
  , noOpt:
      ArgParser.flag [ "--no-opt" ]
        "Disable the optimiser; keep only normalisation, so dictionaries stay applied."
        # ArgParser.boolean
  , buildOnly:
      ArgParser.flag [ "--build-only" ]
        "Compile and link, but do not run the program."
        # ArgParser.boolean
  , guestArgs:
      ArgParser.rest
        "Arguments after `--`, passed to the program as its own argv."
        # ArgParser.default []
  , emitIr:
      ArgParser.argument [ "--emit-ir" ]
        "Trace the named module's per-round optimiser ANF to `<module>.ir` under the build\n\
        \directory. A trace, not a stop — the build still completes."
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
-- | The status this command asks the process to end with: the program's own when one ran, and 0 when
-- | the work was a build.
cmd :: forall r. Options -> Run (ENV + PROC + LOG + FS + EXCEPT String + EFFECT + r) Int
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
        entryKey = opts.entryModule <> ".main"
        mainTerm = TmApp (TmVar entryKey) (TmLit (LInt 0))
        image = (link artifacts Ffi.resolver mainTerm) { isEffect = true }
      -- `-m` names the module whose `main` runs. A module without one links to an entry that
      -- references an unbound global, which would surface as a stuck run with a name in it rather
      -- than as "you pointed me at the wrong module" — so it is refused here, where the answer is
      -- known. (Reachability starts AT the entry, so its presence among the linked definitions is
      -- exactly the question.)
      unless (Array.any (\(key /\ _) -> key == entryKey) image.gdefs) $ throw
        ( Fmt.fmt @"{entry} defines no `main`, so there is no program to run (`-m` names the module whose `main` is the entry point)"
            { entry: opts.entryModule }
        )
      -- boot's frozen VM keeps a copy under its own name: it reads neither an arity-carrying
      -- `ForeignRef` (§4(a)) nor a tree-shaped `case` (§4(b)), and the two runners are still held to
      -- the same OUTPUT — which needs one compilation to produce something each of them can run.
      -- The qualifier is on the legacy side now, because the default moved (ADR-0110 §6, pinned).
      bootPath <- FS.joinPath [ opts.outDir, "app.boot.pvm" ]
      FS.writeText bootPath (imageToString image)
      -- Both forms, from ONE compilation, for as long as the two VMs coexist (ADR-0110 §6): boot's
      -- frozen VM reads `app.pvm` and knows neither an arity-carrying `ForeignRef` (§4(a)) nor a
      -- tree-shaped `case` (§4(b)), while the owned VM needs both. Their instruction counts no longer
      -- have to agree — tree dispatch changed what a step is, and step C's calibration is taken and
      -- recorded — but the two runners are still held to the same OUTPUT, and that needs one
      -- compilation to produce something each of them can run.
      --
      -- The owned image is named for its role rather than its version: the stamp inside says which
      -- format it is, and a filename repeating the number would need renaming at every bump.
      appPath <- FS.joinPath [ opts.outDir, "app.pvm" ]
      case imageToStringWithArities (nativeLeafArities products.foreignSigs) image of
        Left err -> throw err
        Right text -> FS.writeText appPath text
      -- What the owned VM needs *beside* the image (ADR-0110 §6 step E): the manifest of keys the
      -- workspace provides, and — since a hosted guest cannot link a ulib `.c` the way a compiled
      -- program does — a loadable provider built from those same sources.
      foreignArtifacts <- ForeignProvider.emitProvider
        { outDir: opts.outDir, ulibDir, referenced: foreignRefKeys image }
      Log.info $ Fmt.fmt @"✓ Build finished → {app}" { app: appPath }
      case foreignArtifacts.provider of
        Nothing -> pure unit
        Just provider -> Log.debug $ Fmt.fmt @"  foreign provider → {provider}" { provider }
      if opts.buildOnly then pure 0 else launch opts appPath foreignArtifacts

-- | Run the linked image on the owned VM (ADR-0110 §6 step E).
-- |
-- | The runner is explicit about what the VM may load, and that is the point rather than a
-- | convenience: ADR-0111 §4 makes loading a provider an explicit act, so the VM discovers nothing
-- | beside the image. What changes here is only *who* is explicit — the launcher, which built the
-- | provider a moment ago and therefore knows it, instead of a person retyping its path. The `Maybe`
-- | comes from the packaging step, never from whether a file happens to sit in the output directory:
-- | an outdir reused from a build that did need one would otherwise hand the VM a stale module.
launch
  :: forall r
   . Options
  -> FilePath
  -> ForeignProvider.ProviderArtifacts
  -> Run (ENV + PROC + LOG + FS + EXCEPT String + r) Int
launch opts image artifacts = do
  vm <- resolveOwnedVm
  let
    provider = maybe [] (\p -> [ "--ffi", p ]) artifacts.provider
    args = provider <> [ "--manifest", artifacts.manifest, "--image", image ]
      <> (if Array.null opts.guestArgs then [] else [ "--" ] <> opts.guestArgs)
  Log.debug $ Fmt.fmt @"  running {vm}" { vm }
  -- The program's status is handed back rather than translated into one of ours. A program that
  -- exits 42 is reporting 42 to whoever ran this command, and a launcher that turned every non-zero
  -- code into 1 would make its own shell contract useless. Its output has already reached the
  -- terminal (the child inherits stdio), so nothing needs restating here.
  --
  -- `Left` is the case where the program did not run at all — the VM could not be spawned, or was
  -- killed — which IS this command's failure to report.
  Proc.execStatus vm args >>= case _ of
    Right code -> pure code
    Left err -> throw (Fmt.fmt @"could not run {image}: {err}" { image, err })

-- | Locate the owned VM. `$PURVASM_VM` names it; there is deliberately no conventional path yet,
-- | because where a purvasm installation puts its executables is an open question (the `dist` layout)
-- | and guessing here would answer it by accident.
resolveOwnedVm :: forall r. Run (ENV + FS + EXCEPT String + r) FilePath
resolveOwnedVm = Env.lookupEnv "PURVASM_VM" >>= case _ of
  Nothing -> throw
    "no VM to run the program with: set $PURVASM_VM to a purvasm VM executable."
  Just path -> FS.exists path >>= case _ of
    true -> pure path
    false -> throw ("$PURVASM_VM points at " <> path <> ", which does not exist")
