-- | `purvasm build`: the native (LLVM) backend (ADR-0082/0087). Drives the backend-neutral
-- | `Purvasm.Compiler.build` driver with the LLVM `Backend` (`llvmBackend`) and a CLI-provided
-- | `CompilerAction` over the `Run` effect stack: `loadModule` reads a module's `corefn.json` (through the
-- | `ulib` overlay, ADR-0055), `emitFile`/`emitEntry` write the per-module `mod_<i>.ll`/`.pmi` and the
-- | `entry.ll` under `<outDir>/_build`, and the inspection hooks realise `--emit-ir` (§ below). Linking the
-- | objects into a native binary is `NativeLink`. The bytecode/VM path is `purvasm run`.
-- |
-- | `--emit-ir <module>` (ADR-0087 §3.1) is a *trace*, not a stop: the build runs to completion, and for
-- | the named module the driver's optimiser-iteration hooks append each round's pretty-printed ANF to a
-- | `Ref` buffer (a `purs-backend-es --trace-rewrite` analogue), flushed to `<module>.ir` when the module
-- | finishes. `--opt-max-iter <N>` bounds the fixpoint loop (clamped to `[1, optMaxIterCap]`).
module Purvasm.CLI.Build where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Fmt as Fmt
import PureScript.CoreFn.Module (Module)
import Purvasm.CLI.Compile (parseModule)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.EmitIr (irHooks)
import Purvasm.CLI.ForeignSigs as ForeignSigs
import Purvasm.CLI.NativeLink as NativeLink
import Purvasm.CLI.Ulib (corefnPathFor, requireUlibDir)
import Purvasm.Compiler (Backend, BuildError(..), CompilerAction, LoadResult(..), build)
import Purvasm.Compiler.Backend.LLVM.Abi (defaultHeapWords)
import Purvasm.Compiler.Backend.LLVM.Driver (LlvmContext, llvmBackend)
import Purvasm.Compiler.Bytecode.Artifact (interfaceToString)
import Purvasm.Compiler.CESK.Translate (nameKey)
import Run (EFFECT, Run, liftEffect)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { corefnDir :: FilePath
  , outDir :: FilePath
  , entryModule :: String
  , entryName :: String
  , value :: Boolean
  , checkForeignSigs :: Boolean
  , noOpt :: Boolean
  , emitLlvm :: Boolean
  -- | `--emit-ir <module>`: trace the named module's per-round ANF to `<module>.ir` (the build still runs
  -- | to completion). `Nothing` ⇒ no tracing.
  , emitIr :: Maybe String
  -- | `--opt-max-iter <N>`: the optimiser fixpoint round cap, clamped to `[1, optMaxIterCap]`.
  , optMaxIter :: Int
  , runtimeLib :: Maybe FilePath
  -- | `--rust-ffi <dir>`: the single app-Rust foreign crate (ADR-0091 §3/§Addendum). Selecting it makes app
  -- | FFI Rust — the crate is bundled with the runtime rlib into one staticlib and linked; C siblings are
  -- | not compiled, only scanned to enforce C-xor-Rust (a co-located `.c` is then an ambiguity error).
  , rustFfi :: Maybe FilePath
  }

-- | The heuristic hard cap on optimiser fixpoint rounds (ADR-0087 §3.1) — the outer-round analogue of
-- | ADR-0082's inner `rewriteLimit` fuel, a non-termination backstop.
optMaxIterCap :: Int
optMaxIterCap = 10

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
        "Name of the entry module which contains the entry binding,\n\
        \the entry point of the whole program. Defaults to `Main`."
        # ArgParser.default "Main"
  , entryName:
      ArgParser.argument [ "--entry-name" ]
        "Name of the entry binding within the entry module. Defaults to `main`."
        # ArgParser.default "main"
  , value:
      ArgParser.flag [ "--value" ]
        "Treat the entry as a bare value (print its Int result) rather than an `Effect`\n\
        \to run. Default treats it as `Effect` (apply to unit, perform)."
        # ArgParser.boolean
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
        "Disable the optimiser (the DictElim + NbE-inliner fixpoint, ADR-0086/0089); the shared\n\
        \driver is then the identity (normalise-only) and dictionaries stay dynamically\n\
        \dispatched (ADR-0104 §3). The un-optimised native path is the optimiser-free reference\n\
        \lowering — a bisection aid separating a codegen bug from an optimiser bug."
        # ArgParser.boolean
  , emitLlvm:
      ArgParser.flag [ "--emit-llvm" ]
        "Stop after emitting the `.ll` objects (no clang/link). Useful for `.ll`-level\n\
        \differentials (e.g. the stage fixpoint, ADR-0104 §2); a full build otherwise links\n\
        \a native executable."
        # ArgParser.boolean
  , emitIr:
      ArgParser.argument [ "--emit-ir" ]
        "Emit human-readable IR per optimiser iterations. Outputs results to \n\
        \{OUTDIR}/_build/{MODULE_NAME}."
        # ArgParser.optional
  , optMaxIter:
      ArgParser.argument [ "--opt-max-iter" ]
        "Maximum optimiser fixpoint rounds per module (clamped to [1, 10]).\n\
        \Defaults to the cap."
        # ArgParser.int
        # ArgParser.default optMaxIterCap
  , runtimeLib:
      ArgParser.argument [ "--runtime-lib" ]
        "Path to the runtime staticlib (libpurvasm_rt.a). Defaults to $PURVASM_RT_A or\n\
        \runtime/target/release/libpurvasm_rt.a."
        # ArgParser.optional
  , rustFfi:
      ArgParser.argument [ "--rust-ffi" ]
        "Path to the Rust crate implementing foreign modules. \n\
        \Specify only when using Rust for foreign modules;\n\
        \cannot be combined with C foreign modules."
        # ArgParser.optional
  }

importNames :: Module -> Array String
importNames m = map (nameKey <<< _.moduleName) m.imports

-- | The local workspace source modules (ADR-0091 §2): cache-db entries whose `.purs` is **not** under
-- | `.spago` (a registry dependency). This is the app-C sibling `.c` search space handed to the linker;
-- | a registry dependency's adjacent `.c` is never app FFI — it stays packaged-provider work (ADR-0091
-- | §2). A local *library* package is included but normally backs its leaves through the runtime/ulib
-- | (no sibling `.c`). A cache-db decode failure ⇒ none (the failure surfaces through FSR where it matters).
projectModules :: ForeignSigs.Env -> Array { key :: String, sourcePath :: FilePath }
projectModules fsEnv = case fsEnv.cacheDb of
  Left _ -> []
  Right cdb ->
    Array.mapMaybe
      ( \(name /\ path) ->
          if String.contains (Pattern ".spago") path then Nothing
          else Just { key: name, sourcePath: path }
      )
      (Map.toUnfoldable cdb)

-- | Load the entry module and the transitive closure of its imports (the `Map`-based CLI helper used by
-- | `--check-foreign-sigs` and the `foreign-sigs` command). The library `Purvasm.Compiler.loadClosure`
-- | drives the actual native build; this is retained for the closure-sweep commands that want a plain
-- | `Array Module`. A module whose `corefn.json` is absent (e.g. `Prim`) is skipped.
loadClosure :: forall r. FilePath -> FilePath -> String -> Run (FS + EXCEPT String + r) (Map String Module)
loadClosure ulibDir corefnDir = go Map.empty
  where
  go visited name
    | Map.member name visited = pure visited
    | otherwise = do
        path <- corefnPathFor ulibDir corefnDir name
        FS.readText path >>= case _ of
          Nothing -> pure visited
          Just src -> case parseModule src of
            Left err -> throw (Fmt.fmt @"{name}: {err}" { name, err })
            Right m -> foldM go (Map.insert name m visited) (importNames m)

-- | Dependency order (imports before importers): a DFS post-order over the loaded closure.
depOrder :: Map String Module -> Array Module
depOrder mods = Array.fromFoldable (List.reverse (snd (foldl visit (Set.empty /\ Nil) names)))
  where
  names = map fst (Map.toUnfoldable mods :: Array (String /\ Module))

  visit acc@(seen /\ done) name
    | Set.member name seen = acc
    | otherwise = case Map.lookup name mods of
        Nothing -> Set.insert name seen /\ done
        Just m ->
          let
            seen1 /\ done1 = foldl visit (Set.insert name seen /\ done) (localDeps m)
          in
            seen1 /\ (m : done1)

  localDeps m = Array.filter (\n -> Map.member n mods) (importNames m)

-- | The CLI's effect row for the native build.
type BuildM r = Run (ENV + LOG + FS + PROC + EXCEPT String + EFFECT + r)

-- | Render a `BuildError` as the CLI's `EXCEPT String` message.
renderBuildError :: BuildError -> String
renderBuildError = case _ of
  EntryMissing name -> "entry module not found: " <> name
  LoadFailed e -> Fmt.fmt @"{name}: {detail}" { name: e.moduleName, detail: e.detail }
  ForeignSigFailed e -> Fmt.fmt @"{name}: {detail}" { name: e.moduleName, detail: e.detail }

-- | The CLI `CompilerAction` over the `Run` stack: single-module CoreFn loading (through the `ulib`
-- | overlay), per-module `mod_<i>.ll` + `.pmi` emission, the `entry.ll` emission, and the `--emit-ir`
-- | trace hooks. `modIdx`/`irBuf` are the State-like buffers ADR-0087 §3.1 threads in the host's `m`.
mkAction
  :: forall r
   . Options
  -> FilePath
  -> FilePath
  -> ForeignSigs.Env
  -> Ref Int
  -> Ref (Array String)
  -> CompilerAction String (BuildM r)
mkAction opts ulibDir buildDir fsEnv modIdx irBuf =
  { workdir: buildDir
  , maxOptimizeIter: clamp 1 optMaxIterCap opts.optMaxIter
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
      i <- liftEffect (Ref.read modIdx)
      liftEffect (Ref.modify_ (_ + 1) modIdx)
      modPath <- FS.joinPath [ buildDir, "mod_" <> show i <> ".ll" ]
      FS.writeText modPath artifact.backendIR
      -- The `.pmi` sibling (ADR-0087 §2): the linker never reads it, but separate compilation wants it.
      pmiPath <- FS.joinPath [ buildDir, artifact.interface.ifaceName <> ".pmi" ]
      FS.writeText pmiPath (interfaceToString artifact.interface)
      Log.info $ Fmt.fmt @"  emitted {name} → mod_{i}.ll" { name: artifact.interface.ifaceName, i: show i }
      pure modPath
  , emitEntry: \ll -> do
      entryPath <- FS.joinPath [ buildDir, "entry.ll" ]
      FS.writeText entryPath ll
      pure entryPath
  , hooks: irHooks opts.emitIr buildDir irBuf
  }

-- | Compile the program natively: drive `Purvasm.Compiler.build` with the LLVM backend and the CLI action,
-- | then (unless `--emit-llvm`) native-link the emitted objects.
cmd :: forall r. Options -> Run (ENV + LOG + FS + PROC + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  Log.info $ Fmt.fmt @"Building (native) from entry {mod}.{name}"
    { mod: opts.entryModule, name: opts.entryName }
  ulibDir <- requireUlibDir
  Log.debug $ Fmt.fmt @"Overlaying patched ulib from {dir}" { dir: ulibDir }
  buildDir <- FS.joinPath [ opts.outDir, "_build" ]
  FS.mkdirP buildDir
  modIdx <- liftEffect (Ref.new 0)
  irBuf <- liftEffect (Ref.new [])
  -- The FSR static inputs (ulib `foreignSigs` + spago cache-db), read once and closed into the action's
  -- `foreignSigsOf` capability (ADR-0090 §2) and reused by the `--check-foreign-sigs` diagnostic.
  fsEnv <- ForeignSigs.loadEnv { ulibDir, corefnDir: opts.corefnDir }
  let
    action = mkAction opts ulibDir buildDir fsEnv modIdx irBuf

    backend :: Backend LlvmContext String
    backend = llvmBackend { isEffect: not opts.value, heapWords: defaultHeapWords, debug: false }
    buildOpts =
      { entryModule: opts.entryModule
      , entryName: opts.entryName
      , isEffect: not opts.value
      , opt: not opts.noOpt
      }
  -- Eager whole-closure FSR sweep as a diagnostic (the build's own per-module `foreignSigsOf` already
  -- consumes signatures, ADR-0090): CST-lexing the foreign frontier is minutes on the native backend, so
  -- this up-front count check is opt-in via `--check-foreign-sigs`.
  when opts.checkForeignSigs do
    mods <- loadClosure ulibDir opts.corefnDir opts.entryModule
    total <- foldM (\n m -> (n + _) <<< Map.size <$> ForeignSigs.moduleForeignSigs fsEnv m) 0 (depOrder mods)
    Log.debug $ Fmt.fmt @"foreign-sigs: {n} signatures resolved" { n: show total }
  build backend action buildOpts >>= case _ of
    Left err -> throw (renderBuildError err)
    Right products -> do
      let n = Array.length products.modules
      Log.info $ Fmt.fmt @"✓ Emitted {n} object(s) → {dir}" { n: show (n + 1), dir: buildDir }
      -- Link the objects into a native executable, unless `--emit-llvm` stops at the IR.
      unless opts.emitLlvm do
        NativeLink.link
          { output: opts.outDir
          , buildDir
          , moduleCount: n
          , runtimeLib: opts.runtimeLib
          , ulibDir
          , appModules: projectModules fsEnv
          , rustFfiDir: opts.rustFfi
          }
