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
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String.Common (joinWith)
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
import Purvasm.CLI.ForeignSigs as ForeignSigs
import Purvasm.CLI.NativeLink as NativeLink
import Purvasm.CLI.Ulib (corefnPathFor, requireUlibDir)
import Purvasm.Compiler (Backend, BuildError(..), CompilerAction, CompilerActionHooks, LoadResult(..), build, defaultHooks)
import Purvasm.Compiler.Backend.LLVM.Abi (defaultHeapWords)
import Purvasm.Compiler.Backend.LLVM.Driver (LlvmContext, llvmBackend)
import Purvasm.Compiler.Bytecode.Artifact (interfaceToString)
import Purvasm.Compiler.CESK.Translate (nameKey)
import Purvasm.Compiler.MiddleEnd.ANF.Pretty (printModuleAnf)
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
        "Reconstruct and check foreign signatures during the build (ADR-0080).\n\
        \Off by default until a build-time consumer lands: source-channel lexing is\n\
        \expensive on the native backend, and the standing checks are the\n\
        \`foreign-sigs` command and tools/foreign-sigs-diff.sh."
        # ArgParser.boolean
  , noOpt:
      ArgParser.flag [ "--no-opt" ]
        "Disable the optimiser (Simplify/Dbe/EffectAnalysis + the NbE inliner); keep only\n\
        \the required lowering (Normalize + DictElim). The un-optimised native path is the\n\
        \`.ll` byte-identity reference for the boot port (ADR-0082 §2), and a bisection aid\n\
        \separating a codegen bug from an optimiser bug."
        # ArgParser.boolean
  , emitLlvm:
      ArgParser.flag [ "--emit-llvm" ]
        "Stop after emitting the `.ll` objects (no clang/link). Useful for the byte-identity\n\
        \differential against boot; a full build otherwise links a native executable."
        # ArgParser.boolean
  , emitIr:
      ArgParser.argument [ "--emit-ir" ]
        "Trace a module's optimiser output: for the named module, append each optimiser round's\n\
        \pretty-printed ANF to `<module>.ir` under _build (a --trace-rewrite analogue). The build\n\
        \still runs to completion (codegen + link); this only turns on tracing (ADR-0087 §3.1)."
        # ArgParser.optional
  , optMaxIter:
      ArgParser.argument [ "--opt-max-iter" ]
        "Maximum optimiser fixpoint rounds per module (clamped to [1, 10]). Defaults to the cap."
        # ArgParser.int
        # ArgParser.default optMaxIterCap
  , runtimeLib:
      ArgParser.argument [ "--runtime-lib" ]
        "Path to the runtime staticlib (libpurvasm_rt.a). Defaults to $PURVASM_RT_A or\n\
        \runtime/target/release/libpurvasm_rt.a."
        # ArgParser.optional
  }

importNames :: Module -> Array String
importNames m = map (nameKey <<< _.moduleName) m.imports

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

-- | The CLI `CompilerAction` over the `Run` stack: single-module CoreFn loading (through the `ulib`
-- | overlay), per-module `mod_<i>.ll` + `.pmi` emission, the `entry.ll` emission, and the `--emit-ir`
-- | trace hooks. `modIdx`/`irBuf` are the State-like buffers ADR-0087 §3.1 threads in the host's `m`.
mkAction
  :: forall r
   . Options
  -> FilePath
  -> FilePath
  -> Ref Int
  -> Ref (Array String)
  -> CompilerAction String (BuildM r)
mkAction opts ulibDir buildDir modIdx irBuf =
  { workdir: buildDir
  , maxOptimizeIter: clamp 1 optMaxIterCap opts.optMaxIter
  , loadModule: \name -> do
      path <- corefnPathFor ulibDir opts.corefnDir name
      FS.readText path >>= case _ of
        Nothing -> pure Missing
        Just src -> case parseModule src of
          Left err -> pure (Failed { moduleName: name, detail: err })
          Right mod -> pure (Loaded { path, mod })
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
  , hooks: irHooks opts buildDir irBuf
  }

-- | The `--emit-ir` trace hooks (ADR-0087 §3.1): for the target module, bracket the driver's optimiser
-- | fixpoint with `onEnterOptimizeIter` (the pre-optimised fixpoint input) / `onContinueOptimizeIter` (each
-- | round's result) / `onLeaveOptimizeIter` (the converged module), appending each round's pretty-printed
-- | ANF to the buffer, then flush it to `<module>.ir` when the module finishes (`onCleanUp`). Under
-- | `--no-opt` the fixpoint never runs, so the trace records that. Without `--emit-ir`, the no-op hooks.
irHooks :: forall r. Options -> FilePath -> Ref (Array String) -> CompilerActionHooks (BuildM r)
irHooks opts buildDir irBuf = case opts.emitIr of
  Nothing -> defaultHooks
  Just target -> defaultHooks
    { onEnterOptimizeIter = \am -> appendFor target am "pre-optimised (fixpoint input)"
    , onContinueOptimizeIter = \n am -> appendFor target am ("round " <> show n)
    , onLeaveOptimizeIter = \am -> appendFor target am "converged"
    , onCleanUp = \name -> when (name == target) (flush target)
    }
  where
  appendFor target am label =
    when (am.name == target) do
      let chunk = "=== " <> label <> " ===\n" <> printModuleAnf am.name am.decls
      liftEffect (Ref.modify_ (\cs -> Array.snoc cs chunk) irBuf)

  flush target = do
    chunks <- liftEffect (Ref.read irBuf)
    liftEffect (Ref.write [] irBuf)
    irPath <- FS.joinPath [ buildDir, target <> ".ir" ]
    let body = if Array.null chunks then "(no optimiser rounds — built with --no-opt)\n" else joinWith "\n\n" chunks
    FS.writeText irPath body
    Log.info $ Fmt.fmt @"  traced ANF IR → {target}.ir" { target }

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
  let
    action = mkAction opts ulibDir buildDir modIdx irBuf

    backend :: Backend LlvmContext String
    backend = llvmBackend { isEffect: not opts.value, heapWords: defaultHeapWords, debug: false }
    buildOpts =
      { entryModule: opts.entryModule
      , entryName: opts.entryName
      , isEffect: not opts.value
      , opt: not opts.noOpt
      }
  -- The ADR-0080 source channel, opt-in until a build-time consumer lands (§3/§4): CST-lexing the foreign
  -- frontier is minutes on the native backend, so `--check-foreign-sigs` runs it on demand.
  when opts.checkForeignSigs do
    mods <- loadClosure ulibDir opts.corefnDir opts.entryModule
    env <- ForeignSigs.loadEnv { ulibDir, corefnDir: opts.corefnDir }
    total <- foldM (\n m -> (n + _) <<< Map.size <$> ForeignSigs.moduleForeignSigs env m) 0 (depOrder mods)
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
          }
