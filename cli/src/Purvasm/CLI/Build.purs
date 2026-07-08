-- | `purvasm build`: the native (LLVM) backend (ADR-0082). Loads the entry module's import closure,
-- | compiles each module **independently** to its own `.ll` object (B2 separate compilation — no
-- | whole-program term), and writes the per-module objects + the init/entry object under
-- | `<outDir>/_build`. Linking the objects into a native binary (clang + `lld --gc-sections` + the
-- | runtime staticlib) is `Backend.NativeLink`, a later step. The bytecode/VM path is `purvasm run`.
module Purvasm.CLI.Build where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
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
import Purvasm.Compiler.Backend.LLVM.Abi (defaultHeapWords)
import Purvasm.Compiler.Backend.LLVM.Driver (nativeIr, nativeSplit)
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.Translate (nameKey)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Expr)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Run (EFFECT, Run)
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
  , emitIr :: Boolean
  , runtimeLib :: Maybe FilePath
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
      ArgParser.flag [ "--emit-ir" ]
        "Emit each module's optimised ANF (post-DictElim and the optimiser) as pretty-printed\n\
        \`<module>.ir` under _build, and stop (no codegen/link). For inspecting the middle-end\n\
        \output (ADR-0085)."
        # ArgParser.boolean
  , runtimeLib:
      ArgParser.argument [ "--runtime-lib" ]
        "Path to the runtime staticlib (libpurvasm_rt.a). Defaults to $PURVASM_RT_A or\n\
        \runtime/target/release/libpurvasm_rt.a."
        # ArgParser.optional
  }

importNames :: Module -> Array String
importNames m = map (nameKey <<< _.moduleName) m.imports

-- | Load the entry module and the transitive closure of its imports (boot's `Link.load`):
-- | a module whose `corefn.json` is absent (e.g. `Prim`) is simply skipped. Each module is
-- | resolved through the `ulib` overlay first (the patched module wins, ADR-0055), falling back
-- | to `corefnDir`.
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

  -- post-order accumulated in reverse (cons is O(1), ADR-0049), reversed once above.
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

-- | The entry expression fed to the backend: a bare value (`--value`, printed) is the entry read
-- | directly; an `Effect` (default) is the entry applied to unit (the `0` convention), run for effects.
entryExprOf :: Options -> Expr
entryExprOf opts =
  let
    key = opts.entryModule <> "." <> opts.entryName
  in
    if opts.value then normalize (TmVar key)
    else normalize (TmApp (TmVar key) (TmLit (LInt 0)))

-- | Compile the program natively: load the closure, emit each module's `.ll` object and the init/entry
-- | object under `<outDir>/_build` (B2 separate compilation).
cmd :: forall r. Options -> Run (ENV + LOG + FS + PROC + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  Log.info $ Fmt.fmt @"Building (native) from entry {mod}.{name}"
    { mod: opts.entryModule, name: opts.entryName }
  ulibDir <- requireUlibDir
  Log.debug $ Fmt.fmt @"Overlaying patched ulib from {dir}" { dir: ulibDir }
  mods <- loadClosure ulibDir opts.corefnDir opts.entryModule
  let ordered = depOrder mods
  when opts.checkForeignSigs do
    env <- ForeignSigs.loadEnv { ulibDir, corefnDir: opts.corefnDir }
    total <- foldM
      (\n m -> (n + _) <<< Map.size <$> ForeignSigs.moduleForeignSigs env m)
      0
      ordered
    Log.debug $ Fmt.fmt @"foreign-sigs: {n} signatures resolved" { n: show total }
  let
    nativeOpts =
      { isEffect: not opts.value
      , heapWords: defaultHeapWords
      , debug: false
      , opt: not opts.noOpt
      }
    entry = entryExprOf opts
  buildDir <- FS.joinPath [ opts.outDir, "_build" ]
  FS.mkdirP buildDir
  if opts.emitIr then
    -- `--emit-ir`: dump each module's optimised ANF (post-DictElim / optimiser) and stop.
    for_ (nativeIr nativeOpts ordered entry) \(Tuple name ir) -> do
      irPath <- FS.joinPath [ buildDir, name <> ".ir" ]
      FS.writeText irPath ir
      Log.info $ Fmt.fmt @"  emitted ANF IR → {name}.ir" { name }
  else do
    let out = nativeSplit nativeOpts ordered entry
    forWithIndex_ out.modules \i (Tuple name ll) -> do
      modPath <- FS.joinPath [ buildDir, "mod_" <> show i <> ".ll" ]
      FS.writeText modPath ll
      Log.info $ Fmt.fmt @"  emitted {name} → mod_{i}.ll" { name, i: show i }
    entryPath <- FS.joinPath [ buildDir, "entry.ll" ]
    FS.writeText entryPath out.entry
    Log.info $ Fmt.fmt @"✓ Emitted {n} object(s) → {dir}"
      { n: show (Array.length out.modules + 1), dir: buildDir }
    -- Link the objects into a native executable, unless `--emit-llvm` stops at the IR (e.g. for the
    -- byte-identity differential, which needs no runtime staticlib).
    unless opts.emitLlvm do
      NativeLink.link
        { output: opts.outDir
        , buildDir
        , moduleCount: Array.length out.modules
        , runtimeLib: opts.runtimeLib
        }
