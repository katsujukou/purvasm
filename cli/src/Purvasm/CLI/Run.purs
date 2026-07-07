module Purvasm.CLI.Run where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl, for_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Fmt as Fmt
import PureScript.CoreFn.Module (Module)
import Purvasm.CLI.Compile (parseModule)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.ForeignSigs as ForeignSigs
import Purvasm.CLI.Ulib (corefnPathFor, requireUlibDir)
import Purvasm.Compiler.Bytecode.Artifact (interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Bytecode.Image (imageToString)
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.Translate (nameKey)
import Purvasm.Compiler.Compile (compileModuleWith)
import Purvasm.Compiler.Ffi as Ffi
import Purvasm.Compiler.Link (link)
import Purvasm.Compiler.Literal (Literal(..))
import Run (EFFECT, Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { corefnDir :: FilePath
  , outDir :: FilePath
  , entryModule :: String
  , checkForeignSigs :: Boolean
  , noOpt :: Boolean
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
        \separating a codegen bug from an optimiser bug. Parsed but inert for now: the\n\
        \optimiser and native backend are not yet ported — wired ahead for the optimiser\n\
        \track to read (`opts.noOpt`)."
        # ArgParser.boolean
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

-- | Compile every module reachable from the entry to its `.pmo`/`.pmi`, in dependency
-- | order. (Linking the closure into a single `app.pvm` is a later step.)
cmd :: forall r. Options -> Run (ENV + LOG + FS + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  Log.info $ Fmt.fmt @"Building from entry {entry}" { entry: opts.entryModule }
  ulibDir <- requireUlibDir
  Log.debug $ Fmt.fmt @"Overlaying patched ulib from {dir}" { dir: ulibDir }
  mods <- loadClosure ulibDir opts.corefnDir opts.entryModule
  -- The ADR-0080 source channel, opt-in until a build-time consumer lands (the native-codegen
  -- port, §3 — which will also want the §4 `.pmi` shape caching): CST-lexing the foreign
  -- frontier through the pure-PS regex engine is minutes on the native backend, so the
  -- standing checks are the `foreign-sigs` command and the boot-registry differential, and
  -- `--check-foreign-sigs` runs the hard diagnostics here on demand.
  let ordered = depOrder mods
  when opts.checkForeignSigs do
    env <- ForeignSigs.loadEnv { ulibDir, corefnDir: opts.corefnDir }
    -- Per-module (ADR-0033): resolve each module's shapes independently — no closure sweep.
    total <- foldM
      (\n m -> (n + _) <<< Map.size <$> ForeignSigs.moduleForeignSigs env m)
      0
      ordered
    Log.debug $ Fmt.fmt @"foreign-sigs: {n} signatures resolved" { n: show total }
  -- ADR-0082 §2: `--opt` runs the optimiser over the ANF; `--no-opt` keeps only required
  -- lowering (byte-identical to boot). Threaded per-module (the optimiser is module-local).
  let artifacts = map (compileModuleWith (not opts.noOpt)) ordered
  buildDir <- FS.joinPath [ opts.outDir, "_build" ]
  FS.mkdirP buildDir
  for_ artifacts \artifact -> do
    pmoPath <- FS.joinPath [ buildDir, artifact.name <> ".pmo" ]
    pmiPath <- FS.joinPath [ buildDir, artifact.name <> ".pmi" ]
    FS.writeText pmoPath (moduleToString artifact)
    -- ADR-0084 §3/§4: the optimiser summary is `--opt`-only and must be absent under `--no-opt`
    -- (byte-identity with boot). It stays `Nothing` in both modes until the optimiser publishes
    -- per-module-optimised ANF (§1; §4's conservative-unknown start); `opts.noOpt` is threaded now so
    -- the suppression is in place the moment a summary exists (the `else` gains `Just (summarize …)`).
    let iface = (interfaceOf artifact) { summary = if opts.noOpt then Nothing else Nothing }
    FS.writeText pmiPath (interfaceToString iface)
    Log.info $ Fmt.fmt @"  compiled {name}" { name: artifact.name }
  -- link the closure into a runnable app.pvm; the entry `<module>.main` is an Effect,
  -- forced by applying it to unit (the `0` convention) at run.
  let
    mainTerm = TmApp (TmVar (opts.entryModule <> ".main")) (TmLit (LInt 0))
    image = (link artifacts Ffi.resolver mainTerm) { isEffect = true }
  appPath <- FS.joinPath [ opts.outDir, "app.pvm" ]
  FS.writeText appPath (imageToString image)
  Log.info $ Fmt.fmt @"✓ Build finished → {app}" { app: appPath }
