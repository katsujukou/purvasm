module Purvasm.CLI.Build where

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
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.Compiler.Bytecode.Artifact (interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Bytecode.Image (imageToString)
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.Translate (nameKey)
import Purvasm.Compiler.Compile (compileModule)
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
  }

importNames :: Module -> Array String
importNames m = map (nameKey <<< _.moduleName) m.imports

-- | Load the entry module and the transitive closure of its imports (boot's `Link.load`):
-- | a module whose `corefn.json` is absent (e.g. `Prim`) is simply skipped.
loadClosure :: forall r. FilePath -> String -> Run (FS + EXCEPT String + r) (Map String Module)
loadClosure corefnDir = go Map.empty
  where
  go visited name
    | Map.member name visited = pure visited
    | otherwise = do
        path <- FS.joinPath [ corefnDir, name, "corefn.json" ]
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
cmd :: forall r. Options -> Run (LOG + FS + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  Log.info $ Fmt.fmt @"Building from entry {entry}" { entry: opts.entryModule }
  mods <- loadClosure opts.corefnDir opts.entryModule
  let artifacts = map compileModule (depOrder mods)
  buildDir <- FS.joinPath [ opts.outDir, "_build" ]
  FS.mkdirP buildDir
  for_ artifacts \artifact -> do
    pmoPath <- FS.joinPath [ buildDir, artifact.name <> ".pmo" ]
    pmiPath <- FS.joinPath [ buildDir, artifact.name <> ".pmi" ]
    FS.writeText pmoPath (moduleToString artifact)
    FS.writeText pmiPath (interfaceToString (interfaceOf artifact))
    Log.info $ Fmt.fmt @"  compiled {name}" { name: artifact.name }
  -- link the closure into a runnable app.pvm; the entry `<module>.main` is an Effect,
  -- forced by applying it to unit (the `0` convention) at run.
  let
    mainTerm = TmApp (TmVar (opts.entryModule <> ".main")) (TmLit (LInt 0))
    image = (link artifacts Ffi.resolver mainTerm) { isEffect = true }
  appPath <- FS.joinPath [ opts.outDir, "app.pvm" ]
  FS.writeText appPath (imageToString image)
  Log.info $ Fmt.fmt @"✓ Build finished → {app}" { app: appPath }
