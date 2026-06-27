-- | `ulib-tools build`: compile the `ulib` registry-package patches (ADR-0038) to corefn for
-- | boot's `--ulib` overlay. The typed port of `ulib-tools/install.sh` (ADR-0043 §1): overlay each
-- | `ulib/<package>/<Module>.purs` over the resolved registry sources plus `purvasm-base`, compile
-- | the lot to corefn with the pinned `purs`, and extract the patched modules into the flat lib
-- | layout `<out>/<Module>/corefn.json`.
module Purvasm.UlibTools.Build
  ( Options
  , Patch
  , options
  , cmd
  , modulePath
  ) where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Fmt as Fmt
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Env as Env
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Run (EFFECT, Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { out :: Maybe FilePath
  , install :: Boolean
  , ulibDir :: FilePath
  , baseDir :: FilePath
  , purs :: String
  , spagoPkgs :: FilePath
  }

options :: ArgParser Options
options = fromRecord
  { out:
      ArgParser.argument [ "--out" ]
        "Output dir for the extracted corefn (<dir>/<Module>/corefn.json)."
        # ArgParser.optional
  , install:
      ArgParser.flag [ "--install" ]
        "Write to the default install location ($PURVASM_ULIB, else 'dist/ulib')."
        # ArgParser.boolean
  , ulibDir:
      ArgParser.argument [ "--ulib" ]
        "ulib patch source dir. Defaults to 'ulib'."
        # ArgParser.default "ulib"
  , baseDir:
      ArgParser.argument [ "--base" ]
        "purvasm-base src dir. Defaults to 'purvasm-base/src'."
        # ArgParser.default "purvasm-base/src"
  , purs:
      ArgParser.argument [ "--purs" ]
        "purs executable. Defaults to 'purs'."
        # ArgParser.default "purs"
  , spagoPkgs:
      ArgParser.argument [ "--spago-pkgs" ]
        "Resolved package-set sources dir. Defaults to '.spago/p'."
        # ArgParser.default ".spago/p"
  }

-- | A patched module: the package subdir it lives under and its dotted module name.
type Patch = { package :: String, modName :: String }

cmd :: forall r. Options -> Run (PROC + ENV + FS + LOG + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  out <- resolveOut opts
  work <- mkTemp
  srcDir <- FS.joinPath [ work, "src" ]
  outputDir <- FS.joinPath [ work, "output" ]
  FS.mkdirP srcDir

  -- 1. resolved package-set sources (registry `<pkg>/src`, git `<pkg>/<ref>/src`) + purvasm-base.
  copyPackageSources opts.spagoPkgs srcDir
  copyTree opts.baseDir srcDir

  -- 2. overlay the patches over the registry modules; drop the registry `.js` so a kept
  --    `foreign import` resolves to a boot intrinsic at link time (ADR-0038).
  patches <- collectPatches opts.ulibDir
  for_ patches (overlayPatch opts.ulibDir srcDir)

  -- 3. compile the whole overlaid set to corefn (purs expands the `**` glob itself).
  srcGlobs <- FS.joinPath [ srcDir, "**", "*.purs" ]
  requireOk =<< Proc.exec opts.purs
    [ "compile", "--codegen", "corefn", "--output", outputDir, srcGlobs ]

  -- 4. extract the patched modules' corefn into the flat lib layout.
  FS.mkdirP out
  for_ patches (extractCorefn outputDir out)

  -- 5. drop the temp tree.
  _ <- Proc.exec "rm" [ "-rf", work ]
  Log.info $ Fmt.fmt @"ulib: wrote patched corefn to {out}" { out }

-- | `--out` wins; otherwise `--install` resolves the default location ($PURVASM_ULIB, else
-- | 'dist/ulib'); with neither, fail loudly.
resolveOut :: forall r. Options -> Run (ENV + EXCEPT String + r) FilePath
resolveOut opts = case opts.out of
  Just o -> pure o
  Nothing
    | opts.install -> fromMaybe "dist/ulib" <$> Env.lookupEnv "PURVASM_ULIB"
    | otherwise -> throw "ulib-tools build: one of --out or --install is required."

mkTemp :: forall r. Run (PROC + EXCEPT String + r) FilePath
mkTemp = String.trim <$> (requireOk =<< Proc.execCapture "mktemp" [ "-d" ])

-- | Recursively copy a directory's *contents* into `dst` (the shell's `cp -R src/. dst/`).
copyTree :: forall r. FilePath -> FilePath -> Run (PROC + EXCEPT String + r) Unit
copyTree src dst = void $ requireOk =<< Proc.exec "cp" [ "-R", src <> "/.", dst <> "/" ]

-- | Copy every resolved package's `src` into `dst`: registry deps as `<pkg>/src`, git deps as
-- | `<pkg>/<ref>/src` (mirrors install.sh's `*/src` and `*/*/src` globs).
copyPackageSources :: forall r. FilePath -> FilePath -> Run (PROC + FS + EXCEPT String + r) Unit
copyPackageSources spagoPkgs dst = do
  pkgs <- listDir spagoPkgs
  for_ pkgs \pkg -> do
    pkgDir <- FS.joinPath [ spagoPkgs, pkg ]
    copyIfSrc pkgDir dst
    refs <- listDir pkgDir
    for_ refs \ref -> do
      refDir <- FS.joinPath [ pkgDir, ref ]
      copyIfSrc refDir dst
  where
  copyIfSrc dir target = do
    srcDir <- FS.joinPath [ dir, "src" ]
    whenM (FS.exists srcDir) (copyTree srcDir target)

-- | Enumerate the patches under `ulibDir`: each `<package>/<Module>.purs` (mindepth 2).
collectPatches :: forall r. FilePath -> Run (FS + r) (Array Patch)
collectPatches ulibDir = do
  pkgs <- listDirFs ulibDir
  map join $ traverse perPackage pkgs
  where
  perPackage pkg = do
    dir <- FS.joinPath [ ulibDir, pkg ]
    files <- listDirFs dir
    pure $ Array.mapMaybe (toPatch pkg) files
  toPatch pkg file = { package: pkg, modName: _ } <$> String.stripSuffix (Pattern ".purs") file

-- | Overlay one patch onto the source tree and drop the sibling registry `.js`.
overlayPatch :: forall r. FilePath -> FilePath -> Patch -> Run (FS + EXCEPT String + r) Unit
overlayPatch ulibDir srcDir { package, modName } = do
  let modPath = modulePath modName
  srcPath <- FS.joinPath [ ulibDir, package, modName <> ".purs" ]
  destPurs <- FS.joinPath [ srcDir, modPath <> ".purs" ]
  destJs <- FS.joinPath [ srcDir, modPath <> ".js" ]
  content <- FS.readText srcPath >>= maybe
    (throw $ Fmt.fmt @"ulib: cannot read patch {srcPath}" { srcPath })
    pure
  FS.dirname destPurs >>= FS.mkdirP
  FS.writeText destPurs content
  whenM (FS.exists destJs) (FS.unlink destJs)

-- | Copy one patched module's compiled corefn into the flat output layout.
extractCorefn :: forall r. FilePath -> FilePath -> Patch -> Run (FS + LOG + EXCEPT String + r) Unit
extractCorefn outputDir out { modName } = do
  from <- FS.joinPath [ outputDir, modName, "corefn.json" ]
  destDir <- FS.joinPath [ out, modName ]
  content <- FS.readText from >>= maybe
    (throw $ Fmt.fmt @"ulib: missing compiled corefn for {modName}" { modName })
    pure
  FS.mkdirP destDir
  destPath <- FS.joinPath [ destDir, "corefn.json" ]
  FS.writeText destPath content
  Log.info $ Fmt.fmt @"ulib: {modName}" { modName }

-- | Directory entries via the FS effect (`Nothing`/absent → empty).
listDir :: forall r. FilePath -> Run (FS + r) (Array String)
listDir = listDirFs

listDirFs :: forall r. FilePath -> Run (FS + r) (Array String)
listDirFs dir = fromMaybe [] <$> FS.readDir dir

-- | A dotted module name to its slash path (`Data.Array.ST` -> `Data/Array/ST`).
modulePath :: String -> String
modulePath = String.replaceAll (Pattern ".") (String.Replacement "/")

-- | Surface a `Proc`/tool failure through `EXCEPT`.
requireOk :: forall r a. Either String a -> Run (EXCEPT String + r) a
requireOk = either throw pure
