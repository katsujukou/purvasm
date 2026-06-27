-- | Shared staging machinery for the `ulib` patches: assemble a `purs`-compilable source tree
-- | (resolved registry sources + `purvasm-base`, optionally overlaid with the patches) and compile
-- | it to corefn. Used by both `build` (extract the patched corefn) and `verify` (diff the patched
-- | vs. registry surface). Path handling goes through the `Filesystem` effect (ADR-0043).
module Purvasm.UlibTools.Stage
  ( Patch
  , modulePath
  , mkTemp
  , requireOk
  , copyTree
  , collectPatches
  , stageRegistry
  , overlayPatches
  , compileCorefn
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Fmt as Fmt
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Run (Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

-- | A patched module: the package subdir it lives under and its dotted module name.
type Patch = { package :: String, modName :: String }

-- | A dotted module name to its slash path (`Data.Array.ST` -> `Data/Array/ST`).
modulePath :: String -> String
modulePath = String.replaceAll (Pattern ".") (String.Replacement "/")

-- | A fresh temp directory (`mktemp -d`), trimmed of the trailing newline.
mkTemp :: forall r. Run (PROC + EXCEPT String + r) FilePath
mkTemp = String.trim <$> (requireOk =<< Proc.execCapture "mktemp" [ "-d" ])

-- | Surface a `Proc`/tool failure through `EXCEPT`.
requireOk :: forall r a. Either String a -> Run (EXCEPT String + r) a
requireOk = either throw pure

-- | Recursively copy a directory's *contents* into `dst` (the shell's `cp -R src/. dst/`). The
-- | `/.` idiom is a shell-cp argument, not a logical path, so it is built by hand (a `joinPath`
-- | would normalise the `.` away).
copyTree :: forall r. FilePath -> FilePath -> Run (PROC + EXCEPT String + r) Unit
copyTree src dst = void $ requireOk =<< Proc.exec "cp" [ "-R", src <> "/.", dst <> "/" ]

-- | Copy the resolved package-set sources (registry `<pkg>/src`, git `<pkg>/<ref>/src`) plus the
-- | `purvasm-base` primitives into `srcDir` — the registry baseline both `build` and `verify` need.
stageRegistry :: forall r. FilePath -> FilePath -> FilePath -> Run (PROC + FS + EXCEPT String + r) Unit
stageRegistry spagoPkgs baseDir srcDir = do
  pkgs <- listDir spagoPkgs
  for_ pkgs \pkg -> do
    pkgDir <- FS.joinPath [ spagoPkgs, pkg ]
    copyIfSrc pkgDir
    refs <- listDir pkgDir
    for_ refs \ref -> do
      refDir <- FS.joinPath [ pkgDir, ref ]
      copyIfSrc refDir
  copyTree baseDir srcDir
  where
  copyIfSrc dir = do
    src <- FS.joinPath [ dir, "src" ]
    whenM (FS.exists src) (copyTree src srcDir)

-- | Enumerate the patches under `ulibDir`: each `<package>/<Module>.purs` (mindepth 2).
collectPatches :: forall r. FilePath -> Run (FS + r) (Array Patch)
collectPatches ulibDir = do
  pkgs <- listDir ulibDir
  map join $ traverse perPackage pkgs
  where
  perPackage pkg = do
    dir <- FS.joinPath [ ulibDir, pkg ]
    files <- listDir dir
    pure $ Array.mapMaybe (toPatch pkg) files
  toPatch pkg file = { package: pkg, modName: _ } <$> String.stripSuffix (Pattern ".purs") file

-- | Overlay the patches over the staged registry modules, dropping each sibling registry `.js` so a
-- | kept `foreign import` resolves to a boot intrinsic at link time (ADR-0038).
overlayPatches :: forall r. FilePath -> FilePath -> Array Patch -> Run (FS + EXCEPT String + r) Unit
overlayPatches ulibDir srcDir patches = for_ patches overlay
  where
  overlay { package, modName } = do
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

-- | Compile a staged source tree to corefn (`purs` expands the `**` glob itself).
compileCorefn :: forall r. String -> FilePath -> FilePath -> Run (PROC + FS + EXCEPT String + r) Unit
compileCorefn purs srcDir outputDir = do
  srcGlobs <- FS.joinPath [ srcDir, "**", "*.purs" ]
  requireOk =<< Proc.exec purs
    [ "compile", "--codegen", "corefn", "--output", outputDir, srcGlobs ]

-- | Directory entries via the FS effect (`Nothing`/absent → empty).
listDir :: forall r. FilePath -> Run (FS + r) (Array String)
listDir dir = fromMaybe [] <$> FS.readDir dir
