-- | `ulib` overlay resolution for `build`/`compile` (ADR-0055). The toolchain's own patched
-- | standard library (`ulib`, a corefn directory of registry-package patches) is an implementation
-- | detail: it is located by the launcher and handed to purvasm through `PURVASM_LIB`, never via a
-- | user flag. This module resolves that directory and the per-module corefn path, mirroring boot's
-- | `Link.load ?ulib_dir` (a presence-driven, last-wins overlay over `--corefn-dir`).
-- |
-- | The overlay is **mandatory** (ADR-0055 correction 2026-06-29): every intended invocation
-- | resolves it (the launcher sets `PURVASM_LIB`; a dev run finds `purvasm_lib`/`dist/ulib`), and a
-- | build against the stock corefn would link JS-only foreigns (e.g. `_jsonParser`) that are unbound
-- | at run. So a missing overlay is a hard build error, not a silent un-overlaid build.
module Purvasm.CLI.Ulib
  ( resolveUlibDir
  , requireUlibDir
  , corefnPathFor
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Env as Env
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Run (Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

-- | The cwd-relative dev defaults tried when `PURVASM_LIB` is unset (ADR-0055): a bare-checkout
-- | run still overlays. The first that exists wins.
devDefaults :: Array FilePath
devDefaults = [ "purvasm_lib", "dist/ulib" ]

-- | Resolve the `ulib` overlay directory (ADR-0055): `PURVASM_LIB` if set and non-empty (the
-- | launcher sets it for npm; a developer sets it for a direct run), else the first existing
-- | cwd-relative dev default, else `Nothing`. Empty is treated as unset, per the `lookupEnv`
-- | contract — so the native `getenv` leaf, which yields "" when the variable is absent, agrees
-- | with Node's `process.env`. Callers turn `Nothing` into a hard error (see `requireUlibDir`).
resolveUlibDir :: forall r. Run (ENV + FS + r) (Maybe FilePath)
resolveUlibDir =
  Env.lookupEnv "PURVASM_LIB" >>= case _ of
    Just dir | dir /= "" -> pure (Just dir)
    _ -> firstExisting devDefaults
  where
  firstExisting dirs = case Array.uncons dirs of
    Nothing -> pure Nothing
    Just { head: d, tail } ->
      FS.exists d >>= if _ then pure (Just d) else firstExisting tail

-- | Resolve the `ulib` overlay directory or fail the build (ADR-0055 correction 2026-06-29). A
-- | no-overlay build links JS-only foreigns that are unbound at run, so we fail fast at build time
-- | with an actionable message rather than emit an almost-certainly-broken artifact.
requireUlibDir :: forall r. Run (ENV + FS + EXCEPT String + r) FilePath
requireUlibDir = resolveUlibDir >>= case _ of
  Just dir -> pure dir
  Nothing -> throw
    "PURVASM_LIB is not set and no ulib overlay was found (looked for $PURVASM_LIB, \
    \./purvasm_lib, ./dist/ulib). Run purvasm via its launcher, set PURVASM_LIB, or stage a \
    \dev ulib (e.g. `sh ulib-tools/prepare-release.sh`)."

-- | The `corefn.json` path for `moduleName`, preferring the `ulib` patch over the stock
-- | `--corefn-dir` when one is present (presence-driven, last-wins) — exactly boot's
-- | `Link.load ?ulib_dir`.
corefnPathFor :: forall r. FilePath -> FilePath -> String -> Run (FS + r) FilePath
corefnPathFor ulibDir corefnDir moduleName = do
  patchPath <- FS.joinPath [ ulibDir, moduleName, "corefn.json" ]
  FS.exists patchPath >>= case _ of
    true -> pure patchPath
    false -> FS.joinPath [ corefnDir, moduleName, "corefn.json" ]
