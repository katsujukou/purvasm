-- | Build the **loadable provider** a hosted guest needs, beside the image it belongs to
-- | ([ADR-0110](../../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §6 step E).
-- |
-- | A ulib native leaf — `Data.Number.isFinite`, `Data.Show.showNumberImpl` — is shipped as `.c`
-- | source and *linked* into a natively compiled program ([ADR-0073](../../../../docs/design-decisions/0073-ulib-shipped-native-foreign-and-link-time-resolution.md)
-- | §2: an object is target-specific and a registry package must stay portable, so a prebuilt one is
-- | explicitly not the distributed form). A program running **inside** the owned VM has no such link,
-- | so it reaches those keys the way
-- | [0111](../../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §4 says a
-- | workspace-provided key is reached: through a module the runner loads. Building it here from the
-- | same `.c`, for whatever host is running, is that convention applied to a new consumer.
-- |
-- | **Every referenced native leaf is accounted for, or the build stops.** Each one must be defined by
-- | the runtime staticlib (resolved at run through `host-runtime`, so it needs no packaging) or mapped
-- | to a `.c` by the staged ulib. A key that is neither is refused *by name* here, because the
-- | alternative is an image that looks complete and fails partway through a run — and because the
-- | quiet version of that failure is a missing `ulib.json` being read as "the workspace provides
-- | nothing", which is a different claim entirely.
module Purvasm.CLI.ForeignProvider
  ( ProviderArtifacts
  , emitProvider
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Fmt as Fmt
import Purvasm.Abi.Mangle (escapeIdent)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Purvasm.CLI.NativeLink (hostIsMacos, loadForeignSources, manifestOfKeys, nmDefinedPvf, resolveInclude, resolveRuntimeLib, sharedObjectFlags)
import Run (Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

-- | What a finalisation wrote for the runner to hand the VM: the manifest of workspace-provided keys,
-- | and the provider that supplies them — `Nothing` when the image references none.
-- |
-- | The runner must take the provider from **here** rather than from whether a file happens to sit in
-- | the output directory: an outdir reused from a build that did need one would otherwise hand the VM
-- | a stale module.
type ProviderArtifacts =
  { manifest :: FilePath
  , provider :: Maybe FilePath
  }

-- | Emit the manifest, and the provider when there is anything to provide.
-- |
-- | `referenced` is every foreign key the image mentions — an over-approximation of what a run needs,
-- | exactly as ADR-0111 §4 intends: a key can sit in a branch that never executes, and neither this
-- | nor the VM has a liveness result to tell the difference. Over-approximating costs a compiled
-- | source; under-approximating is a program that fails partway through.
emitProvider
  :: forall r
   . { outDir :: FilePath, ulibDir :: FilePath, referenced :: Set String }
  -> Run (ENV + PROC + FS + LOG + EXCEPT String + r) ProviderArtifacts
emitProvider opts = do
  workspace <- loadForeignSources opts.ulibDir
  let
    provided = Map.filterKeys (\k -> Set.member k opts.referenced) workspace
    fromUlib = Map.keys provided
  -- Classification only matters when there is something to classify, so a foreign-free program needs
  -- neither a staged ulib nor a runtime staticlib to reach here.
  unless (Set.isEmpty opts.referenced) do
    runtime <- runtimeSymbols
    let
      -- Membership is tested by MANGLING the key, never by demangling the symbol: `escapeIdent` is
      -- total and lossless in that direction, while its inverse is the approximation the codebase
      -- keeps for diagnostics (`NativeLink.demangleKey`). Getting this backwards would drop a key
      -- whose name escapes to more than `_2e` and report it as unprovided.
      hostProvides k = Set.member ("pvf_" <> escapeIdent k) runtime
      unaccounted = Array.filter (\k -> not (hostProvides k)) (Set.toUnfoldable (Set.difference opts.referenced fromUlib))
    unless (Array.null unaccounted) (unaccountedFor unaccounted)
  manifest <- FS.joinPath [ opts.outDir, "app.manifest" ]
  FS.writeText manifest (manifestOfKeys (Set.toUnfoldable fromUlib))
  providerPath <- FS.joinPath [ opts.outDir, "app.provider.so" ]
  provider <-
    if Map.isEmpty provided then do
      -- An outdir is reused, and a build that provides nothing must not leave the last one's module
      -- behind for a runner to find.
      stale <- FS.exists providerPath
      when stale (FS.unlink providerPath)
      pure Nothing
    else do
      -- One `.c` can provide several keys (ulib's `Data.Number` provides seven), so the sources are
      -- deduped: compiling one twice would define every symbol in it twice.
      compile providerPath (Array.nub (Array.fromFoldable (Map.values provided))) (Set.toUnfoldable fromUlib)
      pure (Just providerPath)
  pure { manifest, provider }
  where
  -- The `pvf_*` the runtime staticlib defines. Those resolve through `host-runtime` at run and are
  -- deliberately absent from the manifest, so the VM leaves them lazy (ADR-0111 §4).
  runtimeSymbols = resolveRuntimeLib Nothing >>= nmDefinedPvf

  unaccountedFor keys = do
    ulibJson <- FS.joinPath [ opts.ulibDir, "ulib.json" ]
    staged <- FS.exists ulibJson
    throw
      ( Fmt.fmt
          @"the image references native leaves nothing can provide: {keys}\n  \
            \Each must be defined by the runtime staticlib or mapped to a `.c` by the purvasm library.\n  {hint}"
          { keys: String.joinWith ", " keys
          , hint:
              if staged then
                "The library at " <> opts.ulibDir
                  <> " declares no source for them — a user module's sibling `.c` is not packaged for a hosted guest yet."
              else
                "The library at " <> opts.ulibDir <> " has no `ulib.json`, so its sources could not be consulted"
                  <> " at all: that path is not a complete purvasm library. Check what $PURVASM_LIB points at."
          }
      )

  compile out sources keys = do
    include <- resolveInclude
    macos <- hostIsMacos
    let
      args = sharedObjectFlags macos <> [ "-O2", "-I" <> include ] <> sources <> [ "-o", out ]
      forKeys = String.joinWith ", " keys
    Log.debug $ Fmt.fmt @"  provider: compiling {n} source(s) for {forKeys}"
      { n: show (Array.length sources), forKeys }
    Proc.exec "clang" args >>= case _ of
      Right _ -> pure unit
      Left err -> throw
        (Fmt.fmt @"could not build the foreign provider for {forKeys}: {err}" { forKeys, err })
