-- | `purvasm build`'s native link step (ADR-0082 §1, ADR-0072 §3): compile each emitted `.ll` object
-- | with `clang -c -O2` and link them with the runtime staticlib into an executable, tree-shaking dead
-- | symbols (`--gc-sections` / `-dead_strip`). A faithful transcription of boot's `emit_native_llvm`
-- | link driver, over the CLI `PROC`/`FS` effects. Slice-1a covers the runtime-only link (no `ulib`
-- | native-foreign `.c`, no Rust crates — those are later slices).
module Purvasm.CLI.NativeLink
  ( LinkOptions
  , link
  ) where

import Prelude

import Data.Array ((..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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
import Run (Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type LinkOptions =
  { output :: FilePath -- ^ the output directory (the executable lands at `<output>/app`)
  , buildDir :: FilePath -- ^ where the `.ll`/`.o` objects live (`<output>/_build`)
  , moduleCount :: Int -- ^ number of `mod_<i>.ll` objects (plus the `entry.ll` object)
  , runtimeLib :: Maybe FilePath -- ^ explicit runtime staticlib path (`--runtime-lib`), else resolved
  }

-- | The default conventional runtime staticlib path (release profile — the inline-ABI objects this
-- | backend emits pair with the release runtime, ADR-0079 §2).
defaultRuntimeLib :: FilePath
defaultRuntimeLib = "runtime/target/release/libpurvasm_rt.a"

-- | Resolve the runtime staticlib: `--runtime-lib`, else `$PURVASM_RT_A`, else the conventional repo
-- | path — each checked to exist (ADR-0071 §1). Fails clearly if none is found.
resolveRuntimeLib
  :: forall r
   . Maybe FilePath
  -> Run (ENV + FS + EXCEPT String + r) FilePath
resolveRuntimeLib = case _ of
  Just p -> requireExisting p
  Nothing -> Env.lookupEnv "PURVASM_RT_A" >>= case _ of
    Just p -> requireExisting p
    Nothing -> requireExisting defaultRuntimeLib
  where
  requireExisting p = FS.exists p >>= case _ of
    true -> pure p
    false -> throw
      ( "runtime staticlib (libpurvasm_rt.a) not found at "
          <> p
          <> ". Pass --runtime-lib PATH, set $PURVASM_RT_A, or `cargo build --release` in runtime/."
      )

-- | Whether the host linker is Apple ld64 (macOS) — its tree-shaking / section flags differ from
-- | GNU ld / lld. Detected via `uname -s`, matching boot.
hostIsMacos :: forall r. Run (PROC + r) Boolean
hostIsMacos = Proc.execCapture "uname" [ "-s" ] >>= case _ of
  Right out -> pure (String.trim out == "Darwin")
  Left _ -> pure false

-- | Compile each `.ll` object and link them with the runtime staticlib into `<output>/app`.
link :: forall r. LinkOptions -> Run (ENV + PROC + FS + LOG + EXCEPT String + r) Unit
link opts = do
  rt <- resolveRuntimeLib opts.runtimeLib
  macos <- hostIsMacos
  let
    -- GNU ld's `--gc-sections` strips only when each function/datum is in its own section; ld64
    -- dead-strips per symbol and needs nothing (ADR-0072 §3).
    sectionFlags = if macos then [] else [ "-ffunction-sections", "-fdata-sections" ]
    deadStrip = if macos then "-Wl,-dead_strip" else "-Wl,--gc-sections"
    tags = map (\i -> "mod_" <> show i) (0 .. (opts.moduleCount - 1)) <> [ "entry" ]
  objs <- traverse (compileObj sectionFlags) tags
  exe <- FS.joinPath [ opts.output, "app" ]
  -- `-lm` for libm calls a native foreign `.c` may emit; harmless with none.
  runClang "link" ([ deadStrip ] <> objs <> [ rt, "-lm", "-o", exe ])
  Log.info $ Fmt.fmt @"✓ Linked native executable → {exe}" { exe }
  where
  compileObj sectionFlags tag = do
    ll <- FS.joinPath [ opts.buildDir, tag <> ".ll" ]
    obj <- FS.joinPath [ opts.buildDir, tag <> ".o" ]
    runClang (tag <> " (clang -c)") ([ "-c", "-O2" ] <> sectionFlags <> [ ll, "-o", obj ])
    pure obj

  runClang label args = Proc.exec "clang" args >>= case _ of
    Right _ -> pure unit
    Left e -> throw (Fmt.fmt @"{label} failed: {e}" { label, e })
