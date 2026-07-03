-- | The purvasm-native CLI entry (ADR-0045). Mirrors `Purvasm.CLI.Main`, but swaps the `Run`
-- | interpreter from the Node backend (`runNode`) to `runPurvasmNative`, which discharges the
-- | `ENV`/`FS`/`LOG` effects to the host-system packages (`purvasm-system`, `purvasm-fs`, ADR-0056)
-- | instead of the `node-*` packages. boot compiles this module as the native entry
-- | (`purvm native -m Purvasm.CLI.Native`); the Node entry (`Purvasm.CLI.Main`) stays for the
-- | dual-target stock-`purs`/Node build.
-- |
-- | This module declares no host leaves of its own: the leaves live in the reusable
-- | `Purvasm.System.*` / `Purvasm.FS` packages, and this interpreter merely wires the cli-lib `Run`
-- | effects to them.
module Purvasm.CLI.Native
  ( main
  , runPurvasmNative
  ) where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Fmt as Fmt
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.CLI.Build as Build
import Purvasm.CLI.Compile as Compile
import Purvasm.CLI.Effect.Env (ENV, Env(..))
import Purvasm.CLI.Effect.Env as Env
import Purvasm.CLI.Effect.Filesystem (FS, FilesystemF(..))
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Options as Options
import Purvasm.FS as File
import Purvasm.System.Env (lookupEnv) as Sys
import Purvasm.System.Process (argv, exit) as Sys
import Run (EFFECT, Run, liftEffect, runBaseEffect)
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

main :: Effect Unit
main = do
  -- Native argv: element 0 is the executable, so drop 1 to get the user arguments.
  cliArgs <- Array.drop 1 <$> Sys.argv
  case Options.parse cliArgs of
    Left err -> Console.error (ArgParser.printArgError err) *> void (Sys.exit 1)
    Right cmd -> runPvm case cmd of
      Options.Compile opts -> Compile.cmd opts
      Options.Build opts -> Build.cmd opts
  where
  runPvm program = do
    res <- runPurvasmNative program
    case res of
      Right a -> pure a
      Left err -> Console.error (Fmt.fmt @"purvasm: {err}" { err }) *> void (Sys.exit 1)

-- | Run a CLI program against the purvasm-native backend: the same effect row as `runNode`, with
-- | `ENV`/`FS`/`LOG` discharged to the host-system packages. `ENV` is read only to resolve
-- | `PURVASM_LIB` for the `ulib` overlay (ADR-0055 refines ADR-0045, which had dropped `ENV`).
runPurvasmNative :: forall a. Run (ENV + FS + LOG + EFFECT + EXCEPT String + ()) a -> Effect (Either String a)
runPurvasmNative m = m
  # Env.interpret nativeEnvHandler
  # FS.interpret nativeFsHandler
  # Log.interpret (Log.terminalHandler { minLevel: Log.Info, color: true, strict: false })
  # Except.runExcept
  # runBaseEffect

-- | Discharge `Env` to `Purvasm.System.Env` (an unset/empty variable is `Nothing`).
nativeEnvHandler :: forall r. Env ~> Run (EFFECT + r)
nativeEnvHandler = case _ of
  LookupEnv name k -> k <$> liftEffect (Sys.lookupEnv name)

-- | Discharge `Filesystem` to `Purvasm.FS`. Only the actions `build`/`compile` use are implemented;
-- | the rest are planned increments (ADR-0045) and crash clearly if reached.
nativeFsHandler :: forall r. FilesystemF ~> Run (EFFECT + r)
nativeFsHandler = case _ of
  ReadText path k -> k <$> liftEffect (File.readTextFile path)
  WriteText path contents next -> liftEffect (File.writeTextFile path contents) $> next
  MkdirP path next -> liftEffect (File.mkdirp path) $> next
  Exists path k -> k <$> liftEffect (File.exists path)
  -- POSIX-only path ops: a pure `/` separator, correct on the boot/posix target this interpreter
  -- runs on. Windows fidelity (a platform-aware host path leaf) is deferred to release (ADR-0045);
  -- the Node interpreter (`runNode`) is already platform-correct via `node:path`.
  JoinPath segments k -> pure (k (intercalate "/" segments))
  Dirname path k -> pure (k (intercalate "/" (Array.dropEnd 1 (String.split (Pattern "/") path))))
  ReadBinary _ _ -> unsafeCrashWith notYet
  WriteBinary _ _ _ -> unsafeCrashWith notYet
  ReadDir _ _ -> unsafeCrashWith notYet
  Unlink _ _ -> unsafeCrashWith notYet
  FileSize _ _ -> unsafeCrashWith notYet
  ResolvePath _ _ _ -> unsafeCrashWith notYet
  where
  notYet = "Filesystem: action not yet implemented on the purvasm-native backend (ADR-0045)"
