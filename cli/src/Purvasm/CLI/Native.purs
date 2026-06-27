-- | The purvasm-native CLI entry (ADR-0045). Mirrors `Purvasm.CLI.Main`, but swaps the `Run`
-- | interpreter from the Node backend (`runNode`) to `runPurvasmNative`, which discharges the
-- | `FS`/`LOG` effects to native IO leaves instead of `node:fs`/`node:process`. boot compiles this
-- | module as the native entry (`purvm native -m Purvasm.CLI.Native`); the Node entry
-- | (`Purvasm.CLI.Main`) stays for the dual-target stock-`purs`/Node build.
-- |
-- | The IO foreigns below carry a `.js` (a `node:fs`/`node:process` implementation) so the module
-- | still builds and runs on Node; on purvasm boot ignores the `.js` and binds each name to a host
-- | leaf (ADR-0038 dual-target).
module Purvasm.CLI.Native
  ( main
  , runPurvasmNative
  ) where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Fmt as Fmt
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.CLI.Build as Build
import Purvasm.CLI.Compile as Compile
import Purvasm.CLI.Effect.Filesystem (FS, FilesystemF(..))
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Options as Options
import Run (EFFECT, Run, liftEffect, runBaseEffect)
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

-- | Native IO leaves (ADR-0045). Effectful: the IO happens when the `Effect` is run. On purvasm
-- | these resolve to boot host leaves; the `.js` is the Node implementation for the dual-target.
foreign import readTextImpl :: String -> Effect String
foreign import existsImpl :: String -> Effect Boolean
foreign import writeTextImpl :: String -> String -> Effect Unit
foreign import mkdirRecImpl :: String -> Effect Unit
foreign import argvImpl :: Effect (Array String)

main :: Effect Unit
main = do
  -- Native argv: element 0 is the executable, so drop 1 to get the user arguments.
  cliArgs <- Array.drop 1 <$> argvImpl
  case Options.parse cliArgs of
    Left err -> Console.error (ArgParser.printArgError err)
    Right cmd -> runPvm case cmd of
      Options.Compile opts -> Compile.cmd opts
      Options.Build opts -> Build.cmd opts
  where
  runPvm program = do
    res <- runPurvasmNative program
    case res of
      Right a -> pure a
      Left err -> Console.error $ Fmt.fmt @"purvasm: {err}" { err }

-- | Run a CLI program against the purvasm-native backend: the same effect row as `runNode`, but
-- | `FS`/`LOG` discharge to native IO leaves. `ENV` is absent — no command uses it (ADR-0045).
runPurvasmNative :: forall a. Run (FS + LOG + EFFECT + EXCEPT String + ()) a -> Effect (Either String a)
runPurvasmNative m = m
  # FS.interpret nativeFsHandler
  # Log.interpret (Log.terminalHandler { minLevel: Log.Info, color: true, strict: false })
  # Except.runExcept
  # runBaseEffect

-- | Discharge `Filesystem` to the native IO leaves. Only the actions `build`/`compile` use are
-- | implemented; the rest are planned increments (ADR-0045) and crash clearly if reached.
nativeFsHandler :: forall r. FilesystemF ~> Run (EFFECT + r)
nativeFsHandler = case _ of
  ReadText path k -> k <$> liftEffect do
    -- compose `Maybe` in PureScript (the leaf boundary is first-order): absent file -> Nothing.
    ok <- existsImpl path
    if ok then Just <$> readTextImpl path else pure Nothing
  WriteText path contents next -> liftEffect (writeTextImpl path contents) $> next
  MkdirP path next -> liftEffect (mkdirRecImpl path) $> next
  Exists path k -> k <$> liftEffect (existsImpl path)
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
