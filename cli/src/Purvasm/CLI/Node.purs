module Purvasm.CLI.Node
  ( runNode
  , defaultLoggerConfig
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either, hush)
import Data.Maybe (isNothing)
import Effect (Effect)
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync as Sync
import Node.Path as Path
import Node.Process as Process
import Purvasm.CLI.Effect.Env (ENV, Env(..))
import Purvasm.CLI.Effect.Env as Env
import Purvasm.CLI.Effect.Filesystem (FS, FilesystemF(..))
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG, LogLevel(..), LoggerConfig)
import Purvasm.CLI.Effect.Log as Log
import Run (EFFECT, Run, liftEffect, runBaseEffect)
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

-- | Run a CLI program (its effect row fully closed) against the synchronous Node backend, with the
-- | global options (e.g. `--verbose`) applied to the logger. `REGISTRY` is interpreted first, into
-- | `PROC` (it asks `spago`), so it must be peeled before `PROC` is.
runNode :: forall a. Run (ENV + FS + LOG + EFFECT + EXCEPT String + ()) a -> Effect (Either String a)
runNode m = m
  # Env.interpret nodeEnvHandler
  # FS.interpret nodeFsHandler
  # Log.interpret (Log.terminalHandler (defaultLoggerConfig { minLevel = Log.Info }))
  # Except.runExcept
  # runBaseEffect

-- | Read an environment variable from `process.env` (`Nothing` if unset).
nodeEnvHandler :: forall r. Env ~> Run (EFFECT + r)
nodeEnvHandler = case _ of
  LookupEnv name k -> k <$> liftEffect (Process.lookupEnv name)

-- | The default console logging config. The bin prototype logged every message via `Console.log`;
-- | mapping its messages to `info` keeps them visible at this level (`debug` is the quieter tier).
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = { minLevel: Info, color: true, strict: false }

nodeFsHandler :: forall r. FilesystemF ~> Run (EFFECT + r)
nodeFsHandler = case _ of
  ReadText path k -> k <$> liftEffect (hush <$> try (Sync.readTextFile UTF8 path))
  ReadBinary path k -> k <$> liftEffect (hush <$> try (readFileBytesImpl path))
  WriteText path contents next -> liftEffect (Sync.writeTextFile UTF8 path contents) $> next
  WriteBinary path bytes next -> liftEffect (writeFileBytesImpl path bytes) $> next
  ReadDir path k -> k <$> liftEffect (hush <$> try (Sync.readdir path))
  Exists path k -> k <$> liftEffect (isNothing <$> Sync.access path)
  FileSize path k -> k <$> liftEffect (hush <$> try (fileSizeImpl path))
  MkdirP path next -> liftEffect (Sync.mkdir' path { recursive: true, mode: permsAll }) $> next
  Unlink path next -> liftEffect (Sync.unlink path) $> next
  JoinPath segments k -> pure (k (Path.concat segments))
  ResolvePath segments last k -> k <$> liftEffect (Path.resolve segments last)

-- | Run an external tool synchronously (`execFileSync`, stdio inherited; throws on non-zero exit).
foreign import execFileImpl :: String -> Array String -> Effect Unit

-- | Like `execFileImpl` but pipes `input` to the child's stdin (stdout/stderr inherited).
foreign import execFileInputImpl :: String -> Array String -> String -> Effect Unit

-- | Read this process's entire stdin synchronously (`fs.readFileSync(0)`).
foreign import readStdinImpl :: Effect String

-- | Run an external tool synchronously and return its captured stdout (`execFileSync` with
-- | `encoding: utf8`); throws on failure, which the handler turns into `Left` via `try`.
foreign import execFileCaptureImpl :: String -> Array String -> Effect String

-- | Read/write a file as bytes. `node:fs` deals in `Buffer`, which *is* a `Uint8Array`, so these
-- | convert at the boundary with no copy — keeping the `Filesystem` effect Node-agnostic.
foreign import readFileBytesImpl :: String -> Effect Uint8Array

foreign import writeFileBytesImpl :: String -> Uint8Array -> Effect Unit

-- | A file's size in bytes (`statSync().size`).
foreign import fileSizeImpl :: String -> Effect Int
