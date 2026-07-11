-- | A small process-execution effect: run external tools synchronously. Shared by the CLIs that
-- | orchestrate the toolchain (`purs`, `git`, `spago`, `purvm`). The backend interpreter lives in
-- | `Purvasm.CLI.Node` (`nodeProcHandler`), which owns the `node:child_process` foreigns.
module Purvasm.CLI.Effect.Process where

import Prelude

import Data.Either (Either)
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data ProcF a
  = Exec String (Array String) (Either String Unit -> a)
  | ExecCapture String (Array String) (Either String String -> a)
  | ExecCaptureQuiet String (Array String) (Either String String -> a)
  | ExecInput String (Array String) String (Either String Unit -> a)

derive instance functorProcF :: Functor ProcF

type PROC r = (proc :: ProcF | r)

_proc :: Proxy "proc"
_proc = Proxy

interpret :: forall r a. (ProcF ~> Run r) -> Run (PROC + r) a -> Run r a
interpret h = Run.interpret (Run.on _proc h Run.send)

-- | Run a tool with stdio inherited; `Left` carries the failure message on non-zero exit.
exec :: forall r. String -> Array String -> Run (PROC + r) (Either String Unit)
exec cmd args = Run.lift _proc (Exec cmd args identity)

-- | Run a tool and capture its stdout as UTF-8 text; `Left` on failure.
execCapture :: forall r. String -> Array String -> Run (PROC + r) (Either String String)
execCapture cmd args = Run.lift _proc (ExecCapture cmd args identity)

-- | Like `execCapture` but **discards the child's stderr** (stdout still captured). For a tool whose
-- | stderr is benign noise that would otherwise leak to the terminal — e.g. `llvm-nm`'s "no symbols" per
-- | empty archive member. `Left` on failure carries a generic message (the dropped stderr is not in it).
execCaptureQuiet :: forall r. String -> Array String -> Run (PROC + r) (Either String String)
execCaptureQuiet cmd args = Run.lift _proc (ExecCaptureQuiet cmd args identity)

-- | Run a tool, piping `input` to its stdin (stdout discarded, stderr inherited); `Left` on failure.
execInput :: forall r. String -> Array String -> String -> Run (PROC + r) (Either String Unit)
execInput cmd args input = Run.lift _proc (ExecInput cmd args input identity)
