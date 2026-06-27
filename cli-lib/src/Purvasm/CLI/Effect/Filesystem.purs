module Purvasm.CLI.Effect.Filesystem where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe)
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type FilePath = String

data FilesystemF a
  = ReadText FilePath (Maybe String -> a)
  | WriteText FilePath String a
  | ReadDir FilePath (Maybe (Array String) -> a)
  | Exists FilePath (Boolean -> a)
  | MkdirP FilePath a
  | Unlink FilePath a
  | ReadBinary FilePath (Maybe Uint8Array -> a)
  | WriteBinary FilePath Uint8Array a
  | FileSize FilePath (Maybe Int -> a)
  -- Path ops are effects too — not for side effects, but for *portability*: 
  -- the path separator and resolution are environment-specific, so the interpreter owns them.
  | JoinPath (Array FilePath) (FilePath -> a)
  | Dirname FilePath (FilePath -> a)
  | ResolvePath (Array FilePath) FilePath (FilePath -> a)

derive instance functorFilesystemF :: Functor FilesystemF

type FS r = (fs :: FilesystemF | r)

_fs :: Proxy "fs"
_fs = Proxy

interpret :: forall r a. (FilesystemF ~> Run r) -> Run (FS + r) a -> Run r a
interpret h = Run.interpret (Run.on _fs h Run.send)

-- | Read a UTF-8 file, `Nothing` if it cannot be read.
readText :: forall r. FilePath -> Run (FS + r) (Maybe String)
readText path = Run.lift _fs (ReadText path identity)

-- | Read a file as bytes, `Nothing` if it cannot be read.
readBinary :: forall r. FilePath -> Run (FS + r) (Maybe Uint8Array)
readBinary path = Run.lift _fs (ReadBinary path identity)

writeText :: forall r. FilePath -> String -> Run (FS + r) Unit
writeText path contents = Run.lift _fs (WriteText path contents unit)

writeBinary :: forall r. FilePath -> Uint8Array -> Run (FS + r) Unit
writeBinary path bytes = Run.lift _fs (WriteBinary path bytes unit)

-- | List a directory's entries, `Nothing` if it cannot be read.
readDir :: forall r. FilePath -> Run (FS + r) (Maybe (Array String))
readDir path = Run.lift _fs (ReadDir path identity)

exists :: forall r. FilePath -> Run (FS + r) Boolean
exists path = Run.lift _fs (Exists path identity)

-- | The size of a file in bytes, `Nothing` if it cannot be stat'd.
fileSize :: forall r. FilePath -> Run (FS + r) (Maybe Int)
fileSize path = Run.lift _fs (FileSize path identity)

-- | Create a directory (and any missing parents).
mkdirP :: forall r. FilePath -> Run (FS + r) Unit
mkdirP path = Run.lift _fs (MkdirP path unit)

unlink :: forall r. FilePath -> Run (FS + r) Unit
unlink path = Run.lift _fs (Unlink path unit)

-- | Join path segments (the interpreter supplies the platform separator / `concat` semantics).
joinPath :: forall r. Array FilePath -> Run (FS + r) FilePath
joinPath segments = Run.lift _fs (JoinPath segments identity)

-- | Resolve path segments to an absolute, normalised path (mirrors `path.resolve(...segs, last)`).
resolvePath :: forall r. Array FilePath -> FilePath -> Run (FS + r) FilePath
resolvePath segments last = Run.lift _fs (ResolvePath segments last identity)

dirname :: forall r. FilePath -> Run (FS + r) FilePath
dirname path = Run.lift _fs (Dirname path identity) 
