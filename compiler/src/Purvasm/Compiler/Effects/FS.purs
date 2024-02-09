module Purvasm.Compiler.Effects.FS where

import Prelude

import Data.Array as Array
import Foreign (Foreign)
import Node.Cbor as Cbor
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic as Glob
import Node.Path as Path
import Run (Run, AFF)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type FilePath = String

data Fs a
  = ExpandGlob FilePath String (Array FilePath -> a)
  | ReadTextFile FilePath (String -> a)
  | ReadCborFile FilePath (Foreign -> a)
  | ConcatPaths (Array FilePath) (FilePath -> a)

derive instance Functor Fs

type FS r = (fs :: Fs | r)

_fs :: Proxy "fs"
_fs = Proxy

expandGlob :: forall r. FilePath -> String -> Run (FS + r) (Array FilePath)
expandGlob cwd glob = Run.lift _fs (ExpandGlob cwd glob identity)

readTextFile :: forall r. FilePath -> Run (FS + r) String
readTextFile file = Run.lift _fs (ReadTextFile file identity)

readCborFile :: forall r. FilePath -> Run (FS + r) Foreign
readCborFile file = Run.lift _fs (ReadCborFile file identity)

concatPaths :: forall r. Array FilePath -> Run (FS + r) FilePath
concatPaths paths = Run.lift _fs (ConcatPaths paths identity)

interpret :: forall a r. (Fs ~> Run r) -> Run (FS + r) a -> Run r a
interpret handler = Run.interpret (Run.on _fs handler Run.send)

handleNode :: forall a r. Fs a -> Run (AFF + r) a
handleNode = case _ of
  ExpandGlob cwd glob reply -> do
    paths <- Run.liftAff $
      Array.fromFoldable <$> Glob.expandGlobs cwd [ glob ]
    pure (reply paths)
  ReadTextFile file reply -> do
    content <- Run.liftAff (FS.readTextFile UTF8 file)
    pure (reply content)
  ReadCborFile file reply -> do
    f <- Run.liftAff do
      Cbor.decodeFirst =<< FS.readFile file
    pure (reply f)
  ConcatPaths paths reply -> do
    pure (reply (Path.concat paths))