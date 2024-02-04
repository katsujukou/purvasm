module Purvasm.Compiler.Effects.FS where

import Prelude

import Data.Array as Array
import Data.Set (Set)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic as Glob
import Node.Path (FilePath)
import Run (Run, AFF)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Fs a
  = ExpandGlob FilePath String (Array FilePath -> a)
  | ReadTextFile FilePath (String -> a)

derive instance Functor Fs

type FS r = (fs :: Fs | r)

_fs :: Proxy "fs"
_fs = Proxy

expandGlob :: forall r. FilePath -> String -> Run (FS + r) (Array FilePath)
expandGlob cwd glob = Run.lift _fs (ExpandGlob cwd glob identity)

readTextFile :: forall r. FilePath -> Run (FS + r) String
readTextFile file = Run.lift _fs (ReadTextFile file identity)

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