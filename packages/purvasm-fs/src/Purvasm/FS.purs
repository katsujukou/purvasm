-- | File-system IO — the host-system rung (ADR-0022/0056), the Node `fs` / OCaml
-- | `In_channel`/`Out_channel` role. First-order leaves wrapped in safe `Effect` APIs; `Maybe` is
-- | composed in PureScript (the leaf boundary is first-order, ADR-0020/0036). On purvasm, boot
-- | binds each leaf to a host leaf; the `.js` is the Node impl for the dual-target (ADR-0038).
module Purvasm.FS
  ( exists
  , readTextFile
  , writeTextFile
  , mkdirp
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

-- The raw leaves are not exported; the safe wrappers below are the public surface.
foreign import readTextImpl :: String -> Effect String
foreign import existsImpl :: String -> Effect Boolean
foreign import writeTextImpl :: String -> String -> Effect Unit
foreign import mkdirRecImpl :: String -> Effect Unit

-- | Whether a path exists.
exists :: String -> Effect Boolean
exists = existsImpl

-- | Read a UTF-8 file; `Nothing` if it does not exist.
readTextFile :: String -> Effect (Maybe String)
readTextFile path = do
  ok <- existsImpl path
  if ok then Just <$> readTextImpl path else pure Nothing

-- | Write a UTF-8 file, overwriting any existing content.
writeTextFile :: String -> String -> Effect Unit
writeTextFile = writeTextImpl

-- | Create a directory and any missing parents (`mkdir -p`).
mkdirp :: String -> Effect Unit
mkdirp = mkdirRecImpl
