-- | Standard-stream IO — the host-system rung (ADR-0068), a sibling of `purvasm-fs` /
-- | `purvasm-system`. Two total line writers to the standard streams; each appends a newline, so this
-- | is the generic capability `Effect.Console` is a `ulib` shadow over (`log`/`info`/`debug` →
-- | `writeLine`, `warn`/`error` → `writeErrLine`). The runtime knows only these generic leaves, not a
-- | JS-derived `Console.log` name. First-order leaves wrapped in a safe `Effect` API (ADR-0020/0036);
-- | on purvasm boot binds each leaf to a host leaf, the `.js` is the Node impl for the dual-target
-- | (ADR-0038).
module Purvasm.Stdio
  ( writeLine
  , writeErrLine
  ) where

import Data.Unit (Unit)
import Effect (Effect)

-- The raw leaves are not exported; the safe wrappers below are the public surface.
foreign import writeLineImpl :: String -> Effect Unit
foreign import writeErrLineImpl :: String -> Effect Unit

-- | Write a line to standard output (the text followed by a newline).
writeLine :: String -> Effect Unit
writeLine = writeLineImpl

-- | Write a line to standard error (the text followed by a newline).
writeErrLine :: String -> Effect Unit
writeErrLine = writeErrLineImpl
