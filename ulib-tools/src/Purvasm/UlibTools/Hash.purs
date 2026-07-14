-- | SHA-256 of a file's raw bytes (`node:crypto`), used by `unicode-gen` to pin its upstream input
-- | (ADR-0101). Kept local to `ulib-tools`, not `cli-lib`: no other command needs hashing yet
-- | (ADR-0043 §1 — `cli-lib` is for genuinely shared node-effect helpers, not speculative reuse).
module Purvasm.UlibTools.Hash
  ( sha256File
  ) where

import Effect (Effect)

-- | The lowercase hex SHA-256 digest of the file at `path`. Hashes the raw bytes (not a UTF-8 text
-- | round-trip), so it pins exactly what was downloaded.
sha256File :: String -> Effect String
sha256File = sha256FileImpl

foreign import sha256FileImpl :: String -> Effect String
