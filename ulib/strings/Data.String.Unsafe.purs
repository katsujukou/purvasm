-- | ulib SHADOW of `strings`' `Data.String.Unsafe` (ADR-0038 / ADR-0006), targeting strings 6.0.1.
-- |
-- | The registry foreigns are UTF-16 JS string ops; here they are the (unchecked) code-point
-- | accessors built on the `Data.String.CodeUnits` shadow — `unsafePartial (fromJust …)`, so an
-- | out-of-bounds index / wrong length traps (the "unsafe" contract) instead of returning `Maybe`.
-- | Self-contained on purvasm (no foreign). The public interface is unchanged.
module Data.String.Unsafe
  ( char
  , charAt
  ) where

import Data.Maybe (fromJust)
import Data.String.CodeUnits as CU
import Partial.Unsafe (unsafePartial)

-- | Returns the character at the given (code-point) index.
-- |
-- | **Unsafe:** traps if the index is out of bounds.
charAt :: Int -> String -> Char
charAt i s = unsafePartial (fromJust (CU.charAt i s))

-- | Converts a string of length `1` to a character.
-- |
-- | **Unsafe:** traps if the length is not `1`.
char :: String -> Char
char s = unsafePartial (fromJust (CU.toChar s))
