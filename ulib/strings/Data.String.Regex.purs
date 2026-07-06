-- | ulib shadow of `Data.String.Regex` (ADR-0081 §2): the JS `RegExp` foreigns are replaced by
-- | the pure-PureScript `Regex.Core` engine (`packages/purvasm-regex`), so regex runs
-- | identically on every backend. `Regex` becomes an ADT with an UNEXPORTED constructor —
-- | interface-compatible with the upstream opaque `foreign import data Regex`.
-- |
-- | Floor discipline (ADR-0081 §1): patterns outside the demanded floor, and flags the engine
-- | does not implement (`g`/`i`/`m`/`s`/`y` — only `u`/none are accepted), fail loudly at
-- | `regex`-construction time; `replace`/`replace'`/`search`/`split` crash loudly if reached
-- | (the ledger records them as undemanded).
module Data.String.Regex
  ( Regex
  , regex
  , source
  , flags
  , renderFlags
  , parseFlags
  , test
  , match
  , replace
  , replace'
  , search
  , split
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex.Flags (RegexFlags(..))
import Partial.Unsafe (unsafeCrashWith)
import Regex.Core as Core

-- | A compiled regular expression (constructor deliberately unexported — the upstream type is
-- | opaque, and every `Regex` in existence is inside the ADR-0081 floor by construction).
data Regex = Regex Core.Regex RegexFlags

instance showRegex :: Show Regex where
  show (Regex c f) = "/" <> Core.source c <> "/" <> renderFlags f

regex :: String -> RegexFlags -> Either String Regex
regex s f@(RegexFlags r) =
  if r.global || r.ignoreCase || r.multiline || r.dotAll || r.sticky then
    Left
      ( "regex: only the `u` flag (or none) is implemented natively (ADR-0081); got `"
          <> renderFlags f
          <> "`"
      )
  else
    Regex <$> Core.compile s <@> f

-- | Returns the pattern source.
source :: Regex -> String
source (Regex c _) = Core.source c

-- | Returns the `RegexFlags` used to construct the given `Regex`.
flags :: Regex -> RegexFlags
flags (Regex _ f) = f

-- | Returns the string representation of the given `RegexFlags`.
renderFlags :: RegexFlags -> String
renderFlags (RegexFlags f) =
  (if f.global then "g" else "")
    <> (if f.ignoreCase then "i" else "")
    <> (if f.multiline then "m" else "")
    <> (if f.dotAll then "s" else "")
    <> (if f.sticky then "y" else "")
    <> (if f.unicode then "u" else "")

-- | Parses the string representation of `RegexFlags`.
parseFlags :: String -> RegexFlags
parseFlags s = RegexFlags
  { global: contains (Pattern "g") s
  , ignoreCase: contains (Pattern "i") s
  , multiline: contains (Pattern "m") s
  , dotAll: contains (Pattern "s") s
  , sticky: contains (Pattern "y") s
  , unicode: contains (Pattern "u") s
  }

-- | Returns `true` if the `Regex` matches the string. Stateless (the upstream doc's
-- | `lastIndex` caveat cannot arise: the `g`/`y` flags are rejected at construction).
test :: Regex -> String -> Boolean
test (Regex c _) s = Core.test c s

-- | Matches the string against the `Regex` and returns an array of matches
-- | if there were any: the head is the full match, the tail the capturing groups
-- | (`Nothing` = the group did not participate) — the upstream `exec` shape.
match :: Regex -> String -> Maybe (NonEmptyArray (Maybe String))
match (Regex c _) s = Core.match c s >>= NEA.fromArray

-- | NOT implemented natively (undemanded, ADR-0081 §2 ledger): crashes loudly if reached.
replace :: Regex -> String -> String -> String
replace _ _ _ = unsafeCrashWith "Data.String.Regex.replace: not implemented natively (ADR-0081)"

-- | NOT implemented natively (undemanded, ADR-0081 §2 ledger): crashes loudly if reached.
replace' :: Regex -> (String -> Array (Maybe String) -> String) -> String -> String
replace' _ _ _ = unsafeCrashWith "Data.String.Regex.replace': not implemented natively (ADR-0081)"

-- | NOT implemented natively (undemanded, ADR-0081 §2 ledger): crashes loudly if reached.
search :: Regex -> String -> Maybe Int
search _ _ = unsafeCrashWith "Data.String.Regex.search: not implemented natively (ADR-0081)"

-- | NOT implemented natively (undemanded, ADR-0081 §2 ledger): crashes loudly if reached.
split :: Regex -> String -> Array String
split _ _ = unsafeCrashWith "Data.String.Regex.split: not implemented natively (ADR-0081)"
