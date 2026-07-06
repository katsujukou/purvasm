-- | `purvasm-regex` (ADR-0081): an ES-flavour regex core in pure PureScript — compile a
-- | pattern (floor-only, loud errors) and match with ES `RegExp.prototype.match`-shaped
-- | results. The `Data.String.Regex` ulib shadow is a thin adapter over this module.
module Regex.Core
  ( Regex
  , compile
  , source
  , match
  , test
  ) where

import Prelude

import Data.Array (index, length, mapWithIndex, slice)
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Regex.Core.Ast (Node)
import Regex.Core.Match (matchAt)
import Regex.Core.Parser (parse)
import Regex.Core.Utf8 (fromCodePoints, toCodePoints)

-- | A compiled pattern. Abstract: consumers construct via `compile` only, so every `Regex`
-- | in existence is inside the ADR-0081 floor.
newtype Regex = Regex { node :: Node, ngroups :: Int, source :: String }

-- | Compile a pattern (no flags here: the core is always code-point/`u`-semantic, ADR-0006;
-- | flags interpretation belongs to the shadow layer). Out-of-floor syntax is a `Left`.
compile :: String -> Either String Regex
compile src = parse src <#> \r -> Regex { node: r.node, ngroups: r.ngroups, source: src }

-- | The original pattern source.
source :: Regex -> String
source (Regex r) = r.source

-- | ES `match` (no `g`): search from the left, first position where the pattern matches;
-- | index 0 = the full match, then one entry per capturing group (`Nothing` = didn't
-- | participate). `Nothing` overall = no match anywhere.
match :: Regex -> String -> Maybe (Array (Maybe String))
match (Regex r) s = go 0
  where
  cps = toCodePoints s
  n = length cps

  go pos =
    if pos > n then Nothing
    else case matchAt r.node cps pos r.ngroups of
      Just m -> Just (render pos m)
      Nothing -> go (pos + 1)

  render start m =
    mapWithIndex
      ( \i _ ->
          if i == 0 then Just (sliceStr start m.end)
          else index m.caps i # join <#> \sp -> sliceStr sp.start sp.end
      )
      m.caps

  sliceStr lo hi = fromCodePoints (slice lo hi cps)

-- | Whether the pattern matches anywhere in the string.
test :: Regex -> String -> Boolean
test r s = isJust (match r s)
