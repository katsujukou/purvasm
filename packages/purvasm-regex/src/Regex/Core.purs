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

import Data.Array (index, mapWithIndex)
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Purvasm.String (byteLength)
import Regex.Core.Ast (Node(..))
import Regex.Core.Match (matchAt)
import Regex.Core.Parser (parse)
import Regex.Core.Utf8 (cpAt, sliceBytes)

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
  n = byteLength s

  -- A pattern that can only match at the start (a leading `^`) is tried at position 0 only —
  -- the search loop would otherwise cost O(|input|) even though every later start fails
  -- immediately (the measured Lexer tax).
  anchored = startsAnchored r.node

  go pos =
    if pos > n then Nothing
    else case matchAt r.node s pos r.ngroups of
      Just m -> Just (render pos m)
      Nothing ->
        if anchored || pos >= n then Nothing
        -- Advance by a whole code point: a byte inside a multi-byte sequence is not a
        -- legal start position (it would mis-decode).
        else go (pos + (cpAt s pos).width)

  render start m =
    mapWithIndex
      ( \i _ ->
          if i == 0 then Just (sliceBytes s start m.end)
          else index m.caps i # join <#> \sp -> sliceBytes s sp.start sp.end
      )
      m.caps

-- Whether every path into the pattern begins with `^` (conservative syntactic check).
startsAnchored :: Node -> Boolean
startsAnchored = case _ of
  AnchorStart -> true
  Seq ns -> case index ns 0 of
    Just h -> startsAnchored h
    Nothing -> false
  Group _ sub -> startsAnchored sub
  Alt ns -> not (Array.null ns) && Array.all startsAnchored ns
  _ -> false

-- | Whether the pattern matches anywhere in the string.
test :: Regex -> String -> Boolean
test r s = isJust (match r s)
