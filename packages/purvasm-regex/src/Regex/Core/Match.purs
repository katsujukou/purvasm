-- | The backtracking matcher (ADR-0081 §1): CPS directly over the string's bytes/units with
-- | inline code-point decode — no whole-input conversion (the 0054 byte-oriented discipline),
-- | so an anchored fail is O(1), not O(|input|). Greedy-only, leftmost-first alternation —
-- | ES semantics restricted to the floor. Positions and capture spans are byte/unit indices.
module Regex.Core.Match
  ( matchAt
  , Captures
  ) where

import Prelude

import Data.Array (index, length, replicate, updateAt)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Purvasm.String (byteLength)
import Regex.Core.Ast (ClsItem(..), Node(..))
import Regex.Core.Unicode (inCategory)
import Regex.Core.Utf8 (cpAt)

-- | Capture spans (byte/unit indices), 1-based by group index (slot 0 unused); `Nothing` =
-- | group didn't participate.
type Captures = Array (Maybe { start :: Int, end :: Int })

-- | Match `node` against `input` starting at byte/unit index `pos`; on success, the end
-- | index and capture spans. `ngroups` sizes the capture array.
matchAt
  :: Node
  -> String
  -> Int
  -> Int
  -> Maybe { end :: Int, caps :: Captures }
matchAt node input pos ngroups =
  go node pos (replicate (ngroups + 1) Nothing) (\end caps -> Just { end, caps })
  where
  len = byteLength input

  -- One consuming step: the code point at `p` (if any) fed to `test`; advance by its width.
  consume p test k caps =
    if p >= len then Nothing
    else do
      let c = cpAt input p
      if test c.cp then k (p + c.width) caps else Nothing

  go :: Node -> Int -> Captures -> _ -> Maybe { end :: Int, caps :: Captures }
  go n pos0 caps k = case n of
    Seq ns -> goSeq ns 0 pos0 caps k
    Alt ns -> goAlt ns 0 pos0 caps k
    Lit cp -> consume pos0 (_ == cp) k caps
    Dot -> consume pos0 (not <<< isLineTerminator) k caps
    Cls negated items -> consume pos0 (classMatches negated items) k caps
    Group idx sub ->
      go sub pos0 caps \end caps' ->
        k end (fromMaybe caps' (updateAt idx (Just { start: pos0, end }) caps'))
    Rep sub mn mx -> goRep sub mn mx 0 pos0 caps k
    AnchorStart -> if pos0 == 0 then k pos0 caps else Nothing
    AnchorEnd -> if pos0 == len then k pos0 caps else Nothing
    -- Negative lookahead: succeeds iff the sub-pattern fails here; consumes nothing.
    -- Captures inside a FAILED sub-match do not persist (ES): continue with the original.
    NegAhead sub -> case go sub pos0 caps (\e c -> Just { end: e, caps: c }) of
      Just _ -> Nothing
      Nothing -> k pos0 caps

  goSeq ns i pos0 caps k =
    case index ns i of
      Nothing -> k pos0 caps
      Just n -> go n pos0 caps (\p c -> goSeq ns (i + 1) p c k)

  goAlt ns i pos0 caps k =
    case index ns i of
      Nothing -> Nothing
      Just n -> case go n pos0 caps k of
        Just r -> Just r
        Nothing -> goAlt ns (i + 1) pos0 caps k

  -- Greedy repetition, `count` iterations consumed so far: try one more (up to max) before
  -- yielding to the continuation; the ES empty-iteration rule stops a zero-width body from
  -- looping forever.
  goRep sub mn mx count pos0 caps k =
    let
      canMore = case mx of
        Nothing -> true
        Just m -> count < m
      more =
        if canMore then
          go sub pos0 caps \p c ->
            if p == pos0 && count + 1 > mn then Nothing
            else goRep sub mn mx (count + 1) p c k
        else Nothing
    in
      case more of
        Just r -> Just r
        Nothing -> if count >= mn then k pos0 caps else Nothing

  classMatches negated items c =
    let
      hit = Array.any (itemMatches c) items
    in
      if negated then not hit else hit

  itemMatches c = case _ of
    CiRange lo hi -> c >= lo && c <= hi
    CiCat cat -> inCategory cat c

  isLineTerminator c = c == 10 || c == 13 || c == 0x2028 || c == 0x2029
