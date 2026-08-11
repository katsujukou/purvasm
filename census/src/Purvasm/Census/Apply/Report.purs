-- | The ADR-0108 §2 apply-census report: per object, the six accounting columns and — for the two
-- | generic ones — the `MissReason` breakdown.
-- |
-- | The census does not classify anything. It counts the classification EVENTS the emitter recorded
-- | while emitting the object, which is what makes the numbers the compiler's rather than the
-- | instrument's (ADR-0108 §1).
module Purvasm.Census.Apply.Report
  ( header
  , renderEvents
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Backend.LLVM.CallClass (CallClass(..), CallEvent(..), MissReason, callClassName, callClasses, callEventClass, missReasonName)

-- | TSV header (a `#` comment line, so pipelines drop it on the prefix).
header :: String
header = "#object\trow\tkey\tcount"

-- | One object's rows:
-- |
-- |   * a `class` row per accounting column — emitted even at zero, so a column that stops
-- |     occurring reads as a zero rather than as a missing line;
-- |   * a `reason` row per (generic class, `MissReason`) pair that occurred.
renderEvents :: String -> Array CallEvent -> String
renderEvents object events =
  joinWith "\n" (map classRow callClasses <> reasonRows) <> "\n"
  where
  classCounts :: Map CallClass Int
  classCounts = foldl (\m e -> Map.insertWith (+) (callEventClass e) 1 m) Map.empty events

  classRow cls = row "class" (callClassName cls) (fromMaybe 0 (Map.lookup cls classCounts))

  -- keyed by (class, reason): a reason means something different in a tail call than in a
  -- non-tail one (different emitted form, different lever), so they are never summed together.
  reasonCounts :: Map (Tuple CallClass MissReason) Int
  reasonCounts = foldl step Map.empty events
    where
    step m = case _ of
      GenericApply r -> Map.insertWith (+) (Tuple CGenericApply r) 1 m
      GenericTail r -> Map.insertWith (+) (Tuple CGenericTail r) 1 m
      _ -> m

  reasonRows =
    map (\(Tuple (Tuple cls r) n) -> row "reason" (callClassName cls <> "/" <> missReasonName r) n)
      (Map.toUnfoldable reasonCounts :: Array _)

  row kind key n = joinWith "\t" [ object, kind, key, show n ]
