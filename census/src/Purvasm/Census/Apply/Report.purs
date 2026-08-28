-- | The ADR-0108 §2 apply-census report: per object, the accounting columns and their breakdowns.
-- |
-- | The census does not classify anything. It counts the classification EVENTS the emitter recorded
-- | while emitting the object, which is what makes the numbers the compiler's rather than the
-- | instrument's (ADR-0108 §1).
-- |
-- | ADR-0113 §3 splits what used to be one breakdown into TWO families that are checked separately:
-- |
-- |   * the OPAQUE family — the generic classes, broken down by `MissReason` (and
-- |     `local-unknown-fn/<origin>` is itself a three-level key);
-- |   * the CANDIDATE family — the `local-deferred` classes, broken down by `CandidateKind`.
-- |
-- | They are not one enumeration and must not be summed into one identity: a candidate does not
-- | produce a `MissReason` at all, so a gate stated over `MissReason` alone would balance while a
-- | candidate went missing.
module Purvasm.Census.Apply.Report
  ( header
  , renderEvents
  , RowKey(..)
  , ParsedRow
  , parseRow
  , Identities
  , checkIdentities
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Backend.LLVM.CallClass (CallClass(..), CallEvent(..), MissReason, callClassName, callClasses, callEventClass, missReasonName, allMissReasons)
import Purvasm.Compiler.Backend.LLVM.Types (CandidateKind, candidateKindName, candidateKinds)

-- | TSV header (a `#` comment line, so pipelines drop it on the prefix).
header :: String
header = "#object\trow\tkey\tcount"

-- | The generic classes a `MissReason` can be reported under. Stated once: the report, the identity
-- | and the gate all read this, so a form added here cannot be forgotten in one of the three.
genericForms :: Array CallClass
genericForms = [ CGenericApply, CGenericTail ]

-- | The ADR-0113 candidate classes, the same way.
candidateForms :: Array CallClass
candidateForms = [ CLocalDeferredApply, CLocalDeferredTail ]

-- | One object's rows:
-- |
-- |   * a `class` row per accounting column;
-- |   * a `reason` row per (generic form × `MissReason`);
-- |   * a `kind` row per (candidate form × `CandidateKind`).
-- |
-- | **Every row of every product is emitted, including the zeros.** A breakdown that printed only
-- | what occurred cannot be told apart from one whose row went missing, and "absent" would then read
-- | as "zero" in exactly the situation the identities exist to catch.
renderEvents :: String -> Array CallEvent -> String
renderEvents object events =
  joinWith "\n" (map classRow callClasses <> reasonRows <> kindRows) <> "\n"
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

  kindCounts :: Map (Tuple CallClass CandidateKind) Int
  kindCounts = foldl step Map.empty events
    where
    step m = case _ of
      LocalDeferredApply k -> Map.insertWith (+) (Tuple CLocalDeferredApply k) 1 m
      LocalDeferredTail k -> Map.insertWith (+) (Tuple CLocalDeferredTail k) 1 m
      _ -> m

  reasonRows = do
    cls <- genericForms
    r <- allMissReasons
    pure (row "reason" (callClassName cls <> "/" <> missReasonName r) (fromMaybe 0 (Map.lookup (Tuple cls r) reasonCounts)))

  kindRows = do
    cls <- candidateForms
    k <- candidateKinds
    pure (row "kind" (callClassName cls <> "/" <> candidateKindName k) (fromMaybe 0 (Map.lookup (Tuple cls k) kindCounts)))

  row kind key n = joinWith "\t" [ object, kind, key, show n ]

-- --- the gate ---------------------------------------------------------------------------------

-- | A parsed report row. The key of a `reason` row is `<class>/<reason>` and a `MissReason` may
-- | ITSELF contain a slash (`local-unknown-fn/<origin>`), so the split is on the FIRST separator
-- | only — splitting on every `/` would drop the origin and silently merge seven rows into one.
data RowKey
  = ClassRow String
  | ReasonRow String String -- ^ class, reason (the reason keeps its own `/…` intact)
  | KindRow String String -- ^ class, candidate kind

derive instance eqRowKey :: Eq RowKey
derive instance ordRowKey :: Ord RowKey

instance showRowKey :: Show RowKey where
  show = case _ of
    ClassRow c -> "class " <> c
    ReasonRow c r -> "reason " <> c <> "/" <> r
    KindRow c k -> "kind " <> c <> "/" <> k

-- | A parsed row, INCLUDING the object it belongs to. The object is not decoration: the identities
-- | are per object, and two objects' rows summed together let one's shortfall be covered by the
-- | other's surplus. Carrying it here is what lets [`checkIdentities`] refuse a mixed set outright
-- | instead of balancing it.
type ParsedRow =
  { object :: String
  , key :: RowKey
  , count :: Int
  }

-- | Parse one TSV line. `Nothing` for the header and for anything malformed — the caller treats an
-- | unparsed row as a failure, never as a zero.
parseRow :: String -> Maybe ParsedRow
parseRow line = case String.split (Pattern "\t") line of
  [ object, kind, key, count ] -> do
    n <- Int.fromString count
    k <- case kind of
      "class" -> Just (ClassRow key)
      "reason" -> map (\(Tuple c r) -> ReasonRow c r) (splitFirst key)
      "kind" -> map (\(Tuple c x) -> KindRow c x) (splitFirst key)
      _ -> Nothing
    Just { object, key: k, count: n }
  _ -> Nothing
  where
  splitFirst s = case String.indexOf (Pattern "/") s of
    Just i -> Just (Tuple (String.take i s) (String.drop (i + 1) s))
    Nothing -> Nothing

-- | The ADR-0113 §3 identities, per object. Three families, checked SEPARATELY — see the module
-- | preamble for why they cannot be one sum.
type Identities =
  { object :: String
  , failures :: Array String
  }

-- | Check one object's rows. Fail-closed throughout: a missing row, an unparsed line, or a
-- | diagnostic row that is not zero is a FAILURE, never a zero and never a warning.
checkIdentities :: String -> Array String -> Identities
checkIdentities object lines =
  { object
  , failures: parseFailures <> objectFailures <> duplicateFailures <> identityFailures <> diagnosticFailures
  }
  where
  parsed = map (\l -> Tuple l (parseRow l)) (Array.filter (\l -> l /= "" && not (String.take 1 l == "#")) lines)
  parseFailures = Array.mapMaybe
    ( \(Tuple l m) -> case m of
        Nothing -> Just ("unparsable row: " <> l)
        Just _ -> Nothing
    )
    parsed
  good = Array.mapMaybe (\(Tuple _ m) -> m) parsed

  -- Rows belonging to another object are REFUSED, not folded in. Without this the identities are
  -- per-object only by convention, and a caller that concatenated two objects' reports would get a
  -- green gate in which one object's shortfall is covered by the other's surplus.
  objectFailures =
    map (\r -> "row belongs to another object: " <> r.object <> " (expected " <> object <> "): " <> show r.key)
      (Array.filter (\r -> r.object /= object) good)

  -- Occurrences per key, counted BEFORE the map is built. `Map.fromFoldable` keeps the last write,
  -- so a row duplicated with the SAME value is invisible to every identity below — the sums all
  -- still balance. Exactly one occurrence of each key is therefore its own requirement.
  -- a foreign row is REPORTED and then EXCLUDED: letting it into the map as well would make one
  -- stray line report three faults (foreign, duplicate, broken sum) and bury the real diagnosis.
  mine = Array.filter (\r -> r.object == object) good

  occurrences :: Map RowKey Int
  occurrences = foldl (\m r -> Map.insertWith (+) r.key 1 m) Map.empty mine
  duplicateFailures =
    map (\(Tuple k n) -> "row appears " <> show n <> " times (exactly one is required): " <> show k)
      (Array.filter (\(Tuple _ n) -> n > 1) (Map.toUnfoldable occurrences :: Array _))

  rows = Map.fromFoldable (map (\r -> Tuple r.key r.count) mine)

  -- a row the report is contracted to emit; its ABSENCE is a failure, not a zero.
  need k = case Map.lookup k rows of
    Just n -> Right' n
    Nothing -> Left' ("missing row: " <> show k)

  identityFailures =
    -- (i) the OPAQUE family: each generic class equals the sum of its reason rows.
    ( genericForms >>= \cls ->
        checkSum ("(i) " <> callClassName cls)
          (ClassRow (callClassName cls))
          (map (\r -> ReasonRow (callClassName cls) (missReasonName r)) allMissReasons)
    )
      <>
        -- (ii) the CANDIDATE family: each local-deferred class equals the sum of its kind rows.
        ( candidateForms >>= \cls ->
            checkSum ("(ii) " <> callClassName cls)
              (ClassRow (callClassName cls))
              (map (\k -> KindRow (callClassName cls) (candidateKindName k)) candidateKinds)
        )

  checkSum label clsKey partKeys =
    case need clsKey, traverseParts partKeys of
      Left' e, _ -> [ label <> ": " <> e ]
      _, Left' e -> [ label <> ": " <> e ]
      Right' total, Right' parts ->
        let
          sum = foldl (+) 0 parts
        in
          if total == sum then [] else [ label <> ": class " <> show total <> " /= Σ parts " <> show sum ]

  traverseParts = foldl
    ( \acc k -> case acc, need k of
        Left' e, _ -> Left' e
        _, Left' e -> Left' e
        Right' xs, Right' n -> Right' (Array.snoc xs n)
    )
    (Right' [])

  -- rows that a correct compiler cannot produce. Kept (a class with no counter cannot be measured)
  -- and pinned at zero INDIVIDUALLY, so one going non-zero is not absorbed by another's zero.
  diagnosticFailures = Array.mapMaybe zeroRow diagnosticKeys
  zeroRow k = case Map.lookup k rows of
    Nothing -> Just ("missing diagnostic row: " <> show k)
    Just 0 -> Nothing
    Just n -> Just ("diagnostic row is non-zero (a compiler bug, not a lever): " <> show k <> " = " <> show n)
  diagnosticKeys = genericForms >>= \cls ->
    map (ReasonRow (callClassName cls))
      [ "unknown-key", "callee-literal", "local-unknown-fn/let-lambda", "local-unknown-fn/grec-lambda" ]

-- A tiny local Either so the module needs no extra dependency for its two failure paths.
data Either' a = Left' String | Right' a
