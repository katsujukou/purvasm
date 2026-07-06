-- | Exercises the `Data.String.Regex` ulib shadow (ADR-0081) as a registry-compiled consumer:
-- | on the native backends the shadow runs the pure-PS `purvasm-regex` engine; on JS the
-- | registry module runs the host `RegExp` — identical deterministic output is the oracle.
-- | Every pattern stays inside the ADR-0081 floor (and uses only the `u`/no-flag modes the
-- | shadow accepts), so all backends agree byte for byte.
module Example.RegexDemo.Main where

import Prelude

import Data.Either (Either(..))
import Data.String.Regex (match, source, test)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  -- The CST Lexer's proper-name shape (the ADR-0081 demanding consumer).
  let properName = unsafeRegex "\\p{Lu}[\\p{L}0-9_']*" unicode
  log ("source        = " <> source properName)
  log ("match Über    = " <> show (match properName "Über.rest"))
  -- Groups, alternation, and non-participation.
  let intPart = unsafeRegex "^(0|[1-9][0-9_]*)(?:\\.([0-9_]+))?" noFlags
  log ("match 1_000.5 = " <> show (match intPart "1_000.5x"))
  log ("match 0       = " <> show (match intPart "0"))
  -- Negative lookahead and a negated class.
  let notComment = unsafeRegex "^-(?!-)[^\\r\\n]*" noFlags
  log ("test -x       = " <> show (test notComment "-x"))
  log ("test --x      = " <> show (test notComment "--x"))
  -- Construction is total: a malformed pattern is a Left, same on every backend.
  log
    ( "bad pattern   = " <>
        case Regex.regex "(a" noFlags of
          Left _ -> "Left"
          Right _ -> "Right"
    )
