-- | JSON-parse workload (ADR-0075): build an `n`-object document and parse it — the byte-parser
-- | cost centre that dominated the self-compile (ADR-0054); on the `js` leg the parser is stock
-- | argonaut's `JSON.parse`, which is exactly the wall-4 comparison. The printed element count
-- | (= `n`) is the structural self-check, independent of any stringify formatting.
module Bench.JsonParse.Main where

import Prelude

import Bench.Common (sizeArg)
import Data.Argonaut.Core (toArray)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length, range)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Console (log, logShow)

doc :: Int -> String
doc n = "[" <> joinWith "," (map item (range 1 n)) <> "]"
  where
  item i = "{\"k\":" <> show i <> ",\"v\":\"abcdefgh\"}"

main :: Effect Unit
main = do
  n <- sizeArg
  either (\e -> log ("parse error: " <> e))
    (logShow <<< length <<< fromMaybe [] <<< toArray)
    (jsonParser (doc n))
