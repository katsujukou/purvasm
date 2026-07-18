-- | ADR-0104 §2 behavioural-gate fixture: dictionary-dispatch-heavy code — the very path bridge
-- | removal (§3) turns on for native `--no-opt` (dispatch stays dynamic once `dictElimExpr` is
-- | gone). Registry-class dispatch (`Show`/`Eq`/`Ord`/`Monoid` across scalar, `Array`, `Maybe`
-- | instances), a local class with two instances, and a `Foldable`+`Monoid` chain (`foldMap`) keep
-- | polymorphic call sites alive at several distinct dictionaries.
module Gate.DictDispatch where

import Prelude

import Data.Array (range)
import Data.Foldable (foldMap, foldl)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (log)

-- | A polymorphic consumer of three superclass-chained dictionaries, applied at four instances.
describe :: forall a. Show a => Ord a => a -> a -> String
describe x y = show x <> "/" <> show y <> "/" <> show (x == y) <> "/" <> show (compare x y)

class Speak a where
  speak :: a -> String

data Dog = Dog

data Cat = Cat

instance speakDog :: Speak Dog where
  speak _ = "woof"

instance speakCat :: Speak Cat where
  speak _ = "meow"

-- | Dispatch through the local class, twice per call so the dictionary is genuinely consulted.
twice :: forall a. Speak a => a -> String
twice a = speak a <> speak a

main :: Effect Unit
main = do
  log (describe 1 2)
  log (describe "a" "a")
  log (describe [ 1, 2 ] [ 1, 3 ])
  log (describe (Just 5) (Nothing :: Maybe Int))
  log (twice Dog <> "/" <> twice Cat)
  log (foldMap show [ 1, 2, 3 ])
  log (foldl (<>) "" [ "x", "y", "z" ])
  -- Bulk dispatch churn: hundreds of dictionary-dispatched `describe`/`show` calls, each
  -- allocating its result string, so the small-heap harness collects in the middle of dispatched
  -- code (and the checksum pins every result).
  log ("bulk=" <> show (foldl (\acc i -> acc + SCU.length (describe i (i + 1)) + SCU.length (foldMap show [ i, i * 2 ])) 0 (range 1 500)))
