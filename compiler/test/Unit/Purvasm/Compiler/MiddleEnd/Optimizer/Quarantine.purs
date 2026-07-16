-- | The quarantine's pure machinery (ADR-0089 Addendum 2026-07-16): the reachable-fact projection
-- | (`relevantFactsOf`) in isolation — the fixpoint-level skip/retry behaviour is covered by the
-- | `optimizeModule` quarantine specs in `Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer`.
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Quarantine where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (InlineCandidate)
import Purvasm.Compiler.MiddleEnd.Optimizer.Quarantine (relevantFactsOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

cand :: Expr -> InlineCandidate
cand body =
  { arity: Just 1
  , size: 5
  , cxLeqDeref: false
  , closed: true
  , argUses: []
  , group: Set.empty
  , body
  }

refTo :: String -> Expr
refTo k = Ret (CAtom (AtomVar k))

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Optimizer.Quarantine" do
  describe "relevantFactsOf" do
    it "terminates on a cyclic candidate graph, recording every reachable key once" do
      let
        lookups =
          { candidate: case _ of
              "Test.A" -> Just (cand (refTo "Test.B"))
              "Test.B" -> Just (cand (refTo "Test.A"))
              _ -> Nothing
          , effect: const Nothing
          }
        facts = relevantFactsOf lookups (refTo "Test.A")
      Set.fromFoldable (Map.keys facts) `shouldEqual` Set.fromFoldable [ "Test.A", "Test.B" ]

    it "closes transitively through candidate bodies" do
      let
        lookups =
          { candidate: case _ of
              "Test.A" -> Just (cand (refTo "Test.B"))
              "Test.B" -> Just (cand (refTo "Test.C"))
              _ -> Nothing
          , effect: const Nothing
          }
        facts = relevantFactsOf lookups (refTo "Test.A")
      Set.fromFoldable (Map.keys facts) `shouldEqual`
        Set.fromFoldable [ "Test.A", "Test.B", "Test.C" ]

    it "records both atom spellings, keys without facts included (retry-biased misses)" do
      let
        lookups = { candidate: const Nothing, effect: const Nothing }
        facts = relevantFactsOf lookups (Ret (CApp (AtomForeign "X.f") [ AtomVar "Y.g" ]))
      Set.fromFoldable (Map.keys facts) `shouldEqual` Set.fromFoldable [ "X.f", "Y.g" ]
      Map.lookup "X.f" facts `shouldEqual` Just { candidate: Nothing, effect: Nothing }
