-- | The ADR-0095 effect analysis invariants: the force/saturation model (I1 — construction ≠
-- | execution), PAP/exact/over-application classification, the SCC least fixpoint, and the
-- | module-level fold. Fixtures mirror boot's `test_effect_analysis.ml` classes where they apply.
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.ForeignSig (ForeignShape)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Data.Lazy (defer)
import Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis (EffectEnv, EffectFact, bindFact, emptyEffectEnv, eperfC, liftShape, moduleEffects, mtouchC, sinkableCall, vsumC, vsumExpr)
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

unit_ :: Atom
unit_ = AtomLit (LInt 0)

-- `log :: String -> Effect Unit` — the ordinary perform-leaf shape (ADR-0034's worked example).
logShape :: ForeignShape
logShape = { arity: 1, vsat: false, retVsat: true }

-- a pure leaf, e.g. `Data.Show.showIntImpl :: Int -> String`.
pureShape :: ForeignShape
pureShape = { arity: 1, vsat: false, retVsat: false }

-- a bare `EffectFn2` value: its 2-arg saturation runs the effect.
effectFn2Shape :: ForeignShape
effectFn2Shape = { arity: 2, vsat: true, retVsat: false }

sigs :: Map.Map String ForeignShape
sigs = Map.fromFoldable
  [ "Effect.Console.log" /\ logShape
  , "Data.Show.showIntImpl" /\ pureShape
  , "M.effectFn2" /\ effectFn2Shape
  ]

-- foreign shapes reach the analysis through the **dirty lift** (ADR-0096 §1).
envOf :: Map.Map String ForeignShape -> EffectEnv
envOf m = emptyEffectEnv (\k -> liftShape <$> Map.lookup k m)

-- hand-made full facts (the shape a dependency's structural summary arrives in).
envOfFacts :: Map.Map String EffectFact -> EffectEnv
envOfFacts m = emptyEffectEnv (\k -> Map.lookup k m)

env0 :: EffectEnv
env0 = envOf sigs

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis" do
  describe "eperfC (the dead-drop predicate)" do
    it "construction is pure (I1): building an Effect thunk does not perform" do
      -- log "x" saturates log — it only *builds* the Effect value.
      eperfC env0 (CApp (AtomForeign "Effect.Console.log") [ var "s" ]) `shouldEqual` false

    it "the saturating force performs: (log s) applied again is over-application → may-perform" do
      -- let t = log s in t unit — t's summary is one level deep; forcing it performs.
      let
        env' = envOf sigs
        tSum = vsumC env' (CApp (AtomForeign "Effect.Console.log") [ var "s" ])
      eperfC (bindFact "t" (defer \_ -> tSum) env') (CApp (var "t") [ unit_ ]) `shouldEqual` true

    it "a pure leaf's exact saturation does not perform" do
      eperfC env0 (CApp (AtomForeign "Data.Show.showIntImpl") [ var "n" ]) `shouldEqual` false

    it "a bare EffectFnN leaf performs at its own saturation (vsat)" do
      -- partial application builds a PAP:
      eperfC env0 (CApp (AtomForeign "M.effectFn2") [ var "a" ]) `shouldEqual` false
      -- exact saturation runs the effect:
      eperfC env0 (CApp (AtomForeign "M.effectFn2") [ var "a", var "b" ]) `shouldEqual` true

    it "an unknown callee is conservatively may-perform" do
      eperfC env0 (CApp (var "opaque") [ var "a" ]) `shouldEqual` true

    it "mutation primops perform; reads and pure primops do not" do
      eperfC env0 (CPrim SetArray [ var "a", int 0, int 1 ]) `shouldEqual` true
      eperfC env0 (CPrim NewArray [ int 3, int 0 ]) `shouldEqual` true
      eperfC env0 (CPrim IndexArray [ var "a", int 0 ]) `shouldEqual` false
      eperfC env0 (CPrim AddInt [ int 1, int 2 ]) `shouldEqual` false

    it "a lambda wrapping a mutable read is vsat=false but mtouch=true (droppable when dead, never movable)" do
      -- read = \a -> IndexArray a 0 — the ADR-0095 §3 example, now carried by the fact itself:
      -- pure by the perform bit (elimination allowed), dirty by the store bit (motion denied).
      let readLam = CLam [ "a" ] (Ret (CPrim IndexArray [ var "a", int 0 ]))
      (vsumC env0 readLam).vsat `shouldEqual` false
      (vsumC env0 readLam).mtouch `shouldEqual` true

  describe "vsumC / vsumExpr (the two-level summary; foreign shapes lift dirty)" do
    it "a perform-leaf saturation yields an effect thunk (retVsat one level down)" do
      vsumC env0 (CApp (AtomForeign "Effect.Console.log") [ var "s" ])
        `shouldEqual` { arity: 0, vsat: true, retVsat: true, mtouch: true, retMtouch: true }

    it "a partial application keeps the callee's bits with the residual arity" do
      vsumC env0 (CApp (AtomForeign "M.effectFn2") [ var "a" ])
        `shouldEqual` { arity: 1, vsat: true, retVsat: false, mtouch: true, retMtouch: true }

    it "a lambda's vsat is its body's eperf; its retVsat is the body value's vsat" do
      -- \s -> log s : saturating runs the body (pure — it builds the thunk); the *result* is
      -- the effect thunk. The classic `Effect`-returning function shape. (The store bits are
      -- dirty here because the leaf lifted dirty.)
      vsumC env0 (CLam [ "s" ] (Ret (CApp (AtomForeign "Effect.Console.log") [ var "s" ])))
        `shouldEqual` { arity: 1, vsat: false, retVsat: true, mtouch: true, retMtouch: true }

    it "a let-chained body threads local facts (vsumExpr)" do
      -- let t = log s in t — the binding's value is the effect thunk.
      ( vsumExpr env0
          ( Let "t" (CApp (AtomForeign "Effect.Console.log") [ var "s" ])
              (Ret (CAtom (var "t")))
          )
      ).vsat `shouldEqual` true

  describe "mtouch equations (ADR-0096 §1 two-level propagation)" do
    let
      -- a structurally clean-perform but store-dirty callee, as a dependency summary would
      -- carry it (a big reader lambda: saturating reads the store).
      readerFact :: EffectFact
      readerFact = { arity: 1, vsat: false, retVsat: false, mtouch: true, retMtouch: true }

      -- a clean two-argument callee (big pure arithmetic).
      cleanFact :: EffectFact
      cleanFact = { arity: 2, vsat: false, retVsat: false, mtouch: false, retMtouch: false }

      -- a closure-returning callee: its own saturation is clean, the returned closure's is not
      -- (the let-returned reader shape — see the lambda test below for the structural source).
      mkReaderFact :: EffectFact
      mkReaderFact = { arity: 1, vsat: false, retVsat: false, mtouch: false, retMtouch: true }

      envF = envOfFacts
        ( Map.fromFoldable
            [ "M.reader" /\ readerFact, "M.clean" /\ cleanFact, "M.mkReader" /\ mkReaderFact ]
        )

    it "PAP: construction is clean, but the summary carries the callee's bits verbatim" do
      let dirtyFacts = Map.fromFoldable [ "M.dirty2" /\ { arity: 2, vsat: false, retVsat: false, mtouch: true, retMtouch: true } ]
      mtouchC (envOfFacts dirtyFacts) (CApp (var "M.dirty2") [ var "a" ]) `shouldEqual` false
      vsumC (envOfFacts dirtyFacts) (CApp (var "M.dirty2") [ var "a" ])
        `shouldEqual` { arity: 1, vsat: false, retVsat: false, mtouch: true, retMtouch: true }

    it "exact saturation: the predicate reads mtouch; the result summary shifts retMtouch down" do
      mtouchC envF (CApp (var "M.reader") [ var "a" ]) `shouldEqual` true
      mtouchC envF (CApp (var "M.clean") [ var "a", var "b" ]) `shouldEqual` false
      -- mkReader's own saturation is clean, but its result's saturation is dirty:
      mtouchC envF (CApp (var "M.mkReader") [ var "a" ]) `shouldEqual` false
      (vsumC envF (CApp (var "M.mkReader") [ var "a" ])).mtouch `shouldEqual` true

    it "over-application is dirty" do
      mtouchC envF (CApp (var "M.clean") [ var "a", var "b", var "c" ]) `shouldEqual` true

    it "the let-returned reader lambda: mtouch=false, retMtouch=true (the ADR fixture shape)" do
      -- outer = \a -> let reader = \u -> IndexArray a 0 in reader — the curried spelling would
      -- uncurry to CLam [a, u] (ADR-0025), so the fixture is pinned to this let-returned form.
      let
        outer = CLam [ "a" ]
          ( Let "reader" (CLam [ "u" ] (Ret (CPrim IndexArray [ var "a", int 0 ])))
              (Ret (CAtom (var "reader")))
          )
      vsumC env0 outer
        `shouldEqual` { arity: 1, vsat: false, retVsat: false, mtouch: false, retMtouch: true }

    it "sinkableCall: exact-saturated clean calls only" do
      sinkableCall envF (CApp (var "M.clean") [ var "a", var "b" ]) `shouldEqual` true
      sinkableCall envF (CApp (var "M.reader") [ var "a" ]) `shouldEqual` false -- dirty
      sinkableCall envF (CApp (var "M.clean") [ var "a" ]) `shouldEqual` false -- PAP, not exact
      sinkableCall envF (CApp (var "opaque") [ var "a" ]) `shouldEqual` false -- unknown
      sinkableCall env0 (CApp (AtomForeign "Data.Show.showIntImpl") [ var "n" ]) `shouldEqual` false -- foreign: dirty lift

  describe "CPerform run-marker equations (ADR-0099 §2, fully conservative initial slice)" do
    it "eperfC / mtouchC are always true (performing a thunk may perform and may touch the store)" do
      eperfC env0 (CPerform (var "t")) `shouldEqual` true
      mtouchC env0 (CPerform (var "t")) `shouldEqual` true

    it "vsumC is the opaque unknownValue" do
      vsumC env0 (CPerform (var "t"))
        `shouldEqual` { arity: 0, vsat: true, retVsat: true, mtouch: true, retMtouch: true }

    it "sinkableCall is false (a run point never sinks)" do
      sinkableCall env0 (CPerform (var "t")) `shouldEqual` false

    it "a \\$u -> perform m thunk has vsat = true (saturating it performs)" do
      -- the load-bearing case: were `eperfC (CPerform _)` false, this lambda's vsat would read
      -- false and the existing dead-drop could delete a live effect.
      (vsumC env0 (CLam [ "$u" ] (Ret (CPerform (var "m"))))).vsat `shouldEqual` true

  describe "moduleEffects (mtouch propagation)" do
    it "a point-free alias chain preserves the dirt" do
      let
        decls =
          [ { recursive: false
            , members:
                [ "M.reader" /\ Ret (CLam [ "a" ] (Ret (CPrim IndexArray [ var "a", int 0 ]))) ]
            }
          , { recursive: false
            , members: [ "M.alias" /\ Ret (CAtom (var "M.reader")) ]
            }
          ]
        facts = moduleEffects (const Nothing) decls
      (map _.mtouch (Map.lookup "M.alias" facts)) `shouldEqual` Just true

    it "a recursive group member calling a reader sibling is dirty at the fixpoint" do
      let
        decls =
          [ { recursive: true
            , members:
                [ "M.f" /\ Ret (CLam [ "x" ] (Ret (CApp (var "M.g") [ var "x" ])))
                , "M.g" /\ Ret (CLam [ "y" ] (Ret (CPrim IndexArray [ var "y", int 0 ])))
                ]
            }
          ]
        facts = moduleEffects (const Nothing) decls
      (map _.mtouch (Map.lookup "M.f" facts)) `shouldEqual` Just true
      (map _.vsat (Map.lookup "M.f" facts)) `shouldEqual` Just false
    it "classifies siblings dependency-directed within the module" do
      let
        decls =
          [ { recursive: false
            , members: [ "M.show5" /\ Ret (CApp (AtomForeign "Data.Show.showIntImpl") [ int 5 ]) ]
            }
          , { recursive: false
            , members: [ "M.alias" /\ Ret (CAtom (var "M.show5")) ]
            }
          ]
        facts = moduleEffects (\k -> liftShape <$> Map.lookup k sigs) decls
      Map.lookup "M.show5" facts `shouldEqual` Just { arity: 0, vsat: false, retVsat: false, mtouch: true, retMtouch: true }
      Map.lookup "M.alias" facts `shouldEqual` Just { arity: 0, vsat: false, retVsat: false, mtouch: true, retMtouch: true }

    it "SCC least fixpoint: a self-recursive effectful loop is not misclassified pure" do
      -- go = \n -> let t = log n in let u = t unit in go n  — saturating go performs.
      let
        decls =
          [ { recursive: true
            , members:
                [ "M.go" /\ Ret
                    ( CLam [ "n" ]
                        ( Let "t" (CApp (AtomForeign "Effect.Console.log") [ var "n" ])
                            ( Let "u" (CApp (var "t") [ unit_ ])
                                (Ret (CApp (var "M.go") [ var "n" ]))
                            )
                        )
                    )
                ]
            }
          ]
        facts = moduleEffects (\k -> liftShape <$> Map.lookup k sigs) decls
      (map _.vsat (Map.lookup "M.go" facts)) `shouldEqual` Just true

    it "SCC least fixpoint: a pure recursive loop stays pure" do
      -- go = \n -> go n (a pure spin; DBE's partial-correctness relaxation is exactly that a
      -- *dead* binding of this class may vanish).
      let
        decls =
          [ { recursive: true
            , members:
                [ "M.go" /\ Ret (CLam [ "n" ] (Ret (CApp (var "M.go") [ var "n" ]))) ]
            }
          ]
        facts = moduleEffects (\k -> liftShape <$> Map.lookup k sigs) decls
      Map.lookup "M.go" facts `shouldEqual` Just { arity: 1, vsat: false, retVsat: false, mtouch: false, retMtouch: false }

    it "a recursive point-free member recovers its arity across the fixpoint" do
      -- M.f = \x y -> M.g x y ; M.g = M.f (a 2-hop alias cycle: both settle at arity 2, pure)
      let
        decls =
          [ { recursive: true
            , members:
                [ "M.f" /\ Ret (CLam [ "x", "y" ] (Ret (CApp (var "M.g") [ var "x", var "y" ])))
                , "M.g" /\ Ret (CAtom (var "M.f"))
                ]
            }
          ]
        facts = moduleEffects (\k -> liftShape <$> Map.lookup k sigs) decls
      (map _.arity (Map.lookup "M.g" facts)) `shouldEqual` Just 2
