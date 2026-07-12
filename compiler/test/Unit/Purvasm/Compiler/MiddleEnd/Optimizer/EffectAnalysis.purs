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
import Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis (EffectEnv, bindFact, emptyEffectEnv, eperfC, moduleEffects, vsumC, vsumExpr)
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

envOf :: Map.Map String ForeignShape -> EffectEnv
envOf m = emptyEffectEnv (\k -> Map.lookup k m)

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

    it "a lambda wrapping a mutable read is vsat=false (droppable when dead, never movable)" do
      -- read = \a -> IndexArray a 0 — the ADR-0095 §3 example: pure by this summary, which is
      -- why the facts license elimination only (motion would cross a SetArray).
      let readLam = CLam [ "a" ] (Ret (CPrim IndexArray [ var "a", int 0 ]))
      (vsumC env0 readLam).vsat `shouldEqual` false

  describe "vsumC / vsumExpr (the two-level summary)" do
    it "a perform-leaf saturation yields an effect thunk (retVsat one level down)" do
      vsumC env0 (CApp (AtomForeign "Effect.Console.log") [ var "s" ])
        `shouldEqual` { arity: 0, vsat: true, retVsat: true }

    it "a partial application keeps the callee's bits with the residual arity" do
      vsumC env0 (CApp (AtomForeign "M.effectFn2") [ var "a" ])
        `shouldEqual` { arity: 1, vsat: true, retVsat: false }

    it "a lambda's vsat is its body's eperf; its retVsat is the body value's vsat" do
      -- \s -> log s : saturating runs the body (pure — it builds the thunk); the *result* is
      -- the effect thunk. The classic `Effect`-returning function shape.
      vsumC env0 (CLam [ "s" ] (Ret (CApp (AtomForeign "Effect.Console.log") [ var "s" ])))
        `shouldEqual` { arity: 1, vsat: false, retVsat: true }

    it "a let-chained body threads local facts (vsumExpr)" do
      -- let t = log s in t — the binding's value is the effect thunk.
      ( vsumExpr env0
          ( Let "t" (CApp (AtomForeign "Effect.Console.log") [ var "s" ])
              (Ret (CAtom (var "t")))
          )
      ).vsat `shouldEqual` true

  describe "moduleEffects (per-module fold + SCC fixpoint)" do
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
        facts = moduleEffects (\k -> Map.lookup k sigs) decls
      Map.lookup "M.show5" facts `shouldEqual` Just { arity: 0, vsat: false, retVsat: false }
      Map.lookup "M.alias" facts `shouldEqual` Just { arity: 0, vsat: false, retVsat: false }

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
        facts = moduleEffects (\k -> Map.lookup k sigs) decls
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
        facts = moduleEffects (\k -> Map.lookup k sigs) decls
      Map.lookup "M.go" facts `shouldEqual` Just { arity: 1, vsat: false, retVsat: false }

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
        facts = moduleEffects (\k -> Map.lookup k sigs) decls
      (map _.arity (Map.lookup "M.g" facts)) `shouldEqual` Just 2
