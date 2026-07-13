-- | GER / impurification (ADR-0099 Slice 2): the canonical `Effect`-glue rewrites, recognised both
-- | as bare GER foreigns and as statically-known `Effect` dispatch, with η-expansion / over-application
-- | splicing and idempotency.
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Impurify where

import Prelude

import Data.Map as Map
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery, emptyMachinery)
import Purvasm.Compiler.MiddleEnd.Optimizer.Impurify (impurifyExpr)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

foreign_ :: String -> Atom
foreign_ = AtomForeign

-- Impurify with no dictionary machinery (bare-foreign recognition only).
imp :: Expr -> Expr
imp = impurifyExpr emptyMachinery

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Optimizer.Impurify" do
  describe "canonical Effect glue (bare GER foreign, ADR-0099 §5)" do
    it "pureE a → \\$u -> a" do
      imp (Ret (CApp (foreign_ "Effect.pureE") [ var "a" ]))
        `shouldEqual` Ret (CLam [ "$ge1" ] (Ret (CAtom (var "a"))))

    it "bindE m k → \\$u -> let x = perform m in let kx = k x in perform kx" do
      imp (Ret (CApp (foreign_ "Effect.bindE") [ var "m", var "k" ]))
        `shouldEqual`
          Ret
            ( CLam [ "$ge1" ]
                ( Let "$ge2" (CPerform (var "m"))
                    ( Let "$ge3" (CApp (var "k") [ var "$ge2" ])
                        (Ret (CPerform (var "$ge3")))
                    )
                )
            )

    it "unsafePerformEffect e → perform e (the eliminator runs the thunk, no \\$u)" do
      -- recognised on the plain qualified `AtomVar` spelling (the ulib use site), no machinery needed.
      imp (Ret (CApp (var "Effect.Unsafe.unsafePerformEffect") [ var "e" ]))
        `shouldEqual` Ret (CPerform (var "e"))

  describe "dispatch recognition via DictMachinery (ADR-0099 §6, the main path)" do
    it "bind bindEffect m k → canonical bindE lowering (impl resolved through the machinery)" do
      let
        machinery :: DictMachinery
        machinery = emptyMachinery
          { accessors = Map.singleton "Control.Bind.bind" "bind"
          , instances = Map.singleton "Effect.bindEffect" (Map.singleton "bind" (foreign_ "Effect.bindE"))
          }
      impurifyExpr machinery (Ret (CApp (var "Control.Bind.bind") [ var "Effect.bindEffect", var "m", var "k" ]))
        `shouldEqual`
          Ret
            ( CLam [ "$ge1" ]
                ( Let "$ge2" (CPerform (var "m"))
                    ( Let "$ge3" (CApp (var "k") [ var "$ge2" ])
                        (Ret (CPerform (var "$ge3")))
                    )
                )
            )

  describe "saturation handling" do
    it "η-expands an under-applied bindE (never declines to a bare foreign)" do
      -- bindE m (1 of 2) → \k -> <bindE m k lowering>
      imp (Ret (CApp (foreign_ "Effect.bindE") [ var "m" ]))
        `shouldEqual`
          Ret
            ( CLam [ "$ge1" ]
                ( Ret
                    ( CLam [ "$ge2" ]
                        ( Let "$ge3" (CPerform (var "m"))
                            ( Let "$ge4" (CApp (var "$ge1") [ var "$ge3" ])
                                (Ret (CPerform (var "$ge4")))
                            )
                        )
                    )
                )
            )

    it "splices a let for an over-applied pureE (thunk then surplus application)" do
      -- pureE a u (2 of 1) → let $ge2 = (\$ge1 -> a) in $ge2 u
      imp (Ret (CApp (foreign_ "Effect.pureE") [ var "a", var "u" ]))
        `shouldEqual`
          Let "$ge2" (CLam [ "$ge1" ] (Ret (CAtom (var "a"))))
            (Ret (CApp (var "$ge2") [ var "u" ]))

  describe "bare GER key in value position (ADR-0099 §5, P1 — never left as a residual foreign)" do
    it "a bare CAtom GER key η-expands to a closed thunk-builder" do
      -- Ret (CAtom Effect.pureE) → \a -> \$u -> a
      imp (Ret (CAtom (foreign_ "Effect.pureE")))
        `shouldEqual`
          Ret (CLam [ "$ge1" ] (Ret (CLam [ "$ge2" ] (Ret (CAtom (var "$ge1"))))))

    it "a GER key stored in a record field is let-hoisted (η-expanded)" do
      imp (Ret (CRecord [ { prop: "m", val: foreign_ "Effect.pureE" } ]))
        `shouldEqual`
          Let "$ge3" (CLam [ "$ge1" ] (Ret (CLam [ "$ge2" ] (Ret (CAtom (var "$ge1"))))))
            (Ret (CRecord [ { prop: "m", val: var "$ge3" } ]))

    it "a GER key passed as a call argument is let-hoisted (η-expanded)" do
      imp (Ret (CApp (var "g") [ foreign_ "Effect.pureE" ]))
        `shouldEqual`
          Let "$ge3" (CLam [ "$ge1" ] (Ret (CLam [ "$ge2" ] (Ret (CAtom (var "$ge1"))))))
            (Ret (CApp (var "g") [ var "$ge3" ]))

  describe "fresh-name hygiene (ADR-0099, P1 — no capture of existing $ge binders)" do
    it "does not capture an existing $ge binder when lowering (fresh seeds above maxGe)" do
      -- \$ge1 -> Effect.pureE $ge1 : the unit binder must NOT reuse $ge1 (which would capture the
      -- outer argument, turning `\_ -> $ge1` into identity). It seeds at $ge2.
      imp (Ret (CLam [ "$ge1" ] (Ret (CApp (foreign_ "Effect.pureE") [ var "$ge1" ]))))
        `shouldEqual`
          Ret (CLam [ "$ge1" ] (Ret (CLam [ "$ge2" ] (Ret (CAtom (var "$ge1"))))))

  describe "idempotency" do
    it "a lowered CPerform tree is stable under re-impurification" do
      let once = imp (Ret (CApp (foreign_ "Effect.bindE") [ var "m", var "k" ]))
      imp once `shouldEqual` once

    it "leaves a non-GER call untouched" do
      let e = Ret (CApp (var "SomeModule.f") [ var "x", var "y" ])
      imp e `shouldEqual` e
