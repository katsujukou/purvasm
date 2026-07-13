-- | GER / impurification (ADR-0099 Slice 2): the canonical `Effect`-glue rewrites, recognised both
-- | as bare GER foreigns and as statically-known `Effect` dispatch, with η-expansion / over-application
-- | splicing and idempotency.
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Impurify where

import Prelude

import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.Module (Decl)
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery, emptyMachinery)
import Purvasm.Compiler.MiddleEnd.Optimizer.Impurify (effectFamilyOf, impurifyExpr)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

foreign_ :: String -> Atom
foreign_ = AtomForeign

-- Impurify with no dictionary machinery (bare-foreign recognition only).
imp :: Expr -> Expr
imp = impurifyExpr emptyMachinery

-- A dict field map keyed by field name.
dict :: Array (Tuple String Atom) -> Map.Map String Atom
dict = Map.fromFoldable

-- The five `Effect` `Monad`-hierarchy instance dicts, correctly shaped (pure/bind hold the GER
-- anchors; superclass fields are dummy locals). `instances` for the validator / gate.
effectInstances :: Map.Map String (Map.Map String Atom)
effectInstances = Map.fromFoldable
  [ Tuple "Effect.functorEffect" (dict [ Tuple "map" (var "loc") ])
  , Tuple "Effect.applyEffect" (dict [ Tuple "apply" (var "loc"), Tuple "Functor0" (var "loc") ])
  , Tuple "Effect.applicativeEffect" (dict [ Tuple "pure" (foreign_ "Effect.pureE"), Tuple "Apply0" (var "loc") ])
  , Tuple "Effect.bindEffect" (dict [ Tuple "bind" (foreign_ "Effect.bindE"), Tuple "Apply0" (var "loc") ])
  , Tuple "Effect.monadEffect" (dict [ Tuple "Applicative0" (var "loc"), Tuple "Bind1" (var "loc") ])
  ]

effectKeys :: Array String
effectKeys = [ "Effect.functorEffect", "Effect.applyEffect", "Effect.applicativeEffect", "Effect.bindEffect", "Effect.monadEffect" ]

-- One recursive `Decl` group over the given instance keys (bodies are dummy — the validator reads
-- shapes from the machinery's `instances`, not the bodies).
recGroup :: Array String -> Array Decl
recGroup keys = [ { recursive: true, members: map (\k -> Tuple k (Ret (CAtom (var "dummy")))) keys } ]

machineryWith :: Map.Map String (Map.Map String Atom) -> Set.Set String -> DictMachinery
machineryWith insts fam = emptyMachinery
  { accessors = Map.fromFoldable [ Tuple "Data.Functor.map" "map", Tuple "Control.Apply.apply" "apply" ]
  , instances = insts
  , effectFamily = fam
  }

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

  describe "Effect-family validator (ADR-0099 Slice 3, sidenote 0014, fail-closed)" do
    it "admits the correctly-shaped Effect Monad-hierarchy group" do
      effectFamilyOf (recGroup effectKeys) (machineryWith effectInstances Set.empty)
        `shouldEqual` Set.fromFoldable effectKeys

    it "rejects a surplus member (a stray {map} dict mixed into the group)" do
      -- {pure:pureE},{bind:bindE},{stray map} etc. — the P1 hole: it must NOT admit `strayMap`.
      let
        insts = Map.insert "X.strayFunctor" (dict [ Tuple "map" (var "loc") ]) effectInstances
      effectFamilyOf (recGroup (effectKeys <> [ "X.strayFunctor" ])) (machineryWith insts Set.empty)
        `shouldEqual` Set.empty

    it "rejects a group missing the bindE anchor" do
      -- bindEffect's `bind` no longer holds Effect.bindE → the {bind} role is unfilled → fail closed.
      let
        insts = Map.insert "Effect.bindEffect" (dict [ Tuple "bind" (var "somethingElse"), Tuple "Apply0" (var "loc") ]) effectInstances
      effectFamilyOf (recGroup effectKeys) (machineryWith insts Set.empty)
        `shouldEqual` Set.empty

    it "rejects a group with an unknown (non-dict) member" do
      effectFamilyOf (recGroup (effectKeys <> [ "Not.ADict" ])) (machineryWith effectInstances Set.empty)
        `shouldEqual` Set.empty

    it "rejects a two-method dict shape (e.g. { apply, map }, not a single-method role)" do
      -- the superclass slot must be a *non-method* field: `{ apply, map }` is mis-shaped and must
      -- not read as the Apply role (P1).
      let
        insts = Map.insert "Effect.applyEffect" (dict [ Tuple "apply" (var "loc"), Tuple "map" (var "loc") ]) effectInstances
      effectFamilyOf (recGroup effectKeys) (machineryWith insts Set.empty)
        `shouldEqual` Set.empty

    it "ignores a non-recursive group (a lone Functor dict is never a family)" do
      effectFamilyOf [ { recursive: false, members: [ Tuple "Effect.functorEffect" (Ret (CAtom (var "d"))) ] } ]
        (machineryWith effectInstances Set.empty)
        `shouldEqual` Set.empty

  describe "map/apply rewrite gated on effectFamily (ADR-0099 §5(a))" do
    it "map functorEffect f m → \\$u -> let a = perform m in f a" do
      impurifyExpr (machineryWith effectInstances (Set.singleton "Effect.functorEffect"))
        (Ret (CApp (var "Data.Functor.map") [ var "Effect.functorEffect", var "f", var "m" ]))
        `shouldEqual`
          Ret (CLam [ "$ge1" ] (Let "$ge2" (CPerform (var "m")) (Ret (CApp (var "f") [ var "$ge2" ]))))

    it "apply applyEffect mf ma → \\$u -> let f = perform mf in let a = perform ma in f a" do
      impurifyExpr (machineryWith effectInstances (Set.singleton "Effect.applyEffect"))
        (Ret (CApp (var "Control.Apply.apply") [ var "Effect.applyEffect", var "mf", var "ma" ]))
        `shouldEqual`
          Ret
            ( CLam [ "$ge1" ]
                ( Let "$ge2" (CPerform (var "mf"))
                    (Let "$ge3" (CPerform (var "ma")) (Ret (CApp (var "$ge2") [ var "$ge3" ])))
                )
            )

    it "leaves map on a NON-Effect-family dict alone (the fail-closed gate)" do
      -- same map dispatch, but the dict is not in effectFamily → not rewritten.
      let e = Ret (CApp (var "Data.Functor.map") [ var "Some.otherFunctor", var "f", var "m" ])
      impurifyExpr (machineryWith effectInstances Set.empty) e `shouldEqual` e

    it "rewrites a map field *projected* off an Effect-family dict (NbE's runtime `gf` form)" do
      -- `functorEffect.map` (a `CAccessor`, what NbE leaves for a group-recursive dict, ADR-0098) →
      -- the η-expanded canonical map function; the enclosing application reduces next NbE round.
      impurifyExpr (machineryWith effectInstances (Set.singleton "Effect.functorEffect"))
        (Ret (CAccessor (var "Effect.functorEffect") "map"))
        `shouldEqual`
          Ret
            ( CLam [ "$ge1", "$ge2" ]
                (Ret (CLam [ "$ge3" ] (Let "$ge4" (CPerform (var "$ge2")) (Ret (CApp (var "$ge1") [ var "$ge4" ])))))
            )

    it "leaves a map projection off a NON-Effect-family dict alone" do
      let e = Ret (CAccessor (var "Some.otherFunctor") "map")
      impurifyExpr (machineryWith effectInstances Set.empty) e `shouldEqual` e

    it "rewrites an apply field *projected* off an Effect-family dict" do
      impurifyExpr (machineryWith effectInstances (Set.singleton "Effect.applyEffect"))
        (Ret (CAccessor (var "Effect.applyEffect") "apply"))
        `shouldEqual`
          Ret
            ( CLam [ "$ge1", "$ge2" ]
                ( Ret
                    ( CLam [ "$ge3" ]
                        ( Let "$ge4" (CPerform (var "$ge1"))
                            (Let "$ge5" (CPerform (var "$ge2")) (Ret (CApp (var "$ge4") [ var "$ge5" ])))
                        )
                    )
                )
            )
