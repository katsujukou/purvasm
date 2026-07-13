-- | DictElim collapses `accessor dict args → impl args` when both accessor and instance are statically
-- | known (ADR-0027; an optimisation in the backend-neutral optimiser, ADR-0086 §3). It must fire on known
-- | dispatch, stay conservative on unknown dictionaries (polymorphic code), and resolve an *imported*
-- | instance from the env (ADR-0085 cross-module).
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.DictElim where

import Prelude

import Data.Set as Set
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (dictElimExpr, emptyMachinery, intrinsicLift, machineryOf, mergeMachinery, noForeignLift)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- The spine now carries each binding's full defining `Expr` (a `Gfun`/`Gcaf` body).

-- A method accessor `\d -> case d of v -> v.φ` projecting field `φ` (a top-level function body).
accessor :: String -> Expr
accessor field =
  Ret
    ( CLam [ "d" ]
        ( Ret
            ( CCase [ AtomVar "d" ]
                [ { binders: [ BVar "v" ]
                  , result: Uncond (Ret (CAccessor (AtomVar "v") field))
                  }
                ]
            )
        )
    )

-- An instance dictionary record `{ φ: impl, … }`, as a bare CExpr and as a CAF body.
recordCexpr :: Array (Tuple String Atom) -> CExpr
recordCexpr fields = CRecord (map (\(Tuple prop val) -> { prop, val }) fields)

instanceRec :: Array (Tuple String Atom) -> Expr
instanceRec = Ret <<< recordCexpr

-- The identity newtype wrapper `\x -> x` (a top-level function body).
identityLam :: Expr
identityLam = Ret (CLam [ "x" ] (Ret (CAtom (AtomVar "x"))))

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.DictElim" do
  describe "dictElimExpr" do
    it "collapses a saturated known dispatch to the impl call" do
      let
        m = machineryOf emptyMachinery
          [ Tuple "Sr.add" (accessor "add")
          , Tuple "Sr.srInt" (instanceRec [ Tuple "add" (AtomForeign "Sr.intAdd") ])
          ]
        call = Ret (CApp (AtomVar "Sr.add") [ AtomVar "Sr.srInt", AtomVar "x", AtomVar "y" ])
      dictElimExpr intrinsicLift Set.empty m call
        `shouldEqual` Ret (CApp (AtomForeign "Sr.intAdd") [ AtomVar "x", AtomVar "y" ])

    it "collapses to a bare atom when no args remain" do
      let
        m = machineryOf emptyMachinery
          [ Tuple "acc" (accessor "z")
          , Tuple "inst" (instanceRec [ Tuple "z" (AtomForeign "theImpl") ])
          ]
      dictElimExpr intrinsicLift Set.empty m (Ret (CApp (AtomVar "acc") [ AtomVar "inst" ]))
        `shouldEqual` Ret (CAtom (AtomForeign "theImpl"))

    it "sees through the identity newtype wrapper on the instance" do
      let
        m = machineryOf emptyMachinery
          [ Tuple "acc" (accessor "fld")
          , Tuple "idFn" identityLam
          , Tuple "inst" (Ret (CApp (AtomVar "idFn") [ AtomVar "rawRec" ]))
          , Tuple "rawRec" (instanceRec [ Tuple "fld" (AtomForeign "impl") ])
          ]
      dictElimExpr intrinsicLift Set.empty m (Ret (CApp (AtomVar "acc") [ AtomVar "inst", AtomVar "a" ]))
        `shouldEqual` Ret (CApp (AtomForeign "impl") [ AtomVar "a" ])

    it "resolves an instance whose record is a local `let` under the newtype wrapper (B2 shape)" do
      -- The real normalised instance shape under B2: `let $a = { φ: impl } in $Dict $a`, where `$a` is
      -- *local* to the binding (not a top-level spine entry as in boot's B1 whole-program spine).
      let
        gkeys = Set.fromFoldable [ "answerIs", "UA$Dict", "ultimateAnswerInt" ]
        m = machineryOf emptyMachinery
          [ Tuple "answerIs" (accessor "answerIs")
          , Tuple "UA$Dict" identityLam
          , Tuple "ultimateAnswerInt"
              ( Let "$a" (recordCexpr [ Tuple "answerIs" (AtomLit (LInt 42)) ])
                  (Ret (CApp (AtomVar "UA$Dict") [ AtomVar "$a" ]))
              )
          ]
      dictElimExpr intrinsicLift gkeys m (Ret (CApp (AtomVar "answerIs") [ AtomVar "ultimateAnswerInt" ]))
        `shouldEqual` Ret (CAtom (AtomLit (LInt 42)))

    it "lifts an impl that is an *intrinsic* foreign key (the overlaid-ulib instance shape)" do
      -- `semiringInt = $Dict { add: Purvasm.Int.add, … }`: the impl is a *cross-module intrinsic
      -- reference* riding a plain qualified key — in no module's gkeys, but liftable under the
      -- *optimiser* policy (Simplify's saturation collapses it right after; boot got the same lift
      -- from post-link gkeys membership; see `intrinsicLift`).
      let
        m = machineryOf emptyMachinery
          [ Tuple "Sr.add" (accessor "add")
          , Tuple "Sr.srInt" (instanceRec [ Tuple "add" (AtomVar "Purvasm.Int.add") ])
          ]
        call = Ret (CApp (AtomVar "Sr.add") [ AtomVar "Sr.srInt", AtomVar "x", AtomVar "y" ])
      dictElimExpr intrinsicLift Set.empty m call
        `shouldEqual` Ret (CApp (AtomVar "Purvasm.Int.add") [ AtomVar "x", AtomVar "y" ])

    it "the bridge policy (noForeignLift) declines the same intrinsic impl — no Simplify follows it" do
      -- The LLVM byte-identity bridge feeds codegen directly: a lifted `AtomVar "Purvasm.Int.add"`
      -- would reach `readVar` unbound, so under `noForeignLift` the dispatch must stay a call.
      let
        m = machineryOf emptyMachinery
          [ Tuple "Sr.add" (accessor "add")
          , Tuple "Sr.srInt" (instanceRec [ Tuple "add" (AtomVar "Purvasm.Int.add") ])
          ]
        call = Ret (CApp (AtomVar "Sr.add") [ AtomVar "Sr.srInt", AtomVar "x", AtomVar "y" ])
      dictElimExpr noForeignLift Set.empty m call `shouldEqual` call

    it "the bridge policy still lifts an AtomForeign impl (a resolved native leaf)" do
      -- `resolveNativeForeigns` runs before the bridge and turns native-leaf impls into
      -- `AtomForeign`, which codegen lowers via its `pvf_*` symbol — liftable under every policy.
      let
        m = machineryOf emptyMachinery
          [ Tuple "Sh.show" (accessor "show")
          , Tuple "Sh.showNumber" (instanceRec [ Tuple "show" (AtomForeign "Data.Show.showNumberImpl") ])
          ]
        call = Ret (CApp (AtomVar "Sh.show") [ AtomVar "Sh.showNumber", AtomVar "x" ])
      dictElimExpr noForeignLift Set.empty m call
        `shouldEqual` Ret (CApp (AtomForeign "Data.Show.showNumberImpl") [ AtomVar "x" ])

    it "declines an impl that is neither a global key nor a known foreign (a local name)" do
      let
        m = machineryOf emptyMachinery
          [ Tuple "acc" (accessor "fld")
          , Tuple "inst" (instanceRec [ Tuple "fld" (AtomVar "someLocal") ])
          ]
        call = Ret (CApp (AtomVar "acc") [ AtomVar "inst", AtomVar "x" ])
      dictElimExpr intrinsicLift Set.empty m call `shouldEqual` call

    it "leaves a call with an unknown (parameter) dictionary untouched" do
      let
        m = machineryOf emptyMachinery [ Tuple "acc" (accessor "fld") ]
        call = Ret (CApp (AtomVar "acc") [ AtomVar "dictParam", AtomVar "x" ])
      dictElimExpr intrinsicLift Set.empty m call `shouldEqual` call

    it "recognises an instance wrapping an *imported* $Dict (instance outside the class's module)" do
      -- `Data.Semiring` owns the class (accessor + identity wrapper); `My.Instances` declares the
      -- instance — the wrapper is cross-module, so recognition must consult the imported identities.
      let
        classMod = machineryOf emptyMachinery
          [ Tuple "Data.Semiring.add" (accessor "add")
          , Tuple "Data.Semiring.Semiring$Dict" identityLam
          ]
        instMod = machineryOf classMod
          [ Tuple "My.Instances.semiringInt"
              ( Let "$a" (recordCexpr [ Tuple "add" (AtomVar "Purvasm.Int.add") ])
                  (Ret (CApp (AtomVar "Data.Semiring.Semiring$Dict") [ AtomVar "$a" ]))
              )
          ]
        full = mergeMachinery instMod classMod
        call = Ret (CApp (AtomVar "Data.Semiring.add") [ AtomVar "My.Instances.semiringInt", AtomVar "m", AtomVar "f" ])
      dictElimExpr intrinsicLift Set.empty full call
        `shouldEqual` Ret (CApp (AtomVar "Purvasm.Int.add") [ AtomVar "m", AtomVar "f" ])

    it "declines a *structural*-foreign impl — it is GER-owned, not an intrinsic primop" do
      -- `Effect.bindE` is a structural (guest-term) foreign, not an intrinsic primop, so `intrinsicLift`
      -- (`isJust <<< intrinsicPrim`) refuses to fold it into the dispatch — it stays for GER (`Impurify`)
      -- to lower to `CPerform`. (The guest-term body is now materialised as a gdef by both backends —
      -- native `synthForeignGdefs`, VM link resolver — not folded here.)
      let
        m = machineryOf emptyMachinery
          [ Tuple "Control.Bind.bind" (accessor "bind")
          , Tuple "Effect.bindEffect" (instanceRec [ Tuple "bind" (AtomVar "Effect.bindE") ])
          ]
        call = Ret (CApp (AtomVar "Control.Bind.bind") [ AtomVar "Effect.bindEffect", AtomVar "m", AtomVar "f" ])
      dictElimExpr intrinsicLift Set.empty m call `shouldEqual` call

    it "resolves an imported instance from the env (cross-module)" do
      let
        -- `Sr` (dependency) publishes its accessor + instance to the env; the consumer only has the call.
        env = machineryOf emptyMachinery
          [ Tuple "Sr.add" (accessor "add")
          , Tuple "Sr.srInt" (instanceRec [ Tuple "add" (AtomForeign "Sr.intAdd") ])
          ]
        consumer = machineryOf emptyMachinery [] -- the consuming module defines no dict machinery of its own
        full = mergeMachinery consumer env
        call = Ret (CApp (AtomVar "Sr.add") [ AtomVar "Sr.srInt", AtomVar "n" ])
      dictElimExpr intrinsicLift Set.empty full call
        `shouldEqual` Ret (CApp (AtomForeign "Sr.intAdd") [ AtomVar "n" ])
