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
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (dictElimExpr, machineryOf, mergeMachinery)
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
        m = machineryOf
          [ Tuple "Sr.add" (accessor "add")
          , Tuple "Sr.srInt" (instanceRec [ Tuple "add" (AtomForeign "Sr.intAdd") ])
          ]
        call = Ret (CApp (AtomVar "Sr.add") [ AtomVar "Sr.srInt", AtomVar "x", AtomVar "y" ])
      dictElimExpr Set.empty m call
        `shouldEqual` Ret (CApp (AtomForeign "Sr.intAdd") [ AtomVar "x", AtomVar "y" ])

    it "collapses to a bare atom when no args remain" do
      let
        m = machineryOf
          [ Tuple "acc" (accessor "z")
          , Tuple "inst" (instanceRec [ Tuple "z" (AtomForeign "theImpl") ])
          ]
      dictElimExpr Set.empty m (Ret (CApp (AtomVar "acc") [ AtomVar "inst" ]))
        `shouldEqual` Ret (CAtom (AtomForeign "theImpl"))

    it "sees through the identity newtype wrapper on the instance" do
      let
        m = machineryOf
          [ Tuple "acc" (accessor "fld")
          , Tuple "idFn" identityLam
          , Tuple "inst" (Ret (CApp (AtomVar "idFn") [ AtomVar "rawRec" ]))
          , Tuple "rawRec" (instanceRec [ Tuple "fld" (AtomForeign "impl") ])
          ]
      dictElimExpr Set.empty m (Ret (CApp (AtomVar "acc") [ AtomVar "inst", AtomVar "a" ]))
        `shouldEqual` Ret (CApp (AtomForeign "impl") [ AtomVar "a" ])

    it "resolves an instance whose record is a local `let` under the newtype wrapper (B2 shape)" do
      -- The real normalised instance shape under B2: `let $a = { φ: impl } in $Dict $a`, where `$a` is
      -- *local* to the binding (not a top-level spine entry as in boot's B1 whole-program spine).
      let
        gkeys = Set.fromFoldable [ "answerIs", "UA$Dict", "ultimateAnswerInt" ]
        m = machineryOf
          [ Tuple "answerIs" (accessor "answerIs")
          , Tuple "UA$Dict" identityLam
          , Tuple "ultimateAnswerInt"
              ( Let "$a" (recordCexpr [ Tuple "answerIs" (AtomLit (LInt 42)) ])
                  (Ret (CApp (AtomVar "UA$Dict") [ AtomVar "$a" ]))
              )
          ]
      dictElimExpr gkeys m (Ret (CApp (AtomVar "answerIs") [ AtomVar "ultimateAnswerInt" ]))
        `shouldEqual` Ret (CAtom (AtomLit (LInt 42)))

    it "leaves a call with an unknown (parameter) dictionary untouched" do
      let
        m = machineryOf [ Tuple "acc" (accessor "fld") ]
        call = Ret (CApp (AtomVar "acc") [ AtomVar "dictParam", AtomVar "x" ])
      dictElimExpr Set.empty m call `shouldEqual` call

    it "resolves an imported instance from the env (cross-module)" do
      let
        -- `Sr` (dependency) publishes its accessor + instance to the env; the consumer only has the call.
        env = machineryOf
          [ Tuple "Sr.add" (accessor "add")
          , Tuple "Sr.srInt" (instanceRec [ Tuple "add" (AtomForeign "Sr.intAdd") ])
          ]
        consumer = machineryOf [] -- the consuming module defines no dict machinery of its own
        full = mergeMachinery consumer env
        call = Ret (CApp (AtomVar "Sr.add") [ AtomVar "Sr.srInt", AtomVar "n" ])
      dictElimExpr Set.empty full call
        `shouldEqual` Ret (CApp (AtomForeign "Sr.intAdd") [ AtomVar "n" ])
