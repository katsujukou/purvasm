-- | Structural invariants of the shared decision-tree builder (`MiddleEnd.MatchCompile`,
-- | ADR-0083) asserted directly on its `DTree` output — the "isolated matcher testability"
-- | the ANF-middle-end placement buys (`compile` is a pure `(scruts, alts) -> DTree`, no
-- | backend). The bytecode *lowering* of the tree is pinned separately, to exact bytes, in
-- | `Test.Unit.Purvasm.Compiler.Bytecode.Lower.Match`.
module Test.Unit.Purvasm.Compiler.MiddleEnd.MatchCompile where

import Prelude

import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.MatchCompile (DTree(..), Proj(..), compile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

bdy :: Int -> Rhs
bdy n = Uncond (Ret (CAtom (AtomLit (LInt n))))

alt :: Array Binder -> Rhs -> Alt
alt binders result = { binders, result }

treeOf :: Array Atom -> Array Alt -> DTree
treeOf scruts alts = (compile scruts alts).tree

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.MatchCompile" do
  describe "compile — scrutinee occurrences" do
    it "binds each scrutinee to a fresh $dt occurrence, in order" do
      let r = compile [ AtomVar "x", AtomVar "y" ] [ alt [ BVar "a", BVar "b" ] (bdy 1) ]
      map fst r.scrutBinds `shouldEqual` [ "$dt1", "$dt2" ]

    it "a single irrefutable row is a Dleaf binding each variable to its occurrence" do
      case treeOf [ AtomVar "x", AtomVar "y" ] [ alt [ BVar "a", BVar "b" ] (bdy 1) ] of
        Dleaf binds _ -> binds `shouldEqual` [ "a" /\ "$dt1", "b" /\ "$dt2" ]
        _ -> fail "expected a Dleaf"

  describe "compile — discriminant switches" do
    it "builds one DswitchCtor with deduped heads and a Dfail default when non-exhaustive" do
      case
        treeOf [ AtomVar "x" ]
          [ alt [ BCtor "Just" [ BVar "a" ] ] (bdy 1)
          , alt [ BCtor "Just" [ BVar "b" ] ] (bdy 2)
          , alt [ BCtor "Nothing" [] ] (bdy 3)
          ]
        of
        DswitchCtor occ arms default -> do
          occ `shouldEqual` "$dt1"
          map fst arms `shouldEqual` [ "Just", "Nothing" ]
          case default of
            Dfail _ -> pure unit
            _ -> fail "expected a Dfail default"
        _ -> fail "expected a DswitchCtor"

    it "a constructor arm extracts each field as a Pfield sub-occurrence" do
      case
        treeOf [ AtomVar "x" ]
          [ alt [ BCtor "Pair" [ BVar "a", BVar "b" ] ] (bdy 1), alt [ BNull ] (bdy 0) ]
        of
        DswitchCtor _ arms _ -> case arms of
          [ _ /\ arm ] -> map (\(o /\ p) -> o /\ p) arm.extracts
            `shouldEqual` [ "$dt2" /\ Pfield 0, "$dt3" /\ Pfield 1 ]
          _ -> fail "expected exactly one ctor arm"
        _ -> fail "expected a DswitchCtor"

    it "a literal case is a DswitchLit keyed by the literal, wildcard becoming the default" do
      case
        treeOf [ AtomVar "n" ]
          [ alt [ BLit (LInt 1) ] (bdy 10), alt [ BLit (LInt 2) ] (bdy 20), alt [ BVar "w" ] (bdy 0) ]
        of
        DswitchLit occ arms default -> do
          occ `shouldEqual` "$dt1"
          map fst arms `shouldEqual` [ LInt 1, LInt 2 ]
          case default of
            Dleaf _ _ -> pure unit
            _ -> fail "expected the wildcard row as the Dleaf default"
        _ -> fail "expected a DswitchLit"

    it "an array case is a DswitchLen extracting elements as Pelem sub-occurrences" do
      case
        treeOf [ AtomVar "xs" ]
          [ alt [ BArray [ BVar "a", BVar "b" ] ] (bdy 1), alt [ BNull ] (bdy 0) ]
        of
        DswitchLen occ arms _ -> do
          occ `shouldEqual` "$dt1"
          case arms of
            [ len /\ arm ] -> do
              len `shouldEqual` 2
              map (\(o /\ p) -> o /\ p) arm.extracts `shouldEqual` [ "$dt2" /\ Pelem 0, "$dt3" /\ Pelem 1 ]
            _ -> fail "expected exactly one length arm"
        _ -> fail "expected a DswitchLen"

    it "switches at each level of a nested constructor pattern" do
      case
        treeOf [ AtomVar "x" ]
          [ alt [ BCtor "Just" [ BCtor "Just" [ BVar "a" ] ] ] (bdy 1), alt [ BNull ] (bdy 0) ]
        of
        DswitchCtor _ [ _ /\ arm ] _ -> case arm.sub of
          DswitchCtor _ _ _ -> pure unit
          _ -> fail "expected a nested DswitchCtor inside the outer arm"
        _ -> fail "expected an outer DswitchCtor"

  describe "compile — records and guards" do
    it "expands a record pattern with no discriminant (DexpandRecord)" do
      case
        treeOf [ AtomVar "r" ]
          [ alt [ BRecord [ { prop: "x", binder: BVar "a" } ] ] (Uncond (Ret (CAtom (AtomVar "a")))) ]
        of
        DexpandRecord occ extracts _ -> do
          occ `shouldEqual` "$dt1"
          map (\(o /\ p) -> o /\ p) extracts `shouldEqual` [ "$dt2" /\ Precord "x" ]
        _ -> fail "expected a DexpandRecord"

    it "a guarded leaf is a Dguard carrying the rows-below as its fall-through subtree" do
      case
        treeOf [ AtomVar "x" ]
          [ alt [ BVar "y" ] (Guarded [ { guard: Ret (CAtom (AtomLit (LBool true))), rhs: Ret (CAtom (AtomLit (LInt 1))) } ])
          , alt [ BNull ] (bdy 0)
          ]
        of
        Dguard binds clauses ft -> do
          binds `shouldEqual` [ "y" /\ "$dt1" ]
          case clauses, ft of
            [ _ ], Dleaf _ _ -> pure unit
            _, _ -> fail "expected one guard clause and a Dleaf fall-through"
        _ -> fail "expected a Dguard"
