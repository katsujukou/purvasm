-- | The census's own invariant is EMISSION FIDELITY, not the lattice (whose laws live with the
-- | compiler module it now reads). What is checked here is the ADR-0107 §2 accounting identity, at
-- | unit scale, against the REAL emitter — both are pure functions of the gdefs, so each shape can
-- | be emitted twice, with the lattice on and off:
-- |
-- |   emitted occurrences  == `fchk` chains with the lattice ON
-- |   total occurrences    == `fchk` chains with the lattice OFF
-- |   elided occurrences   == the difference (the chains the lattice deleted)
-- |
-- | The shapes below are the ones where a plausible-looking walk diverges — above all a decision
-- | tree that duplicates a row across specialised submatrices, which is why proof sites and
-- | emission occurrences are distinct identities in the first place.
module Test.Unit.Purvasm.Census.ByNeed where

import Prelude

import Purvasm.Compiler.Backend.LLVM.ForeignRef (ForeignCallMode(..), ForeignClosureMode(..))

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Purvasm.Census.ByNeed (Census, SiteClass(..), censusEntry, censusGdefs, censusOf, elidedSites, emittedSites, siteCount, totalSites)
import Purvasm.Compiler.Backend.LLVM.ByNeed (activationFacts, noFacts)
import Purvasm.Compiler.Backend.LLVM.Program (gdefKeys, moduleLl)
import Purvasm.Compiler.Backend.LLVM.Types (Gdef(..))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr, CExprF(..), Expr, ExprF(..), Rhs, RhsF(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

-- | Census a `Gfun`-shaped activation (the census's own entry point for a top-level function).
censusFun :: Array String -> Expr -> Census
censusFun ps body = censusGdefs [ Gfun "M.f" ps body ]

classCount :: Array String -> Expr -> SiteClass -> { elided :: Int, emitted :: Int }
classCount ps body cls = siteCount (censusFun ps body) cls

-- | The force chains the REAL emitter emits for these gdefs, with the ADR-0107 lattice on or off.
-- | `Abi.forceValue` is the only producer of an `fchk` block label, so counting those labels counts
-- | chains. `gkeys` = the object's own keys, so a sibling reference resolves locally.
emittedChains :: Boolean -> Array Gdef -> Int
emittedChains byNeed gdefs =
  let
    keys = Set.fromFoldable (gdefs >>= gdefKeys)
    ir = moduleLl { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, profileApply: false, byNeed, foreignCall: DirectApplyAndTail, foreignClosure: Hoisted } keys gdefs
  in
    Array.length (String.split (Pattern "\nfchk") ir) - 1

spec :: Spec Unit
spec = describe "Purvasm.Census.ByNeed" do
  describe "occurrence counting" do
    it "counts one occurrence per forced VARIABLE operand, per recipe" do
      classCount [ "p" ] (Ret (CPrim AddInt [ var "p", var "p" ])) SPrimOperand
        `shouldEqual` { elided: 0, emitted: 2 }
      classCount [ "p" ] (Ret (CIf (var "p") (Ret (CAtom (int 1))) (Ret (CAtom (int 2))))) SIfCond
        `shouldEqual` { elided: 0, emitted: 1 }
      classCount [ "p" ] (Ret (CAccessor (var "p") "f")) SAccessorBase
        `shouldEqual` { elided: 0, emitted: 1 }

    it "counts no occurrence for a literal operand (no chain is emitted)" do
      totalSites (censusFun [] (Ret (CPrim AddInt [ int 1, int 2 ]))) `shouldEqual` 0

    it "counts the CUpdate BASE only — update values are unforced reads" do
      let body = Ret (CUpdate (var "p") [ { prop: "a", val: var "q" }, { prop: "b", val: var "r" } ])
      totalSites (censusFun [ "p", "q", "r" ] body) `shouldEqual` 1

    it "reports an occurrence as elided exactly when the compiler proves it" do
      let body = Let "a" (CAtom (int 1)) (Ret (CPrim AddInt [ var "a", var "p" ]))
      classCount [ "p" ] body SPrimOperand `shouldEqual` { elided: 1, emitted: 1 }

    it "counts a guard result once per clause, whatever its shape" do
      let
        alt =
          { binders: [ BVar "y" ]
          , result: Guarded
              [ { guard: Ret (CPrim EqInt [ var "y", int 1 ]), rhs: Ret (CAtom (int 1)) }
              , { guard: Ret (CAtom (var "y")), rhs: Ret (CAtom (int 2)) }
              ]
          }
        body = Ret (CCase [ var "p" ] [ alt ])
      -- a `CPrim EqInt` result is proven (elided); a bare pattern-binder guard is not.
      classCount [ "p" ] body SGuardResult `shouldEqual` { elided: 1, emitted: 1 }

    -- The discrimination that motivates the proof-site / emission-occurrence split: a wildcard row
    -- survives into every specialised submatrix, so when a submatrix still has a refutable column
    -- its body becomes a leaf in the inner default as well as the outer one — emitted twice.
    it "counts a shared wildcard row once per decision-tree leaf, not once per source row" do
      let
        alts =
          [ { binders: [ BCtor "Just" [ BLit (LInt 1) ] ], result: Uncond (Ret (CAtom (int 10))) }
          , { binders: [ BCtor "Just" [ BLit (LInt 2) ] ], result: Uncond (Ret (CAtom (int 20))) }
          , { binders: [ BNull ], result: Uncond (Ret (CAccessor (var "q") "f")) }
          ]
        body = Ret (CCase [ var "p" ] alts)
      classCount [ "p", "q" ] body SAccessorBase `shouldEqual` { elided: 0, emitted: 2 }
      classCount [ "p", "q" ] body SCaseScrutinee `shouldEqual` { elided: 0, emitted: 1 }

    it "gives a nested lambda its own activation (no outer Never leaks into a capture)" do
      let
        inner = Ret (CPrim AddInt [ var "a", var "u" ])
        body = Let "a" (CAtom (int 1)) (Ret (CLam unit [ "u" ] inner))
      classCount [] body SPrimOperand `shouldEqual` { elided: 0, emitted: 2 }

    it "leaves the entry stub un-elided (it has no plan, so its facts are empty)" do
      let entry = Let "a" (CAtom (int 1)) (Ret (CPrim AddInt [ var "a", var "a" ]))
      elidedSites (censusEntry true entry) `shouldEqual` 0
      emittedSites (censusEntry true entry) `shouldEqual` 2
      siteCount (censusEntry false (Ret (CAtom (var "m")))) SEntryValue `shouldEqual` { elided: 0, emitted: 1 }
      totalSites (censusOf noFacts (Ret (CAtom (var "m"))) (censusFun [] (Ret (CAtom (int 0))))) `shouldEqual` 0

  describe "accounting identity against the real emitter" do
    let
      matrix =
        [ Tuple "prim operands (unprovable)" (Gfun "M.f" [ "p", "q" ] (Ret (CPrim AddInt [ var "p", var "q" ])))
        , Tuple "prim operands (proven)"
            (Gfun "M.f" [] (Let "a" (CAtom (int 1)) (Ret (CPrim AddInt [ var "a", var "a" ]))))
        , Tuple "literal operands" (Gfun "M.f" [] (Ret (CPrim AddInt [ int 1, int 2 ])))
        , Tuple "if condition (proven)"
            (Gfun "M.f" [] (Let "b" (CPrim EqInt [ int 1, int 1 ]) (Ret (CIf (var "b") (Ret (CAtom (int 1))) (Ret (CAtom (int 2)))))))
        , Tuple "accessor base (proven ctor)"
            (Gfun "M.f" [ "p" ] (Let "r" (CRecord [ { prop: "a", val: var "p" } ]) (Ret (CAccessor (var "r") "a"))))
        , Tuple "accessor base (param)" (Gfun "M.f" [ "p" ] (Ret (CAccessor (var "p") "f")))
        , Tuple "update base (proven)"
            (Gfun "M.f" [ "q" ] (Let "r" (CRecord [ { prop: "a", val: var "q" } ]) (Ret (CUpdate (var "r") [ { prop: "a", val: var "q" } ]))))
        , Tuple "let alias of a literal" (Gfun "M.f" [] (Let "a" (CAtom (int 1)) (Let "b" (CAtom (var "a")) (Ret (CPrim AddInt [ var "b", int 1 ])))))
        , Tuple "shadowed name (poisoned)"
            ( Gfun "M.f" [ "p" ]
                ( Let "x" (CAtom (int 1))
                    ( Let "y" (CIf (var "p") (Let "x" (CApp unit (var "M.sibling") [ int 0 ]) (Ret (CAtom (var "x")))) (Ret (CAtom (int 0))))
                        (Ret (CPrim AddInt [ var "x", var "y" ]))
                    )
                )
            )
        , Tuple "nested lambda body"
            (Gfun "M.f" [ "p" ] (Ret (CLam unit [ "u" ] (Ret (CPrim AddInt [ var "p", var "u" ])))))
        , Tuple "letrec member"
            ( Gfun "M.f" [ "p" ]
                (LetRec [ { var: "r", rhs: Ret (CLam unit [ "u" ] (Ret (CAccessor (var "u") "f"))) } ] (Ret (CAtom (var "r"))))
            )
        , Tuple "grec group member"
            (Grec [ Tuple "M.r" (Ret (CLam unit [ "u" ] (Ret (CAccessor (var "u") "f")))) ])
        , Tuple "case: ctor row and wildcard row"
            ( Gfun "M.f" [ "p", "q" ]
                ( Ret
                    ( CCase [ var "p" ]
                        [ { binders: [ BCtor "Just" [ BVar "y" ] ], result: Uncond (Ret (CAccessor (var "y") "f")) }
                        , { binders: [ BNull ], result: Uncond (Ret (CAccessor (var "q") "f")) }
                        ]
                    )
                )
            )
        , Tuple "case: nested literals under a ctor (wildcard row reached twice)"
            ( Gfun "M.f" [ "p", "q" ]
                ( Ret
                    ( CCase [ var "p" ]
                        [ { binders: [ BCtor "Just" [ BLit (LInt 1) ] ], result: Uncond (Ret (CAtom (int 10))) }
                        , { binders: [ BCtor "Just" [ BLit (LInt 2) ] ], result: Uncond (Ret (CAtom (int 20))) }
                        , { binders: [ BNull ], result: Uncond (Ret (CAccessor (var "q") "f")) }
                        ]
                    )
                )
            )
        , Tuple "case: guard chain with fall-through"
            ( Gfun "M.f" [ "p", "q" ]
                ( Ret
                    ( CCase [ var "p" ]
                        [ { binders: [ BVar "y" ]
                          , result: Guarded
                              [ { guard: Ret (CAtom (var "y")), rhs: Ret (CAtom (int 1)) }
                              , { guard: Ret (CPrim EqInt [ var "y", int 2 ]), rhs: Ret (CAccessor (var "q") "f") }
                              ]
                          }
                        , { binders: [ BNull ], result: Uncond (Ret (CAtom (int 0))) }
                        ]
                    )
                )
            )
        , Tuple "case: scrutinee proven by a local ctor"
            ( Gfun "M.f" [ "p" ]
                ( Let "s" (CCtor "Just" 1 [ var "p" ])
                    ( Ret
                        ( CCase [ var "s" ]
                            [ { binders: [ BCtor "Just" [ BVar "y" ] ], result: Uncond (Ret (CAtom (var "y"))) }
                            , { binders: [ BNull ], result: Uncond (Ret (CAtom (int 0))) }
                            ]
                        )
                    )
                )
            )
        , Tuple "caf body over a sibling global" (Gcaf "M.c" (Ret (CAccessor (var "M.sibling") "f")))
        ]

      -- every shape is emitted alongside a defined sibling global, so a cross-gdef reference
      -- resolves to a local `$root` load instead of crashing `readVar` as unbound.
      sibling = Gcaf "M.sibling" (Ret (CAtom (int 0)))
    for_ matrix \(Tuple label gdef) ->
      it label do
        let
          gdefs = [ sibling, gdef ]
          census = censusGdefs gdefs
          on = emittedChains true gdefs
          off = emittedChains false gdefs
        { label, emitted: emittedSites census, total: totalSites census, elided: elidedSites census }
          `shouldEqual` { label, emitted: on, total: off, elided: off - on }

  describe "the census reads the compiler's facts, not its own" do
    it "asks activationFacts for every activation it enters" do
      -- if the census kept a private lattice this would be the place it could drift: the elided
      -- count must equal what the compiler's own decision set says about the same body.
      let
        body = Let "a" (CAtom (int 1)) (Ret (CPrim AddInt [ var "a", var "p" ]))
        facts = activationFacts [ "p" ] body
      elidedSites (censusOf facts body (censusFun [] (Ret (CAtom (int 0))))) `shouldEqual` 1

