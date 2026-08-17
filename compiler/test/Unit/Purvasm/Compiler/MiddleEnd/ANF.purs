-- | `ANF.foldAtoms` — the read-only atom walk. Two properties are load-bearing and neither is
-- | type-enforceable, so both are pinned here:
-- |
-- | * **fidelity** — it must see EVERY atom position `mapAtoms` rewrites. The two are independent
-- |   case trees (one rebuilds, one accumulates), so a field added to a node, or dropped from one
-- |   arm, is exactly the drift no compiler check catches. ADR-0109 §2.2-amended derives the entry
-- |   object's hoisted-cell set from this walk: a missed `AtomForeign` leaves live module code
-- |   reading a cell the entry never defined. The matrix below drives one fixture containing every
-- |   `CExpr`/`Rhs` form with a DISTINCT atom in every atom position, and asserts the exact list;
-- | * **stack safety** — the walk runs over generated ANF, where a `Let` spine is as long as the
-- |   program. The 100k-spine fixtures below are the regression the worklist exists for (spine
-- |   recursion dies on them at the default stack).
module Test.Unit.Purvasm.Compiler.MiddleEnd.ANF where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..), foldAtoms, mapAtoms)
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Atoms collected in walk order.
atomsOf :: Expr -> Array Atom
atomsOf = Array.reverse <<< List.toUnfoldable <<< foldAtoms (\acc a -> a : acc) Nil

v :: String -> Atom
v = AtomVar

-- | Every `CExpr` form, every `Rhs` form, and both `Expr` binders, each atom position carrying its
-- | own name. The expected list below is written from THIS shape, so a dropped field shortens it.
matrix :: Expr
matrix =
  Let "l0" (CAtom (v "a.atom"))
    ( LetRec
        [ { var: "r0", rhs: Ret (CApp (v "a.recFn") [ v "a.recArg" ]) }
        , { var: "r1", rhs: Ret (CPerform (v "a.recPerform")) }
        ]
        ( Let "l1" (CLam [ "p" ] (Ret (CAtom (v "a.lamBody"))))
            ( Let "l2" (CPrim AddInt [ v "a.prim0", v "a.prim1" ])
                ( Let "l3" (CCtor "C" 2 [ v "a.ctor0", v "a.ctor1" ])
                    ( Let "l4" (CArray [ v "a.arr0", v "a.arr1" ])
                        ( Let "l5" (CRecord [ { prop: "p0", val: v "a.rec0" }, { prop: "p1", val: v "a.rec1" } ])
                            ( Let "l6" (CAccessor (v "a.accBase") "field")
                                ( Let "l7" (CUpdate (v "a.updBase") [ { prop: "p0", val: v "a.upd0" } ])
                                    ( Let "l8" (CIf (v "a.ifCond") (Ret (CAtom (v "a.ifThen"))) (Ret (CAtom (v "a.ifElse"))))
                                        ( Ret
                                            ( CCase [ v "a.scrut0", v "a.scrut1" ]
                                                [ { binders: [ BVar "b0", BVar "b1" ]
                                                  , result: Uncond (Ret (CAtom (v "a.uncond")))
                                                  }
                                                , { binders: [ BNull, BNull ]
                                                  , result: Guarded
                                                      [ { guard: Ret (CAtom (v "a.guard0")), rhs: Ret (CAtom (v "a.rhs0")) }
                                                      , { guard: Ret (CAtom (v "a.guard1")), rhs: Ret (CAtom (v "a.rhs1")) }
                                                      ]
                                                  }
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

-- | A `Let` spine `n` deep, each step naming its own atom — the shape generated ANF actually has.
spine :: Int -> Expr
spine n = go n (Ret (CAtom (v "end")))
  where
  go i acc
    | i <= 0 = acc
    | otherwise = go (i - 1) (Let ("x" <> show i) (CAtom (v ("s" <> show i))) acc)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.ANF" do
  describe "foldAtoms — the per-node fidelity matrix" do
    it "sees every atom position of every node form, in source order" do
      atomsOf matrix `shouldEqual` map v
        [ "a.atom"
        , "a.recFn"
        , "a.recArg"
        , "a.recPerform"
        , "a.lamBody"
        , "a.prim0"
        , "a.prim1"
        , "a.ctor0"
        , "a.ctor1"
        , "a.arr0"
        , "a.arr1"
        , "a.rec0"
        , "a.rec1"
        , "a.accBase"
        -- the update BASE precedes its fields (the order the lowering meets them)
        , "a.updBase"
        , "a.upd0"
        , "a.ifCond"
        , "a.ifThen"
        , "a.ifElse"
        , "a.scrut0"
        , "a.scrut1"
        , "a.uncond"
        , "a.guard0"
        , "a.rhs0"
        , "a.guard1"
        , "a.rhs1"
        ]

    -- The tie to `mapAtoms`, in the direction the matrix alone cannot check: if the MAP visited a
    -- position the FOLD does not, that position keeps its original atom under a rewrite-everything
    -- map, and the fold's post-map list would still contain it. So a post-map walk that is
    -- all-marker proves the two trees cover the same positions (given the matrix pins the fold's
    -- own coverage against the source).
    it "agrees with mapAtoms about which positions exist" do
      let marker = AtomLit (LInt 0)
      atomsOf (mapAtoms (const marker) matrix) `shouldEqual` Array.replicate (Array.length (atomsOf matrix)) marker

  describe "foldAtoms — stack safety" do
    -- WIDTH, not just depth: the work stack is pushed from an array at every branching node, so a
    -- 100k-binding group / 100k-arm case / 100k-clause guard is the other way to reach the host
    -- stack. Each of these dies if `push` stops being the FFI fold.
    it "walks a 100k-binding LetRec group" do
      let
        bs = map (\i -> { var: "b" <> show i, rhs: Ret (CAtom (v ("g" <> show i))) }) (Array.range 1 100_000)
      Array.length (atomsOf (LetRec bs (Ret (CAtom (v "body"))))) `shouldEqual` 100_001

    it "walks a 100k-arm CCase" do
      let
        alts = map (\i -> { binders: [ BNull ], result: Uncond (Ret (CAtom (v ("m" <> show i)))) }) (Array.range 1 100_000)
      Array.length (atomsOf (Ret (CCase [ v "s" ] alts))) `shouldEqual` 100_001

    it "walks a 100k-clause guarded alternative" do
      let
        gs = map (\i -> { guard: Ret (CAtom (v ("q" <> show i))), rhs: Ret (CAtom (v ("w" <> show i))) }) (Array.range 1 100_000)
        alts = [ { binders: [ BNull ], result: Guarded gs } ]
      Array.length (atomsOf (Ret (CCase [ v "s" ] alts))) `shouldEqual` 200_001

    it "walks a 100k-deep Let spine at the default stack" do
      Array.length (atomsOf (spine 100_000)) `shouldEqual` 100_001

    it "walks a 100k-deep spine nested under a guard clause" do
      let
        body = Ret (CCase [ v "s" ] [ { binders: [ BNull ], result: Guarded [ { guard: spine 100_000, rhs: Ret (CAtom (v "r")) } ] } ])
      Array.length (atomsOf body) `shouldEqual` (1 + 100_001 + 1)
