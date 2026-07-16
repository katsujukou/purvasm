-- | Invariants of `case` lowering (`Lower.Match`, ADR-0031) the types cannot enforce.
-- | New discriminant paths (literal, array length) are pinned exactly; the structural
-- | properties of the decision tree — one shared switch per scrutinee with deduped heads,
-- | a wildcard becoming the default, nested switches, records imposing no switch, guards,
-- | named binders, multiple scrutinees, and a `Fail` only when non-exhaustive — are
-- | checked robustly without depending on exact offsets. Driven through the real
-- | `lowerAtom`/`lowerExpr` (the `Lowerers` that `Lower` supplies).
module Test.Unit.Purvasm.Compiler.Bytecode.Lower.Match where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Bytecode.Lower (lowerAtom, lowerExpr)
import Purvasm.Compiler.Bytecode.Lower.Match (Lowerers, compileNaive, compileTree)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.Primitive (PrimOp(LtInt))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

lw :: Lowerers
lw = { atom: lowerAtom, body: lowerExpr }

-- | An unconditional body returning the literal `n`.
bdy :: Int -> Rhs
bdy n = Uncond (Ret (CAtom (AtomLit (LInt n))))

alt :: Array Binder -> Rhs -> Alt
alt binders result = { binders, result }

tree :: Array Atom -> Array Alt -> CodeBlock
tree = compileTree lw false

-- predicates / extractors over emitted bytecode
isSwitch :: Instruction -> Boolean
isSwitch = case _ of
  SwitchCtor _ _ -> true
  SwitchLit _ _ -> true
  SwitchLen _ _ -> true
  _ -> false

countOf :: (Instruction -> Boolean) -> CodeBlock -> Int
countOf p = Array.length <<< Array.filter p

isJumpUnless :: Instruction -> Boolean
isJumpUnless = case _ of
  JumpUnless _ -> true
  _ -> false

switchCount :: CodeBlock -> Int
switchCount = countOf isSwitch

failCount :: CodeBlock -> Int
failCount = countOf case _ of
  Fail _ -> true
  _ -> false

ctorHeads :: CodeBlock -> Array String
ctorHeads = fromMaybe [] <<< Array.findMap case _ of
  SwitchCtor cs _ -> Just (map (\(t /\ _) -> t) cs)
  _ -> Nothing

litHeads :: CodeBlock -> Array Literal
litHeads = fromMaybe [] <<< Array.findMap case _ of
  SwitchLit cs _ -> Just (map (\(l /\ _) -> l) cs)
  _ -> Nothing

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Bytecode.Lower.Match" do
  stackSafetySpec
  describe "compileTree — exact bytecode for new discriminants" do
    it "lowers a literal case to one SwitchLit with a wildcard default" do
      tree [ AtomVar "n" ]
        [ alt [ BLit (LInt 1) ] (bdy 10)
        , alt [ BLit (LInt 2) ] (bdy 20)
        , alt [ BVar "x" ] (bdy 0)
        ]
        `shouldEqual`
          [ Load "n"
          , Bind "$dt1"
          , Load "$dt1"
          , SwitchLit [ LInt 1 /\ 0, LInt 2 /\ 2 ] 4
          , PushInt 10
          , Jump 6
          , PushInt 20
          , Jump 4
          , Load "$dt1"
          , Bind "x"
          , PushInt 0
          , Jump 0
          ]

    it "lowers an array case to one SwitchLen, projecting elements" do
      tree [ AtomVar "xs" ]
        [ alt [ BArray [ BVar "a" ] ] (Uncond (Ret (CAtom (AtomVar "a"))))
        , alt [ BVar "z" ] (bdy 0)
        ]
        `shouldEqual`
          [ Load "xs"
          , Bind "$dt1"
          , Load "$dt1"
          , SwitchLen [ 1 /\ 0 ] 7
          , Load "$dt1"
          , Proj_arr 0
          , Bind "$dt2"
          , Load "$dt2"
          , Bind "a"
          , Load "a"
          , Jump 4
          , Load "$dt1"
          , Bind "z"
          , PushInt 0
          , Jump 0
          ]

  describe "compileTree — decision-tree structure" do
    it "shares one constructor switch across alternatives, deduping repeated tags" do
      let
        code = tree [ AtomVar "x" ]
          [ alt [ BCtor "Just" [ BVar "a" ] ] (bdy 1)
          , alt [ BCtor "Just" [ BVar "b" ] ] (bdy 2)
          , alt [ BCtor "Nothing" [] ] (bdy 3)
          ]
      switchCount code `shouldEqual` 1
      ctorHeads code `shouldEqual` [ "Just", "Nothing" ]

    it "deduplicates repeated literal heads" do
      litHeads
        ( tree [ AtomVar "n" ]
            [ alt [ BLit (LInt 1) ] (bdy 1)
            , alt [ BLit (LInt 1) ] (bdy 2)
            , alt [ BLit (LInt 2) ] (bdy 3)
            , alt [ BVar "w" ] (bdy 0)
            ]
        )
        `shouldEqual` [ LInt 1, LInt 2 ]

    it "switches at each level of a nested constructor pattern" do
      switchCount
        ( tree [ AtomVar "x" ]
            [ alt [ BCtor "Just" [ BCtor "Just" [ BVar "a" ] ] ] (Uncond (Ret (CAtom (AtomVar "a"))))
            , alt [ BNull ] (bdy 0)
            ]
        )
        `shouldEqual` 2

    it "binds an as-pattern's name while still switching on its constructor" do
      let
        code = tree [ AtomVar "x" ]
          [ alt [ BNamed "n" (BCtor "Just" [ BVar "a" ]) ] (Uncond (Ret (CAtom (AtomVar "n"))))
          , alt [ BNull ] (bdy 0)
          ]
      switchCount code `shouldEqual` 1
      Array.any (eq (Bind "n")) code `shouldEqual` true

    it "binds each of several scrutinees in order" do
      Array.take 4 (tree [ AtomVar "x", AtomVar "y" ] [ alt [ BVar "a", BVar "b" ] (bdy 1) ])
        `shouldEqual` [ Load "x", Bind "$dt1", Load "y", Bind "$dt2" ]

    it "expands a record pattern with field access and no discriminant switch" do
      let
        code = tree [ AtomVar "r" ]
          [ alt [ BRecord [ { prop: "x", binder: BVar "a" } ] ] (Uncond (Ret (CAtom (AtomVar "a")))) ]
      switchCount code `shouldEqual` 0
      Array.any (eq (GetField "x")) code `shouldEqual` true

    it "evaluates a guard chain (JumpUnless) at the matched leaf" do
      let
        code = tree [ AtomVar "x" ]
          [ alt [ BVar "y" ]
              ( Guarded
                  [ { guard: Ret (CPrim LtInt [ AtomLit (LInt 0), AtomVar "y" ]), rhs: Ret (CAtom (AtomLit (LInt 1))) }
                  , { guard: Ret (CAtom (AtomLit (LBool true))), rhs: Ret (CAtom (AtomLit (LInt 2))) }
                  ]
              )
          ]
      countOf isJumpUnless code `shouldEqual` 2

  describe "compileTree — exhaustiveness" do
    it "falls through to a single Fail when no alternative is total" do
      failCount (tree [ AtomVar "x" ] [ alt [ BCtor "Just" [ BVar "a" ] ] (bdy 1) ])
        `shouldEqual` 1

    it "emits no Fail when a wildcard covers the default" do
      failCount
        ( tree [ AtomVar "x" ]
            [ alt [ BCtor "Just" [ BVar "a" ] ] (bdy 1), alt [ BNull ] (bdy 0) ]
        )
        `shouldEqual` 0

  describe "compileNaive — per-alternative re-testing" do
    it "binds the scrutinee, switches on the tag, projects the field, and falls through to Fail" do
      compileNaive lw false [ AtomVar "x" ]
        [ alt [ BCtor "Just" [ BVar "y" ] ] (Uncond (Ret (CAtom (AtomVar "y")))) ]
        `shouldEqual`
          [ Load "x"
          , Bind "$nv1"
          , Load "$nv1"
          , SwitchCtor [ "Just" /\ 0 ] 7
          , Load "$nv1"
          , Proj 0
          , Bind "$nv2"
          , Load "$nv2"
          , Bind "y"
          , Load "y"
          , Jump 1
          , Fail "case: no matching alternative"
          ]

    it "re-tests each alternative, so two constructor alternatives emit two tag switches" do
      switchCount
        ( compileNaive lw false [ AtomVar "x" ]
            [ alt [ BCtor "Just" [ BVar "a" ] ] (bdy 1), alt [ BCtor "Nothing" [] ] (bdy 2) ]
        )
        `shouldEqual` 2

-- --- stack safety (2026-07-16 bugfix) --------------------------------------------------------------

-- | The measured overflow path: `emitChunk` sequenced one `State` bind — one live host frame —
-- | per instruction of an already-lowered body, and a `PureScript.CST.Parser`-scale alternative
-- | (tens of thousands of instructions) exhausted the default stack during the VM-target
-- | self-compile. A stub `Lowerers` whose body is 50k instructions drives that exact seam under
-- | the test runner's default stack; the assertion stays shallow on purpose.
stackSafetySpec :: Spec Unit
stackSafetySpec = describe "stack safety (data-sized assembler spines)" do
  it "assembles a 50k-instruction leaf body on the default host stack (emitChunk)" do
    let
      hugeLw :: Lowerers
      hugeLw = { atom: lowerAtom, body: \_ _ -> Array.replicate 50000 (PushInt 1) }
      out = compileTree hugeLw false [ AtomLit (LInt 0) ] [ alt [ BVar "x" ] (bdy 1) ]
    (Array.length out >= 50000) `shouldEqual` true
