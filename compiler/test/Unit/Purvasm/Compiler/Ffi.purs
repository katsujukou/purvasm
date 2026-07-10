-- | Invariants of the foreign `resolver` ladder: an intrinsic key eta-expands to its
-- | primop, a structural key gives its guest term, a foreign *constant* gives a value,
-- | and a native leaf (or unknown name) declines (`Nothing`) — the linker then leaves it
-- | to the host. The exact terms are pinned end-to-end by the byte-identical `app.pvm`
-- | tests; these lock the ladder's shape per rung.
module Test.Unit.Purvasm.Compiler.Ffi where

import Prelude

import Data.Maybe (Maybe(..))
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.Ffi (intrinsicPrim, resolver)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Ffi" do
  it "eta-expands an intrinsic primop (arity 2)" do
    resolver "Data.Semiring.intAdd"
      `shouldEqual` Just (TmLam "$0" (TmLam "$1" (TmPrim AddInt [ TmVar "$0", TmVar "$1" ])))

  it "eta-expands the Int bitwise family on both the Data.Int.Bits and Purvasm.Int names" do
    resolver "Data.Int.Bits.and"
      `shouldEqual` Just (TmLam "$0" (TmLam "$1" (TmPrim AndInt [ TmVar "$0", TmVar "$1" ])))
    resolver "Data.Int.Bits.zshr"
      `shouldEqual` Just (TmLam "$0" (TmLam "$1" (TmPrim ZshrInt [ TmVar "$0", TmVar "$1" ])))
    resolver "Data.Int.Bits.complement"
      `shouldEqual` Just (TmLam "$0" (TmPrim ComplementInt [ TmVar "$0" ]))
    resolver "Purvasm.Int.shl"
      `shouldEqual` Just (TmLam "$0" (TmLam "$1" (TmPrim ShlInt [ TmVar "$0", TmVar "$1" ])))
    resolver "Purvasm.Int.complement"
      `shouldEqual` Just (TmLam "$0" (TmPrim ComplementInt [ TmVar "$0" ]))

  it "resolves a foreign constant to a value" do
    resolver "Data.Unit.unit" `shouldEqual` Just (TmLit (LInt 0))
    resolver "Prim.undefined" `shouldEqual` Just (TmLit (LInt 0))

  it "gives a structural foreign its guest term (pureE = \\a _ -> a)" do
    resolver "Effect.pureE" `shouldEqual` Just (TmLam "a" (TmLam "$u" (TmVar "a")))

  it "declines a native leaf and an unknown name (host-resolved / unresolved)" do
    resolver "Effect.Console.log" `shouldEqual` Nothing
    resolver "Data.Show.showIntImpl" `shouldEqual` Nothing
    resolver "Nonexistent.foreign" `shouldEqual` Nothing

  it "intrinsicPrim reads the primop and arity off an eta-expanded intrinsic" do
    intrinsicPrim "Data.Semiring.intAdd" `shouldEqual` Just { op: AddInt, arity: 2 }
    intrinsicPrim "Purvasm.Int.zshr" `shouldEqual` Just { op: ZshrInt, arity: 2 }
    intrinsicPrim "Data.Int.Bits.complement" `shouldEqual` Just { op: ComplementInt, arity: 1 }
    intrinsicPrim "Purvasm.Array.unsafeSet" `shouldEqual` Just { op: SetArray, arity: 3 }

  it "intrinsicPrim declines non-eta intrinsics, structural foreigns, and unknown names" do
    -- identity lambda (charId), composite body (intDegree), constant (unit): intrinsic but not eta-primop
    intrinsicPrim "Purvasm.Char.toCodePoint" `shouldEqual` Nothing
    intrinsicPrim "Data.EuclideanRing.intDegree" `shouldEqual` Nothing
    intrinsicPrim "Data.Unit.unit" `shouldEqual` Nothing
    -- structural rung and unresolved names never denote a primop
    intrinsicPrim "Effect.pureE" `shouldEqual` Nothing
    intrinsicPrim "Nonexistent.foreign" `shouldEqual` Nothing
