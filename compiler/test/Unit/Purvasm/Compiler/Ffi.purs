-- | Invariants of the foreign `resolver` ladder: an intrinsic key eta-expands to its
-- | primop, a structural key gives its guest term, a foreign *constant* gives a value,
-- | and a native leaf (or unknown name) declines (`Nothing`) — the linker then leaves it
-- | to the host. The exact terms are pinned end-to-end by the byte-identical `app.pvm`
-- | tests; these lock the ladder's shape per rung.
module Test.Unit.Purvasm.Compiler.Ffi where

import Prelude

import Data.Maybe (Maybe(..))
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.Ffi (resolver)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Ffi" do
  it "eta-expands an intrinsic primop (arity 2)" do
    resolver "Data.Semiring.intAdd"
      `shouldEqual` Just (TmLam "$0" (TmLam "$1" (TmPrim AddInt [ TmVar "$0", TmVar "$1" ])))

  it "resolves a foreign constant to a value" do
    resolver "Data.Unit.unit" `shouldEqual` Just (TmLit (LInt 0))
    resolver "Prim.undefined" `shouldEqual` Just (TmLit (LInt 0))

  it "gives a structural foreign its guest term (pureE = \\a _ -> a)" do
    resolver "Effect.pureE" `shouldEqual` Just (TmLam "a" (TmLam "$u" (TmVar "a")))

  it "declines a native leaf and an unknown name (host-resolved / unresolved)" do
    resolver "Effect.Console.log" `shouldEqual` Nothing
    resolver "Data.Show.showIntImpl" `shouldEqual` Nothing
    resolver "Nonexistent.foreign" `shouldEqual` Nothing
