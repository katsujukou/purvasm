-- | The `Codegen` monad's counter/buffer discipline is what byte-identity (ADR-0082 §2) rests on:
-- | `ssa` resets per function while `lbl`/`fns` stay module-global monotonic, buffers preserve emission
-- | order, and a buffer renders as every line followed by `"\n"` (empty → `""`). These are exactly the
-- | invariants boot's `ctx` mutation guarantees, so they get a direct test.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Monad where

import Prelude

import Data.List (List(..))
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple, fst, snd)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, Ctx, beginFn, emit, emitGlobal, flushFn, fresh, freshFn, freshLabel, makeCx, renderBuffer, runCodegen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- The default fresh state used across these cases.
run :: forall a. Codegen a -> Tuple a Ctx
run = runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, inlineAbi: true })

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Monad" do
  describe "fresh / freshLabel / freshFn" do
    it "pre-increments SSA temps starting at %t1" do
      let
        r = fst $ run do
          a <- fresh
          b <- fresh
          c <- fresh
          pure [ a, b, c ]
      r `shouldEqual` [ "%t1", "%t2", "%t3" ]

    it "numbers labels off one module-global counter shared across prefixes" do
      let
        r = fst $ run do
          a <- freshLabel "then"
          b <- freshLabel "else"
          c <- freshLabel "endif"
          pure [ a, b, c ]
      r `shouldEqual` [ "then1", "else2", "endif3" ]

    it "numbers lifted functions off the module-global fn counter" do
      let
        r = fst $ run do
          a <- freshFn "fn_"
          b <- freshFn "recfn_"
          pure [ a, b ]
      r `shouldEqual` [ "fn_1", "recfn_2" ]

  describe "beginFn" do
    it "resets the SSA counter per function but leaves lbl/fns monotonic" do
      let
        result = fst $ run do
          t1 <- fresh -- %t1
          l1 <- freshLabel "then" -- then1
          beginFn -- reset ssa only
          t2 <- fresh -- %t1 again
          l2 <- freshLabel "else" -- else2 (counter kept climbing)
          pure [ t1, l1, t2, l2 ]
      result `shouldEqual` [ "%t1", "then1", "%t1", "else2" ]

  describe "emit / renderBuffer" do
    it "preserves emission order and terminates every line with a newline" do
      let ctx = snd $ run (emit "line a" *> emit "line b" *> emit "line c")
      renderBuffer ctx.fn `shouldEqual` "line a\nline b\nline c\n"

    it "renders an empty buffer as the empty string" do
      renderBuffer (Nil :: List String) `shouldEqual` ""

    it "keeps the globals buffer independent of the function buffer" do
      let ctx = snd $ run (emit "fn0" *> emitGlobal "glob0" *> emit "fn1")
      renderBuffer ctx.fn `shouldEqual` "fn0\nfn1\n"
      renderBuffer ctx.globals `shouldEqual` "glob0\n"

  describe "flushFn" do
    it "moves the current function into the module buffer in order, then clears it" do
      let
        ctx = snd $ run do
          emit "f1 line1"
          emit "f1 line2"
          flushFn
          emit "f2 line1"
          flushFn
      renderBuffer ctx.md `shouldEqual` "f1 line1\nf1 line2\nf2 line1\n"
      ctx.fn `shouldEqual` (Nil :: List String)
