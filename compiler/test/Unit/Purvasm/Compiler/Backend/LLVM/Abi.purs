-- | The ctx-header inline fast paths are the port's highest-risk emitters: every function roots its
-- | params through them, so a single wrong SSA number or reordered line diverges the whole `.ll`.
-- | Each emitter is run in isolation and asserted against L2-owned golden block shapes (ADR-0079;
-- | provenance: boot's `--no-opt` output, e.g. the `rchk`/`schk` blocks of `Slice1`/`Example.Fib.Lib`).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Abi where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.Tuple (snd)
import Purvasm.Compiler.Backend.LLVM.Abi (abiSettle, abiStamp, declarations, forceValue)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, makeCx, renderBuffer, runCodegen)
import Purvasm.Compiler.Backend.LLVM.Value (unsafeTestVal)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Render the function-buffer text produced by an emitter run from a fresh release-mode context.
emitted :: forall a. Codegen a -> String
emitted m = renderBuffer (snd (runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, byNeed: true }) m)).fn

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Abi" do
  describe "declarations / abiStamp" do
    it "declares every pv_* symbol, in order, with no trailing newline" do
      let ds = split (Pattern "\n") declarations
      length ds `shouldEqual` 67
      Array.head ds `shouldEqual` pure "declare ptr @pv_runtime_new(i64)"
      Array.last ds `shouldEqual` pure "declare i64 @pv_force_if_byneed(ptr, i64)"

    it "emits the versioned ABI stamp in release mode, nothing under --debug" do
      abiStamp true `shouldEqual`
        ( "@pv_ctx_abi_v1 = external global i8\n"
            <> "@pv_abi_stamp = internal constant ptr @pv_ctx_abi_v1\n"
            <> "@llvm.used = appending global [1 x ptr] [ptr @pv_abi_stamp], section \"llvm.metadata\"\n"
        )
      abiStamp false `shouldEqual` ""

  describe "abiSettle" do
    it "emits the 3-block pending-tail settle with the r/pv_settle phi" do
      emitted (abiSettle (unsafeTestVal "%r")) `shouldEqual`
        ( "  br label %schk1\n"
            <> "schk1:\n"
            <> "  %t2 = getelementptr i8, ptr %ctx, i64 24\n"
            <> "  %t1 = load i64, ptr %t2\n"
            <> "  %t3 = icmp ne i64 %t1, 0\n"
            <> "  br i1 %t3, label %sslow2, label %sdone3\n"
            <> "sslow2:\n"
            <> "  %t4 = call i64 @pv_settle(ptr %ctx, i64 %r)\n"
            <> "  br label %sdone3\n"
            <> "sdone3:\n"
            <> "  %t5 = phi i64 [ %r, %schk1 ], [ %t4, %sslow2 ]\n"
        )

  describe "forceValue" do
    it "emits the 3-block immediate-fast-path force with the v/forced phi" do
      emitted (forceValue (unsafeTestVal "%v")) `shouldEqual`
        ( "  br label %fchk1\n"
            <> "fchk1:\n"
            <> "  %t1 = and i64 %v, 1\n"
            <> "  %t2 = icmp ne i64 %t1, 0\n"
            <> "  br i1 %t2, label %fdone3, label %fslow2\n"
            <> "fslow2:\n"
            <> "  %t3 = call i64 @pv_force_if_byneed(ptr %ctx, i64 %v)\n"
            <> "  br label %fdone3\n"
            <> "fdone3:\n"
            <> "  %t4 = phi i64 [ %v, %fchk1 ], [ %t3, %fslow2 ]\n"
        )
