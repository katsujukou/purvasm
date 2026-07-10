-- | The ctx-header inline fast paths are the highest byte-identity risk in the port: every function
-- | roots its params through them, so a single wrong SSA number or reordered line diverges the whole
-- | `.ll`. Each emitter is run in isolation and asserted against boot's exact block shape (ADR-0079;
-- | cf. boot's `--no-opt` output, e.g. the `rchk`/`schk` blocks of `Slice1`/`Example.Fib.Lib`).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Abi where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.Tuple (snd)
import Purvasm.Compiler.Backend.LLVM.Abi (abiFrameOpen, abiGet, abiPopFrame, abiRoot, abiSettle, abiStamp, declarations, forceValue)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, makeCx, renderBuffer, runCodegen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Render the function-buffer text produced by an emitter run from a fresh release-mode context.
emitted :: forall a. Codegen a -> String
emitted m = renderBuffer (snd (runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true }) m)).fn

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

  describe "abiFrameOpen / abiGet / abiPopFrame" do
    it "opens a frame by reading roots_len (getelementptr emitted before the load)" do
      emitted abiFrameOpen `shouldEqual`
        ( "  %t2 = getelementptr i8, ptr %ctx, i64 8\n"
            <> "  %t1 = load i64, ptr %t2\n"
        )

    it "reads a handle's value via roots_base + slot" do
      emitted (abiGet "%h") `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
        )

    it "pops a frame by storing the mark back into roots_len" do
      emitted (abiPopFrame "%m") `shouldEqual`
        ( "  %t1 = getelementptr i8, ptr %ctx, i64 8\n"
            <> "  store i64 %m, ptr %t1\n"
        )

  describe "abiRoot" do
    it "emits the 4-block in-capacity fast path with the len/pv_root phi" do
      emitted (abiRoot "%v") `shouldEqual`
        ( "  br label %rchk1\n"
            <> "rchk1:\n"
            <> "  %t1 = getelementptr i8, ptr %ctx, i64 8\n"
            <> "  %t2 = load i64, ptr %t1\n"
            <> "  %t4 = getelementptr i8, ptr %ctx, i64 16\n"
            <> "  %t3 = load i64, ptr %t4\n"
            <> "  %t5 = icmp eq i64 %t2, %t3\n"
            <> "  br i1 %t5, label %rslow3, label %rfast2\n"
            <> "rfast2:\n"
            <> "  %t6 = load ptr, ptr %ctx\n"
            <> "  %t7 = getelementptr i64, ptr %t6, i64 %t2\n"
            <> "  store i64 %v, ptr %t7\n"
            <> "  %t8 = add i64 %t2, 1\n"
            <> "  store i64 %t8, ptr %t1\n"
            <> "  br label %rdone4\n"
            <> "rslow3:\n"
            <> "  %t9 = call i64 @pv_root(ptr %ctx, i64 %v)\n"
            <> "  br label %rdone4\n"
            <> "rdone4:\n"
            <> "  %t10 = phi i64 [ %t2, %rfast2 ], [ %t9, %rslow3 ]\n"
        )

  describe "abiSettle" do
    it "emits the 3-block pending-tail settle with the r/pv_settle phi" do
      emitted (abiSettle "%r") `shouldEqual`
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
      emitted (forceValue "%v") `shouldEqual`
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
