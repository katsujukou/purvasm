-- | The root-lifetime API's emission goldens (ADR-0105 §2): the frame open and rooting fast
-- | paths are the port's highest-risk emitters (a single wrong SSA number diverges the whole
-- | `.ll`); the fused pop+continuation shapes (`retWith`/`musttailWith`/`tailcallWith`/
-- | `entryTeardown`) are the ONLY pop forms; and the init wrappers own the permanent-tier
-- | phase order — the framed golden pins `open → body → pop → permanent root` as one emission,
-- | with the permanent block rendered by the SAME `rootBlockAt` shape as the transient one
-- | (one rooting emission, two lifetimes).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Root where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Purvasm.Compiler.Backend.LLVM.Mangle (mangle)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, Ctx, makeCx, renderBuffer, renderChunks, runCodegen)
import Control.Monad.Error.Class (try)
import Data.Either (isLeft)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Purvasm.Compiler.Backend.LLVM.Value (rootedVal, unsafeTestVal, vImm, vRootedGlobal)
import Purvasm.Compiler.Backend.LLVM.Root (emitGfunInit, emitInitFnFramed, ensureRooted, entryTeardown, musttailWith, openFrame, retWith, tailcallWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

runWith :: forall a. Boolean -> Codegen a -> Tuple a Ctx
runWith inlineAbi = runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi, defined: Set.empty, profileApply: false, byNeed: true })

-- Render the function-buffer text produced by an emitter run from a fresh release-mode context.
emittedWith :: forall a. Boolean -> Codegen a -> String
emittedWith inlineAbi m = renderBuffer (snd (runWith inlineAbi m)).fn

emitted :: forall a. Codegen a -> String
emitted = emittedWith true

-- Force a pure emission inside the Effect runtime so its guard `unsafeCrashWith` surfaces as
-- a caught exception (the Monad-test deferral pattern).
expectCrash :: forall a. (Unit -> a) -> Aff Unit
expectCrash thunk = do
  r <- try (liftEffect (void (map (\_ -> thunk unit) (pure unit))))
  isLeft r `shouldEqual` true

-- The release-mode frame-open lines from a fresh context (mark `%t1`, header gep `%t2`).
openLines :: String
openLines =
  "  %t2 = getelementptr i8, ptr %ctx, i64 8\n"
    <> "  %t1 = load i64, ptr %t2\n"

-- The release-mode pop-back-to-`%t1` lines with the gep at SSA index `n`.
popLines :: Int -> String
popLines n =
  "  %t" <> show n <> " = getelementptr i8, ptr %ctx, i64 8\n"
    <> "  store i64 %t1, ptr %t"
    <> show n
    <> "\n"

-- The pinned in-capacity rooting block (ADR-0079) for value `v`, with label numbering starting
-- at `lb` and SSA numbering at `t<sb+1>` — used for BOTH tiers, which is itself the
-- one-rooting-emission pin.
rootBlockAt :: Int -> Int -> String -> String
rootBlockAt lb sb v =
  let
    t n = "%t" <> show (sb + n)
    def name k = name <> show (lb + k)
    ref name k = "%" <> def name k
  in
    ("  br label " <> ref "rchk" 0 <> "\n")
      <> (def "rchk" 0 <> ":\n")
      <> ("  " <> t 1 <> " = getelementptr i8, ptr %ctx, i64 8\n")
      <> ("  " <> t 2 <> " = load i64, ptr " <> t 1 <> "\n")
      <> ("  " <> t 4 <> " = getelementptr i8, ptr %ctx, i64 16\n")
      <> ("  " <> t 3 <> " = load i64, ptr " <> t 4 <> "\n")
      <> ("  " <> t 5 <> " = icmp eq i64 " <> t 2 <> ", " <> t 3 <> "\n")
      <> ("  br i1 " <> t 5 <> ", label " <> ref "rslow" 2 <> ", label " <> ref "rfast" 1 <> "\n")
      <> (def "rfast" 1 <> ":\n")
      <> ("  " <> t 6 <> " = load ptr, ptr %ctx\n")
      <> ("  " <> t 7 <> " = getelementptr i64, ptr " <> t 6 <> ", i64 " <> t 2 <> "\n")
      <> ("  store i64 " <> v <> ", ptr " <> t 7 <> "\n")
      <> ("  " <> t 8 <> " = add i64 " <> t 2 <> ", 1\n")
      <> ("  store i64 " <> t 8 <> ", ptr " <> t 1 <> "\n")
      <> ("  br label " <> ref "rdone" 3 <> "\n")
      <> (def "rslow" 2 <> ":\n")
      <> ("  " <> t 9 <> " = call i64 @pv_root(ptr %ctx, i64 " <> v <> ")\n")
      <> ("  br label " <> ref "rdone" 3 <> "\n")
      <> (def "rdone" 3 <> ":\n")
      <> ("  " <> t 10 <> " = phi i64 [ " <> t 2 <> ", " <> ref "rfast" 1 <> " ], [ " <> t 9 <> ", " <> ref "rslow" 2 <> " ]\n")

initWrapper :: String -> String -> String
initWrapper key body =
  "define void @" <> mangle key <> "$init(ptr %ctx) {\nentry:\n" <> body <> "  ret void\n}\n\n"

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Root" do
  describe "openFrame" do
    it "opens a frame by reading roots_len (getelementptr emitted before the load)" do
      emitted openFrame `shouldEqual` openLines

  describe "ensureRooted (ADR-0106 slice 1)" do
    it "roots a Fresh value: the 4-block in-capacity fast path with the len/pv_root phi" do
      emitted (openFrame >>= \tok -> void (ensureRooted (Just tok) (unsafeTestVal "%v"))) `shouldEqual`
        (openLines <> rootBlockAt 1 2 "%v")

    it "is a single pv_root entry call under --debug" do
      emittedWith false (openFrame >>= \tok -> void (ensureRooted (Just tok) (unsafeTestVal "%v"))) `shouldEqual`
        ( "  %t1 = call i64 @pv_frame(ptr %ctx)\n"
            <> "  %t2 = call i64 @pv_root(ptr %ctx, i64 %v)\n"
        )

    it "REUSES an already-rooted value's slot in the same frame: zero emission" do
      emitted
        ( openFrame >>= \tok -> do
            rv <- ensureRooted (Just tok) (unsafeTestVal "%v")
            void (ensureRooted (Just tok) (rootedVal rv))
        ) `shouldEqual` (openLines <> rootBlockAt 1 2 "%v")

    it "a LocalSlot presented to a DIFFERENT frame fail-closed crashes" do
      expectCrash \_ -> emitted
        ( do
            tokA <- openFrame
            rv <- ensureRooted (Just tokA) (unsafeTestVal "%v")
            tokB <- openFrame
            void (ensureRooted (Just tokB) (rootedVal rv))
        )

    it "a LocalSlot with NO frame fail-closed crashes; a Fresh with no frame hits the backstop" do
      expectCrash \_ -> emitted
        ( do
            tok <- openFrame
            rv <- ensureRooted (Just tok) (unsafeTestVal "%v")
            void (ensureRooted Nothing (rootedVal rv))
        )
      expectCrash \_ -> emitted (void (ensureRooted Nothing (unsafeTestVal "%v")))

    it "a GlobalSlot reuses with and without a frame: zero emission" do
      emitted (void (ensureRooted Nothing (vRootedGlobal "@g$root"))) `shouldEqual` ""
      emitted (openFrame >>= \tok -> void (ensureRooted (Just tok) (vRootedGlobal "@g$root"))) `shouldEqual` openLines

  describe "retWith / musttailWith / tailcallWith / entryTeardown (the fused pop forms)" do
    it "retWith pops iff a frame is open, then rets" do
      emitted (openFrame >>= \tok -> retWith (Just tok) (unsafeTestVal "%v")) `shouldEqual`
        (openLines <> popLines 3 <> "  ret i64 %v\n")
      emitted (retWith Nothing (unsafeTestVal "%v")) `shouldEqual` "  ret i64 %v\n"

    it "pops via the pv_pop_frame entry call under --debug" do
      emittedWith false (openFrame >>= \tok -> retWith (Just tok) (unsafeTestVal "%v")) `shouldEqual`
        ( "  %t1 = call i64 @pv_frame(ptr %ctx)\n"
            <> "  call void @pv_pop_frame(ptr %ctx, i64 %t1)\n"
            <> "  ret i64 %v\n"
        )

    it "musttailWith pops BEFORE the musttail call, then rets its result" do
      emitted (openFrame >>= \tok -> musttailWith (Just tok) { dsym: "foo$d", env: unsafeTestVal "%e", args: [ unsafeTestVal "%a" ] }) `shouldEqual`
        ( openLines <> popLines 3
            <> "  %t4 = musttail call tailcc i64 @foo$d(ptr %ctx, i64 %e, i64 %a)\n"
            <> "  ret i64 %t4\n"
        )

    it "tailcallWith stashes the pending tail, pops, and returns unit to the trampoline" do
      emitted (openFrame >>= \tok -> tailcallWith (Just tok) { fv: unsafeTestVal "%f", argp: "%p", nargs: 2 }) `shouldEqual`
        ( openLines
            <> "  call void @pv_tailcall(ptr %ctx, i64 %f, ptr %p, i64 2)\n"
            <> popLines 3
            <> "  ret i64 1\n"
        )

    it "entryTeardown pops, frees the runtime, and returns from @main" do
      emitted (openFrame >>= entryTeardown) `shouldEqual`
        ( openLines <> popLines 3
            <> "  call void @pv_runtime_free(ptr %ctx)\n"
            <> "  ret i32 0\n"
        )

  describe "emitGfunInit / emitInitFnFramed (wrapper-owned permanent-tier phase order)" do
    it "the frameless Gfun init is a fixed shape: build the closure, permanent-root it" do
      let
        Tuple _ ctx = runWith true (emitGfunInit "TestK" 2)
      renderChunks ctx.md `shouldEqual`
        initWrapper "TestK"
          ( ("  %t1 = ptrtoint ptr @" <> mangle "TestK" <> " to i64\n")
              <> "  %t2 = call i64 @pv_make_closure(ptr %ctx, i64 %t1, i32 2, i64 1)\n"
              <> rootBlockAt 1 2 "%t2"
              <> ("  store i64 %t12, ptr @" <> mangle "TestK" <> "$root\n")
          )

    it "a GlobalSlot candidate HANDLE-COPIES into its $root: no snapshot, no root block (ADR-0106)" do
      let
        Tuple _ ctx = runWith true
          (emitInitFnFramed "TestK" \_ -> pure [ Tuple "TestK" (vRootedGlobal "@src$root") ])
      renderChunks ctx.md `shouldEqual`
        initWrapper "TestK"
          ( openLines
              <> popLines 3 -- the transient frame closes with nothing in it...
              <> "  %t4 = load i64, ptr @src$root\n" -- ...and the permanent tier copies the
              <> ("  store i64 %t4, ptr @" <> mangle "TestK" <> "$root\n") -- source's stable handle
          )

    it "framed init pins the phase order: open, body, POP, then the permanent root" do
      let
        Tuple _ ctx = runWith true
          ( emitInitFnFramed "TestK" \tok -> do
              _ <- ensureRooted (Just tok) (unsafeTestVal "%v")
              pure [ Tuple "TestK" (unsafeTestVal "%w") ]
          )
      renderChunks ctx.md `shouldEqual`
        initWrapper "TestK"
          ( openLines
              <> rootBlockAt 1 2 "%v" -- the body's TRANSIENT root, inside the frame
              <> popLines 13 -- the frame closes...
              <> rootBlockAt 5 13 "%w" -- ...and only then is the permanent root planted
              <> ("  store i64 %t23, ptr @" <> mangle "TestK" <> "$root\n")
          )

    it "the framed wrapper's pop subsumes a frame the body opened and leaked (wrapper-mark pin)" do
      let
        Tuple _ ctx = runWith true
          ( emitInitFnFramed "TestK" \_ -> do
              _ <- openFrame
              pure [ Tuple "TestK" (unsafeTestVal "%v") ]
          )
      -- the pop stores %t1 — the WRAPPER's mark — so the body's leaked frame (mark %t3) is
      -- subsumed and the permanent root still lands in the init region.
      renderChunks ctx.md `shouldEqual`
        initWrapper "TestK"
          ( openLines
              <> "  %t4 = getelementptr i8, ptr %ctx, i64 8\n"
              <> "  %t3 = load i64, ptr %t4\n"
              <> popLines 5
              <> rootBlockAt 1 5 "%v"
              <> ("  store i64 %t15, ptr @" <> mangle "TestK" <> "$root\n")
          )

    it "is a single pv_root entry call under --debug" do
      let
        Tuple _ ctx = runWith false (emitGfunInit "TestK" 0)
      renderChunks ctx.md `shouldEqual`
        initWrapper "TestK"
          ( ("  %t1 = ptrtoint ptr @" <> mangle "TestK" <> " to i64\n")
              <> "  %t2 = call i64 @pv_make_closure(ptr %ctx, i64 %t1, i32 0, i64 1)\n"
              <> "  %t3 = call i64 @pv_root(ptr %ctx, i64 %t2)\n"
              <> ("  store i64 %t3, ptr @" <> mangle "TestK" <> "$root\n")
          )
