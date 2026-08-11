-- | The `Codegen` monad's counter/buffer discipline is what deterministic emission (the L2-owned
-- | goldens and the ADR-0104 §2 stage fixpoint compare emitted text) rests on: `ssa` resets per
-- | function while `lbl`/`fns` stay module-global monotonic, buffers preserve emission order, and a
-- | buffer renders as every line followed by `"\n"` (empty → `""`). These are exactly the invariants
-- | boot's `ctx` mutation guaranteed, so they get a direct test.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Monad where

import Prelude

import Data.List (List(..))
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array as Array
import Control.Monad.Error.Class (try)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, Ctx, beginFn, bumpEpoch, emit, emitAnfLabel, emitDefine, emitGuestRet, emitGuestStore, emitModule, emitPhi, emitRetResolved, emitStringConstant, foldA, forA, forA_, forWithIndexA, fresh, freshFn, freshLabel, makeCx, closeHopArm, mintLoad, renderBuffer, renderChunks, renderFnBody, resolveGuest, runCodegen, snapshotReloads, snapshotVal, takeFn, armIncomingAt, touchVal, unsafeEmitChainLabel, mintFrameOwner)
import Purvasm.Compiler.Backend.LLVM.Value (Val, mkRootedLocal, rootedVal, unsafeMkFrameOwner, vImm, vRootedGlobal)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- The default fresh state used across these cases.
run :: forall a. Codegen a -> Tuple a Ctx
run = runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: Set.empty, byNeed: true })

-- The --debug entry-call state (the pv_get reload leg).
runDebug :: forall a. Codegen a -> Tuple a Ctx
runDebug = runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: false, defined: Set.empty, byNeed: true })

-- A local rooted token owned by the DEFAULT activation (makeCx starts at actId 0) — the
-- ADR-0106 consumption check passes until a beginFn mints a new activation.
tRooted :: String -> Val
tRooted h = rootedVal (mkRootedLocal h (unsafeMkFrameOwner { actId: 0, frameId: 1 }))

-- Force a pure emission inside the Effect runtime so its guard `unsafeCrashWith` surfaces as a
-- caught exception: evaluation is deferred into the Effect closure — strict evaluation at
-- construction would throw outside `try`'s reach and kill the runner instead.
expectCrash :: forall a. (Unit -> a) -> Aff Unit
expectCrash thunk = do
  r <- try (liftEffect (void (map (\_ -> thunk unit) (pure unit))))
  isLeft r `shouldEqual` true

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Monad" do
  spineSpec
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

    it "keeps the globals buffer independent of the function buffer (derived string constants only)" do
      let Tuple r ctx = run (emit "fn0" *> emitStringConstant "ab" <* emit "fn1")
      r `shouldEqual` Just { name: "@.str.1", len: 2 }
      renderBuffer ctx.fn `shouldEqual` "fn0\nfn1\n"
      renderChunks ctx.globals `shouldEqual` "@.str.1 = private unnamed_addr constant [2 x i8] c\"ab\"\n"

    it "emits nothing for the empty string (the null-pointer case)" do
      let Tuple r ctx = run (emitStringConstant "")
      r `shouldEqual` Nothing
      renderChunks ctx.globals `shouldEqual` ""

    it "a hostile guest string cannot break out of the c\"…\" constant (all parts derived)" do
      -- newline and quote are guest DATA: the escaper renders them as byte escapes, so the
      -- buffer holds exactly ONE well-formed constant line with the derived name and an
      -- intact closing quote — call-looking TEXT may remain, but only as bytes inside the
      -- constant, never as an instruction position.
      let ctx = snd $ run (emitStringConstant "a\"\n call i64 @evil(ptr %ctx)")
      let rendered = renderChunks ctx.globals
      Array.length (String.split (String.Pattern "\n") rendered) `shouldEqual` 2
      String.take 8 rendered `shouldEqual` "@.str.1 "
      String.drop (String.length rendered - 2) rendered `shouldEqual` "\"\n"

  describe "takeFn / emitModule / renderChunks" do
    it "takes the rendered function body and clears the line buffer" do
      let
        Tuple body ctx = run do
          emit "f1 line1"
          emit "f1 line2"
          takeFn
      renderFnBody body `shouldEqual` "f1 line1\nf1 line2\n"
      ctx.fn `shouldEqual` (Nil :: List String)

    it "concatenates module chunks verbatim, preserving their own newlines" do
      let
        ctx = snd $ run do
          emitModule "define @a {\nentry:\n  ret\n}\n\n"
          emitModule "define @b {\nentry:\n  ret\n}\n\n"
      renderChunks ctx.md
        `shouldEqual` "define @a {\nentry:\n  ret\n}\n\ndefine @b {\nentry:\n  ret\n}\n\n"

    it "emitDefine wraps a validated body in the header and the fixed footer" do
      let
        ctx = snd $ run do
          emit "  ret i64 %v"
          body <- takeFn
          emitDefine "define internal i64 @f(ptr %ctx) {\nentry:\n" body
      renderChunks ctx.md
        `shouldEqual` "define internal i64 @f(ptr %ctx) {\nentry:\n  ret i64 %v\n}\n\n"

  describe "the value-token machine (ADR-0105 §6.2, round-2 surface)" do
    it "a minted value renders through a consumer at its mint epoch" do
      renderBuffer (snd (run (mintLoad "%slot" >>= emitGuestRet))).fn `shouldEqual`
        ("  %t1 = load i64, ptr %slot\n" <> "  ret i64 %t1\n")

    it "an immune raw word passes across bumps" do
      renderBuffer (snd (run (bumpEpoch *> emitGuestRet (vImm "1")))).fn `shouldEqual` "  ret i64 1\n"

    it "a stale token crashes at its consuming renderer (read/use separated by a safepoint)" do
      expectCrash \_ -> run do
        v <- mintLoad "%slot"
        bumpEpoch
        emitGuestRet v

    it "an alias hands the token back unchanged — staleness survives the alias" do
      expectCrash \_ -> run do
        v <- mintLoad "%slot"
        let aliased = v
        bumpEpoch
        emitGuestRet aliased

    it "an incoming freezes fused with its arm's close and survives OTHER arms' bumps" do
      let
        body = renderBuffer
          ( snd $ run do
              v <- mintLoad "%slot"
              inc <- closeHopArm { hop: "arm1", merge: "join" } v
              bumpEpoch
              void (emitPhi "%r" [ inc ])
          ).fn
      body `shouldEqual`
        ( "  %t1 = load i64, ptr %slot\n"
            <> "  br label %arm1\n"
            <> "arm1:\n"
            <> "  br label %join\n"
            <> "  %r = phi i64 [ %t1, %arm1 ]\n"
        )

    it "closing an arm AFTER a same-arm bump crashes (the fused freeze IS the verification)" do
      expectCrash \_ -> run do
        v <- mintLoad "%slot"
        bumpEpoch
        closeHopArm { hop: "arm1", merge: "join" } v

    it "an SSA register cannot be laundered as an epoch-immune raw word" do
      expectCrash \_ -> vImm "%t9"

    it "whitespace cannot smuggle an SSA register past the immediate grammar (round 3)" do
      expectCrash \_ -> vImm " %t9"
      expectCrash \_ -> vImm "\t%t9"
      expectCrash \_ -> vImm "1 %t9"
      expectCrash \_ -> vImm ""
      expectCrash \_ -> vImm "-"

  describe "the renderer-owned reload cache (ADR-0105 §6.4, 2b-2 phase 2)" do
    it "a rooted miss reloads just before the consuming instruction (inline shape)" do
      renderBuffer (snd (run (emitGuestRet (tRooted "%h")))).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  ret i64 %t3\n"
        )

    it "a rooted miss reloads via pv_get under --debug" do
      renderBuffer (snd (runDebug (emitGuestRet (tRooted "%h")))).fn `shouldEqual`
        ( "  %t1 = call i64 @pv_get(ptr %ctx, i64 %h)\n"
            <> "  ret i64 %t1\n"
        )

    it "a hit at the same epoch reuses the SSA with no emission" do
      renderBuffer
        ( snd $ run do
            emitGuestStore (tRooted "%h") "%p"
            emitGuestStore (tRooted "%h") "%q"
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  store i64 %t3, ptr %p\n"
            <> "  store i64 %t3, ptr %q\n"
        )

    it "a safepoint bump misses and re-reloads (never a crash — the Rooted arm's contract)" do
      renderBuffer
        ( snd $ run do
            emitGuestStore (tRooted "%h") "%p"
            bumpEpoch
            emitGuestStore (tRooted "%h") "%q"
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  store i64 %t3, ptr %p\n"
            <> "  %t4 = load ptr, ptr %ctx\n"
            <> "  %t5 = getelementptr i64, ptr %t4, i64 %h\n"
            <> "  %t6 = load i64, ptr %t5\n"
            <> "  store i64 %t6, ptr %q\n"
        )

    it "the cache key is the slot identity: distinct handles never share a reload" do
      renderBuffer
        ( snd $ run do
            emitGuestStore (tRooted "%h1") "%p"
            emitGuestStore (tRooted "%h2") "%q"
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h1\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  store i64 %t3, ptr %p\n"
            <> "  %t4 = load ptr, ptr %ctx\n"
            <> "  %t5 = getelementptr i64, ptr %t4, i64 %h2\n"
            <> "  %t6 = load i64, ptr %t5\n"
            <> "  store i64 %t6, ptr %q\n"
        )

    it "an ANF arm label restores the branch-point snapshot: pre-branch reloads are shared" do
      -- `%h` reloads BEFORE the snapshot (it dominates both arms); the arm labels restore
      -- the snapshot, so the arm consumptions HIT — no per-arm re-reload (the §6.4
      -- path-sensitive refinement of pin c's always-clear form).
      renderBuffer
        ( snd $ run do
            emitGuestStore (tRooted "%h") "%p"
            snap <- snapshotReloads
            emitAnfLabel snap "arm1"
            emitGuestStore (tRooted "%h") "%q"
            emitAnfLabel snap "arm2"
            emitGuestStore (tRooted "%h") "%r"
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  store i64 %t3, ptr %p\n"
            <> "arm1:\n"
            <> "  store i64 %t3, ptr %q\n"
            <> "arm2:\n"
            <> "  store i64 %t3, ptr %r\n"
        )

    it "an arm-minted reload does NOT leak into the sibling arm (restore drops it — dominance)" do
      renderBuffer
        ( snd $ run do
            snap <- snapshotReloads
            emitAnfLabel snap "arm1"
            emitGuestStore (tRooted "%h") "%p"
            emitAnfLabel snap "arm2"
            emitGuestStore (tRooted "%h") "%q"
        ).fn `shouldEqual`
        ( "arm1:\n"
            <> "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  store i64 %t3, ptr %p\n"
            <> "arm2:\n"
            <> "  %t4 = load ptr, ptr %ctx\n"
            <> "  %t5 = getelementptr i64, ptr %t4, i64 %h\n"
            <> "  %t6 = load i64, ptr %t5\n"
            <> "  store i64 %t6, ptr %q\n"
        )

    it "a pre-branch entry restored after a bumping arm misses on epoch, not on absence" do
      renderBuffer
        ( snd $ run do
            emitGuestStore (tRooted "%h") "%p"
            snap <- snapshotReloads
            emitAnfLabel snap "arm1"
            bumpEpoch
            emitAnfLabel snap "join"
            emitGuestStore (tRooted "%h") "%q"
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  store i64 %t3, ptr %p\n"
            <> "arm1:\n"
            <> "join:\n"
            <> "  %t4 = load ptr, ptr %ctx\n"
            <> "  %t5 = getelementptr i64, ptr %t4, i64 %h\n"
            <> "  %t6 = load i64, ptr %t5\n"
            <> "  store i64 %t6, ptr %q\n"
        )

    it "a global rooted token loads its $root handle then the slot" do
      renderBuffer (snd (run (emitGuestRet (vRootedGlobal "@g$root")))).fn `shouldEqual`
        ( "  %t1 = load i64, ptr @g$root\n"
            <> "  %t2 = load ptr, ptr %ctx\n"
            <> "  %t3 = getelementptr i64, ptr %t2, i64 %t1\n"
            <> "  %t4 = load i64, ptr %t3\n"
            <> "  ret i64 %t4\n"
        )

    it "a phi incoming accepts a HOT rooted token and crashes on a cold one (fail-closed)" do
      renderBuffer
        ( snd $ run do
            touchVal (tRooted "%h")
            inc <- armIncomingAt { from: "a", startNext: "b" } (tRooted "%h")
            void (emitPhi "%r" [ inc ])
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "b:\n"
            <> "  %r = phi i64 [ %t3, %a ]\n"
        )
      expectCrash \_ -> run do
        inc <- armIncomingAt { from: "a", startNext: "b" } (tRooted "%cold")
        emitPhi "%r" [ inc ]

    it "snapshotVal reads back into a Fresh token: later staleness crashes instead of reloading" do
      renderBuffer
        ( snd $ run do
            v <- snapshotVal (tRooted "%h")
            emitGuestRet v
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  ret i64 %t3\n"
        )
      expectCrash \_ -> run do
        v <- snapshotVal (tRooted "%h")
        bumpEpoch
        emitGuestRet v

    it "a LocalSlot from activation A consumed after beginFn (activation B) crashes (ADR-0106)" do
      -- the consumption-side ActivationId check itself — NOT the ensureRooted path: the
      -- token reaches a renderer directly and must not reload a dead slot.
      expectCrash \_ -> run do
        emitGuestStore (tRooted "%h") "%p"
        beginFn
        emitGuestStore (tRooted "%h") "%q"

    it "a token owned by the CURRENT activation consumes normally after beginFn (positive)" do
      renderBuffer
        ( snd $ run do
            beginFn
            emitGuestStore (rootedVal (mkRootedLocal "%h" (unsafeMkFrameOwner { actId: 1, frameId: 2 }))) "%q"
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  store i64 %t3, ptr %q\n"
        )

    it "ActivationId / frame-counter overflow is fail-closed — never wraps into reuse (ADR-0106)" do
      expectCrash \_ -> runCodegen ((makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: Set.empty, byNeed: true }) { actId = 2147483646 }) beginFn
      expectCrash \_ -> runCodegen ((makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: Set.empty, byNeed: true }) { frameSeq = 2147483646 }) mintFrameOwner

    it "a safepoint between resolve and the ret crashes fail-closed (§6.4)" do
      expectCrash \_ -> run do
        r <- mintLoad "%slot" >>= resolveGuest
        bumpEpoch
        emitRetResolved r

    it "non-safepoint emission between resolve and the ret passes (the pop-shaped positive)" do
      renderBuffer
        ( snd $ run do
            r <- mintLoad "%slot" >>= resolveGuest
            emit "  store i64 0, ptr %root"
            emitRetResolved r
        ).fn `shouldEqual`
        ( "  %t1 = load i64, ptr %slot\n"
            <> "  store i64 0, ptr %root\n"
            <> "  ret i64 %t1\n"
        )

  describe "the raw-call guards (ADR-0105 §1 negative tests)" do
    it "emit rejects a raw call line" do
      expectCrash \_ -> run (emit "  %t1 = call i64 @evil(ptr %ctx)")
    it "emit rejects a column-zero call (line-start-normalised detection)" do
      expectCrash \_ -> run (emit "call i64 @evil(ptr %ctx)")
    it "emitModule rejects a call-carrying chunk" do
      expectCrash \_ -> run (emitModule "define @x {\n  %t1 = call i64 @evil(ptr %ctx)\n}\n")
    it "emitModule rejects a column-zero call inside a chunk" do
      expectCrash \_ -> run (emitModule "define @x {\ncall i64 @evil(ptr %ctx)\n}\n")
    it "emitDefine rejects a call-carrying header" do
      expectCrash \_ -> run (takeFn >>= emitDefine "define @x( call i64 ) {\n")
    it "the guards pass call-free text through" do
      renderBuffer (snd (run (emit "  ret i64 1"))).fn `shouldEqual` "  ret i64 1\n"

    it "emit rejects raw label text (§6.4 phase-2 round 2: the restore discipline is mechanical)" do
      expectCrash \_ -> run (emit "arm1:")
      expectCrash \_ -> run (emit "fchk9:")

    it "emit rejects embedded newlines — the one-line contract the guards rest on (round 3)" do
      -- two labels smuggled in one string: the mid-string colon defeats a single-line label
      -- test, so the newline itself must be the rejection.
      expectCrash \_ -> run (emit "arm1:\nnext:")
      -- an instruction+label mix, and the CR spelling
      expectCrash \_ -> run (emit "  %t1 = add i64 1, 1\nfoo:")
      expectCrash \_ -> run (emit "arm1:\r\nnext:")

    it "the safe label forms validate the name grammar — arbitrary text cannot reach raw emission (round 3)" do
      expectCrash \_ -> run do
        snap <- snapshotReloads
        emitAnfLabel snap "arm1:\n  %t1 = call i64 @evil(ptr %ctx)\nnext"
      expectCrash \_ -> run do
        snap <- snapshotReloads
        emitAnfLabel snap "arm1:extra"
      expectCrash \_ -> run (unsafeEmitChainLabel "a b")
      expectCrash \_ -> run (unsafeEmitChainLabel "a:b")
      expectCrash \_ -> run (unsafeEmitChainLabel "a\nb")
      expectCrash \_ -> run (unsafeEmitChainLabel "a\tb")
      expectCrash \_ -> run (unsafeEmitChainLabel "")

    it "the disciplined label forms pass: emitAnfLabel restores, the chain form keeps the cache" do
      renderBuffer
        ( snd $ run do
            emitGuestStore (tRooted "%h") "%p"
            unsafeEmitChainLabel "fchk1"
            emitGuestStore (tRooted "%h") "%q"
        ).fn `shouldEqual`
        ( "  %t1 = load ptr, ptr %ctx\n"
            <> "  %t2 = getelementptr i64, ptr %t1, i64 %h\n"
            <> "  %t3 = load i64, ptr %t2\n"
            <> "  store i64 %t3, ptr %p\n"
            <> "fchk1:\n"
            <> "  store i64 %t3, ptr %q\n"
        )

    it "the label guard does not false-positive indented instructions or br operands" do
      renderBuffer (snd (run (emit "  br label %then1"))).fn `shouldEqual` "  br label %then1\n"

-- --- the stack-safe spine combinators (2026-07-16 bugfix) -----------------------------------------
--
-- These four back every width-sized traversal in the LLVM backend, so their contract is pinned
-- directly: left-to-right effect order (observed through the emission buffer), result order,
-- index correctness, `foldl` direction, the empty spine, and — on one representative — stack
-- safety at a width no `Data.Traversable`-based version survives on the default host stack.

spineSpec :: Spec Unit
spineSpec = describe "forA / forA_ / forWithIndexA / foldA (stack-safe spine combinators)" do
  it "forA sequences effects left to right and returns results in element order" do
    let
      Tuple r ctx = run (forA [ "a", "b", "c" ] (\x -> emit x $> (x <> "!")))
    r `shouldEqual` [ "a!", "b!", "c!" ]
    renderBuffer ctx.fn `shouldEqual` "a\nb\nc\n"

  it "forA_ sequences effects left to right" do
    let
      Tuple _ ctx = run (forA_ [ "x", "y" ] emit)
    renderBuffer ctx.fn `shouldEqual` "x\ny\n"

  it "forWithIndexA passes ascending indices alongside the elements" do
    let
      Tuple r ctx = run (forWithIndexA [ "a", "b" ] (\i x -> emit (show i <> x) $> Tuple i x))
    r `shouldEqual` [ Tuple 0 "a", Tuple 1 "b" ]
    renderBuffer ctx.fn `shouldEqual` "0a\n1b\n"

  it "foldA folds left (foldl direction), sequencing effects in element order" do
    let
      Tuple r ctx = run (foldA (\acc x -> emit x $> (acc <> x)) "z" [ "a", "b", "c" ])
    -- foldl: ((z <> a) <> b) <> c
    r `shouldEqual` "zabc"
    renderBuffer ctx.fn `shouldEqual` "a\nb\nc\n"

  it "all four are identities on the empty spine" do
    let
      Tuple r1 ctx1 = run (forA ([] :: Array Int) (const fresh))
      Tuple r2 _ = run (forWithIndexA ([] :: Array Int) (\_ _ -> fresh))
      Tuple r3 _ = run (foldA (\acc _ -> pure (acc + 1)) 0 ([] :: Array Int))
      Tuple _ ctx4 = run (forA_ ([] :: Array Int) (const (emit "never")))
    r1 `shouldEqual` []
    r2 `shouldEqual` []
    r3 `shouldEqual` 0
    renderBuffer ctx1.fn `shouldEqual` ""
    renderBuffer ctx4.fn `shouldEqual` ""

  it "survives a 200k-element spine on the default host stack (the reason these exist)" do
    let
      Tuple r _ = run (foldA (\acc _ -> pure (acc + 1)) 0 (Array.replicate 200000 unit))
      Tuple rs _ = run (forA (Array.replicate 200000 unit) (const (pure unit)))
    r `shouldEqual` 200000
    Array.length rs `shouldEqual` 200000
