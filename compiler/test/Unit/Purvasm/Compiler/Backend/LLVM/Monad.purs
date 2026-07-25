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
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, Ctx, beginFn, emit, emitDefine, emitModule, emitStringConstant, foldA, forA, forA_, forWithIndexA, fresh, freshFn, freshLabel, makeCx, renderBuffer, renderChunks, renderFnBody, runCodegen, takeFn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- The default fresh state used across these cases.
run :: forall a. Codegen a -> Tuple a Ctx
run = runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true })

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
