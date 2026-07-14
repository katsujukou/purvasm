-- | The B2 native-build end-to-end gate: compile a module **from its CoreFn** (not a hand-built ANF)
-- | through the neutral `Purvasm.Compiler.build` driver + the LLVM `Backend` (`llvmBackend`), and assert
-- | the emitted module/entry objects.
-- |
-- | Three checks, each locking a distinct backend contract:
-- |
-- |  * **`slice1` byte-identity** — the pure-value baseline: `Slice1`'s single, fully-reachable binding
-- |    (`identInt`) emits module + init/entry objects byte-identical to boot's `--no-opt` `.ll`
-- |    (fixtures under `test/fixtures/slice1/`). B2's "emit every module binding" and boot B1's DCE'd
-- |    output coincide here, so byte-identity is a clean gate. Exercises the backend-private `DictElim`
-- |    bridge (ADR-0086 Addendum), since the driver runs no `DictElim` under `--no-opt`.
-- |  * **structural-foreign materialisation** (`structural-ref` fixture) — a module declaring the
-- |    structural foreign `Effect.Ref._new` must emit its **guest-term body** (the one-cell mutable array,
-- |    ADR-0071 §6 / ADR-0072 §9) as a synthesised gdef, **before** the module's own decls (boot's
-- |    key-sorted `foreign_groups` prepended to `module_reached`). Locks that `synthForeignGdefs` resolves
-- |    the *structural* rung (not only intrinsics) and the foreign-first ordering.
-- |  * **bare `Effect` entry** — an `isEffect` build hands `pv_run_effect` the entry's **bare** root value
-- |    (which itself applies it to unit, ADR-0067 §2); the entry expression must **not** pre-apply it to
-- |    unit (that would double-perform). Locks `entryExprOf`'s bare-reference form.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Driver where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Module (Module)
import Purvasm.Compiler (ForeignSigMap, LoadResult(..), Options, build, defaultHooks)
import Purvasm.Compiler.Backend.LLVM.Driver (llvmBackend)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

parseModule :: String -> Either String Module
parseModule src = jsonParser src >>= (lmap printJsonDecodeError <<< decodeModule)

-- | Build a single-module program through the neutral driver + LLVM backend, capturing the emitted module
-- | object IR(s) and the init/entry object IR. `foreignSigs` stubs the ADR-0090 FSR channel: the structural
-- | fixtures pass `Map.empty` (`Effect.Ref._new` is a *structural* term the resolver materialises without a
-- | shape), while the native-leaf fixture supplies its leaf's reconstructed `ForeignShape`.
buildIR
  :: { name :: String, mod :: Module, foreignSigs :: ForeignSigMap }
  -> Options
  -> Aff { mods :: Array String, entry :: Maybe String }
buildIR fixture opts = do
  modBuf <- liftEffect (Ref.new [])
  entryBuf <- liftEffect (Ref.new Nothing)
  let
    backend = llvmBackend { isEffect: opts.isEffect, heapWords: 1048576, debug: false }
    action =
      { workdir: "."
      , maxOptimizeIter: 1
      , loadModule: \name -> pure (if name == fixture.name then Loaded { path: fixture.name, mod: fixture.mod } else Missing)
      , foreignSigsOf: \_ -> pure (Right fixture.foreignSigs)
      , emitFile: \artifact -> Ref.modify_ (\a -> Array.snoc a artifact.backendIR) modBuf $> "mod.ll"
      , emitEntry: \o -> Ref.write (Just o) entryBuf $> "entry.ll"
      , hooks: defaultHooks
      }
  _ <- liftEffect (build backend action opts)
  mods <- liftEffect (Ref.read modBuf)
  entry <- liftEffect (Ref.read entryBuf)
  pure { mods, entry }

-- | The number of (non-overlapping) occurrences of `needle` in `hay`.
countOccurrences :: String -> String -> Int
countOccurrences needle hay = Array.length (String.split (Pattern needle) hay) - 1

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Driver" do
  describe "llvmBackend via Purvasm.Compiler.build (B2 per-module, CoreFn → .ll)" do
    it "compiles Slice1 from CoreFn to byte-identical module + entry objects" do
      src <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/corefn.json")
      expectedMod <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/mod_0.ll")
      expectedEntry <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/entry.ll")
      case parseModule src of
        Left err -> fail ("CoreFn decode failed: " <> err)
        Right mod -> do
          -- `Slice1` imports `Prim`, reported `Missing` (skipped), so the closure is exactly `[Slice1]`.
          -- The entry `Slice1.identInt` (a bare `--value` entry); `isEffect: false` / `opt: false` matches
          -- boot's `--no-opt` reference.
          out <- buildIR { name: "Slice1", mod, foreignSigs: Map.empty }
            { entryModule: "Slice1", entryName: "identInt", isEffect: false, opt: false }
          out.mods `shouldEqual` [ expectedMod ]
          out.entry `shouldEqual` Just expectedEntry

    -- P2-1: the structural rung must be materialised as a guest-term gdef (reverting `synthForeignGdefs`
    -- to the intrinsic-only rung leaves `Effect.Ref._new` unsynthesised, so every assertion below fails).
    it "materialises the structural foreign Effect.Ref._new as a one-cell-array gdef, foreign-first" do
      src <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/structural-ref/corefn.json")
      case parseModule src of
        Left err -> fail ("CoreFn decode failed: " <> err)
        Right mod -> do
          out <- buildIR { name: "Effect.Ref", mod, foreignSigs: Map.empty }
            { entryModule: "Effect.Ref", entryName: "newRef", isEffect: false, opt: false }
          let ir = fromMaybe "" (Array.head out.mods)
          -- (a) `_new` is synthesised at all — its root-handle global exists.
          unless (String.contains (Pattern "@pv_g_Effect_2eRef_2e_5fnew$root = global i64 0") ir)
            (fail ("expected a synthesised @…_5fnew$root global; module IR:\n" <> ir))
          -- (b) the synthesised body is the *resolver's* arity-2 `refNew` (the fixture's declared type is
          --     arity-1 `a -> a`, so an arity-2 `$d` proves the body came from the structural resolver,
          --     not the fixture) …
          unless (String.contains (Pattern "define internal tailcc i64 @pv_g_Effect_2eRef_2e_5fnew$d(ptr %ctx, i64 %env, i64 %p0, i64 %p1) {") ir)
            (fail ("expected the arity-2 refNew direct entry; module IR:\n" <> ir))
          -- … and it builds the one-cell mutable array (`NewArray 1` → the immediate `3`, then `SetArray`).
          unless (String.contains (Pattern "call i64 @pv_prim_new_array(ptr %ctx, i64 3)") ir)
            (fail ("expected the one-cell array allocation in refNew; module IR:\n" <> ir))
          -- (c) foreign-first in the **module object**: `_new` (a resolved foreign) precedes the
          --     module's own `newRef` decl (root-global emission order).
          case String.indexOf (Pattern "@pv_g_Effect_2eRef_2e_5fnew$root") ir, String.indexOf (Pattern "@pv_g_Effect_2eRef_2enewRef$root") ir of
            Just i, Just j
              | i < j -> pure unit
              | otherwise -> fail ("foreign _new$root (" <> show i <> ") must precede the newRef$root decl (" <> show j <> ")")
            _, _ -> fail ("both _new$root and newRef$root globals must be present; module IR:\n" <> ir)
          -- (d) foreign-first at **init time**: `pv_init_all` (in the entry object) calls `_new$init`
          --     before `newRef$init`. `newRef` is a bare alias CAF (`newRef = _new`), so its init reads
          --     `_new$root` — the order is *load-bearing* (a reversed `allForeigns <> allDecls` in
          --     `lowerEntry` would init `newRef` off an uninitialised `_new$root`), not just a byte order.
          let entryIr = fromMaybe "" out.entry
          case String.indexOf (Pattern "call void @pv_g_Effect_2eRef_2e_5fnew$init(ptr %ctx)") entryIr, String.indexOf (Pattern "call void @pv_g_Effect_2eRef_2enewRef$init(ptr %ctx)") entryIr of
            Just i, Just j
              | i < j -> pure unit
              | otherwise -> fail ("pv_init_all must call _new$init (" <> show i <> ") before newRef$init (" <> show j <> "); entry IR:\n" <> entryIr)
            _, _ -> fail ("both _new$init and newRef$init calls must be present in pv_init_all; entry IR:\n" <> entryIr)

    -- P2-2: an `isEffect` entry hands `pv_run_effect` the *bare* entry thunk (run_effect applies unit
    -- itself). Reverting `entryExprOf` to the unit-applied form makes the stub `pv_apply` the entry before
    -- `pv_run_effect`, double-performing.
    it "an isEffect entry runs pv_run_effect once on the bare root (no pre-apply)" do
      src <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/corefn.json")
      case parseModule src of
        Left err -> fail ("CoreFn decode failed: " <> err)
        Right mod -> do
          out <- buildIR { name: "Slice1", mod, foreignSigs: Map.empty }
            { entryModule: "Slice1", entryName: "identInt", isEffect: true, opt: false }
          let entryIr = fromMaybe "" out.entry
          -- exactly one performance …
          countOccurrences "call i64 @pv_run_effect(" entryIr `shouldEqual` 1
          -- … on the bare entry value: the `@main` stub loads `identInt$root` and hands *that* to
          -- `pv_run_effect`; it never applies the entry to unit first. The reverted (unit-applied) form
          -- evaluates `identInt unit`, emitting a pre-apply before `pv_run_effect` — a saturated direct
          -- call (`call tailcc …identInt$d`) for a function entry, or a generic `call …@pv_apply` — so the
          -- absence of *both* call forms discriminates. (`declare`s in the ABI header are not calls.)
          when (String.contains (Pattern "call tailcc i64 @pv_g_Slice1_2eidentInt$d(") entryIr)
            (fail ("the entry must not pre-apply the bare Effect thunk (found a direct entry call); entry IR:\n" <> entryIr))
          when (String.contains (Pattern "call i64 @pv_apply(") entryIr)
            (fail ("the entry must not pre-apply the bare Effect thunk (found a pv_apply call); entry IR:\n" <> entryIr))

    -- A nullary `Effect` native leaf (FSR `arity 0, retVsat` — e.g. `argvImpl :: Effect (Array String)`)
    -- must lower to an **arity-1** leaf closure (it *is* the effect thunk; boot's `Ffi.foreign_arity`).
    -- Reverting `leafClosureArity` to the raw `s.arity` builds it at arity 0, so `run`'s unit application
    -- over-applies the already-fired leaf onto its own result — the `argvImpl` "not callable (kind Array)"
    -- heap corruption that blocked `bench-effect-ref`.
    it "builds a nullary-Effect native leaf closure at arity 1 (not the raw FSR arity 0)" do
      src <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/nullary-effect-leaf/corefn.json")
      case parseModule src of
        Left err -> fail ("CoreFn decode failed: " <> err)
        Right mod -> do
          -- The test supplies the FSR shape a nullary `Effect` leaf reconstructs to (`arity 0, retVsat`).
          let sigs = Map.fromFoldable [ Tuple "Test.Ffi.getThing" { arity: 0, vsat: false, retVsat: true } ]
          out <- buildIR { name: "Test.Ffi", mod, foreignSigs: sigs }
            { entryModule: "Test.Ffi", entryName: "answer", isEffect: false, opt: false }
          let ir = fromMaybe "" (Array.head out.mods)
          -- the leaf is referenced by its link-time `@pvf_` symbol …
          unless (String.contains (Pattern "ptrtoint ptr @pvf_Test_2eFfi_2egetThing to i64") ir)
            (fail ("expected the @pvf_ leaf reference; module IR:\n" <> ir))
          -- … wrapped in exactly one no-capture closure (the sole `make_closure` here), at **arity 1**
          -- (the trailing `i64 1` is the immediate-unit env sentinel). The revert emits `i32 0` here.
          countOccurrences "call i64 @pv_make_closure(" ir `shouldEqual` 1
          unless (String.contains (Pattern ", i32 1, i64 1)") ir)
            (fail ("the nullary-Effect leaf closure must be arity 1; module IR:\n" <> ir))
          when (String.contains (Pattern ", i32 0, i64 1)") ir)
            (fail ("the nullary-Effect leaf closure must not be arity 0 (over-applies on run); module IR:\n" <> ir))
