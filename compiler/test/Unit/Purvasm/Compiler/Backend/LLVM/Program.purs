-- | Regression gate for the codegen `gkeys` set (`moduleLl`/`entryLl`): the optimiser's Specialize pass
-- | (ADR-0089) materialises caller-homed `$spec$` clones as new module-local top-level gdefs *during*
-- | optimisation, so they are absent from the pre-optimisation whole-program `gkeys` derived in
-- | `Driver.context`. The emitters must fold each object's own post-optimiser gdef keys (`defined`) into
-- | `gkeys`, else `readVar` crashes on a clone reference as "unbound variable" (the native self-host `--opt`
-- | failure at `Foreign.Object.$spec$…fromFoldable…ordString`).
-- |
-- | These drive the pure `Gdef` → `.ll` emitters directly with a `defined ⊄ gkeys` mismatch (what
-- | Specialize produces), which the CoreFn-driven `Driver` E2E cannot stage without a real
-- | specialisation-triggering fixture. Two invariants: (1) a clone reference resolves to a **local**
-- | `$root` load and is **not** declared `external` (it is defined here); (2) a genuine cross-module
-- | reference (in `gkeys`, not in `defined`) stays `external` — the fold must not over-absorb it.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Program where

import Prelude

import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Effect.Aff (Aff)
import Purvasm.Compiler.Backend.LLVM.Mangle (mangle)
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions)
import Purvasm.Compiler.Backend.LLVM.Program (entryLl, moduleLl)
import Purvasm.Compiler.Backend.LLVM.Types (Gdef(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

-- | The clone key stands for a Specialize `$spec$` clone: present in `defined` (materialised during opt),
-- | absent from `gkeys` (which was built pre-opt).
cloneKey :: String
cloneKey = "M.clone"

-- | Assert `hay` contains `needle`, else fail with the full IR for triage.
shouldContain :: String -> String -> Aff Unit
shouldContain hay needle =
  unless (String.contains (Pattern needle) hay)
    (fail ("expected to find `" <> needle <> "`; IR:\n" <> hay))

-- | Assert `hay` does not contain `needle`, else fail with the full IR for triage.
shouldNotContain :: String -> String -> Aff Unit
shouldNotContain hay needle =
  when (String.contains (Pattern needle) hay)
    (fail ("expected NOT to find `" <> needle <> "`; IR:\n" <> hay))

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Program" do
  describe "moduleLl folds `defined` into codegen gkeys (Specialize `$spec$` clones)" do
    it "resolves a module-local clone reference to a local $root load (not a crash, not external)" do
      let
        -- pre-opt whole-program globals: the clone is absent; `Other.ext` is another module's export.
        opts :: MakeCxOptions
        opts =
          { gkeys: Set.fromFoldable [ "M.user", "M.userExt", "Other.ext" ]
          , xfns: Map.empty
          , foreignArity: Map.empty
          , inlineAbi: true
          }
        -- post-opt object keys: the clone is now materialised here; `Other.ext` is *not* defined here.
        defined = Set.fromFoldable [ cloneKey, "M.user", "M.userExt" ]
        gdefs =
          [ Gcaf cloneKey (Ret (CAtom (AtomLit (LInt 1))))
          , Gcaf "M.user" (Ret (CAtom (AtomVar cloneKey))) -- references the local clone
          , Gcaf "M.userExt" (Ret (CAtom (AtomVar "Other.ext"))) -- references a genuine external
          ]
        ir = moduleLl opts defined gdefs
      -- (1) the clone reference took the gkeys branch → a local root load (before the fix: `unsafeCrashWith`).
      ir `shouldContain` ("load i64, ptr @" <> mangle cloneKey <> "$root")
      -- (2) the clone is defined here, so it is emitted as a local global and never declared `external`.
      ir `shouldContain` ("@" <> mangle cloneKey <> "$root = global i64 0")
      ir `shouldNotContain` ("@" <> mangle cloneKey <> "$root = external global i64")
      -- (3) the fold does not over-absorb: a genuine cross-module reference stays `external`.
      ir `shouldContain` ("@" <> mangle "Other.ext" <> "$root = external global i64")

  describe "entryLl folds every gdef key into gkeys (reachability + readVar)" do
    it "includes a clone referenced by the entry in pv_init_all (else its $root stays the 0 sentinel)" do
      let
        opts :: MakeCxOptions
        opts =
          { gkeys: Set.empty -- pre-opt: the clone did not exist
          , xfns: Map.empty
          , foreignArity: Map.empty
          , inlineAbi: true
          }
        gdefs = [ Gcaf cloneKey (Ret (CAtom (AtomLit (LInt 1)))) ]
        entry = Ret (CAtom (AtomVar cloneKey)) -- the entry references the clone directly
        ir = entryLl opts false 1048576 gdefs entry
      -- reachability saw the clone (seed = fv(entry) ∩ gkeys), so pv_init_all calls its `$init`. Before the
      -- fix, `reachableGdefs Set.empty` yields an empty seed → the clone's `$init` is never called (and the
      -- entry stub's `readVar` crashes first).
      ir `shouldContain` ("call void @" <> mangle cloneKey <> "$init(ptr %ctx)")
