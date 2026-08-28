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

import Purvasm.Compiler.Backend.LLVM.ForeignRef (ForeignCallMode(..), ForeignClosureMode(..))

import Data.Array as Array
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Effect.Aff (Aff)
import Purvasm.Compiler.Backend.LLVM.Mangle (mangle)
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions)
import Purvasm.Compiler.Backend.LLVM.CallClass (callEventClass)
import Purvasm.Compiler.Backend.LLVM.Program (entryLl, moduleLl, moduleLlWithEvents)
import Purvasm.Compiler.Backend.LLVM.Types (Gdef(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr, CExprF(..), Expr, ExprF(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

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
          , defined: Set.empty
          , profileApply: false
          , byNeed: true
          , foreignCall: DirectApplyAndTail
          , foreignClosure: Hoisted
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
          , defined: Set.empty
          , profileApply: false
          , byNeed: true
          , foreignCall: DirectApplyAndTail
          , foreignClosure: Hoisted
          }
        gdefs = [ Gcaf cloneKey (Ret (CAtom (AtomLit (LInt 1)))) ]
        entry = Ret (CAtom (AtomVar cloneKey)) -- the entry references the clone directly
        ir = entryLl opts false 1048576 gdefs entry
      -- reachability saw the clone (seed = fv(entry) ∩ gkeys), so pv_init_all calls its `$init`. Before the
      -- fix, `reachableGdefs Set.empty` yields an empty seed → the clone's `$init` is never called (and the
      -- entry stub's `readVar` crashes first).
      ir `shouldContain` ("call void @" <> mangle cloneKey <> "$init(ptr %ctx)")

  -- --- ADR-0109 slice A: the hoisted leaf-closure cells --------------------------------------------

  -- The cells are program-wide and DEFINED in the entry object, initialised once by
  -- `@pv_fclo_init` before every gdef init. What makes the ownership load-bearing is the LINK
  -- CONTRACT: a cell's initialiser is the only live reference to `@pvf_<key>`, so initialising a
  -- leaf that only unreachable code mentions would demand a provider the tree has never needed
  -- (found live: `Control.Extend.arrayExtend`, referenced by a dead instance and stripped by
  -- `-Wl,-dead_strip`, failed the link the moment a per-object init referenced it).
  describe "the hoisted leaf-closure cells (ADR-0109 §2.2)" do
    let
      leafOpts :: MakeCxOptions
      leafOpts =
        { gkeys: Set.fromFoldable [ "M.used", "M.dead" ]
        , xfns: Map.empty
        , foreignArity: Map.fromFoldable [ Tuple "M.live" 1, Tuple "M.deadLeaf" 1 ]
        , inlineAbi: true
        , defined: Set.empty
        , profileApply: false
        , byNeed: true
        , foreignCall: DirectApplyAndTail
        , foreignClosure: Hoisted
        }
      -- `M.used` is reachable from the entry and calls the live leaf; `M.dead` is not reachable and
      -- is the only mention of the dead one.
      leafGdefs =
        [ Gcaf "M.used" (Ret (CApp unit (AtomForeign "M.live") [ AtomLit (LInt 1) ]))
        , Gcaf "M.dead" (Ret (CApp unit (AtomForeign "M.deadLeaf") [ AtomLit (LInt 1) ]))
        ]
      leafEntry = Ret (CAtom (AtomVar "M.used"))

    it "defines and initialises a cell for a leaf the REACHABLE program can execute" do
      let ir = entryLl leafOpts false 1048576 leafGdefs leafEntry
      ir `shouldContain` "@pvf_M_2elive$fclo = global i64 0"
      ir `shouldContain` "define void @pv_fclo_init(ptr %ctx) {"
      ir `shouldContain` "ptrtoint ptr @pvf_M_2elive to i64"
      ir `shouldContain` ", ptr @pvf_M_2elive$fclo"
      -- and it runs FIRST: a gdef init may call code that reads a cell, so any later position
      -- would read the `0` sentinel.
      ir `shouldContain` "define void @pv_init_all(ptr %ctx) {\nentry:\n  call void @pv_fclo_init(ptr %ctx)\n"

    it "does NOT reference a leaf only unreachable code mentions (the link contract)" do
      let ir = entryLl leafOpts false 1048576 leafGdefs leafEntry
      -- The dead gdef's own object still emits its reference and its `declare`; both are stripped
      -- with the code that holds them. What must never happen is the ENTRY pulling the symbol in:
      -- that would turn "referenced anywhere" into "must have a provider", a strictly stronger
      -- contract than the tree satisfies.
      ir `shouldNotContain` "@pvf_M_2edeadLeaf"

    it "a module object DECLARES the cells it reads and defines none" do
      let
        ir = moduleLl (leafOpts { defined = Set.fromFoldable [ "M.used" ] })
          (Set.fromFoldable [ "M.used" ])
          [ Gcaf "M.used" (Ret (CApp unit (AtomForeign "M.live") [ AtomLit (LInt 1) ])) ]
      ir `shouldContain` "@pvf_M_2elive$fclo = external global i64"
      ir `shouldNotContain` "@pvf_M_2elive$fclo = global i64 0"
      -- one declare, and the closure is NOT rebuilt here (slice A's whole point)
      ir `shouldContain` "declare i64 @pvf_M_2elive(ptr, i64, ptr, i64)"
      ir `shouldNotContain` "call i64 @pv_make_closure"
      ir `shouldNotContain` "ptrtoint ptr @pvf_M_2elive"

    -- The §5.2 COUNTERFACTUAL leg. "Off" must be the pre-slice-A program, not an approximation of
    -- it: an A/B whose off leg is a third variant measures nothing about the change.
    it "PerUse restores the pre-slice-A lowering exactly (no cell, no init, closure per reference)" do
      let
        perUse = leafOpts { foreignClosure = PerUse }
        entryIr = entryLl perUse false 1048576 leafGdefs leafEntry
        modIr = moduleLl (perUse { defined = Set.fromFoldable [ "M.used" ] })
          (Set.fromFoldable [ "M.used" ])
          [ Gcaf "M.used" (Ret (CApp unit (AtomForeign "M.live") [ AtomLit (LInt 1) ])) ]
      -- the reference builds its own closure again …
      modIr `shouldContain` "ptrtoint ptr @pvf_M_2elive to i64"
      modIr `shouldContain` "call i64 @pv_make_closure"
      -- … and NOTHING of the hoisted machinery survives, in either object
      modIr `shouldNotContain` "$fclo"
      entryIr `shouldNotContain` "$fclo"
      entryIr `shouldNotContain` "@pv_fclo_init"
      -- the ordinary leaf declare is kept: the reference still needs its symbol
      modIr `shouldContain` "declare i64 @pvf_M_2elive(ptr, i64, ptr, i64)"

    it "the two legs differ only in the closure strategy: same call events, same dispatch text" do
      let
        gdefs = [ Gcaf "M.used" (Ret (CApp unit (AtomForeign "M.live") [ AtomLit (LInt 1) ])) ]
        defined = Set.fromFoldable [ "M.used" ]
        out mode = moduleLlWithEvents (leafOpts { foreignClosure = mode, defined = defined }) defined gdefs
        hoisted = out Hoisted
        perUse = out PerUse
        applies ir = Array.length (String.split (Pattern "call i64 @pv_apply(") ir) - 1
      -- the CLASSIFICATION is leg-invariant (ADR-0109 §1.2's property, here for slice A): the legs
      -- differ in how the callee is materialised, never in what the call site was decided to be.
      map callEventClass hoisted.events `shouldEqual` map callEventClass perUse.events
      applies hoisted.ir `shouldEqual` applies perUse.ir
