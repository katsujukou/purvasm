-- | ADR-0108 §1: the call classification. Two invariants are load-bearing and neither is
-- | type-enforceable:
-- |
-- | * **every leaf of `directTarget`'s tree is reached and named** — the reasons rank the work of a
-- |   whole optimisation track, so a leaf that silently collapses into a neighbour's bucket
-- |   mis-ranks it. The matrix below drives one call through each leaf, including the two that only
-- |   the ownership input can separate (`own-object-not-fn` vs `dep-no-direct-fact`);
-- | * **a self-call shortcut that does not apply is NOT an outcome** — it falls through and can
-- |   still resolve directly, which is why there is no "self shape" reason. That fall-through is
-- |   tested explicitly, because a priority-ordered classifier would get it wrong.
-- |
-- | The event side is checked against the REAL emitter: the same gdefs are emitted, and the
-- | recorded events must agree with what the `.ll` contains, per call form. That is ADR-0108 §2's
-- | accounting identity at unit scale — the census will assert the same thing per object.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.CallClass where

import Prelude

import Control.Monad.State.Class (modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Purvasm.Compiler.Backend.LLVM.CallClass (CallTarget(..), Form(..), callForm, missReasonName, AllocSite(..), CallClass(..), CallEvent(..), MissReason(..), allocSiteName, allocSiteSlot, allocSites, callClassName, callClasses, callEventClass, profileSlot, profileSlotNames)
import Purvasm.Compiler.Backend.LLVM.Emit (directTarget)
import Purvasm.Compiler.Backend.LLVM.ForeignRef (ForeignCallMode(..), ForeignClosureMode(..), ForeignRef, refArity, refKey)
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions, foreignRef, makeCx, mintParam, runCodegen)
import Purvasm.Compiler.Backend.LLVM.Program (entryLlWithEvents, gdefKeys, moduleLlWithEvents)
import Purvasm.Compiler.Backend.LLVM.Types (BindingV(..), EnvSrc(..), FnInfo, Gdef(..), bindDirectFnVar, bindDirectVar)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

fnInfo :: String -> Int -> FnInfo
fnInfo dsym arity = { dsym, arity, src: SSentinel }

-- | A codegen context with the pieces `directTarget` consults. `defined` is the ADR-0108 ownership
-- | input; `gfns` is set through the state because the emitter registers it while emitting.
opts :: { gkeys :: Array String, xfns :: Array (Tuple String FnInfo), defined :: Array String } -> MakeCxOptions
opts o =
  { gkeys: Set.fromFoldable o.gkeys
  , xfns: Map.fromFoldable o.xfns
  -- ADR-0109 §1.2: a foreign callee now RESOLVES here — `directTarget` mints its `ForeignRef`
  -- through the one safe producer, which reads this map (and crashes without it, ADR-0090). The
  -- fixture leaves are `M.leaf` (arity 1) and `M.two` (arity 2), so both the saturated and the
  -- unsaturated rows below have a fact to be classified against.
  , foreignArity: Map.fromFoldable [ Tuple "M.leaf" 1, Tuple "M.two" 2 ]
  , inlineAbi: true
  , defined: Set.fromFoldable o.defined
  , profileApply: false
  , byNeed: true
  , foreignCall: DirectApplyOnly
  , foreignClosure: Hoisted
  }

-- | Classify one call in a context, with `gfns`/`selfCtx` installed as the emitter would have.
type ClassifyCfg =
  { gkeys :: Array String
  , gfns :: Array (Tuple String FnInfo)
  , xfns :: Array (Tuple String FnInfo)
  , defined :: Array String
  , self :: Maybe { name :: String, captureHandle :: Maybe String, fnInfo :: FnInfo }
  -- | when set, `localFn` is bound in the environment as a known local lambda
  , localFn :: Maybe FnInfo
  }

classify :: ClassifyCfg -> Atom -> Int -> CallTarget
classify cfg callee nargs = fst $ runCodegen (makeCx (opts { gkeys: cfg.gkeys, xfns: cfg.xfns, defined: cfg.defined })) do
  modify_ \c -> c { gfns = Map.fromFoldable cfg.gfns }
  -- the self context's env word: a parameter token stands in for the real binding.
  for_ cfg.self \s -> do
    envw <- mintParam 0
    modify_ \c -> c
      { selfCtx = Just
          { name: s.name, captureHandle: s.captureHandle, envBind: DirectV envw, fnInfo: s.fnInfo }
      }
  v <- mintParam 1
  let env0 = bindDirectVar Nil "someLocal" v
  env <- case cfg.localFn of
    Nothing -> pure env0
    Just info -> do
      fv <- mintParam 2
      pure (bindDirectFnVar env0 "localFn" fv info)
  directTarget env callee nargs

-- | Occurrences of a fixed needle.
countOf :: String -> String -> Int
countOf needle hay = Array.length (String.split (Pattern needle) hay) - 1

base :: ClassifyCfg
base = { gkeys: [], gfns: [], xfns: [], defined: [], self: Nothing, localFn: Nothing }

-- | A leaf reference for the event samples. Minted through `foreignRef` — the only safe producer —
-- | in a context that carries its arity fact, exactly as the emitter would.
leafRef :: ForeignRef
leafRef = fst (runCodegen (makeCx (opts { gkeys: [], xfns: [], defined: [] }) { foreignArity = Map.fromFoldable [ Tuple "M.leaf" 1 ] }) (foreignRef "M.leaf"))

-- | The `fchk`-style counters for the emitted `.ll`, per ADR-0108 §2's call forms.
emittedForms :: Array Gdef -> { pvApply :: Int, pvTailcall :: Int, musttail :: Int, guestDirect :: Int, pvfDirect :: Int }
emittedForms gdefs =
  let
    keys = Set.fromFoldable (gdefs >>= gdefKeys)
    ir =
      ( moduleLlWithEvents
          { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, profileApply: false, byNeed: true, foreignCall: DirectApplyOnly, foreignClosure: Hoisted }
          keys
          gdefs
      ).ir
    -- CALL sites only: `declare i64 @pv_apply(…)` and the `define … tailcc` headers carry the same
    -- symbols, so the needles include the call keyword (a `declare`-matching needle is how this
    -- test first reported a constant +1 in every column).
    occurrences needle = Array.length (String.split (Pattern needle) ir) - 1
  in
    { pvfDirect: occurrences "= call i64 @pvf_"
    , pvApply: occurrences "call i64 @pv_apply("
    , pvTailcall: occurrences "call void @pv_tailcall("
    , musttail: occurrences "musttail call"
    -- a `musttail call tailcc i64 @…` line matches BOTH needles, so the musttail sites are
    -- subtracted out: they are their own column.
    , guestDirect: occurrences "call tailcc i64 @" - occurrences "musttail call tailcc i64 @"
    }

-- | The recorded events for the same gdefs, counted by accounting column.
recordedForms :: Array Gdef -> { pvApply :: Int, pvTailcall :: Int, musttail :: Int, guestDirect :: Int, pvfDirect :: Int }
recordedForms gdefs =
  let
    keys = Set.fromFoldable (gdefs >>= gdefKeys)
    events =
      ( moduleLlWithEvents
          { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, profileApply: false, byNeed: true, foreignCall: DirectApplyOnly, foreignClosure: Hoisted }
          keys
          gdefs
      ).events
    count cls = Array.length (Array.filter (\e -> callEventClass e == cls) events)
  in
    { pvfDirect: count CForeignDirectApply + count CForeignDirectTail
    -- the DEFERRED forms lower to the generic dispatch, so they are accounted in those columns
    , pvApply: count CGenericApply + count CStructuralApply + count CForeignDeferredApply
    , pvTailcall: count CGenericTail + count CForeignDeferredTail
    , musttail: count CDirectMusttail
    , guestDirect: count CDirectNonTail + count CWrapperEntry
    }

-- | The events an object's emission recorded, for the tests that inspect them directly.
eventsOf :: Array Gdef -> Array CallEvent
eventsOf gdefs =
  let
    keys = Set.fromFoldable (gdefs >>= gdefKeys)
  in
    ( moduleLlWithEvents
        { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, profileApply: false, byNeed: true, foreignCall: DirectApplyOnly, foreignClosure: Hoisted }
        keys
        gdefs
    ).events

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.CallClass" do
  describe "directTarget names every leaf of its decision tree" do
    it "a non-variable callee, split by which atom it is (ADR-0108 §4)" do
      -- the split is TOTAL: ANF has three atoms and the third is the variable case above.
      -- ADR-0109 §1.2: the SATURATED leaf is a terminal outcome of the classifier, not a miss …
      case classify base (AtomForeign "M.leaf") 1 of
        ForeignTarget ref -> do
          refKey ref `shouldEqual` "M.leaf"
          refArity ref `shouldEqual` 1
        other -> fail ("expected a ForeignTarget, got " <> show other)
      -- … and `callee-foreign` narrows to exactly the arity disagreement, in both A/B legs.
      classify base (AtomForeign "M.two") 1 `shouldEqual` GenericTarget MissCalleeForeign
      classify base (AtomForeign "M.leaf") 2 `shouldEqual` GenericTarget MissCalleeForeign
      classify base (int 1) 1 `shouldEqual` GenericTarget MissCalleeLiteral

    it "a local binding with no known-function fact" do
      classify base (var "someLocal") 1 `shouldEqual` GenericTarget MissLocalUnknownFn

    it "a local known function at the wrong arity — and the right one" do
      let cfg = base { localFn = Just (fnInfo "fn_1$d" 2) }
      classify cfg (var "localFn") 1 `shouldEqual` GenericTarget MissArityLocal
      classify cfg (var "localFn") 2 `shouldEqual` GuestTarget (fnInfo "fn_1$d" 2)

    it "a variable that is neither a local binding nor a known global" do
      classify base (var "nope") 1 `shouldEqual` GenericTarget MissUnknownKey

    it "an own-object function fact at the wrong arity (never falls through to the surface)" do
      classify
        (base { gkeys = [ "M.f" ], gfns = [ Tuple "M.f" (fnInfo "M.f$d" 2) ], defined = [ "M.f" ] })
        (var "M.f")
        1 `shouldEqual` GenericTarget MissArityOwnModule
      -- … and the saturated call is direct
      classify
        (base { gkeys = [ "M.f" ], gfns = [ Tuple "M.f" (fnInfo "M.f$d" 2) ], defined = [ "M.f" ] })
        (var "M.f")
        2 `shouldEqual` GuestTarget (fnInfo "M.f$d" 2)

    -- The two leaves that ONLY the ownership input separates: same key, same absent facts, and the
    -- answer differs purely by whether this object defines it.
    it "an own-object key that is not a function vs a dependency with no published fact" do
      let cfg = base { gkeys = [ "M.c" ] }
      classify (cfg { defined = [ "M.c" ] }) (var "M.c") 1 `shouldEqual` GenericTarget MissOwnObjectNotFn
      classify (cfg { defined = [] }) (var "M.c") 1 `shouldEqual` GenericTarget MissDepNoDirectFact

    it "a published cross-module fact at the wrong arity — and the right one" do
      let cfg = base { gkeys = [ "Other.g" ], xfns = [ Tuple "Other.g" (fnInfo "Other.g$d" 3) ] }
      classify cfg (var "Other.g") 1 `shouldEqual` GenericTarget MissArityCrossModule
      classify cfg (var "Other.g") 3 `shouldEqual` GuestTarget (fnInfo "Other.g$d" 3)

  describe "the self-call shortcut is not a leaf (no MissSelfShape)" do
    it "falls through to the global fact when its arity does not match, and still goes direct" do
      let
        cfg = base
          { gkeys = [ "M.loop" ]
          , gfns = [ Tuple "M.loop" (fnInfo "M.loop$d" 1) ]
          , defined = [ "M.loop" ]
          , self = Just { name: "M.loop", captureHandle: Nothing, fnInfo: fnInfo "M.loop$d" 2 }
          }
      -- the self shortcut wants arity 2; this call has 1 argument, so it falls through to `gfns`,
      -- which DOES match — a priority-ordered classifier would have reported a miss here.
      classify cfg (var "M.loop") 1 `shouldEqual` GuestTarget (fnInfo "M.loop$d" 1)

    it "takes the shortcut when the shape does match" do
      let
        cfg = base
          { gkeys = [ "M.loop" ]
          , gfns = [ Tuple "M.loop" (fnInfo "M.loop$d" 1) ]
          , defined = [ "M.loop" ]
          , self = Just { name: "M.loop", captureHandle: Nothing, fnInfo: fnInfo "M.self$d" 2 }
          }
      classify cfg (var "M.loop") 2 `shouldEqual` GuestTarget (fnInfo "M.self$d" 2)

  -- ADR-0108 §2's identity, at unit scale: the events must account for what the emitter emitted,
  -- in the FORM it emitted it. `pv_apply` covers generic-apply AND structural-apply; the generic
  -- TAIL class is a `pv_tailcall` and appears in no `pv_apply` count; `guestDirect` covers direct
  -- call sites AND wrapper entries.
  describe "recorded events account for the emitted call forms" do
    let
      sibling = Gcaf "M.sibling" (Ret (CAtom (int 0)))
      matrix =
        -- NOTE both of these are generic TAIL calls: a `Gfun`'s `Ret (CApp …)` is in tail position,
        -- whatever precedes it. `generic-apply` (the non-tail `pv_apply`) is exercised by the
        -- `Gcaf` shape below, whose body is not a tail context.
        [ Tuple "generic tail (unknown callee, called directly)"
            (Gfun "M.f" [ "g", "x" ] (Ret (CApp (var "g") [ var "x" ])))
        , Tuple "generic tail (unknown callee, after a let)"
            (Gfun "M.f" [ "g", "x" ] (Let "r" (CPrim AddInt [ var "x", int 1 ]) (Ret (CApp (var "g") [ var "r" ]))))
        , Tuple "structural apply (unsaturated ctor with a supplied field)"
            (Gfun "M.f" [ "x" ] (Ret (CCtor "Pair" 2 [ var "x" ])))
        , Tuple "nullary unsaturated ctor (the builder alone — no apply)"
            (Gfun "M.f" [] (Ret (CCtor "Pair" 2 [])))
        , Tuple "a lambda and a call through it"
            (Gfun "M.f" [ "x" ] (Let "k" (CLam [ "y" ] (Ret (CAtom (var "y")))) (Ret (CApp (var "k") [ var "x" ]))))
        , Tuple "generic apply (a caf body is not a tail context)"
            (Gcaf "M.c" (Ret (CApp (var "M.sibling") [ int 1 ])))
        , Tuple "direct non-tail call to an own-object function"
            -- the call sits in a `Let` RHS, so it is NOT in tail position: `guestDirect` +
            -- `DirectNonTail`. Every other shape here reaches its direct target in tail position
            -- (a `musttail`), so without this one the `direct-nontail` column is never exercised.
            (Gfun "M.f" [ "x" ] (Let "r" (CApp (var "M.callee") [ var "x" ]) (Ret (CPrim AddInt [ var "r", int 1 ]))))
        ]
    for_ matrix \(Tuple label gdef) ->
      it label do
        -- `M.callee` is an own-object function, so the direct-call shapes above resolve.
        let gdefs = [ sibling, Gfun "M.callee" [ "y" ] (Ret (CAtom (var "y"))), gdef ]
        { label, forms: recordedForms gdefs } `shouldEqual` { label, forms: emittedForms gdefs }

    -- Labels in the matrix above claim a column each; these two check the claims, because a shape
    -- whose name says one column and whose emission takes another still passes the totals test.
    it "the caf shape really records GenericApply (a caf body is not a tail context)" do
      let
        gdefs = [ Gcaf "M.sibling" (Ret (CAtom (int 0))), Gcaf "M.c" (Ret (CApp (var "M.sibling") [ int 1 ])) ]
        classes = map callEventClass (eventsOf gdefs)
      Array.length (Array.filter (_ == CGenericApply) classes) `shouldEqual` 1
      Array.length (Array.filter (_ == CGenericTail) classes) `shouldEqual` 0

    it "a Gfun's Ret (CApp …) really records GenericTail (it IS a tail context)" do
      let
        gdefs = [ Gfun "M.f" [ "g", "x" ] (Ret (CApp (var "g") [ var "x" ])) ]
        classes = map callEventClass (eventsOf gdefs)
      Array.length (Array.filter (_ == CGenericTail) classes) `shouldEqual` 1
      Array.length (Array.filter (_ == CGenericApply) classes) `shouldEqual` 0

    it "the direct non-tail shape really records DirectNonTail (not a musttail)" do
      let
        gdefs =
          [ Gfun "M.callee" [ "y" ] (Ret (CAtom (var "y")))
          , Gfun "M.f" [ "x" ] (Let "r" (CApp (var "M.callee") [ var "x" ]) (Ret (CPrim AddInt [ var "r", int 1 ])))
          ]
        classes = map callEventClass (eventsOf gdefs)
      Array.length (Array.filter (_ == CDirectNonTail) classes) `shouldEqual` 1
      Array.length (Array.filter (_ == CDirectMusttail) classes) `shouldEqual` 0

  describe "the accounting columns are a closed enumeration" do
    it "callClasses lists every class exactly once" do
      -- 6 from ADR-0108 + the 4 ADR-0109 foreign classes (direct/deferred × apply/tail)
      Array.length callClasses `shouldEqual` 10
      Array.length (Array.nub callClasses) `shouldEqual` Array.length callClasses
      Array.nub (map callClassName callClasses) `shouldEqual` map callClassName callClasses

    it "every event shape maps into it" do
      let
        samples =
          [ DirectNonTail (fnInfo "d" 1)
          , DirectMusttail (fnInfo "d" 1)
          , GenericApply MissCalleeForeign
          , GenericTail MissCalleeForeign
          , StructuralApply
          , WrapperEntry
          , ForeignDirectApply leafRef
          , ForeignDirectTail leafRef
          , ForeignDeferredApply leafRef
          , ForeignDeferredTail leafRef
          ]
      Array.sort (map callEventClass samples) `shouldEqual` Array.sort callClasses

  -- The ownership override is the fix that keeps the entry object — the program's largest — from
  -- misclassifying every unpublished dependency function as its own. `directTarget`'s unit rows
  -- above prove the classification; this proves `entryLl` actually hands it the EMPTY set, even
  -- when its caller's options carry a non-empty one.
  describe "the entry object owns nothing (ADR-0108 §1)" do
    it "classifies a global callee as a dependency even when `defined` says otherwise" do
      let
        callee = "Dep.k"
        gdefs = [ Gfun "Dep.k" [ "y" ] (Ret (CAtom (var "y"))) ]
        -- deliberately hostile options: `defined` contains the callee, as a module object's would.
        opts' =
          { gkeys: Set.fromFoldable [ callee ]
          , xfns: Map.empty
          , foreignArity: Map.empty
          , inlineAbi: true
          , defined: Set.fromFoldable [ callee ]
          , profileApply: false
          , byNeed: true
          , foreignCall: DirectApplyOnly
          , foreignClosure: Hoisted
          }
        -- a call at the WRONG arity, so no direct fact matches and the leaf is an ownership question
        entry = Ret (CApp (var callee) [ int 1, int 2 ])
        events = (entryLlWithEvents opts' true 1024 gdefs entry).events
        reasonOf = case _ of
          GenericApply r -> Just r
          GenericTail r -> Just r
          _ -> Nothing
        reasons = Array.mapMaybe reasonOf events
      Array.filter (_ == MissDepNoDirectFact) reasons `shouldEqual` [ MissDepNoDirectFact ]
      Array.filter (_ == MissOwnObjectNotFn) reasons `shouldEqual` []

  -- ADR-0108 §3. The slot layout is an ABI: an instrumented program hands these names to the
  -- runtime, which sizes and labels its counters from them. What must hold is that the names and
  -- the indices are ONE mapping (the runtime is handed the names but computes nothing), and that
  -- the classes which cannot execute a dispatch get no slot at all.
  describe "the dynamic profile's slot space" do
    it "has a slot for every (generic form × executable reason), plus structural-apply" do
      -- 2 forms × 8 reasons (every MissReason except the unreachable `unknown-key`) + 1, then the
      -- ADR-0109 §5.1 allocation sites. `callee-literal` HAS a slot although it is expected to read
      -- zero: ADR-0108 §4 measures it rather than assuming it, and a class with no counter cannot
      -- be measured.
      Array.length profileSlotNames `shouldEqual` (17 + 4 + Array.length allocSites)
      Array.nub profileSlotNames `shouldEqual` profileSlotNames
      Array.filter (String.contains (Pattern "unknown-key")) profileSlotNames `shouldEqual` []

    it "maps each event to the slot whose NAME describes it (names and indices are one mapping)" do
      let
        named ev = profileSlot ev >>= \i -> Array.index profileSlotNames i
      named (GenericApply MissCalleeLiteral) `shouldEqual` Just "generic-apply/callee-literal"
      named (GenericApply MissLocalUnknownFn) `shouldEqual` Just "generic-apply/local-unknown-fn"
      named (GenericTail MissLocalUnknownFn) `shouldEqual` Just "generic-tail/local-unknown-fn"
      named (GenericApply MissArityCrossModule) `shouldEqual` Just "generic-apply/arity-cross-module"
      named StructuralApply `shouldEqual` Just "structural-apply"

    it "gives no slot to what cannot execute a dispatch" do
      -- direct calls are not dispatches; a wrapper entry is per function; `unknown-key` cannot be
      -- emitted at all (§1) — instrumenting it would reserve a counter pinned at zero.
      profileSlot (DirectNonTail (fnInfo "M.f$d" 1)) `shouldEqual` Nothing
      profileSlot (DirectMusttail (fnInfo "M.f$d" 1)) `shouldEqual` Nothing
      profileSlot WrapperEntry `shouldEqual` Nothing
      profileSlot (GenericApply MissUnknownKey) `shouldEqual` Nothing
      profileSlot (GenericTail MissUnknownKey) `shouldEqual` Nothing

    it "keeps every slot index inside the layout it declares" do
      let
        slots =
          Array.mapMaybe profileSlot
            [ GenericApply MissCalleeForeign, GenericTail MissArityLocal, StructuralApply ]
            <> map allocSiteSlot allocSites
      Array.filter (\i -> i < 0 || i >= Array.length profileSlotNames) slots `shouldEqual` []

  -- ADR-0108 §5 / ADR-0109 §5.1. The allocation sites share the registration and the bump ABI with
  -- the dispatch slots, and are kept identifiable by NAME: the §3 identities sum `generic-apply/`,
  -- `generic-tail/` and `structural-apply`, so a site row must be unable to enter any of those sums.
  describe "the allocation-site slots" do
    it "names each site under its own prefix, out of reach of the dispatch sums" do
      let
        names = map allocSiteName allocSites
      Array.filter (String.contains (Pattern "alloc/site/")) names `shouldEqual` names
      Array.filter (\n -> String.contains (Pattern "generic-apply/") n || String.contains (Pattern "generic-tail/") n) names
        `shouldEqual` []
      Array.filter (_ == callClassName CStructuralApply) names `shouldEqual` []
      allocSiteName SiteForeignMaterialise `shouldEqual` "alloc/site/foreign-materialise"

    it "maps each site to the slot whose NAME describes it, ABOVE every dispatch slot" do
      -- the dispatch slots keep the low indices: adding a site must not renumber one.
      let
        dispatch = Array.mapMaybe profileSlot
          [ GenericApply MissCalleeForeign, GenericTail MissArityLocal, StructuralApply ]
        maxDispatch = fromMaybe (-1) (Array.last (Array.sort dispatch))
      for_ allocSites \s -> do
        Array.index profileSlotNames (allocSiteSlot s) `shouldEqual` Just (allocSiteName s)
        (allocSiteSlot s > maxDispatch) `shouldEqual` true

  describe "instrumentation is opt-in and inert when off" do
    it "emits no profile symbols in a normal build" do
      let
        gdefs = [ Gfun "M.f" [ "g", "x" ] (Ret (CApp (var "g") [ var "x" ])) ]
        keys = Set.fromFoldable (gdefs >>= gdefKeys)
        irOf profileApply =
          ( moduleLlWithEvents
              { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, profileApply, byNeed: true, foreignCall: DirectApplyOnly, foreignClosure: Hoisted }
              keys
              gdefs
          ).ir
      String.contains (Pattern "pv_applyprofile") (irOf false) `shouldEqual` false
      String.contains (Pattern "call void @pv_applyprofile_bump") (irOf true) `shouldEqual` true
      -- the declares travel with the instrumentation, never with the shipped block
      String.contains (Pattern "declare void @pv_applyprofile_bump") (irOf false) `shouldEqual` false
      String.contains (Pattern "declare void @pv_applyprofile_bump") (irOf true) `shouldEqual` true

    -- ADR-0108 §3 counts DISPATCHES. A bump emitted before operand materialisation would count
    -- intentions instead: `evalAtoms`/`argBuffer` can force a by-need cell or allocate, and what
    -- runs between the bump and the dispatch is exactly the code that might not reach it. The
    -- contract is therefore positional, and positional contracts drift silently — so it is pinned
    -- on the emitted text, per instrumented dispatch form.
    it "emits each bump immediately before its dispatch, after operand materialisation" do
      let
        -- one generic apply (non-tail, `$r` forces the result), one generic tail, one structural
        -- apply (an unsaturated 2-ary constructor applied to one field).
        gdefs =
          [ Gfun "M.ap" [ "g", "x" ] (Let "$r" (CApp (var "g") [ var "x" ]) (Ret (CAtom (var "$r"))))
          , Gfun "M.tl" [ "g", "x" ] (Ret (CApp (var "g") [ var "x" ]))
          , Gfun "M.st" [ "x" ] (Ret (CCtor "M.Pair" 2 [ var "x" ]))
          ]
        keys = Set.fromFoldable (gdefs >>= gdefKeys)
        ir =
          ( moduleLlWithEvents
              { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, profileApply: true, byNeed: true, foreignCall: DirectApplyOnly, foreignClosure: Hoisted }
              keys
              gdefs
          ).ir
        -- Instruction lines only: `declare void @pv_applyprofile_bump(…)` matches a bare symbol
        -- needle, so the walk is anchored to the two-space instruction indent — the same caveat the
        -- census harness carries.
        lines = Array.filter (String.contains (Pattern "  ")) (String.split (Pattern "\n") ir)
        symbolOf l = fromMaybe l (Array.head (String.split (Pattern "(") (fromMaybe l (Array.last (String.split (Pattern "@") l)))))
        -- The FIRST call after each bump. Between the two there may be pure loads (the callee is
        -- re-read from its root, ADR-0105's verify-then-use), which cannot fail to reach the
        -- dispatch; what must not appear is another CALL — a force, an allocation or a
        -- materialisation that could divert or allocate between counting and dispatching.
        -- The drill (§4) puts a SECOND profile call between the slot bump and the dispatch, so the
        -- invariant is "no call other than a profile call intervenes" — the profile family is
        -- allowed to precede its own dispatch, nothing else is.
        nextCallAfter i =
          Array.findMap
            ( \l ->
                if String.contains (Pattern "call ") l && not (String.contains (Pattern "@pv_applyprofile_") l) then Just (symbolOf l)
                else Nothing
            )
            (Array.drop (i + 1) lines)
        followers =
          Array.mapMaybe
            ( \i -> case Array.index lines i of
                Just l | String.contains (Pattern "@pv_applyprofile_bump") l -> nextCallAfter i
                _ -> Nothing
            )
            (Array.range 0 (Array.length lines - 1))
      -- one bump per instrumented dispatch, and each one's next call IS its dispatch
      Array.length followers `shouldEqual` 3
      Array.sort (Array.nub followers) `shouldEqual` [ "pv_apply", "pv_tailcall" ]

    -- ADR-0108 §4. The drill answers "which foreign, at what arity status" — so what is pinned is
    -- the KEY's content, not merely that a call happened: a key that lost its arity status, or
    -- reported `known-match` for a partial application, would still reconcile perfectly against the
    -- slot counter while answering the question wrongly.
    it "drills a foreign callee by symbol, form and arity status — and nothing else" do
      let
        gdefs =
          [ Gfun "M.ap" [ "x" ] (Let "$r" (CApp (AtomForeign "M.leaf") [ var "x" ]) (Ret (CAtom (var "$r"))))
          , Gfun "M.tl" [ "x" ] (Ret (CApp (AtomForeign "M.leaf") [ var "x" ]))
          , Gfun "M.part" [ "x" ] (Ret (CApp (AtomForeign "M.two") [ var "x" ]))
          -- a NON-foreign generic dispatch: drilled classes are opt-in, so this must add no key
          , Gfun "M.other" [ "g", "x" ] (Ret (CApp (var "g") [ var "x" ]))
          ]
        keys = Set.fromFoldable (gdefs >>= gdefKeys)
        ir =
          ( moduleLlWithEvents
              { gkeys: keys
              , xfns: Map.empty
              , foreignArity: Map.fromFoldable [ Tuple "M.leaf" 1, Tuple "M.two" 2 ]
              , inlineAbi: true
              , defined: keys
              , profileApply: true
              , byNeed: true
              , foreignCall: DirectApplyOnly
              , foreignClosure: Hoisted
              }
              keys
              gdefs
          ).ir
        has needle = String.contains (Pattern needle) ir
        keyCalls = Array.length (String.split (Pattern "call void @pv_applyprofile_key(") ir) - 1
      -- saturated foreign call, both forms
      has "M.leaf|apply|known-match" `shouldEqual` true
      has "M.leaf|tail|known-match" `shouldEqual` true
      -- a 2-ary leaf applied to one argument is exactly what a direct lowering could NOT capture
      has "M.two|tail|known-mismatch" `shouldEqual` true
      has "M.two|tail|known-match" `shouldEqual` false
      -- three foreign dispatches → three keys; the local-unknown callee contributes none
      keyCalls `shouldEqual` 3

    it "emits no drill at all when instrumentation is off" do
      let
        gdefs = [ Gfun "M.f" [ "x" ] (Ret (CApp (AtomForeign "M.leaf") [ var "x" ])) ]
        keys = Set.fromFoldable (gdefs >>= gdefKeys)
        irOf profileApply =
          ( moduleLlWithEvents
              { gkeys: keys
              , xfns: Map.empty
              , foreignArity: Map.fromFoldable [ Tuple "M.leaf" 1 ]
              , inlineAbi: true
              , defined: keys
              , profileApply
              , byNeed: true
              , foreignCall: DirectApplyOnly
              , foreignClosure: Hoisted
              }
              keys
              gdefs
          ).ir
      String.contains (Pattern "pv_applyprofile_key") (irOf false) `shouldEqual` false
      -- and the key text itself must not leak into a shipped object as a dead string constant
      String.contains (Pattern "M.leaf|") (irOf false) `shouldEqual` false
      -- `Ret (CApp …)` is a TAIL dispatch, so this is the tail key — the form axis is real
      String.contains (Pattern "M.leaf|tail|known-match") (irOf true) `shouldEqual` true

    it "records the same events either way (instrumentation observes, it does not classify)" do
      let
        gdefs = [ Gfun "M.f" [ "g", "x" ] (Ret (CApp (var "g") [ var "x" ])) ]
        keys = Set.fromFoldable (gdefs >>= gdefKeys)
        eventsWith profileApply =
          map callEventClass
            ( moduleLlWithEvents
                { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, profileApply, byNeed: true, foreignCall: DirectApplyOnly, foreignClosure: Hoisted }
                keys
                gdefs
            ).events
      eventsWith true `shouldEqual` eventsWith false

  -- ADR-0109 slice B. The lowering is a function of (target × form × knob) and every arm must be
  -- observable in the emitted text: a saturated leaf becomes a call to its OWN entry in both forms,
  -- the counterfactual restores the generic dispatch exactly, and the residue keeps its meaning in
  -- BOTH legs — which is the property that lets the A/B's two legs be compared at all.
  describe "the saturated native-leaf call (ADR-0109 §2/§3)" do
    let
      leafOpts mode =
        { gkeys: Set.empty
        , xfns: Map.empty
        , foreignArity: Map.fromFoldable [ Tuple "M.leaf" 1, Tuple "M.two" 2 ]
        , inlineAbi: true
        , defined: Set.empty
        , profileApply: false
        , byNeed: true
        , foreignCall: mode
        , foreignClosure: Hoisted
        }
      -- `M.ap` calls the leaf non-tail, `M.tl` in tail position INSIDE a direct entry
      -- (`inDirect = true`), `M.part` is the unsaturated residue, and `M.caf` is a leaf call from a
      -- `Gcaf` init — a DIFFERENT activation kind, whose body is emitted non-tail (it must produce a
      -- value to root, not return), so its leaf call is an apply. Measured, not assumed: the first
      -- draft of this fixture claimed it was the `tail && not inDirect` case and the counts said
      -- otherwise. That state is unreachable in emission and is pinned on `callForm` instead.
      leafGdefs =
        [ Gfun "M.ap" [ "x" ] (Let "$r" (CApp (AtomForeign "M.leaf") [ var "x" ]) (Ret (CAtom (var "$r"))))
        , Gfun "M.tl" [ "x" ] (Ret (CApp (AtomForeign "M.leaf") [ var "x" ]))
        , Gfun "M.part" [ "x" ] (Ret (CApp (AtomForeign "M.two") [ var "x" ]))
        , Gcaf "M.caf" (Ret (CApp (AtomForeign "M.leaf") [ int 7 ]))
        ]
      outOf mode =
        let
          keys = Set.fromFoldable (leafGdefs >>= gdefKeys)
        in
          moduleLlWithEvents ((leafOpts mode) { defined = keys, gkeys = keys }) keys leafGdefs
      classesOf mode = Array.sort (map callEventClass (outOf mode).events)
      formsOf mode ir = { pvf: countOf "= call i64 @pvf_M_2eleaf(ptr %ctx, " ir, apply: countOf "= call i64 @pv_apply(" ir, tailcall: countOf "call void @pv_tailcall(" ir }

    -- The THREE stages are the point (ADR-0109 slices B and C are separate checkpoints): a
    -- two-state knob could not express the middle one, and a build that flipped both forms at once
    -- would net the two slices together — slice B's endpoint is precisely that the trampoline
    -- counters do NOT move.
    it "ViaApply: every eligible leaf call is a generic dispatch" do
      -- two applies (M.ap, M.caf) and two trampoline stores (M.tl's leaf, M.part's residue)
      formsOf ViaApply (outOf ViaApply).ir `shouldEqual` { pvf: 0, apply: 2, tailcall: 2 }

    it "DirectApplyOnly (slice B): the apply form is direct, the TAIL counters do not move" do
      let ir = (outOf DirectApplyOnly).ir
      -- BOTH apply-form leaf calls become direct, no generic apply is left, and the trampoline
      -- count is EXACTLY the ViaApply one — that invariance is slice B's mechanical endpoint, and
      -- it is what makes slice C separately measurable.
      formsOf DirectApplyOnly ir `shouldEqual` { pvf: 2, apply: 0, tailcall: 2 }
      -- no settle on the direct path: a `pvf_` entry never leaves a pending tail (§3 clause 1)
      countOf "call i64 @pv_settle(" ir `shouldEqual` 0

    it "DirectApplyAndTail (slice C): both forms direct; only the residue still bounces" do
      formsOf DirectApplyAndTail (outOf DirectApplyAndTail).ir `shouldEqual` { pvf: 3, apply: 0, tailcall: 1 }

    it "the residue keeps its meaning in ALL THREE stages (the A/B's precondition)" do
      let
        reasons out = Array.sort
          ( Array.mapMaybe
              ( case _ of
                  GenericApply r -> Just (missReasonName r)
                  GenericTail r -> Just (missReasonName r)
                  _ -> Nothing
              )
              out.events
          )
      -- exactly ONE `callee-foreign` in each: the unsaturated `M.two`. An eligible call is never
      -- the residue, whichever stage lowered it.
      reasons (outOf ViaApply) `shouldEqual` [ "callee-foreign" ]
      reasons (outOf DirectApplyOnly) `shouldEqual` [ "callee-foreign" ]
      reasons (outOf DirectApplyAndTail) `shouldEqual` [ "callee-foreign" ]

    -- Event forms follow the DECISION, per stage. `M.ap`/`M.caf` are applies, `M.tl` and the
    -- residue are tails — and that split is the same in all three stages, because the stage picks
    -- the recipe, never the form.
    it "the apply/tail event split is the same in every stage" do
      let
        isTail = case _ of
          ForeignDirectTail _ -> true
          ForeignDeferredTail _ -> true
          GenericTail _ -> true
          _ -> false
        isApply = case _ of
          ForeignDirectApply _ -> true
          ForeignDeferredApply _ -> true
          GenericApply _ -> true
          _ -> false
      for_ [ ViaApply, DirectApplyOnly, DirectApplyAndTail ] \mode -> do
        Array.length (Array.filter isTail (outOf mode).events) `shouldEqual` 2
        Array.length (Array.filter isApply (outOf mode).events) `shouldEqual` 2

    it "the classes are the decision, and an eligible call is never a MissReason" do
      classesOf ViaApply `shouldEqual`
        Array.sort [ CForeignDeferredApply, CForeignDeferredApply, CForeignDeferredTail, CGenericTail, CWrapperEntry, CWrapperEntry, CWrapperEntry ]
      classesOf DirectApplyOnly `shouldEqual`
        Array.sort [ CForeignDirectApply, CForeignDirectApply, CForeignDeferredTail, CGenericTail, CWrapperEntry, CWrapperEntry, CWrapperEntry ]
      classesOf DirectApplyAndTail `shouldEqual`
        Array.sort [ CForeignDirectApply, CForeignDirectApply, CForeignDirectTail, CGenericTail, CWrapperEntry, CWrapperEntry, CWrapperEntry ]

    -- The FORM DERIVATION itself, over its whole input space. `tail && not inDirect` is currently
    -- unreachable in emission — every tail context is a lifted body, which sets `inDirect` — so no
    -- fixture can reach the state the round-5 defect lived in. Pinning the function is the honest
    -- substitute: it is where the wrong answer was, and it stays checked if a future activation kind
    -- ever makes the state reachable.
    it "callForm is target-aware over every (tail × inDirect) state" do
      let
        guest = GuestTarget (fnInfo "M.f$d" 1)
        leaf = ForeignTarget leafRef
        generic = GenericTarget MissLocalUnknownFn
        at t d = { tail: t, inDirect: d }
      -- non-tail: everything is an apply
      for_ [ guest, leaf, generic ] \tgt -> do
        callForm (at false true) tgt `shouldEqual` FApply
        callForm (at false false) tgt `shouldEqual` FApply
      -- tail INSIDE a direct entry: everything is a tail
      for_ [ guest, leaf, generic ] \tgt -> callForm (at true true) tgt `shouldEqual` FTail
      -- tail OUTSIDE one: the guest form falls back to call-then-ret (no `%env` to hand over),
      -- while a leaf call and a generic dispatch are still tails. THIS is the row the single
      -- derivation got wrong for two of the three targets.
      callForm (at true false) guest `shouldEqual` FApply
      callForm (at true false) leaf `shouldEqual` FTail
      callForm (at true false) generic `shouldEqual` FTail
