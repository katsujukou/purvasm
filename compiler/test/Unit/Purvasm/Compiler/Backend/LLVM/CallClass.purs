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
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Purvasm.Compiler.Backend.LLVM.CallClass (CallClass(..), CallEvent(..), MissReason(..), callClassName, callClasses, callEventClass)
import Purvasm.Compiler.Backend.LLVM.Emit (directTarget)
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions, makeCx, mintParam, runCodegen)
import Purvasm.Compiler.Backend.LLVM.Program (entryLlWithEvents, gdefKeys, moduleLlWithEvents)
import Purvasm.Compiler.Backend.LLVM.Types (BindingV(..), EnvSrc(..), FnInfo, Gdef(..), bindDirectFnVar, bindDirectVar)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
  , foreignArity: Map.empty
  , inlineAbi: true
  , defined: Set.fromFoldable o.defined
  , byNeed: true
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

classify :: ClassifyCfg -> Atom -> Int -> Either MissReason FnInfo
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

base :: ClassifyCfg
base = { gkeys: [], gfns: [], xfns: [], defined: [], self: Nothing, localFn: Nothing }

-- | The `fchk`-style counters for the emitted `.ll`, per ADR-0108 §2's call forms.
emittedForms :: Array Gdef -> { pvApply :: Int, pvTailcall :: Int, musttail :: Int, guestDirect :: Int }
emittedForms gdefs =
  let
    keys = Set.fromFoldable (gdefs >>= gdefKeys)
    ir =
      ( moduleLlWithEvents
          { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, byNeed: true }
          keys
          gdefs
      ).ir
    -- CALL sites only: `declare i64 @pv_apply(…)` and the `define … tailcc` headers carry the same
    -- symbols, so the needles include the call keyword (a `declare`-matching needle is how this
    -- test first reported a constant +1 in every column).
    occurrences needle = Array.length (String.split (Pattern needle) ir) - 1
  in
    { pvApply: occurrences "call i64 @pv_apply("
    , pvTailcall: occurrences "call void @pv_tailcall("
    , musttail: occurrences "musttail call"
    -- a `musttail call tailcc i64 @…` line matches BOTH needles, so the musttail sites are
    -- subtracted out: they are their own column.
    , guestDirect: occurrences "call tailcc i64 @" - occurrences "musttail call tailcc i64 @"
    }

-- | The recorded events for the same gdefs, counted by accounting column.
recordedForms :: Array Gdef -> { pvApply :: Int, pvTailcall :: Int, musttail :: Int, guestDirect :: Int }
recordedForms gdefs =
  let
    keys = Set.fromFoldable (gdefs >>= gdefKeys)
    events =
      ( moduleLlWithEvents
          { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, byNeed: true }
          keys
          gdefs
      ).events
    count cls = Array.length (Array.filter (\e -> callEventClass e == cls) events)
  in
    { pvApply: count CGenericApply + count CStructuralApply
    , pvTailcall: count CGenericTail
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
        { gkeys: keys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: keys, byNeed: true }
        keys
        gdefs
    ).events

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.CallClass" do
  describe "directTarget names every leaf of its decision tree" do
    it "a non-variable callee" do
      classify base (int 1) 1 `shouldEqual` Left MissCalleeNotVar
      classify base (AtomForeign "M.leaf") 1 `shouldEqual` Left MissCalleeNotVar

    it "a local binding with no known-function fact" do
      classify base (var "someLocal") 1 `shouldEqual` Left MissLocalUnknownFn

    it "a local known function at the wrong arity — and the right one" do
      let cfg = base { localFn = Just (fnInfo "fn_1$d" 2) }
      classify cfg (var "localFn") 1 `shouldEqual` Left MissArityLocal
      classify cfg (var "localFn") 2 `shouldEqual` Right (fnInfo "fn_1$d" 2)

    it "a variable that is neither a local binding nor a known global" do
      classify base (var "nope") 1 `shouldEqual` Left MissUnknownKey

    it "an own-object function fact at the wrong arity (never falls through to the surface)" do
      classify
        (base { gkeys = [ "M.f" ], gfns = [ Tuple "M.f" (fnInfo "M.f$d" 2) ], defined = [ "M.f" ] })
        (var "M.f")
        1 `shouldEqual` Left MissArityOwnModule
      -- … and the saturated call is direct
      classify
        (base { gkeys = [ "M.f" ], gfns = [ Tuple "M.f" (fnInfo "M.f$d" 2) ], defined = [ "M.f" ] })
        (var "M.f")
        2 `shouldEqual` Right (fnInfo "M.f$d" 2)

    -- The two leaves that ONLY the ownership input separates: same key, same absent facts, and the
    -- answer differs purely by whether this object defines it.
    it "an own-object key that is not a function vs a dependency with no published fact" do
      let cfg = base { gkeys = [ "M.c" ] }
      classify (cfg { defined = [ "M.c" ] }) (var "M.c") 1 `shouldEqual` Left MissOwnObjectNotFn
      classify (cfg { defined = [] }) (var "M.c") 1 `shouldEqual` Left MissDepNoDirectFact

    it "a published cross-module fact at the wrong arity — and the right one" do
      let cfg = base { gkeys = [ "Other.g" ], xfns = [ Tuple "Other.g" (fnInfo "Other.g$d" 3) ] }
      classify cfg (var "Other.g") 1 `shouldEqual` Left MissArityCrossModule
      classify cfg (var "Other.g") 3 `shouldEqual` Right (fnInfo "Other.g$d" 3)

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
      classify cfg (var "M.loop") 1 `shouldEqual` Right (fnInfo "M.loop$d" 1)

    it "takes the shortcut when the shape does match" do
      let
        cfg = base
          { gkeys = [ "M.loop" ]
          , gfns = [ Tuple "M.loop" (fnInfo "M.loop$d" 1) ]
          , defined = [ "M.loop" ]
          , self = Just { name: "M.loop", captureHandle: Nothing, fnInfo: fnInfo "M.self$d" 2 }
          }
      classify cfg (var "M.loop") 2 `shouldEqual` Right (fnInfo "M.self$d" 2)

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
      Array.length callClasses `shouldEqual` 6
      Array.length (Array.nub callClasses) `shouldEqual` Array.length callClasses
      Array.nub (map callClassName callClasses) `shouldEqual` map callClassName callClasses

    it "every event shape maps into it" do
      let
        samples =
          [ DirectNonTail (fnInfo "d" 1)
          , DirectMusttail (fnInfo "d" 1)
          , GenericApply MissCalleeNotVar
          , GenericTail MissCalleeNotVar
          , StructuralApply
          , WrapperEntry
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
          , byNeed: true
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

