-- | ADR-0113 §3.1: the bind-time fact channel, checked where it is OBSERVABLE — at the call sites
-- | whose classification it decides.
-- |
-- | These are not tests of `lift`'s or `buildGrec`'s internals. Each fixture builds a program, emits
-- | it, and reads the `CallEvent`s the emitter recorded, because that is the only thing the census
-- | and the profile can see. A fact derived but never reaching a classification would pass an
-- | internal assertion and still measure nothing.
-- |
-- | Slices 1–2 lower every candidate as DEFERRED, so a recoverable site shows up as
-- | `LocalDeferredApply`/`LocalDeferredTail` carrying its `CandidateKind`, and an opaque one as
-- | `GenericApply`/`GenericTail` carrying its `BindOrigin`. The two populations are disjoint by
-- | construction and that is what these rows pin.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.LocalFacts where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..))
import Data.String as String
import Data.Set as Set
import Purvasm.Compiler.Backend.LLVM.CallClass (CallEvent(..), MissReason(..))
import Purvasm.Compiler.Backend.LLVM.ForeignRef (ForeignCallMode(..), ForeignClosureMode(..))
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions)
import Purvasm.Compiler.Backend.LLVM.Program (moduleLlWithEvents)
import Purvasm.Compiler.Backend.LLVM.Types (BindOrigin(..), CandidateKind(..), EnvSrc(..), FnInfo, Gdef(..), bindOriginName, bindOrigins, capturableFact, unFact)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

-- | The emitter options every fixture runs under: the SHIPPED configuration. There is no
-- | local-facts knob to set — slices 1–2 removed the possibility rather than defaulting it, so a
-- | fixture cannot accidentally measure an unapproved lowering.
opts :: Array String -> MakeCxOptions
opts gkeys =
  { gkeys: Set.fromFoldable gkeys
  , xfns: Map.empty
  , foreignArity: Map.empty
  , inlineAbi: true
  , defined: Set.fromFoldable gkeys
  , profileApply: false
  , byNeed: true
  , foreignCall: DirectApplyAndTail
  , foreignClosure: Hoisted
  }

-- | Emit a module and keep only what ADR-0113 classifies: the candidate events with their kind, and
-- | the opaque ones with their origin. Everything else (direct calls, wrapper entries, foreign
-- | classes) is another ADR's business and would only make a diff unreadable.
localEvents
  :: Array String
  -> Array Gdef
  -> { deferred :: Array CandidateKind, opaque :: Array BindOrigin, direct :: Array FnInfo, ir :: String }
localEvents gkeys gdefs =
  let
    out = moduleLlWithEvents (opts gkeys) (Set.fromFoldable gkeys) gdefs
  in
    { deferred: Array.mapMaybe pickDeferred out.events
    , opaque: Array.mapMaybe pickOpaque out.events
    -- the DIRECT events are carried so an ACTIVE fact can be asserted POSITIVELY. Checking only
    -- that the two generic populations are empty would pass just as happily if the call site
    -- vanished altogether.
    , direct: Array.mapMaybe pickDirect out.events
    , ir: out.ir
    }
  where
  pickDeferred = case _ of
    LocalDeferredApply k -> Just k
    LocalDeferredTail k -> Just k
    _ -> Nothing
  pickOpaque = case _ of
    GenericApply (MissLocalUnknownFn o) -> Just o
    GenericTail (MissLocalUnknownFn o) -> Just o
    _ -> Nothing
  pickDirect = case _ of
    DirectNonTail info -> Just info
    DirectMusttail info -> Just info
    _ -> Nothing

-- | Occurrences of the INSTRUCTION form of a generic dispatch. Not `@pv_apply`: every object
-- | `declare`s that symbol, so a substring needle matches a module with no dispatch in it at all —
-- | the unanchored-needle trap ADR-0109 §"defects" records being caught twice.
applyInstructions :: String -> Int
applyInstructions ir = Array.length (String.split (Pattern "call i64 @pv_apply(") ir) - 1

-- | `let f = \x -> x in <body>` — the canonical ACTIVE fact, used as the source every chain below
-- | recovers from.
letLambda :: String -> Expr -> Expr
letLambda name = Let name (CLam [ "x" ] (Ret (CAtom (var "x"))))

-- | A saturated call to `f`, in non-tail position so the form is `apply`.
callOf :: String -> Expr
callOf f = Let "r" (CApp (var f) [ int 1 ]) (Ret (CAtom (var "r")))

-- | The same call in TAIL position, so the form is `tail` — a `pv_tailcall` store rather than a
-- | `pv_apply` dispatch. The two forms are different emitted operations, so a candidate has to be
-- | pinned deferred in BOTH (ADR-0113 §4: `apply` and `tail` are never summed together).
tailCallOf :: String -> Expr
tailCallOf f = Ret (CApp (var f) [ int 1 ])

-- | Whole-program events, keeping the form as well as the kind: the matrix below is over
-- | `CandidateKind × Form`, and a row that lost the form would pass on the wrong operation.
deferredWithForm :: Array String -> Array Gdef -> Array (Tuple CandidateKind String)
deferredWithForm gkeys gdefs =
  Array.mapMaybe pick (moduleLlWithEvents (opts gkeys) (Set.fromFoldable gkeys) gdefs).events
  where
  pick = case _ of
    LocalDeferredApply k -> Just (Tuple k "apply")
    LocalDeferredTail k -> Just (Tuple k "tail")
    _ -> Nothing

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.LocalFacts (ADR-0113 §3.1)" do

  describe "the shadowing rule — a name that names a Grec member is NOT the enclosing binding" do
    -- THE regression this fixture exists for. `buildGrec` once derived every member capture from the
    -- enclosing `env` and overwrote only the FUNCTION members afterwards, so a VALUE member that
    -- shadowed an enclosing known lambda kept the OUTER binding's fact — a candidate the emitter
    -- could never act on, counted in the census that exists to size that population.
    it "a value member shadowing an outer known lambda is OPAQUE, not a candidate" do
      let
        -- outer `x` is a known lambda; the group rebinds `x` as a VALUE member; the lambda member
        -- `g` captures that `x` and calls it.
        prog =
          [ Gcaf "M.top"
              ( letLambda "x"
                  ( LetRec
                      [ { var: "x", rhs: Ret (CAtom (int 7)) }
                      , { var: "g", rhs: Ret (CLam [ "u" ] (callOf "x")) }
                      ]
                      (Ret (CAtom (var "g")))
                  )
              )
          ]
        got = localEvents [ "M.top" ] prog
      -- the call to `x` inside `g` must be OPAQUE: inside the group `x` is the value member's cell.
      got.deferred `shouldEqual` []
      Array.elem OCapture got.opaque `shouldEqual` true

    it "the same shape WITHOUT the shadowing member does recover the outer fact" do
      -- the discriminating half: identical but for the shadowing member, so a fixture that passed
      -- by accident (because nothing was recovered anywhere) fails here.
      let
        prog =
          [ Gcaf "M.top"
              ( letLambda "x"
                  ( LetRec
                      [ { var: "g", rhs: Ret (CLam [ "u" ] (callOf "x")) } ]
                      (Ret (CAtom (var "g")))
                  )
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.deferred `shouldEqual` [ Capture ]

  describe "the three Grec capture populations" do
    it "a function sibling is ACTIVE (a direct call), not a candidate and not opaque" do
      let
        prog =
          [ Gcaf "M.top"
              ( LetRec
                  [ { var: "f", rhs: Ret (CLam [ "x" ] (Ret (CAtom (var "x")))) }
                  , { var: "g", rhs: Ret (CLam [ "u" ] (callOf "f")) }
                  ]
                  (Ret (CAtom (var "g")))
              )
          ]
        got = localEvents [ "M.top" ] prog
      -- an ACTIVE fact at a matching arity is a DIRECT call: it appears in neither population…
      got.deferred `shouldEqual` []
      got.opaque `shouldEqual` []
      -- …and it must be POSITIVELY there. A `Grec` member's fact is `SForceCell` at its own arity,
      -- and the emitted IR must carry the `tailcc` call to the very `dsym` the event names — so the
      -- event and the emission are checked against each other rather than each against nothing.
      case Array.find (\i -> i.src == SForceCell) got.direct of
        Nothing -> fail ("no direct call to a Grec member was recorded; direct = " <> show got.direct)
        Just info -> do
          info.arity `shouldEqual` 1
          String.contains (Pattern ("call tailcc i64 @" <> info.dsym <> "(")) got.ir `shouldEqual` true

    it "a value sibling is OPAQUE (there is no direct entry to call)" do
      let
        prog =
          [ Gcaf "M.top"
              ( LetRec
                  [ { var: "v", rhs: Ret (CAtom (int 3)) }
                  , { var: "g", rhs: Ret (CLam [ "u" ] (callOf "v")) }
                  ]
                  (Ret (CAtom (var "g")))
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.deferred `shouldEqual` []
      Array.elem OCapture got.opaque `shouldEqual` true

    it "an OUTSIDE capture of an enclosing known lambda is a recoverable candidate" do
      let
        prog =
          [ Gcaf "M.top"
              ( letLambda "outer"
                  ( LetRec
                      [ { var: "g", rhs: Ret (CLam [ "u" ] (callOf "outer")) } ]
                      (Ret (CAtom (var "g")))
                  )
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.deferred `shouldEqual` [ Capture ]

  describe "transitivity — the derivation reads the whole channel, not just the active arm" do
    it "an alias chain of depth 2 stays recoverable at the last link" do
      let
        -- f (known) -> a = f -> b = a; the call is on `b`.
        prog =
          [ Gcaf "M.top"
              ( letLambda "f"
                  ( Let "a" (CAtom (var "f"))
                      (Let "b" (CAtom (var "a")) (callOf "b"))
                  )
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.deferred `shouldEqual` [ AliasLocal ]

    it "a capture of an alias is classified Capture — the kind is re-stamped at the LAST bind site" do
      -- the kind must describe where a lowering would have to act, not where the fact originated.
      let
        prog =
          [ Gcaf "M.top"
              ( letLambda "f"
                  ( Let "a" (CAtom (var "f"))
                      (Let "g" (CLam [ "u" ] (callOf "a")) (Ret (CAtom (var "g"))))
                  )
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.deferred `shouldEqual` [ Capture ]

  describe "the opaque origins are attributed, not lumped" do
    it "a parameter call is OParam" do
      let
        prog = [ Gfun "M.f" [ "k" ] (callOf "k") ]
        got = localEvents [ "M.f" ] prog
      got.deferred `shouldEqual` []
      got.opaque `shouldEqual` [ OParam ]

    it "a non-lambda let is OLetValue" do
      let
        prog = [ Gfun "M.f" [ "k" ] (Let "v" (CAtom (int 1)) (callOf "v")) ]
        got = localEvents [ "M.f" ] prog
      got.deferred `shouldEqual` []
      got.opaque `shouldEqual` [ OLetValue ]

  describe "the CapturableFact boundary (ADR-0113 §2)" do
    -- `capturableFact` is pinned over its WHOLE input space here rather than through a fixture,
    -- because the emitter cannot currently build an `SSelf` env entry to reach it — the same place
    -- ADR-0109 pinned `callForm`'s unreachable arm. The constructor is not exported, so this is the
    -- only way a `CapturableFact` comes into being anywhere in the compiler.
    it "refuses SSelf and accepts every other EnvSrc" do
      (capturableFact { dsym: "d", arity: 1, src: SSelf } # isJust) `shouldEqual` false
      for_ [ SSentinel, SClosureEnv, SForceCell ] \src ->
        (capturableFact { dsym: "d", arity: 1, src } # isJust) `shouldEqual` true

    it "round-trips the fact it accepted" do
      let info = { dsym: "pv_g_M_2ef$d", arity: 2, src: SClosureEnv }
      map unFact (capturableFact info) `shouldEqual` Just info

  describe "a candidate is never lowered directly in slices 1-2" do
    -- The structural guarantee is the ABSENCE of a knob: `LocalFactsMode`, `decideLocal`,
    -- `EmitLocalDirect` and `EmitLocalArity` do not exist, so no caller — public API included —
    -- can select a direct lowering. That is checked by `tools/seam-audit.sh` (a type that is not
    -- there cannot be asserted about here); what this row pins is the behavioural consequence.
    it "every recoverable site emits the generic dispatch, whatever the shipped knobs say" do
      let
        prog =
          [ Gcaf "M.top"
              ( letLambda "f"
                  (Let "g" (CLam [ "u" ] (callOf "f")) (Ret (CAtom (var "g"))))
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.deferred `shouldEqual` [ Capture ]
      -- the recovered fact reached NO direct call, and the site still goes through the dispatcher.
      -- Counted on the INSTRUCTION form and pinned at exactly one, so the row cannot be satisfied
      -- by the `declare` line every object carries, nor by some other call drifting in.
      applyInstructions got.ir `shouldEqual` 1
      got.direct `shouldEqual` []

  describe "the CandidateKind × Form matrix — every candidate defers, in both forms" do
    -- The seam audit pins that the slice-3 VOCABULARY is absent; this pins the BEHAVIOUR, over the
    -- whole product. An audit alone could be passed by a differently-named direct path; a fixture on
    -- one kind or one form alone could be passed by a path it never exercises.
    --
    -- Every shape puts the call inside a LIFTED LAMBDA, because that is the only tail context here:
    -- a `Gcaf` body must produce a value to root, so `Ret (CApp …)` there is an `apply`, not a
    -- `tail` (ADR-0109). Building the tail rows in the `Gcaf` body instead would have silently
    -- measured the apply form twice.
    let
      keys = [ "M.top", "M.known" ]
      -- each shape binds a candidate of its kind and hands its callee NAME to the call builder
      shapes =
        [ Tuple Capture \call ->
            letLambda "f" (Let "g" (CLam [ "u" ] (call "f")) (Ret (CAtom (var "g"))))
        , Tuple AliasLocal \call ->
            letLambda "f"
              (Let "g" (CLam [ "u" ] (Let "a" (CAtom (var "f")) (call "a"))) (Ret (CAtom (var "g"))))
        , Tuple AliasGlobal \call ->
            Let "g" (CLam [ "u" ] (Let "a" (CAtom (var "M.known")) (call "a"))) (Ret (CAtom (var "g")))
        ]
      -- the global the AliasGlobal row aliases: a top-level function, so `gfns` carries its fact
      withKnown gdefs = Array.cons (Gfun "M.known" [ "x" ] (Ret (CAtom (var "x")))) gdefs

    for_ shapes \(Tuple kind build) ->
      for_ [ Tuple "apply" callOf, Tuple "tail" tailCallOf ] \(Tuple formName call) ->
        it ("defers a " <> show kind <> " candidate in the " <> formName <> " form") do
          let
            prog = withKnown [ Gcaf "M.top" (build call) ]
            got = deferredWithForm keys prog
          -- EXACT, not membership: each shape builds exactly one candidate site, so an extra
          -- event (a kind counted twice, a form counted in both) fails here rather than hiding
          -- behind the expected row.
          got `shouldEqual` [ Tuple kind formName ]
          -- and NOTHING was lowered directly off a recovered fact
          (localEvents keys prog).direct `shouldEqual` []

  describe "the BindOrigin enumeration is total, and its two diagnostic rows are zero" do
    -- `bindOrigins` is what the census columns and the ADR-0113 §3 identities are stated over, so a
    -- missing entry would silently shrink a sum rather than fail it.
    it "names every origin exactly once, with distinct report tokens" do
      Array.length bindOrigins `shouldEqual` 7
      Array.nub bindOrigins `shouldEqual` bindOrigins
      Array.nub (map bindOriginName bindOrigins) `shouldEqual` map bindOriginName bindOrigins

    -- `OLetLambda` and `OGrecLambda` are DIAGNOSTIC rows: those binders always stamp an ACTIVE fact,
    -- so a binding of that origin can never be opaque. They keep their slots (a class with no
    -- counter cannot be measured) and are pinned at zero — the same treatment ADR-0108 gives
    -- `callee-literal` and `unknown-key`.
    it "never classifies a let-bound or Grec lambda as opaque, in a program full of both" do
      let
        prog =
          [ Gcaf "M.top"
              ( letLambda "lam"
                  ( LetRec
                      [ { var: "rec", rhs: Ret (CLam [ "x" ] (Ret (CAtom (var "x")))) } ]
                      -- call both, so the sites exist and are classified
                      (Let "a" (CApp (var "lam") [ int 1 ]) (callOf "rec"))
                  )
              )
          ]
        got = localEvents [ "M.top" ] prog
      Array.elem OLetLambda got.opaque `shouldEqual` false
      Array.elem OGrecLambda got.opaque `shouldEqual` false
      -- the POSITIVE half: both calls really happened and really were direct, so the row above is
      -- not passing because the program classified nothing at all.
      Array.length got.direct `shouldEqual` 2

    it "does classify the other origins as opaque when nothing is derivable" do
      -- the discriminating counterpart: same emitter, origins that legitimately have no fact.
      let
        prog = [ Gfun "M.f" [ "k" ] (Let "v" (CAtom (int 1)) (Let "r" (CApp (var "k") [ int 1 ]) (callOf "v"))) ]
        got = localEvents [ "M.f" ] prog
      Array.sort (Array.nub got.opaque) `shouldEqual` Array.sort [ OParam, OLetValue ]

  describe "every bind PATH is exercised, not just the enumeration (§3.1 (4))" do
    -- The totality row above checks that `bindOrigins` lists seven things. It cannot catch a bind
    -- site that stamps the WRONG origin, because that is a property of the emitter rather than of
    -- the list. These rows drive each remaining path through real emission.
    it "a Grec VALUE member called from the group body is OGrecValue" do
      -- called from the LetRec body, so the name resolves through `buildGrec`'s own bind — not as a
      -- capture, which is what the earlier Grec row exercises.
      let
        prog =
          [ Gcaf "M.top"
              ( LetRec
                  [ { var: "v", rhs: Ret (CAtom (int 3)) } ]
                  (callOf "v")
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.opaque `shouldEqual` [ OGrecValue ]
      got.deferred `shouldEqual` []

    it "a decision-tree leaf binder is OMatchBinder" do
      let
        prog =
          [ Gfun "M.f" [ "s" ]
              ( Ret
                  ( CCase [ var "s" ]
                      [ { binders: [ BVar "b" ], result: Uncond (callOf "b") } ]
                  )
              )
          ]
        got = localEvents [ "M.f" ] prog
      got.opaque `shouldEqual` [ OMatchBinder ]
      got.deferred `shouldEqual` []

  describe "the derivation is transitive at depth, not just at one link" do
    it "an alias chain of depth 3 is still recoverable at the last link" do
      -- f (known) -> a = f -> b = a -> c = b; the call is on `c`. A one-level derivation
      -- (`_.knownFn`) recovers `a` and stops; this row fails for it at `b`.
      let
        prog =
          [ Gcaf "M.top"
              ( letLambda "f"
                  ( Let "a" (CAtom (var "f"))
                      (Let "b" (CAtom (var "a")) (Let "c" (CAtom (var "b")) (callOf "c")))
                  )
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.deferred `shouldEqual` [ AliasLocal ]

    it "a capture of a capture is recoverable, and is Capture at the innermost site" do
      -- outer lambda captures the known `f`; an INNER lambda captures that capture and calls it.
      -- The fact has to survive two `lift`s, and the kind must describe the innermost bind site.
      let
        prog =
          [ Gcaf "M.top"
              ( letLambda "f"
                  ( Let "outer"
                      ( CLam [ "u" ]
                          (Let "inner" (CLam [ "w" ] (callOf "f")) (Ret (CAtom (var "inner"))))
                      )
                      (Ret (CAtom (var "outer")))
                  )
              )
          ]
        got = localEvents [ "M.top" ] prog
      got.deferred `shouldEqual` [ Capture ]
      got.opaque `shouldEqual` []
