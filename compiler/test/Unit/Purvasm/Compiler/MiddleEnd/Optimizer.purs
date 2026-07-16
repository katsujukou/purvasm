-- | The optimiser seam (ADR-0086 §3): one `optimizeModule` pass must compose `DictElim` then the
-- | NbE inliner (a resolved method call on literal operands collapses all the way to its folded
-- | constant in a single round, ADR-0089), resolve dispatch against the dependency env
-- | (`extendSummary` threading) exactly like a local one, and never persist a summary today
-- | (`persistedSummary = Nothing`, ADR-0084 §5 — the `.pmi` stays byte-for-byte boot's).
-- |
-- | The NbE pass α-renames every binder to the reserved `$q<n>` supply and folds the accessor's
-- | irrefutable single-`BVar` case — the expected shapes below are the *normalised* decls, not the
-- | inputs.
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, Decl)
import Data.Map as Map
import Purvasm.Compiler.Backend.LLVM.Interface (gdefKindMap)
import Purvasm.Compiler.Backend.LLVM.Program (classifyDecl)
import Purvasm.Compiler.Bytecode.Artifact (kindToTag)
import Purvasm.Compiler.MiddleEnd.Optimizer (OuterKind(..), emptyBuildEnv, extendSummary, localFactsOf, optimizeModule, persistedSummary, publishedForeignSigs)
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (fvExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Quarantine (RejectionEvent(..), emptyQuarantine)
import Purvasm.Compiler.MiddleEnd.Optimizer.Specialize (isSpecKey)
import Data.Set as Set
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

nonrec :: String -> Expr -> Decl
nonrec k e = { recursive: false, members: [ k /\ e ] }

-- The dictionary machinery of a one-class, one-instance module: the method accessor
-- `\d -> case d of v -> v.add` and the instance record `{ add: intAdd }` (the impl an
-- intrinsic foreign, so it is liftable and saturates to a primop).
accessorDecl :: String -> Decl
accessorDecl k = nonrec k
  ( Ret
      ( CLam [ "d" ]
          ( Ret
              ( CCase [ var "d" ]
                  [ { binders: [ BVar "v" ]
                    , result: Uncond (Ret (CAccessor (var "v") "add"))
                    }
                  ]
              )
          )
      )
  )

instanceDecl :: String -> Decl
instanceDecl k = nonrec k
  (Ret (CRecord [ { prop: "add", val: AtomForeign "Data.Semiring.intAdd" } ]))

-- A method call `add semiringInt 1 2` against the given accessor/instance keys.
useDecl :: String -> String -> String -> Decl
useDecl k acc inst = nonrec k
  (Ret (CApp (var acc) [ var inst, AtomLit (LInt 1), AtomLit (LInt 2) ]))

-- `AddInt(1, 2)` constant-folds under the NbE pass (VM-exact folding, ADR-0089 §5).
collapsed :: Expr
collapsed = Ret (CAtom (AtomLit (LInt 3)))

-- The accessor after normalisation: the irrefutable single-`BVar` case folds and the param is
-- `$q`-renamed.
accessorOpt :: Expr
accessorOpt = Ret (CLam [ "$q1" ] (Ret (CAccessor (var "$q1") "add")))

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Optimizer" do
  backstopSpec
  quarantineSpec
  specializeRetrySpec
  describe "optimizeModule" do
    it "collapses a locally-known method call to its folded constant in one pass (DictElim then NbE)" do
      let
        am :: AnfModule
        am =
          { name: "Test.M"
          , decls:
              [ accessorDecl "Test.M.add"
              , instanceDecl "Test.M.semiringInt"
              , useDecl "Test.M.three" "Test.M.add" "Test.M.semiringInt"
              ]
          }
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) emptyQuarantine am
      map _.members r.module.decls `shouldEqual`
        [ [ "Test.M.add" /\ accessorOpt ]
        , (instanceDecl "Test.M.semiringInt").members
        , [ "Test.M.three" /\ collapsed ]
        ]

    it "collapses through a floated dictionary application (the purs one-time-extraction shape)" do
      -- add1 = add semiringInt (its own top-level binding, the shape purs floats); the loop body
      -- calls add1 — DictElim turns add1 into the intAdd alias, and the NbE sibling facts
      -- collapse the call to the (folded) primitive.
      let
        am :: AnfModule
        am =
          { name: "Test.M"
          , decls:
              [ accessorDecl "Test.M.add"
              , instanceDecl "Test.M.semiringInt"
              , nonrec "Test.M.add1"
                  (Ret (CApp (var "Test.M.add") [ var "Test.M.semiringInt" ]))
              , nonrec "Test.M.three"
                  (Ret (CApp (var "Test.M.add1") [ AtomLit (LInt 1), AtomLit (LInt 2) ]))
              ]
          }
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) emptyQuarantine am
      map _.members r.module.decls `shouldEqual`
        [ [ "Test.M.add" /\ accessorOpt ]
        , (instanceDecl "Test.M.semiringInt").members
        , [ "Test.M.add1" /\ Ret (CAtom (AtomForeign "Data.Semiring.intAdd")) ]
        , [ "Test.M.three" /\ collapsed ]
        ]

    it "drops a dead pure call through the own-foreign fact channel (ADR-0095: LocalFacts.foreignSigs → oracle)" do
      -- f = \x -> let dead = pureImpl x in x — the callee's shape arrives the way the driver
      -- injects the module's own reconstructed sigs (`lf { foreignSigs = ownSigs }`).
      let
        am :: AnfModule
        am =
          { name: "Test.M"
          , decls:
              [ nonrec "Test.M.f"
                  ( Ret
                      ( CLam [ "x" ]
                          ( Let "dead" (CApp (AtomForeign "Test.M.pureImpl") [ var "x" ])
                              (Ret (CAtom (var "x")))
                          )
                      )
                  )
              ]
          }
        lf = (localFactsOf emptyBuildEnv am)
          { foreignSigs = Map.singleton "Test.M.pureImpl" { arity: 1, vsat: false, retVsat: false } }
        r = optimizeModule emptyBuildEnv lf emptyQuarantine am
      map _.members r.module.decls `shouldEqual`
        [ [ "Test.M.f" /\ Ret (CLam [ "$q1" ] (Ret (CAtom (var "$q1")))) ] ]

    it "keeps the same dead call when no fact is available (the conservative default end-to-end)" do
      let
        am :: AnfModule
        am =
          { name: "Test.M"
          , decls:
              [ nonrec "Test.M.f"
                  ( Ret
                      ( CLam [ "x" ]
                          ( Let "dead" (CApp (AtomForeign "Test.M.pureImpl") [ var "x" ])
                              (Ret (CAtom (var "x")))
                          )
                      )
                  )
              ]
          }
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) emptyQuarantine am
      map _.members r.module.decls `shouldEqual`
        [ [ "Test.M.f" /\ Ret
              ( CLam [ "$q1" ]
                  ( Let "$q2" (CApp (AtomForeign "Test.M.pureImpl") [ var "$q1" ])
                      (Ret (CAtom (var "$q1")))
                  )
              )
          ]
        ]

    it "drops a dead pure cross-module call via the dependency's published effects (extendSummary)" do
      -- Test.Dep.big is pure but too large to publish as an inline candidate (≥ the 64 publish
      -- bound), so the *only* channel that can prove the user's dead call droppable is the
      -- ADR-0095 effects summary riding extendSummary.
      let
        chain :: Int -> Expr
        chain 0 = Ret (CAtom (var "x"))
        chain n = Let ("v" <> show n) (CPrim AddInt [ var "x", var "x" ]) (chain (n - 1))

        dep :: AnfModule
        dep = { name: "Test.Dep", decls: [ nonrec "Test.Dep.big" (Ret (CLam [ "x" ] (chain 20))) ] }
        depResult = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv dep) emptyQuarantine dep
        env = extendSummary emptyBuildEnv depResult.summary

        user :: AnfModule
        user =
          { name: "Test.User"
          , decls:
              [ nonrec "Test.User.g"
                  ( Ret
                      ( CLam [ "y" ]
                          ( Let "dead" (CApp (var "Test.Dep.big") [ var "y" ])
                              (Ret (CAtom (var "y")))
                          )
                      )
                  )
              ]
          }
        r = optimizeModule env (localFactsOf env user) emptyQuarantine user
      map _.members r.module.decls `shouldEqual`
        [ [ "Test.User.g" /\ Ret (CLam [ "$q1" ] (Ret (CAtom (var "$q1")))) ] ]

    it "sinks a live single-use cross-module clean call via the dependency's published effects (ADR-0096)" do
      -- Test.Dep.big is pure arithmetic and too large to publish as an inline candidate, so the
      -- effects summary is the only channel that can prove the user's live call movable: it
      -- sinks past the pinned unknown call `g w` to its use site. The chain must be **live**
      -- (each step feeds the next) — an all-dead chain would be swept by the dependency's own
      -- NbE pass, leaving a tiny publishable body and no residual call to sink. Each step is
      -- deliberately **linear** (`prev + 1`, one use of the previous binding): the diamond
      -- spelling (`prev + prev`) trips a pre-existing engine blow-up (the multi-use small-deref
      -- clause compounds 2^depth at quote), which is a separate finding, not this test's topic.
      let
        liveChain :: Int -> String -> Expr
        liveChain 0 prev = Ret (CAtom (var prev))
        liveChain n prev =
          Let ("v" <> show n) (CPrim AddInt [ var prev, AtomLit (LInt 1) ])
            (liveChain (n - 1) ("v" <> show n))

        dep :: AnfModule
        dep = { name: "Test.Dep", decls: [ nonrec "Test.Dep.big" (Ret (CLam [ "x" ] (liveChain 20 "x"))) ] }
        depResult = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv dep) emptyQuarantine dep
        env = extendSummary emptyBuildEnv depResult.summary

        user :: AnfModule
        user =
          { name: "Test.User"
          , decls:
              [ nonrec "Test.User.h"
                  ( Ret
                      ( CLam [ "y" ]
                          ( Let "x" (CApp (var "Test.Dep.big") [ var "y" ])
                              ( Let "z" (CApp (var "g") [ var "y" ])
                                  (Ret (CCtor "T" 2 [ var "x", var "z" ]))
                              )
                          )
                      )
                  )
              ]
          }
        r = optimizeModule env (localFactsOf env user) emptyQuarantine user
      map _.members r.module.decls `shouldEqual`
        [ [ "Test.User.h" /\ Ret
              ( CLam [ "$q1" ]
                  ( Let "$q2" (CApp (var "g") [ var "$q1" ])
                      ( Let "$q3" (CApp (var "Test.Dep.big") [ var "$q1" ])
                          (Ret (CCtor "T" 2 [ var "$q3", var "$q2" ]))
                      )
                  )
              )
          ]
        ]

    it "never sinks a live foreign-shaped call (the ADR-0096 dirty lift, end-to-end)" do
      let
        am :: AnfModule
        am =
          { name: "Test.M"
          , decls:
              [ nonrec "Test.M.f"
                  ( Ret
                      ( CLam [ "x" ]
                          ( Let "a" (CApp (AtomForeign "Test.M.pureImpl") [ var "x" ])
                              ( Let "z" (CApp (var "g") [ var "x" ])
                                  (Ret (CCtor "T" 2 [ var "a", var "z" ]))
                              )
                          )
                      )
                  )
              ]
          }
        lf = (localFactsOf emptyBuildEnv am)
          { foreignSigs = Map.singleton "Test.M.pureImpl" { arity: 1, vsat: false, retVsat: false } }
        r = optimizeModule emptyBuildEnv lf emptyQuarantine am
      map _.members r.module.decls `shouldEqual`
        [ [ "Test.M.f" /\ Ret
              ( CLam [ "$q1" ]
                  ( Let "$q2" (CApp (AtomForeign "Test.M.pureImpl") [ var "$q1" ])
                      ( Let "$q3" (CApp (var "g") [ var "$q1" ])
                          (Ret (CCtor "T" 2 [ var "$q2", var "$q3" ]))
                      )
                  )
              )
          ]
        ]

    it "resolves an imported accessor/instance from the dependency env (extendSummary)" do
      let
        dep :: AnfModule
        dep =
          { name: "Test.Dep"
          , decls: [ accessorDecl "Test.Dep.add", instanceDecl "Test.Dep.semiringInt" ]
          }
        depResult = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv dep) emptyQuarantine dep
        env = extendSummary emptyBuildEnv depResult.summary

        user :: AnfModule
        user =
          { name: "Test.User"
          , decls: [ useDecl "Test.User.three" "Test.Dep.add" "Test.Dep.semiringInt" ]
          }
        r = optimizeModule env (localFactsOf env user) emptyQuarantine user
      map _.members r.module.decls `shouldEqual` [ [ "Test.User.three" /\ collapsed ] ]

    it "threads inline candidates to dependents (ADR-0089 slice 2: extendSummary → gate site A)" do
      -- Test.Dep.inc = \x -> x + 1 (small: publishable); the user's saturated call inlines and
      -- constant-folds across the module boundary.
      let
        dep :: AnfModule
        dep =
          { name: "Test.Dep"
          , decls:
              [ nonrec "Test.Dep.inc"
                  (Ret (CLam [ "x" ] (Ret (CPrim AddInt [ var "x", AtomLit (LInt 1) ]))))
              ]
          }
        depResult = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv dep) emptyQuarantine dep
        env = extendSummary emptyBuildEnv depResult.summary

        user :: AnfModule
        user =
          { name: "Test.User"
          , decls: [ nonrec "Test.User.two" (Ret (CApp (var "Test.Dep.inc") [ AtomLit (LInt 1) ])) ]
          }
        r = optimizeModule env (localFactsOf env user) emptyQuarantine user
      map _.members r.module.decls `shouldEqual`
        [ [ "Test.User.two" /\ Ret (CAtom (AtomLit (LInt 2))) ] ]

    it "publishes the foreign shapes referenced by inline candidates (the private-foreign leak fix)" do
      -- Test.Dep.wrap = \x -> privImpl(x): the wrapper is a published candidate, so its (possibly
      -- non-exported) foreign's shape must travel to consumers; an unreferenced own foreign must not.
      let
        shape = { arity: 1, vsat: false, retVsat: false }

        dep :: AnfModule
        dep =
          { name: "Test.Dep"
          , decls:
              [ nonrec "Test.Dep.wrap"
                  (Ret (CLam [ "x" ] (Ret (CApp (AtomForeign "Test.Dep.privImpl") [ var "x" ]))))
              ]
          }
        lf = (localFactsOf emptyBuildEnv dep)
          { foreignSigs = Map.fromFoldable
              [ "Test.Dep.privImpl" /\ shape
              , "Test.Dep.unusedImpl" /\ { arity: 2, vsat: true, retVsat: false }
              ]
          }
        r = optimizeModule emptyBuildEnv lf emptyQuarantine dep
      publishedForeignSigs r.summary `shouldEqual` Map.singleton "Test.Dep.privImpl" shape

    it "never persists a summary today (the .pmi core stays boot's)" do
      let
        am :: AnfModule
        am = { name: "Test.M", decls: [ instanceDecl "Test.M.semiringInt" ] }
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) emptyQuarantine am
      isNothing (persistedSummary r.summary) `shouldEqual` true

  describe "binding-surface guard (ADR-0099 §4a)" do
    it "the module-level guard re-shares a lambda whose pre-opt kind was non-lambda" do
      -- The raw body here is a lambda, but `outerKinds` is pinned to `OKNonLambda` — simulating
      -- Slice 2, where `Impurify` turns a non-lambda CAF into this lambda *before* NbE, so
      -- `nbeBinding`'s own guard sees a lambda input and does not fire; only the module-level
      -- `enforceOuterKinds`, keyed on the pre-opt kind, catches it. (Deleting `enforceOuterKinds`
      -- fails this test: the output stays a bare `Ret (CLam …)`.)
      let
        am :: AnfModule
        am = { name: "Test.M", decls: [ nonrec "Test.M.f" (Ret (CLam [ "$u" ] (Ret (CAtom (AtomLit (LInt 1)))))) ] }
        lf = (localFactsOf emptyBuildEnv am) { outerKinds = Map.singleton "Test.M.f" OKNonLambda }
        r = optimizeModule emptyBuildEnv lf emptyQuarantine am
        wrapped = Let "$q0" (CLam [ "$q1" ] (Ret (CAtom (AtomLit (LInt 1))))) (Ret (CAtom (var "$q0")))
      map _.members r.module.decls `shouldEqual` [ [ "Test.M.f" /\ wrapped ] ]

    it "keeps the .pmi ExportKind mode-stable: a CAF that optimises to a lambda stays Ecaf" do
      -- f = let g = \x -> x in g — a non-lambda CAF (Ecaf) that NbE reduces to \x -> x; a guard
      -- re-wraps it so its ExportKind stays Ecaf, matching `--no-opt` (the raw decls). Without a
      -- guard the optimised body is a bare lambda → Efn, and the `.pmi` interface diverges by
      -- mode. Compares the `--opt` ExportKind map against the `--no-opt` (raw) one.
      let
        am :: AnfModule
        am =
          { name: "Test.M"
          , decls: [ nonrec "Test.M.f" (Let "g" (CLam [ "x" ] (Ret (CAtom (var "x")))) (Ret (CAtom (var "g")))) ]
          }
        optDecls = (optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) emptyQuarantine am).module.decls
        kindTags decls =
          map (\(k /\ g) -> k /\ kindToTag g)
            (Map.toUnfoldable (gdefKindMap (map classifyDecl decls)))
            :: Array _
      kindTags optDecls `shouldEqual` kindTags am.decls

-- --- round-growth backstop (ADR-0089 self-compile extension) -------------------------------------

-- | A ~50-node *live* builder body (fields keep every padded record alive, so unfolding it
-- | re-materialises the whole thing) that qualifies for the scrutinised-known-arg tier: the dict
-- | parameter is projected, nothing is applied in head position.
bigBuilderDecl :: String -> Decl
bigBuilderDecl k = nonrec k (Ret (CLam [ "d" ] (body 12)))
  where
  body :: Int -> Expr
  body 0 =
    Let "x" (CAccessor (var "d") "f")
      ( Ret
          ( CRecord
              ( [ { prop: "p", val: var "x" } ]
                  <> map (\i -> { prop: "w" <> show i, val: var ("w" <> show i) }) (Array.range 1 12)
              )
          )
      )
  body n = Let ("w" <> show n) (CRecord [ { prop: "k", val: AtomLit (LInt n) } ]) (body (n - 1))

backstopSpec :: Spec Unit
backstopSpec = describe "round-growth backstop (ADR-0089 self-compile extension)" do
  it "keeps the round input when a binding's output grows past the cap, and the summary derives from it" do
    let
      -- ten saturated known-arg calls: each unfold re-materialises ~50 live nodes, so the round
      -- output would sit far past 4x the ~45-node input (and past the floor).
      bigBody :: Expr
      bigBody = calls 10
        where
        calls :: Int -> Expr
        calls 0 = Ret (CRecord (map (\i -> { prop: "r" <> show i, val: var ("r" <> show i) }) (Array.range 1 10)))
        calls n = Let ("r" <> show n) (CApp (var "Test.M.b") [ var "Test.M.dict" ]) (calls (n - 1))

      am :: AnfModule
      am =
        { name: "Test.M"
        , decls:
            [ nonrec "Test.M.dict" (Ret (CRecord [ { prop: "f", val: AtomLit (LInt 1) } ]))
            , bigBuilderDecl "Test.M.b"
            , nonrec "Test.M.big" bigBody
            ]
        }
      r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) emptyQuarantine am
      bigOut = Array.concatMap _.members r.module.decls # Array.find (\(k /\ _) -> k == "Test.M.big")
    -- the backstop kept the (dictElim'd = identical) round input for the exploding binding.
    map (\(_ /\ e) -> e) bigOut `shouldEqual` Just bigBody

-- --- sticky backstop quarantine (ADR-0089 Addendum 2026-07-16) -----------------------------------

-- | `bigBuilderDecl`, parameterised by its padding depth — a different depth is a structurally
-- | different candidate body (the "reachable candidate changed" lever).
bigBuilderDeclN :: Int -> String -> Decl
bigBuilderDeclN pads k = nonrec k (Ret (CLam [ "d" ] (body pads)))
  where
  body :: Int -> Expr
  body 0 =
    Let "x" (CAccessor (var "d") "f")
      ( Ret
          ( CRecord
              ( [ { prop: "p", val: var "x" } ]
                  <> map (\i -> { prop: "w" <> show i, val: var ("w" <> show i) }) (Array.range 1 pads)
              )
          )
      )
  body n = Let ("w" <> show n) (CRecord [ { prop: "k", val: AtomLit (LInt n) } ]) (body (n - 1))

-- | `n` saturated calls to `Test.M.b` around a live record tail (each unfold re-materialises the
-- | builder's ~50 live nodes — the blow-up shape), optionally with a live reference to
-- | `Test.M.eff` (the effect-fact-only lever).
quarantineBigBody :: Int -> Boolean -> Expr
quarantineBigBody n0 withEff =
  if withEff then Let "e0" (CApp (var "Test.M.eff") [ AtomLit (LInt 0) ]) (calls n0)
  else calls n0
  where
  calls :: Int -> Expr
  calls 0 = Ret
    ( CRecord
        ( (if withEff then [ { prop: "e", val: var "e0" } ] else [])
            <> map (\i -> { prop: "r" <> show i, val: var ("r" <> show i) }) (Array.range 1 n0)
        )
    )
  calls n = Let ("r" <> show n) (CApp (var "Test.M.b") [ var "Test.M.dict" ]) (calls (n - 1))

-- | A >64-node padding chain — size-bounded out of candidate publication in **both** spellings —
-- | as a lambda (effect arity 1) or a bare value chain (effect arity 0): at the `RelevantFacts`
-- | level the two differ *only* in their effect fact.
effDecl :: Boolean -> Decl
effDecl asLambda = nonrec "Test.M.eff" (if asLambda then Ret (CLam [ "x" ] chain) else chain)
  where
  chain = pad 20

  pad :: Int -> Expr
  pad 0 = Ret (CAtom (var "p1"))
  pad n = Let ("p" <> show n) (CRecord [ { prop: "k", val: AtomLit (LInt n) } ]) (pad (n - 1))

quarantineAm :: { withEff :: Boolean, calls :: Int } -> AnfModule
quarantineAm opts =
  { name: "Test.M"
  , decls:
      [ nonrec "Test.M.dict" (Ret (CRecord [ { prop: "f", val: AtomLit (LInt 1) } ]))
      , nonrec "Test.M.other" (Ret (CAtom (AtomLit (LInt 1))))
      , bigBuilderDeclN 12 "Test.M.b"
      ]
        <> (if opts.withEff then [ effDecl true ] else [])
        <> [ nonrec "Test.M.big" (quarantineBigBody opts.calls opts.withEff) ]
  }

firedKeys :: Array RejectionEvent -> Array String
firedKeys = Array.mapMaybe case _ of
  BackstopFired r -> Just r.key
  BackstopSummary _ -> Nothing

bigBodyOf :: AnfModule -> Maybe Expr
bigBodyOf m =
  map (\(_ /\ e) -> e)
    (Array.find (\(k /\ _) -> k == "Test.M.big") (Array.concatMap _.members m.decls))

replaceMember :: String -> Expr -> AnfModule -> AnfModule
replaceMember key e m = m { decls = map (\d -> d { members = map rep d.members }) m.decls }
  where
  rep (k /\ _) | k == key = k /\ e
  rep kv = kv

dropMember :: String -> AnfModule -> AnfModule
dropMember key m = m
  { decls = Array.filter (not <<< Array.null <<< _.members)
      (map (\d -> d { members = Array.filter (\(k /\ _) -> k /= key) d.members }) m.decls)
  }

-- | The manual driver loop — successive `optimizeModule` rounds threading module + quarantine,
-- | exactly as `Purvasm.Compiler.build`'s fixpoint does (the driver-side hook dispatch and
-- | summary counting are thin plumbing over these events).
quarantineSpec :: Spec Unit
quarantineSpec = describe "sticky backstop quarantine (ADR-0089 Addendum)" do
  let
    am = quarantineAm { withEff: false, calls: 10 }
    lf = localFactsOf emptyBuildEnv am
    round q m = optimizeModule emptyBuildEnv lf q m
    r1 = round emptyQuarantine am
    r2 = round r1.quarantine r1.module
    r3 = round r2.quarantine r2.module

  it "records the rejection per observation: fire, genuine-fact retry, then skip" do
    firedKeys r1.events `shouldEqual` [ "Test.M.big" ]
    -- round 2: the builder/dict candidates were α-renamed by round 1's own normalisation — a
    -- genuine reachable-fact change, so the quarantine retries (and the retry trips again).
    firedKeys r2.events `shouldEqual` [ "Test.M.big" ]
    -- round 3: everything stable — the attempt is skipped: no event, no rebuild.
    r3.events `shouldEqual` []

  it "the quarantined final term is identical to the memoryless behaviour's" do
    let
      m1 = (round emptyQuarantine am).module
      m2 = (round emptyQuarantine m1).module
      m3 = (round emptyQuarantine m2).module
    r3.module `shouldEqual` m3
    bigBodyOf r3.module `shouldEqual` Just (quarantineBigBody 10 false)

  it "does not re-attempt when an unreachable fact changes (RelevantFacts is restricted)" do
    -- `Test.M.other` is outside big's reachable set; changing it must not invalidate the record.
    let r4 = round r2.quarantine (replaceMember "Test.M.other" (Ret (CAtom (AtomLit (LInt 99)))) r2.module)
    r4.events `shouldEqual` []

  it "re-attempts when a reachable candidate's body changes" do
    let
      builder13 = case Array.head (bigBuilderDeclN 13 "Test.M.b").members of
        Just (_ /\ e) -> e
        Nothing -> Ret (CAtom (AtomLit (LInt 0)))
      r4 = round r2.quarantine (replaceMember "Test.M.b" builder13 r2.module)
    -- the retry ran and (still blowing up under the changed builder) tripped again.
    firedKeys r4.events `shouldEqual` [ "Test.M.big" ]

  it "re-attempts when a candidate disappears (Just → Nothing); a passing retry drops the record" do
    let
      r4 = round r2.quarantine (dropMember "Test.M.b" r2.module)
    -- no candidate to unfold: the retry runs, stays small, passes — no event…
    r4.events `shouldEqual` []
    -- …and NbE really ran (a retry, not a skip): the body left the kept-input form.
    (bigBodyOf r4.module == Just (quarantineBigBody 10 false)) `shouldEqual` false
    -- the record was dropped on success: the next stable round neither skips a record nor fires.
    (round r4.quarantine r4.module).events `shouldEqual` []

  it "re-attempts when only a reachable effect fact changes (candidate Nothing on both sides)" do
    let
      amE = quarantineAm { withEff: true, calls: 12 }
      lfE = localFactsOf emptyBuildEnv amE
      roundE q m = optimizeModule emptyBuildEnv lfE q m
      e1 = roundE emptyQuarantine amE
      e2 = roundE e1.quarantine e1.module
      e3 = roundE e2.quarantine e2.module
    -- sanity: the eff-bearing module reaches the stable skip state like the base one.
    firedKeys e1.events `shouldEqual` [ "Test.M.big" ]
    e3.events `shouldEqual` []
    -- swap the >64-node eff binding from a lambda to a bare value chain: its candidate stays
    -- `Nothing` (size-bounded out of publication) — only its effect fact moves (arity 1 → 0).
    let
      effValue = case Array.head (effDecl false).members of
        Just (_ /\ e) -> e
        Nothing -> Ret (CAtom (AtomLit (LInt 0)))
      e4 = roundE e2.quarantine (replaceMember "Test.M.eff" effValue e2.module)
    firedKeys e4.events `shouldEqual` [ "Test.M.big" ]

-- --- ADR-0089 Addendum: the real-Specialize invalidation path ------------------------------------
--
-- The manual-swap fixture above exercises body-inequality invalidation by hand; this one drives it
-- through the real pipeline: the *rejected* binding carries a specializable call (callee = a
-- dependency-published candidate lambda projecting its dict param; dict argument = a dependency
-- global — the ADR-0093 §1 pins), the backstop keeps the round input (preserving the call site the
-- NbE unfold would have consumed), post-NbE Specialize rewrites that site to a `$spec$` clone
-- inside the kept body, and the next round's pre-NbE input therefore differs from the recorded
-- one — the quarantine's *body* check invalidates and the NbE attempt re-runs.
specializeRetrySpec :: Spec Unit
specializeRetrySpec = describe "quarantine × Specialize (real body-rewrite invalidation)" do
  it "a Specialize-rewritten rejected body re-attempts on the next round" do
    let
      -- the dependency: a record dictionary and a small builder *projecting* its dict param
      -- (the argUses fact Specialize masks on)
      depAm :: AnfModule
      depAm =
        { name: "TP"
        , decls:
            [ nonrec "TP.dictS" (Ret (CRecord [ { prop: "f", val: AtomLit (LInt 1) } ]))
            , nonrec "TP.f"
                ( Ret
                    ( CLam [ "d", "x" ]
                        ( Let "p0" (CAccessor (var "d") "f")
                            (Ret (CRecord [ { prop: "p", val: var "p0" }, { prop: "q", val: var "x" } ]))
                        )
                    )
                )
            ]
        }
      depR = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv depAm) emptyQuarantine depAm
      env = extendSummary emptyBuildEnv depR.summary

      -- the tripping module: the blow-up machinery plus one specializable call inside `big`
      am :: AnfModule
      am =
        { name: "TS"
        , decls:
            [ nonrec "TS.dict" (Ret (CRecord [ { prop: "f", val: AtomLit (LInt 1) } ]))
            , bigBuilderDeclN 12 "TS.b"
            , nonrec "TS.big" bigWithSpecSite
            ]
        }
      bigWithSpecSite =
        Let "s0" (CApp (var "TP.f") [ var "TP.dictS", AtomLit (LInt 7) ])
          (calls 10)
        where
        calls :: Int -> Expr
        calls 0 = Ret
          ( CRecord
              ( [ { prop: "s", val: var "s0" } ]
                  <> map (\i -> { prop: "r" <> show i, val: var ("r" <> show i) }) (Array.range 1 10)
              )
          )
        calls n = Let ("r" <> show n) (CApp (var "TS.b") [ var "TS.dict" ]) (calls (n - 1))

      lf = localFactsOf env am
      r1 = optimizeModule env lf emptyQuarantine am
      r2 = optimizeModule env lf r1.quarantine r1.module

      bigBodyR1 = map (\(_ /\ e) -> e)
        (Array.find (\(k /\ _) -> k == "TS.big") (Array.concatMap _.members r1.module.decls))
      specRefsOf = case _ of
        Just e -> Set.filter isSpecKey (fvExpr Set.empty e)
        Nothing -> Set.empty

    -- round 1: the blow-up trips; the kept body was then rewritten by Specialize —
    -- the module gained a `$spec$` clone, and the *rejected binding itself* references it.
    firedKeys r1.events `shouldEqual` [ "TS.big" ]
    Array.any (\(k /\ _) -> isSpecKey k) (Array.concatMap _.members r1.module.decls)
      `shouldEqual` true
    Set.isEmpty (specRefsOf bigBodyR1) `shouldEqual` false
    -- round 2: the pre-NbE input no longer matches the record (body inequality) — retry fires.
    firedKeys r2.events `shouldEqual` [ "TS.big" ]
