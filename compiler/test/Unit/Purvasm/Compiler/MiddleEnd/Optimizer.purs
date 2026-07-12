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
import Purvasm.Compiler.MiddleEnd.Optimizer (emptyBuildEnv, extendSummary, localFactsOf, optimizeModule, persistedSummary, publishedForeignSigs)
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
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) am
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
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) am
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
        r = optimizeModule emptyBuildEnv lf am
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
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) am
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
        depResult = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv dep) dep
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
        r = optimizeModule env (localFactsOf env user) user
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
        depResult = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv dep) dep
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
        r = optimizeModule env (localFactsOf env user) user
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
        r = optimizeModule emptyBuildEnv lf am
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
        depResult = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv dep) dep
        env = extendSummary emptyBuildEnv depResult.summary

        user :: AnfModule
        user =
          { name: "Test.User"
          , decls: [ useDecl "Test.User.three" "Test.Dep.add" "Test.Dep.semiringInt" ]
          }
        r = optimizeModule env (localFactsOf env user) user
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
        depResult = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv dep) dep
        env = extendSummary emptyBuildEnv depResult.summary

        user :: AnfModule
        user =
          { name: "Test.User"
          , decls: [ nonrec "Test.User.two" (Ret (CApp (var "Test.Dep.inc") [ AtomLit (LInt 1) ])) ]
          }
        r = optimizeModule env (localFactsOf env user) user
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
        r = optimizeModule emptyBuildEnv lf dep
      publishedForeignSigs r.summary `shouldEqual` Map.singleton "Test.Dep.privImpl" shape

    it "never persists a summary today (the .pmi core stays boot's)" do
      let
        am :: AnfModule
        am = { name: "Test.M", decls: [ instanceDecl "Test.M.semiringInt" ] }
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) am
      isNothing (persistedSummary r.summary) `shouldEqual` true

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
      r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) am
      bigOut = Array.concatMap _.members r.module.decls # Array.find (\(k /\ _) -> k == "Test.M.big")
    -- the backstop kept the (dictElim'd = identical) round input for the exploding binding.
    map (\(_ /\ e) -> e) bigOut `shouldEqual` Just bigBody
