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

import Data.Maybe (isNothing)
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, Decl)
import Purvasm.Compiler.MiddleEnd.Optimizer (emptyBuildEnv, extendSummary, localFactsOf, optimizeModule, persistedSummary)
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

    it "never persists a summary today (the .pmi core stays boot's)" do
      let
        am :: AnfModule
        am = { name: "Test.M", decls: [ instanceDecl "Test.M.semiringInt" ] }
        r = optimizeModule emptyBuildEnv (localFactsOf emptyBuildEnv am) am
      isNothing (persistedSummary r.summary) `shouldEqual` true
