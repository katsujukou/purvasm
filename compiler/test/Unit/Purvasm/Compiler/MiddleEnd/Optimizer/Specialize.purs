-- | ADR-0093 dictionary specialization: the pass is judged through `specializeModule` directly
-- | (syntactic in/out) plus one `optimizeModule` round-trip for the state-free monotonicity claim
-- | (re-running on the output emits nothing new).
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Specialize where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.Ffi (intrinsicPrim)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe (candidatesOf)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (InlineCandidate)
import Purvasm.Compiler.MiddleEnd.Optimizer.Specialize (isSpecKey, specializeModule)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

nonrec :: String -> Expr -> { recursive :: Boolean, members :: Array (Tuple String Expr) }
nonrec k e = { recursive: false, members: [ k /\ e ] }

cand :: Maybe Int -> Expr -> InlineCandidate
cand arity body =
  { arity, size: 30, cxLeqDeref: false, closed: false, argUses: [], group: Set.empty, body }

-- A dependency builder `\d -> let q = Dep.fwd(d) in {out: q}` — the pass-through shape — and the
-- dependency dictionary it is applied to.
deps :: Map String InlineCandidate
deps = Map.fromFoldable
  [ Tuple "Dep.builder"
      ( cand (Just 1)
          ( Ret
              ( CLam [ "d" ]
                  ( Let "q" (CApp (var "Dep.fwd") [ var "d" ])
                      (Ret (CRecord [ { prop: "out", val: var "q" } ]))
                  )
              )
          )
      )
  , Tuple "Dep.dict" (cand Nothing (Ret (CRecord [ { prop: "f", val: int 1 } ])))
  , Tuple "Dep.plain" (cand Nothing (Ret (CAtom (int 0))))
  , Tuple "Dep.binop"
      ( cand (Just 2)
          ( Ret
              ( CLam [ "a", "b" ]
                  (Ret (CRecord [ { prop: "x", val: var "a" }, { prop: "y", val: var "b" } ]))
              )
          )
      )
  , Tuple "Dep.grouped"
      ( (cand (Just 1) (Ret (CLam [ "d" ] (Ret (CRecord [])))))
          { group = Set.fromFoldable [ "Dep.grouped", "Dep.grouped2" ] }
      )
  , Tuple "Dep.groupedAlias"
      ((cand Nothing (Ret (CApp (var "Dep.grouped") [ var "Dep.dict" ]))) { group = Set.singleton "Dep.grouped" })
  ]

run :: Array { recursive :: Boolean, members :: Array (Tuple String Expr) } -> Array { recursive :: Boolean, members :: Array (Tuple String Expr) }
run = specializeModule "M" (Set.singleton "M.use") deps

cloneKey :: String
cloneKey = "M.$spec$Dep$_builder$0_Dep$_dict"

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Optimizer.Specialize" do
  describe "specializeModule (ADR-0093)" do
    it "clones a fully-masked callee as a CAF and rewrites the site to the clone reference" do
      let
        out = run [ nonrec "M.use" (Ret (CApp (var "Dep.builder") [ var "Dep.dict" ])) ]
      map _.members out `shouldEqual`
        [ [ cloneKey /\
              Let "d" (CAtom (var "Dep.dict"))
                ( Let "q" (CApp (var "Dep.fwd") [ var "d" ])
                    (Ret (CRecord [ { prop: "out", val: var "q" } ]))
                )
          ]
        , [ "M.use" /\ Ret (CAtom (var cloneKey)) ]
        ]

    it "dedups two sites of one instantiation into one clone, and is stable on its own output" do
      let
        e = Let "a" (CApp (var "Dep.builder") [ var "Dep.dict" ])
          ( Let "b" (CApp (var "Dep.builder") [ var "Dep.dict" ])
              (Ret (CRecord [ { prop: "l", val: var "a" }, { prop: "r", val: var "b" } ]))
          )
        out = run [ nonrec "M.use" e ]
        cloneCount ds = Array.length (Array.filter (\d -> Array.any (\(Tuple k _) -> isSpecKey k) d.members) ds)
      cloneCount out `shouldEqual` 1
      -- state-free monotonicity: a second pass over the output emits nothing new.
      run out `shouldEqual` out

    it "the same dictionary at a different parameter index is a distinct clone (positional key)" do
      let
        out = run
          [ nonrec "M.u1" (Ret (CApp (var "Dep.binop") [ var "Dep.dict", var "M.x" ]))
          , nonrec "M.u2" (Ret (CApp (var "Dep.binop") [ var "M.x", var "Dep.dict" ]))
          ]
        keys = out >>= \d -> Array.mapMaybe (\(Tuple k _) -> if isSpecKey k then Just k else Nothing) d.members
      keys `shouldEqual`
        [ "M.$spec$Dep$_binop$0_Dep$_dict", "M.$spec$Dep$_binop$1_Dep$_dict" ]

    it "a partially-masked callee keeps its remaining parameters and arguments in order" do
      let
        out = run [ nonrec "M.use" (Ret (CApp (var "Dep.binop") [ var "Dep.dict", var "M.x" ])) ]
        clone = out >>= \d -> Array.mapMaybe (\(Tuple k e) -> if isSpecKey k then Just e else Nothing) d.members
      clone `shouldEqual`
        [ Ret
            ( CLam [ "b" ]
                ( Let "a" (CAtom (var "Dep.dict"))
                    (Ret (CRecord [ { prop: "x", val: var "a" }, { prop: "y", val: var "b" } ]))
                )
            )
        ]
      (out >>= \d -> Array.mapMaybe (\(Tuple k e) -> if k == "M.use" then Just e else Nothing) d.members)
        `shouldEqual` [ Ret (CApp (var "M.$spec$Dep$_binop$0_Dep$_dict") [ var "M.x" ]) ]

    it "a non-dictionary argument alone never specializes" do
      let
        e0 = [ nonrec "M.use" (Ret (CApp (var "Dep.builder") [ var "Dep.plain" ])) ]
      run e0 `shouldEqual` e0

    it "a Rec-group member callee is refused; a grouped alias argument is accepted" do
      let
        asCallee = [ nonrec "M.use" (Ret (CApp (var "Dep.grouped") [ var "Dep.dict" ])) ]
        asArg = [ nonrec "M.use" (Ret (CApp (var "Dep.builder") [ var "Dep.groupedAlias" ])) ]
      run asCallee `shouldEqual` asCallee
      ( Array.length
          ( run asArg >>= \d ->
              Array.filter (\(Tuple k _) -> isSpecKey k) d.members
          )
      ) `shouldEqual` 1

    it "a local dictionary never masks in v1 (CAF-clone init order)" do
      let
        -- "M.use" is in localKeys: a would-be dictionary argument that is module-local.
        e0 = [ nonrec "M.caller" (Ret (CApp (var "Dep.builder") [ var "M.use" ])) ]
      specializeModule "M" (Set.fromFoldable [ "M.use", "M.caller" ])
        (Map.insert "M.use" (cand Nothing (Ret (CRecord []))) deps)
        e0 `shouldEqual` e0

    it "an existing member with the clone's exact key is reused, never duplicated" do
      let
        preexisting = nonrec cloneKey (Ret (CAtom (int 9)))
        out = run
          [ preexisting
          , nonrec "M.use" (Ret (CApp (var "Dep.builder") [ var "Dep.dict" ]))
          ]
      out `shouldEqual`
        [ preexisting
        , nonrec "M.use" (Ret (CAtom (var cloneKey)))
        ]

    it "a wrapper referencing a clone never publishes as a candidate (no cross-module clone leak)" do
      let
        out = run [ nonrec "M.use" (Ret (CApp (var "Dep.builder") [ var "Dep.dict" ])) ]
        -- the post-specialize summary derivation: neither the clone itself nor the rewritten
        -- wrapper (whose body now references the module-private clone) may travel.
        published = candidatesOf intrinsicPrim Map.empty out
      Map.lookup "M.use" published `shouldEqual` Nothing
      Array.filter isSpecKey (Set.toUnfoldable (Map.keys published)) `shouldEqual` []
