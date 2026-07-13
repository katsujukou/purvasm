-- | The NbE general inliner's invariants (ADR-0089): the blow-up regression fixtures (§7 — the
-- | known failure shapes as permanent executable tests), the effect-soundness pins (§5), and the
-- | `Simplify` positive/negative suite transferred as NbE equivalents (§8 slice-1 test-transfer —
-- | the retirement gate). The engine α-renames every binder to the reserved `$q<n>` supply, so
-- | expected terms carry `$q` binders (deterministic per binding); free names are verbatim.
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Nbe where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Ffi (intrinsicPrim)
import Purvasm.Compiler.ForeignSig (ForeignShape)
import Purvasm.Compiler.Literal (Literal(..))
import Data.Set as Set
import Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis (EffectFact, liftShape)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Analysis (inlineMarks)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Module (Decl)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe (candidatesOf, nbeBinding, nbeEnvOf)
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

nonrec :: String -> Expr -> Decl
nonrec k e = { recursive: false, members: [ k /\ e ] }

-- ADR-0097 blow-up chains (all levels live; each level uses the previous twice) ------------------

-- v1 = x + x; vN = v(N-1) + v(N-1); ret vN.
primDiamond :: Int -> Expr
primDiamond n = go 1
  where
  v i = var ("v" <> show i)
  go i
    | i > n = Ret (CAtom (v n))
    | otherwise =
        let
          prev = if i == 1 then var "x" else v (i - 1)
        in
          Let ("v" <> show i) (CPrim AddInt [ prev, prev ]) (go (i + 1))

-- w_i = v(i-1); v_i = w_i + w_i — the doubling veiled behind a transparent alias.
aliasDiamond :: Int -> Expr
aliasDiamond n = go 1
  where
  v i = var ("v" <> show i)
  w i = var ("w" <> show i)
  go i
    | i > n = Ret (CAtom (v n))
    | otherwise =
        let
          prev = if i == 1 then var "x" else v (i - 1)
        in
          Let ("w" <> show i) (CAtom prev)
            (Let ("v" <> show i) (CPrim AddInt [ w i, w i ]) (go (i + 1)))

-- l1 = \a -> a + x; l_i = \a -> Pair(l(i-1), l(i-1)) — small non-closed lambdas over the chain.
lamDiamond :: Int -> Expr
lamDiamond n = go 1
  where
  l i = var ("l" <> show i)
  go i
    | i > n = Ret (CAtom (l n))
    | otherwise =
        let
          rhs =
            if i == 1 then CLam [ "a" ] (Ret (CPrim AddInt [ var "a", var "x" ]))
            else CLam [ "a" ] (Ret (CCtor "Pair" 2 [ l (i - 1), l (i - 1) ]))
        in
          Let ("l" <> show i) rhs (go (i + 1))

-- P1 (alias laundering): v_i = { a: prev, b: prev } (KnownSize, single-use → marked); w_i = v_i
-- (Trivial alias, used twice by v_{i+1}). Each v_i's direct use count is 1 (only w_i), so the
-- KnownSize single-use exemption applies — but the alias carries the fan-out. Without policy
-- inheritance (ShareOnly), the alias re-exposes the record as multi-use and the chain is 2^depth.
knownAliasDiamond :: Int -> Expr
knownAliasDiamond n = go 1
  where
  recOf a = CRecord [ { prop: "a", val: a }, { prop: "b", val: a } ]
  go i
    | i > n = Ret (CAtom (var ("w" <> show n)))
    | otherwise =
        let
          prev = if i == 1 then var "x" else var ("w" <> show (i - 1))
        in
          Let ("v" <> show i) (recOf prev)
            (Let ("w" <> show i) (CAtom (var ("v" <> show i))) (go (i + 1)))

-- P2: a small **non-closed** lambda (references free `x`) whose body re-materialises a
-- param-dependent inner small-deref `t = a + a` six times. Its syntactic size is under 16 (the
-- free-var-only `M` keeps it), but its **materialised** body is over 16, so the recursive `M` must
-- strip it when it is used more than once — sharing the lambda instead of duplicating it.
p2Lambda :: Expr
p2Lambda =
  Let "l"
    ( CLam [ "a" ]
        ( Let "t" (CPrim AddInt [ var "a", var "a" ])
            (Ret (CArray [ var "t", var "t", var "t", var "t", var "t", var "t", var "x" ]))
        )
    )
    (Ret (CArray [ var "l", var "l" ]))

-- A clean (pure, store-untouching) single-use call for the ADR-0096 sink oracle.
cleanCallFact :: EffectFact
cleanCallFact = { arity: 1, vsat: false, retVsat: false, mtouch: false, retMtouch: false }

-- Count `CLam` nodes: a lambda that was shared appears once; a duplicated one appears N times.
countLams :: Expr -> Int
countLams = goE
  where
  goE = case _ of
    Ret c -> goC c
    Let _ c rest -> goC c + goE rest
    LetRec bs rest -> Array.foldl (\acc b -> acc + goE b.rhs) 0 bs + goE rest
  goC = case _ of
    CLam _ b -> 1 + goE b
    CIf _ t e -> goE t + goE e
    CCase _ alts -> Array.foldl (\acc a -> acc + goAlt a) 0 alts
    _ -> 0
  goAlt a = case a.result of
    Uncond e -> goE e
    Guarded gs -> Array.foldl (\acc g -> acc + goE g.guard + goE g.rhs) 0 gs

-- A local node counter for the linear-size ceilings (independent of the engine's own size pass).
nodeCount :: Expr -> Int
nodeCount = countE
  where
  countE = case _ of
    Ret c -> countC c
    Let _ c rest -> 1 + countC c + countE rest
    LetRec bs rest -> 1 + Array.foldl (\acc b -> acc + countE b.rhs) 0 bs + countE rest
  countC = case _ of
    CAtom _ -> 1
    CLam _ b -> 1 + countE b
    CApp _ as -> 1 + Array.length as
    CPrim _ as -> 1 + Array.length as
    CCtor _ _ as -> 1 + Array.length as
    CArray as -> 1 + Array.length as
    CRecord fs -> 1 + Array.length fs
    CAccessor _ _ -> 2
    CUpdate _ ups -> 1 + Array.length ups
    CIf _ t e -> 1 + countE t + countE e
    CCase ss alts -> 1 + Array.length ss + Array.foldl (\acc a -> acc + countAlt a) 0 alts
    CPerform _ -> 2
  countAlt a = case a.result of
    Uncond e -> countE e
    Guarded gs -> Array.foldl (\acc g -> acc + countE g.guard + countE g.rhs) 0 gs

-- Normalise one body with no module siblings…
nbe :: Expr -> Expr
nbe = nbeWith []

-- …or against the given sibling decls (the gate-site-A channel)…
nbeWith :: Array Decl -> Expr -> Expr
nbeWith decls = nbeBinding (nbeEnvOf intrinsicPrim Map.empty decls) (const Nothing) "Test.binding"

-- …or with foreign-shape effect facts (ADR-0095's drop oracle — shapes lift **dirty**, so no
-- motion can ever come from these, ADR-0096 §1)…
nbeEffects :: Map.Map String ForeignShape -> Expr -> Expr
nbeEffects sigs = nbeBinding (nbeEnvOf intrinsicPrim Map.empty []) (\k -> liftShape <$> Map.lookup k sigs) "Test.binding"

-- …or with full effect facts (the shape a dependency's structural summary arrives in — the
-- ADR-0096 sink oracle)…
nbeFacts :: Map.Map String EffectFact -> Expr -> Expr
nbeFacts facts = nbeBinding (nbeEnvOf intrinsicPrim Map.empty []) (\k -> Map.lookup k facts) "Test.binding"

-- …or against dependency decls published through the slice-2 candidate channel, plus local
-- sibling decls (mirrors `optimizeModule`'s wiring: deps ride `BuildEnv.inlines`, and the
-- closedness classifier sees every module's top-level keys).
nbeCross :: Array Decl -> Array Decl -> Expr -> Expr
nbeCross depDecls locals =
  nbeBinding (nbeEnvOf intrinsicPrim (candidatesOf intrinsicPrim Map.empty depDecls) locals) (const Nothing) "Test.binding"

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Optimizer.Nbe" do
  describe "blow-up fixtures (ADR-0089 §7)" do
    it "diamond: a multi-use non-reducing binding stays one shared let per level (never 2^depth)" do
      -- let d1 = {a: base, b: base} in let d2 = {a: d1, b: d1} in {a: d2, b: d2}
      let
        recOf a = CRecord [ { prop: "a", val: a }, { prop: "b", val: a } ]
      nbe
        ( Let "d1" (recOf (var "base"))
            (Let "d2" (recOf (var "d1")) (Ret (recOf (var "d2"))))
        )
        `shouldEqual`
          Let "$q1" (recOf (var "base"))
            (Let "$q2" (recOf (var "$q1")) (Ret (recOf (var "$q2"))))

    it "a large dispatch-shaped sibling stays a call at the gate (the 0001 blow-up class)" do
      -- M.big = \x -> <a 20-deep let chain, size ≥ 64>: every gate-A clause fails.
      let
        chain 0 = Ret (CAtom (var "x"))
        chain n = Let ("v" <> show n) (CPrim AddInt [ var "x", var "x" ]) (chain (n - 1))
        big = nonrec "M.big" (Ret (CLam [ "x" ] (chain 20)))
        call = Ret (CApp (var "M.big") [ var "y" ])
      nbeWith [ big ] call `shouldEqual` call

    it "positive control: a small sibling unfolds and case-of-known-constructor collapses" do
      -- M.small = \x -> case x of Just(v) -> v; _ -> 0  — applied to a known Just(5).
      let
        small = nonrec "M.small"
          ( Ret
              ( CLam [ "x" ]
                  ( Ret
                      ( CCase [ var "x" ]
                          [ { binders: [ BCtor "Just" [ BVar "v" ] ]
                            , result: Uncond (Ret (CAtom (var "v")))
                            }
                          , { binders: [ BNull ], result: Uncond (Ret (CAtom (int 0))) }
                          ]
                      )
                  )
              )
          )
      nbeWith [ small ]
        ( Let "j" (CCtor "Just" 1 [ int 5 ])
            (Ret (CApp (var "M.small") [ var "j" ]))
        )
        `shouldEqual` Ret (CAtom (int 5))

    it "effect-reorder trap: pinned neutral calls keep their sequencing, even used in reverse" do
      let
        trap =
          Let "a" (CApp (var "f") [ var "x" ])
            ( Let "b" (CApp (var "g") [ var "y" ])
                (Ret (CPrim AddInt [ var "b", var "a" ]))
            )
      nbe trap `shouldEqual`
        Let "$q1" (CApp (var "f") [ var "x" ])
          ( Let "$q2" (CApp (var "g") [ var "y" ])
              (Ret (CPrim AddInt [ var "$q2", var "$q1" ]))
          )

    it "a dead neutral call is kept absent purity facts (the conservative default)" do
      nbe (Let "a" (CApp (var "f") [ var "x" ]) (Ret (CAtom (int 1))))
        `shouldEqual` Let "$q1" (CApp (var "f") [ var "x" ]) (Ret (CAtom (int 1)))

  describe "materialised-size audit (ADR-0097 §4)" do
    -- The prim-version of the diamond: vN = v(N-1) + v(N-1), every level live. The small-deref
    -- clause marks each level for re-materialisation; without the audit the marks compound
    -- `2^depth` and quote stack-overflows (crashed at depth 12). The audit strips the over-budget
    -- multi-use marks, restoring the kept/stripped/kept alternation.
    it "the depth-3 diamond alternates kept/stripped/kept exactly" do
      -- v1 kept (M=3): materialised as the two `x+x`; v2 stripped (M=7≥5): the shared `$q3`;
      -- v3 kept: re-materialised into the tail as `$q3 + $q3` (= v2 + v2).
      nbe (primDiamond 3)
        `shouldEqual`
          Let "$q1" (CPrim AddInt [ var "x", var "x" ])
            ( Let "$q2" (CPrim AddInt [ var "x", var "x" ])
                ( Let "$q3" (CPrim AddInt [ var "$q1", var "$q2" ])
                    (Ret (CPrim AddInt [ var "$q3", var "$q3" ]))
                )
            )

    it "the depth-4 diamond shares the doubling node instead of copying it 2^depth times" do
      -- v2 shared as `$q3`; v3 kept, re-materialised twice (`$q4`, `$q5`) into v4's two uses.
      nbe (primDiamond 4)
        `shouldEqual`
          Let "$q1" (CPrim AddInt [ var "x", var "x" ])
            ( Let "$q2" (CPrim AddInt [ var "x", var "x" ])
                ( Let "$q3" (CPrim AddInt [ var "$q1", var "$q2" ])
                    ( Let "$q4" (CPrim AddInt [ var "$q3", var "$q3" ])
                        ( Let "$q5" (CPrim AddInt [ var "$q3", var "$q3" ])
                            (Ret (CPrim AddInt [ var "$q4", var "$q5" ]))
                        )
                    )
                )
            )

    it "positive control: a one-level multi-use small prim still re-materialises (M=3 < 5)" do
      -- let v1 = x + x in v1 + v1 — the small-deref clause's intended win, preserved: v1 is
      -- inlined at both use sites (never shared) because its materialised size stays under bound.
      nbe (Let "v1" (CPrim AddInt [ var "x", var "x" ]) (Ret (CPrim AddInt [ var "v1", var "v1" ])))
        `shouldEqual`
          Let "$q1" (CPrim AddInt [ var "x", var "x" ])
            (Let "$q2" (CPrim AddInt [ var "x", var "x" ]) (Ret (CPrim AddInt [ var "$q1", var "$q2" ])))

    -- The crash regressions: at these depths the pre-audit engine 2^depth-materialises and
    -- stack-overflows *the whole suite* inside quote — a loud pin. Post-audit the term stays
    -- linear in depth, so a small linear ceiling proves the exponential is gone.
    it "the prim diamond at depth 20 normalises to a linear-size term (never 2^depth)" do
      (nodeCount (nbe (primDiamond 20)) < 8 * 20) `shouldEqual` true

    it "the alias-veiled diamond is caught through the rename (transparency)" do
      -- w_i = v(i-1); v_i = w_i + w_i — the doubling hides behind an alias, yet M(w)=M(v(i-1))
      -- carries the size through, so the audit strips at the same nodes.
      (nodeCount (nbe (aliasDiamond 20)) < 8 * 20) `shouldEqual` true

    it "the lambda-clause diamond stays bounded (small non-closed lambdas over marked binders)" do
      -- l_i = \a -> Pair(l(i-1), l(i-1)) — the small-lambda clause's version of the same chain.
      (nodeCount (nbe (lamDiamond 20)) < 16 * 20) `shouldEqual` true

    it "P1: a KnownSize construction laundered through aliases stays linear (policy inheritance)" do
      -- w_i = v_i (alias) carries the fan-out that v_i's single-use exemption dodges; the alias must
      -- inherit v_i's ShareOnly policy and strip at multi-use, or the record chain re-explodes 2^depth.
      (nodeCount (nbe (knownAliasDiamond 20)) < 8 * 20) `shouldEqual` true

    it "P1: a single-use clean call is not duplicated through a multi-use alias" do
      -- x = clean(a) (single-use → ADR-0096 sink); w = x; T(w, w). The alias inherits the call's
      -- ShareOnly policy and is shared, so the call materialises once — never duplicated at both uses.
      nbeFacts (Map.singleton "M.clean" cleanCallFact)
        ( Let "x" (CApp (var "M.clean") [ var "a" ])
            (Let "w" (CAtom (var "x")) (Ret (CCtor "T" 2 [ var "w", var "w" ])))
        )
        `shouldEqual`
          Let "$q1" (CApp (var "M.clean") [ var "a" ])
            (Ret (CCtor "T" 2 [ var "$q1", var "$q1" ]))

    it "P2: a small non-closed lambda whose materialised body exceeds the bound is shared, not copied" do
      -- \a -> let t = a+a in [t×6, x]: syntactic size < 16 but the materialised body is larger, and
      -- the lambda is used twice. The recursive materialised size (not the free-var approximation)
      -- strips the mark, so the optimised term holds the lambda once (shared), not two copies.
      countLams (nbe p2Lambda) `shouldEqual` 1

    it "P1(motion): a call embedded in a value used twice is shared, not duplicated (same capture)" do
      -- x = clean(a) (single-use); p = x.field; [p, p]. `p` embeds the call, so it inherits the
      -- call's CapBranch motion cap and is stripped at multi-use even at CapNone — else the call
      -- would execute once per `p` copy. The call is bound once; `p` is the shared projection.
      nbeFacts (Map.singleton "M.clean" cleanCallFact)
        ( Let "x" (CApp (var "M.clean") [ var "a" ])
            (Let "p" (CAccessor (var "x") "field") (Ret (CArray [ var "p", var "p" ])))
        )
        `shouldEqual`
          Let "$q1" (CApp (var "M.clean") [ var "a" ])
            (Let "$q2" (CAccessor (var "$q1") "field") (Ret (CArray [ var "$q2", var "$q2" ])))

    it "P1(motion): a call embedded in a value used inside a lambda stays shared outside it" do
      -- x = clean(a); r = {value: x}; l = \u -> r; [l, l]. Not an alias — `r` embeds the call. The
      -- record inherits the call's ShareOnly + CapBranch motion cap; `r` is used at CapClosure
      -- (inside l), which exceeds CapBranch, so `r` is shared *outside* the lambda. The call is bound
      -- once at the top (never duplicated, never moved into the closure — the ADR-0096 boundary).
      nbeFacts (Map.singleton "M.clean" cleanCallFact)
        ( Let "x" (CApp (var "M.clean") [ var "a" ])
            ( Let "r" (CRecord [ { prop: "value", val: var "x" } ])
                (Let "l" (CLam [ "u" ] (Ret (CAtom (var "r")))) (Ret (CArray [ var "l", var "l" ])))
            )
        )
        `shouldEqual`
          Let "$q1" (CApp (var "M.clean") [ var "a" ])
            ( Let "$q2" (CRecord [ { prop: "value", val: var "$q1" } ])
                ( Let "$q4" (CLam [ "$q3" ] (Ret (CAtom (var "$q2"))))
                    (Let "$q6" (CLam [ "$q5" ] (Ret (CAtom (var "$q2")))) (Ret (CArray [ var "$q4", var "$q6" ])))
                )
            )

  describe "GER run marker: CPerform (ADR-0099 slice 1)" do
    it "perform of a known unit-lambda β-reduces to the body in place" do
      -- let t = \$u -> 42 in perform t  ==>  42
      nbe (Let "t" (CLam [ "$u" ] (Ret (CAtom (int 42)))) (Ret (CPerform (var "t"))))
        `shouldEqual` Ret (CAtom (int 42))

    it "perform of an unknown thunk stays a pinned CPerform marker" do
      -- \m -> perform m  stays  \m -> perform m  (never dissolved to `m unit`)
      nbe (Ret (CLam [ "m" ] (Ret (CPerform (var "m")))))
        `shouldEqual` Ret (CLam [ "$q1" ] (Ret (CPerform (var "$q1"))))

    it "a neutral perform is sequenced in place and its result is usable" do
      -- \m -> let r = perform m in r
      nbe (Ret (CLam [ "m" ] (Let "r" (CPerform (var "m")) (Ret (CAtom (var "r"))))))
        `shouldEqual`
          Ret (CLam [ "$q1" ] (Let "$q2" (CPerform (var "$q1")) (Ret (CAtom (var "$q2")))))

    it "a dead neutral perform is kept (a pinned effect barrier, not dropped)" do
      -- let a = perform t in 1 — `a` is unused, but performing `t` is an effect that must run.
      nbe (Let "a" (CPerform (var "t")) (Ret (CAtom (int 1))))
        `shouldEqual` Let "$q1" (CPerform (var "t")) (Ret (CAtom (int 1)))

  describe "dead-drop with purity facts (ADR-0095 §3, the dead-only branch)" do
    it "fires: a dead pure call is dropped in place" do
      nbeEffects (Map.singleton "M.pureFn" { arity: 1, vsat: false, retVsat: false })
        (Let "a" (CApp (var "M.pureFn") [ var "x" ]) (Ret (CAtom (int 1))))
        `shouldEqual` Ret (CAtom (int 1))

    it "a dead Effect-thunk construction is dropped (I1: construction ≠ execution)" do
      -- let a = log s in 1 — builds the thunk, never forces it.
      nbeEffects (Map.singleton "M.log" { arity: 1, vsat: false, retVsat: true })
        (Let "a" (CApp (var "M.log") [ var "s" ]) (Ret (CAtom (int 1))))
        `shouldEqual` Ret (CAtom (int 1))

    it "a dead effectful force is kept (the thunk's saturation may perform)" do
      -- let t = log s in let u = t unit in 1 — u's rhs forces the thunk (over-application of
      -- t's arity-0 summary → may-perform); t stays live through u.
      nbeEffects (Map.singleton "M.log" { arity: 1, vsat: false, retVsat: true })
        ( Let "t" (CApp (var "M.log") [ var "s" ])
            (Let "u" (CApp (var "t") [ int 0 ]) (Ret (CAtom (int 1))))
        )
        `shouldEqual`
          Let "$q1" (CApp (var "M.log") [ var "s" ])
            (Let "$q2" (CApp (var "$q1") [ int 0 ]) (Ret (CAtom (int 1))))

    it "a dead bare-EffectFnN saturation is kept (vsat: saturating it IS the effect)" do
      nbeEffects (Map.singleton "M.effectFn2" { arity: 2, vsat: true, retVsat: false })
        (Let "a" (CApp (var "M.effectFn2") [ var "x", var "y" ]) (Ret (CAtom (int 1))))
        `shouldEqual`
          Let "$q1" (CApp (var "M.effectFn2") [ var "x", var "y" ]) (Ret (CAtom (int 1)))

    it "no-motion guard: a live single-use pure call never sinks, even across a SetArray" do
      -- let a = M.read arr in let w = SetArray arr 0 1 in a — sinking `a` past the write would
      -- read the mutated cell (ADR-0095 §3: vsat=false does not license motion).
      nbeEffects (Map.singleton "M.read" { arity: 1, vsat: false, retVsat: false })
        ( Let "a" (CApp (var "M.read") [ var "arr" ])
            (Let "w" (CPrim SetArray [ var "arr", int 0, int 1 ]) (Ret (CAtom (var "a"))))
        )
        `shouldEqual`
          Let "$q1" (CApp (var "M.read") [ var "arr" ])
            (Let "$q2" (CPrim SetArray [ var "arr", int 0, int 1 ]) (Ret (CAtom (var "$q1"))))

    it "the guard's dual: the same callee's *dead* call is dropped (nothing observes it)" do
      nbeEffects (Map.singleton "M.read" { arity: 1, vsat: false, retVsat: false })
        ( Let "a" (CApp (var "M.read") [ var "arr" ])
            (Let "w" (CPrim SetArray [ var "arr", int 0, int 1 ]) (Ret (CAtom (int 1))))
        )
        `shouldEqual` Let "$q1" (CPrim SetArray [ var "arr", int 0, int 1 ]) (Ret (CAtom (int 1)))

  describe "single-use clean-call sink (ADR-0096 §2)" do
    let
      cleanFact :: EffectFact
      cleanFact = { arity: 1, vsat: false, retVsat: false, mtouch: false, retMtouch: false }

      readerFact :: EffectFact
      readerFact = { arity: 1, vsat: false, retVsat: false, mtouch: true, retMtouch: true }

      dirty2Fact :: EffectFact
      dirty2Fact = { arity: 2, vsat: false, retVsat: false, mtouch: true, retMtouch: true }

      mkReaderFact :: EffectFact
      mkReaderFact = { arity: 1, vsat: false, retVsat: false, mtouch: false, retMtouch: true }

    it "fires (CapNone): a live single-use clean call sinks past a pinned call to its use site" do
      nbeFacts (Map.singleton "M.clean" cleanFact)
        ( Let "x" (CApp (var "M.clean") [ var "a" ])
            ( Let "z" (CApp (var "g") [ var "w" ])
                (Ret (CCtor "T" 2 [ var "x", var "z" ]))
            )
        )
        `shouldEqual`
          Let "$q1" (CApp (var "g") [ var "w" ])
            ( Let "$q2" (CApp (var "M.clean") [ var "a" ])
                (Ret (CCtor "T" 2 [ var "$q2", var "$q1" ]))
            )

    it "fires (CapBranch): a clean call sinks into the branch arm that uses it" do
      -- the sole use is the arm's tail, so the sunk computation reifies as the arm's tail call —
      -- executed only on the path that uses it (the conditional-execution win).
      nbeFacts (Map.singleton "M.clean" cleanFact)
        ( Let "x" (CApp (var "M.clean") [ var "a" ])
            (Ret (CIf (var "c") (Ret (CAtom (var "x"))) (Ret (CAtom (int 0)))))
        )
        `shouldEqual`
          Ret
            ( CIf (var "c")
                (Ret (CApp (var "M.clean") [ var "a" ]))
                (Ret (CAtom (int 0)))
            )

    it "multi-use stays pinned (duplication)" do
      let
        term =
          Let "x" (CApp (var "M.clean") [ var "a" ])
            (Ret (CCtor "T" 2 [ var "x", var "x" ]))
      nbeFacts (Map.singleton "M.clean" cleanFact) term
        `shouldEqual`
          Let "$q1" (CApp (var "M.clean") [ var "a" ])
            (Ret (CCtor "T" 2 [ var "$q1", var "$q1" ]))

    it "CapClosure stays pinned (multi-execution)" do
      let
        term =
          Let "x" (CApp (var "M.clean") [ var "a" ])
            (Ret (CLam [ "u" ] (Ret (CAtom (var "x")))))
      nbeFacts (Map.singleton "M.clean" cleanFact) term
        `shouldEqual`
          Let "$q1" (CApp (var "M.clean") [ var "a" ])
            (Ret (CLam [ "$q2" ] (Ret (CAtom (var "$q1")))))

    it "a store-reading callee never sinks, even live single-use across a SetArray" do
      let
        term =
          Let "r" (CApp (var "M.reader") [ var "arr" ])
            (Let "w" (CPrim SetArray [ var "arr", int 0, int 1 ]) (Ret (CAtom (var "r"))))
      nbeFacts (Map.singleton "M.reader" readerFact) term
        `shouldEqual`
          Let "$q1" (CApp (var "M.reader") [ var "arr" ])
            (Let "$q2" (CPrim SetArray [ var "arr", int 0, int 1 ]) (Ret (CAtom (var "$q1"))))

    it "PAP residual: the PAP construction is droppable when dead, its saturation never sinks" do
      -- dead PAP of a dirty callee: dropped (clean construction).
      nbeFacts (Map.singleton "M.dirty2" dirty2Fact)
        (Let "g" (CApp (var "M.dirty2") [ var "a" ]) (Ret (CAtom (int 1))))
        `shouldEqual` Ret (CAtom (int 1))
      -- live residual saturation: `g b` is exact on the PAP's summary and carries the callee's
      -- dirt — no sink even at single use under a branch.
      nbeFacts (Map.singleton "M.dirty2" dirty2Fact)
        ( Let "g" (CApp (var "M.dirty2") [ var "a" ])
            ( Let "x" (CApp (var "g") [ var "b" ])
                (Ret (CIf (var "c") (Ret (CAtom (var "x"))) (Ret (CAtom (int 0)))))
            )
        )
        `shouldEqual`
          Let "$q1" (CApp (var "M.dirty2") [ var "a" ])
            ( Let "$q2" (CApp (var "$q1") [ var "b" ])
                (Ret (CIf (var "c") (Ret (CAtom (var "$q2"))) (Ret (CAtom (int 0)))))
            )

    it "retMtouch pins the result's exact saturation at the marks level (bidirectional)" do
      -- `x = r []` is a zero-argument application — the only *exact* saturation an arity-0
      -- result summary admits. Eval collapses it to an alias (`applySem f [] = f`), so the
      -- normalised *term* cannot witness the shift (the outer clean call legitimately sinks
      -- either way); the **mark decision** can: with the correct `retMtouch → mtouch` shift
      -- `x` is never marked, and with the shift broken (retMtouch dropped) it would be.
      let
        term =
          Let "r" (CApp (var "M.mkReader") [ var "arr" ])
            ( Let "x" (CApp (var "r") [])
                (Ret (CIf (var "c") (Ret (CAtom (var "x"))) (Ret (CAtom (int 0)))))
            )
        marksWith fact = inlineMarks (\k -> Map.lookup k (Map.singleton "M.mkReader" fact)) term
      -- correct shift: the result's saturation is dirty → no sink mark on x.
      Set.member "x" (marksWith mkReaderFact) `shouldEqual` false
      -- the broken-shift simulation (a ret-clean closure returner): x would be marked — the
      -- assertion that makes a silent retMtouch regression fail loudly.
      Set.member "x" (marksWith (mkReaderFact { retMtouch = false })) `shouldEqual` true

    it "reader-closure return: the clean outer call may move, the returned closure's may not" do
      -- r = mkReader a (clean saturation, retMtouch dirty); x = r u (over-applies r's arity-0
      -- summary → pinned). The hazard shifted retMtouch → mtouch across the exact application.
      let
        term =
          Let "r" (CApp (var "M.mkReader") [ var "arr" ])
            ( Let "x" (CApp (var "r") [ int 0 ])
                (Ret (CIf (var "c") (Ret (CAtom (var "x"))) (Ret (CAtom (int 0)))))
            )
      nbeFacts (Map.singleton "M.mkReader" mkReaderFact) term
        `shouldEqual`
          Let "$q1" (CApp (var "M.mkReader") [ var "arr" ])
            ( Let "$q2" (CApp (var "$q1") [ int 0 ])
                (Ret (CIf (var "c") (Ret (CAtom (var "$q2"))) (Ret (CAtom (int 0)))))
            )

    it "a foreign-shaped pure callee never sinks (the dirty lift — ADR-0096 §1)" do
      let
        term =
          Let "x" (CApp (var "M.pure") [ var "a" ])
            (Ret (CIf (var "c") (Ret (CAtom (var "x"))) (Ret (CAtom (int 0)))))
      nbeEffects (Map.singleton "M.pure" { arity: 1, vsat: false, retVsat: false }) term
        `shouldEqual`
          Let "$q1" (CApp (var "M.pure") [ var "a" ])
            (Ret (CIf (var "c") (Ret (CAtom (var "$q1"))) (Ret (CAtom (int 0)))))

    it "a recursive group member is never unfolded (recursion stays a call)" do
      let
        loop =
          LetRec [ { var: "go", rhs: Ret (CLam [ "i" ] (Ret (CApp (var "go") [ var "i" ]))) } ]
            (Ret (CApp (var "go") [ int 0 ]))
      nbe loop `shouldEqual`
        LetRec [ { var: "$q1", rhs: Ret (CLam [ "$q2" ] (Ret (CApp (var "$q1") [ var "$q2" ]))) } ]
          (Ret (CApp (var "$q1") [ int 0 ]))

    it "a recursive sibling decl is never published for unfolding" do
      let
        recDecl = { recursive: true, members: [ "M.r" /\ Ret (CAtom (AtomForeign "Data.Semiring.intAdd")) ] }
        call = Ret (CApp (var "M.r") [ var "p", var "q" ])
      nbeWith [ recDecl ] call `shouldEqual` call

  describe "reductions" do
    it "constant-folds a primop on literals (VM-exact)" do
      nbe (Ret (CPrim AddInt [ int 2, int 3 ])) `shouldEqual` Ret (CAtom (int 5))
      nbe (Ret (CPrim DivInt [ int 7, int 0 ])) `shouldEqual` Ret (CAtom (int 0))

    it "folds a projection on a known record through a shared let (round 2)" do
      nbe
        ( Let "r" (CRecord [ { prop: "f", val: int 7 } ])
            (Ret (CAccessor (var "r") "f"))
        )
        `shouldEqual` Ret (CAtom (int 7))

    it "drops a dead value binding" do
      nbe (Let "r" (CRecord [ { prop: "f", val: int 7 } ]) (Ret (CAtom (int 1))))
        `shouldEqual` Ret (CAtom (int 1))

    it "keeps a case whose decidable match lands on a guarded alternative (guard order observable)" do
      let
        guarded =
          Let "j" (CCtor "Just" 1 [ int 5 ])
            ( Ret
                ( CCase [ var "j" ]
                    [ { binders: [ BCtor "Just" [ BVar "v" ] ]
                      , result: Guarded [ { guard: Ret (CAtom (var "p")), rhs: Ret (CAtom (var "v")) } ]
                      }
                    ]
                )
            )
      nbe guarded `shouldEqual`
        Let "$q1" (CCtor "Just" 1 [ int 5 ])
          ( Ret
              ( CCase [ var "$q1" ]
                  [ { binders: [ BCtor "Just" [ BVar "$q2" ] ]
                    , result: Guarded [ { guard: Ret (CAtom (var "p")), rhs: Ret (CAtom (var "$q2")) } ]
                    }
                  ]
              )
          )

  describe "Simplify test-transfer (ADR-0089 §8: the retirement gate)" do
    it "copy-propagation: drops `let x = <atom>` and resolves x to the atom" do
      nbe (Let "x" (CAtom (int 7)) (Ret (CAtom (var "x"))))
        `shouldEqual` Ret (CAtom (int 7))

    it "copy-propagation: chases an alias chain to the underlying atom" do
      nbe
        ( Let "x" (CAtom (var "y"))
            (Let "z" (CAtom (var "x")) (Ret (CPrim AddInt [ var "z", var "z" ])))
        )
        `shouldEqual` Ret (CPrim AddInt [ var "y", var "y" ])

    it "sinks a single-use pure primop to its use site (Simplify kept it; sinking is strictly finer)" do
      nbe (Let "x" (CPrim AddInt [ var "a", var "b" ]) (Ret (CAtom (var "x"))))
        `shouldEqual` Ret (CPrim AddInt [ var "a", var "b" ])

    it "inlines a flat, parameter-closed callee at a fully-applied call (binding drops when dead)" do
      nbe
        ( Let "f" (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ])))
            (Ret (CApp (var "f") [ var "p", var "q" ]))
        )
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "reduces an under-applied call to the specialised closure (β on the partial application)" do
      -- the body was CAF-shaped, so the specialised lambda is re-shared under $q0 (the P1
      -- binding-surface guard: ExportKind must stay mode-stable).
      nbe
        ( Let "f" (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ])))
            (Ret (CApp (var "f") [ var "p" ]))
        )
        `shouldEqual`
          Let "$q0" (CLam [ "$q1" ] (Ret (CPrim AddInt [ var "p", var "$q1" ])))
            (Ret (CAtom (var "$q0")))

    it "inlines a non-flat callee (nested if) — the capability Simplify's flat gate could not express" do
      nbe
        ( Let "f"
            (CLam [ "a" ] (Ret (CIf (var "a") (Ret (CAtom (int 1))) (Ret (CAtom (int 0))))))
            (Ret (CApp (var "f") [ var "p" ]))
        )
        `shouldEqual` Ret (CIf (var "p") (Ret (CAtom (int 1))) (Ret (CAtom (int 0))))

    it "scope: an alias never resolves through a shadowing case binder (α-renamed apart)" do
      -- alt 1 rebinds `x` (must shadow the outer alias); alt 2 does not (the alias applies → y).
      nbe
        ( Let "x" (CAtom (var "y"))
            ( Ret
                ( CCase [ var "s" ]
                    [ { binders: [ BCtor "Just" [ BVar "x" ] ]
                      , result: Uncond (Ret (CPrim AddInt [ var "x", var "x" ]))
                      }
                    , { binders: [ BNull ], result: Uncond (Ret (CAtom (var "x"))) }
                    ]
                )
            )
        )
        `shouldEqual`
          Ret
            ( CCase [ var "s" ]
                [ { binders: [ BCtor "Just" [ BVar "$q1" ] ]
                  , result: Uncond (Ret (CPrim AddInt [ var "$q1", var "$q1" ]))
                  }
                , { binders: [ BNull ], result: Uncond (Ret (CAtom (var "y"))) }
                ]
            )

    it "folds an irrefutable single-BVar case even on an unknown scrutinee" do
      -- `case s of x -> x` ≡ `s`: the row is decidable regardless of the scrutinee's value.
      nbe
        ( Ret
            ( CCase [ var "s" ]
                [ { binders: [ BVar "x" ], result: Uncond (Ret (CAtom (var "x"))) } ]
            )
        )
        `shouldEqual` Ret (CAtom (var "s"))

    it "collapses an exactly-saturated intrinsic-foreign call to its primop (both spellings)" do
      nbe (Ret (CApp (AtomForeign "Data.Semiring.intAdd") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])
      nbe (Ret (CApp (var "Data.Semiring.intAdd") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "saturates an intrinsic through a local alias spine" do
      nbe
        ( Let "f" (CAtom (AtomForeign "Data.Semiring.intAdd"))
            (Ret (CApp (var "f") [ var "p", var "q" ]))
        )
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "leaves an under- or over-applied intrinsic call to the link-time closure" do
      let under = Ret (CApp (AtomForeign "Data.Semiring.intAdd") [ var "p" ])
      let over = Ret (CApp (AtomForeign "Data.Semiring.intAdd") [ var "p", var "q", var "r" ])
      nbe under `shouldEqual` under
      nbe over `shouldEqual` over

    it "leaves a non-intrinsic foreign call untouched" do
      let e = Ret (CApp (AtomForeign "Effect.Console.log") [ var "p" ])
      nbe e `shouldEqual` e

    it "collapses a call through a sibling top-level alias (the floated dictionary application)" do
      nbeWith [ nonrec "M.add1" (Ret (CAtom (AtomForeign "Data.Semiring.intAdd"))) ]
        (Ret (CApp (var "M.add1") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "inlines a small sibling top-level function at a saturated call" do
      nbeWith [ nonrec "M.f" (Ret (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ])))) ]
        (Ret (CApp (var "M.f") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

  describe "review pins (P1/P2)" do
    it "a CAF body never becomes a bare lambda (the .pmi ExportKind stays mode-stable)" do
      -- f = let g = \x -> x in g  reduces to the lambda; the binding surface re-shares it under
      -- the reserved $q0 so classifyDecl/ExportKind (and the .pmi hash) match --no-opt.
      let
        caf =
          Let "g" (CLam [ "x" ] (Ret (CAtom (var "x"))))
            (Ret (CAtom (var "g")))
        wrapped =
          Let "$q0" (CLam [ "$q1" ] (Ret (CAtom (var "$q1"))))
            (Ret (CAtom (var "$q0")))
      nbe caf `shouldEqual` wrapped
      -- and the wrap is a fixpoint across driver rounds
      nbe wrapped `shouldEqual` wrapped

    it "applies marks discovered on an already-normal input (a later DictElim can expose them)" do
      -- The input is already $q-normal, so the first quote is the identity — the sinkable pure
      -- primop must still be found and applied, not skipped by premature convergence.
      nbe (Let "$q1" (CPrim AddInt [ var "a", var "b" ]) (Ret (CAtom (var "$q1"))))
        `shouldEqual` Ret (CPrim AddInt [ var "a", var "b" ])

    it "a large multi-use lambda over globals is not 'closed' (no unconditional inline)" do
      let
        chain 0 = Ret (CAtom (var "v1"))
        chain n = Let ("v" <> show n) (CApp (var "M.g") [ var "x" ]) (chain (n - 1))
        e =
          Let "f" (CLam [ "x" ] (chain 5))
            (Ret (CRecord [ { prop: "a", val: var "f" }, { prop: "b", val: var "f" } ]))
        sharedLambda = case nbe e of
          Let _ (CLam _ _) _ -> true
          _ -> false
      sharedLambda `shouldEqual` true

  describe "cross-module candidates (ADR-0089 slice 2)" do
    it "inlines a small dependency lambda at a saturated call" do
      nbeCross [ nonrec "D.f" (Ret (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ])))) ] []
        (Ret (CApp (var "D.f") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "publishes a strictly under-applied pure partial application with its residual arity" do
      -- D.add1 = intAdd(1): residual arity 1; a saturated call through it constant-folds.
      nbeCross [ nonrec "D.add1" (Ret (CApp (AtomForeign "Data.Semiring.intAdd") [ int 1 ])) ] []
        (Ret (CApp (var "D.add1") [ int 41 ]))
        `shouldEqual` Ret (CAtom (int 42))

    it "never publishes a saturated CAF application (an init-once computation)" do
      -- D.x = D.g(1, 2) executes at init; inlining it would re-execute it per use site.
      let
        deps =
          [ nonrec "D.g" (Ret (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ]))))
          , nonrec "D.x" (Ret (CApp (var "D.g") [ int 1, int 2 ]))
          ]
        call = Ret (CApp (var "D.x") [ var "p" ])
      nbeCross deps [] call `shouldEqual` call

    it "publishes the lambda inside a slice-1 binding-surface wrap" do
      nbeCross
        [ nonrec "D.w"
            ( Let "$q0" (CLam [ "x" ] (Ret (CPrim AddInt [ var "x", int 1 ])))
                (Ret (CAtom (var "$q0")))
            )
        ]
        []
        (Ret (CApp (var "D.w") [ int 41 ]))
        `shouldEqual` Ret (CAtom (int 42))

    it "collapses a local partial-application CAF over a dependency target (the floated-wrapper shape)" do
      -- dep: D.lt = \d a b -> d(a, b); local sibling: L.lt1 = D.lt(L.cmp); the call saturates
      -- through both hops and lands on the (unknown) comparator.
      let
        deps = [ nonrec "D.lt" (Ret (CLam [ "d", "a", "b" ] (Ret (CApp (var "d") [ var "a", var "b" ])))) ]
        locals = [ nonrec "L.lt1" (Ret (CApp (var "D.lt") [ var "L.cmp" ])) ]
      nbeCross deps locals (Ret (CApp (var "L.lt1") [ var "p", var "q" ]))
        `shouldEqual` Ret (CApp (var "L.cmp") [ var "p", var "q" ])

  describe "structural guest terms (compiler-global rung)" do
    -- ADR-0094: the `Data.Ord.*Impl` / `eqArrayImpl` / `arrayMap` guest terms are retired — the
    -- ulib shadow bodies fold through the ordinary candidate channel instead; the positive is the
    -- E2E fold-parity harness (real `dist/ulib` artifacts). The retired keys must now be plain
    -- native-leaf-shaped: untouched by any compiler-global rung.
    it "a retired sliced key is no longer a structural rung (ADR-0094)" do
      let e = Ret (CApp (AtomForeign "Data.Ord.ordIntImpl") [ var "l", var "e", var "g", int 1, int 2 ])
      nbe e `shouldEqual` e

    it "does NOT unfold a GER-owned Effect combinator: pureE left the structural rung (ADR-0099 §3)" do
      -- `Effect.pureE`/`bindE` are GER-owned — the structural rung no longer serves them (Impurify
      -- lowers them to `CPerform` ANF instead), so NbE leaves a saturated `Effect.pureE a u`
      -- untouched for GER, rather than unfolding the guest term to `a`.
      let e = Ret (CApp (AtomForeign "Effect.pureE") [ var "a", var "u" ])
      nbe e `shouldEqual` e

    it "resolves the literal builtins (Prim.undefined) so superclass forcing β-reduces" do
      nbe
        ( Let "g" (CLam [ "$u" ] (Ret (CAtom (int 7))))
            (Ret (CApp (var "g") [ var "Prim.undefined" ]))
        )
        `shouldEqual` Ret (CAtom (int 7))

    it "leaves a native leaf untouched (not in any compiler-global rung)" do
      let e = Ret (CApp (AtomForeign "Data.Show.showIntImpl") [ var "p" ])
      nbe e `shouldEqual` e

  describe "let-wrapped value CAFs (publication through pure-value chains)" do
    it "peeks a published record CAF through its chain and β-reduces the projected method" do
      -- D.ord = let cmp = \x y -> LtInt(x,y) in { compare: cmp } — the real ulib instance shape.
      let
        deps =
          [ nonrec "D.ord"
              ( Let "cmp" (CLam [ "x", "y" ] (Ret (CPrim LtInt [ var "x", var "y" ])))
                  (Ret (CRecord [ { prop: "compare", val: var "cmp" } ]))
              )
          ]
        consumer =
          Let "f" (CAccessor (var "D.ord") "compare")
            (Ret (CApp (var "f") [ var "p", var "q" ]))
      nbeCross deps [] consumer `shouldEqual` Ret (CPrim LtInt [ var "p", var "q" ])

    it "does not publish a chain containing a computation binding" do
      -- the let is a call: an init-once computation — the whole binding stays opaque.
      let
        deps =
          [ nonrec "D.bad"
              ( Let "r" (CApp (var "D.mk") [ int 1 ])
                  (Ret (CRecord [ { prop: "f", val: var "r" } ]))
              )
          ]
        consumer = Ret (CAccessor (var "D.bad") "f")
      nbeCross deps [] consumer `shouldEqual` consumer

  describe "fold-guaranteed case-of-case (ADR-0089 Addendum, slice 3)" do
    it "distributes through the nested comparison tree and folds every leaf (the fib shape)" do
      -- let r = LtInt(a,b)
      -- in let s = case r of true -> LT; _ -> (let q = EqInt(a,b) in case q of true -> EQ; _ -> GT)
      -- in case s of LT -> true; _ -> false
      let
        ordering tag = CCtor tag 0 []
        inner =
          CCase [ var "r" ]
            [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (ordering "LT")) }
            , { binders: [ BNull ]
              , result: Uncond
                  ( Let "q" (CPrim EqInt [ var "a", var "b" ])
                      ( Ret
                          ( CCase [ var "q" ]
                              [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (ordering "EQ")) }
                              , { binders: [ BNull ], result: Uncond (Ret (ordering "GT")) }
                              ]
                          )
                      )
                  )
              }
            ]
        outer =
          [ { binders: [ BCtor "LT" [] ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
          , { binders: [ BNull ], result: Uncond (Ret (CAtom (AtomLit (LBool false)))) }
          ]
        e =
          Let "r" (CPrim LtInt [ var "a", var "b" ])
            (Let "s" inner (Ret (CCase [ var "s" ] outer)))
      -- distribution + constant-arm collapse (the EqInt sub-case: both leaves false) + dead-pure
      -- drop (the now-unused EqInt binding) + boolean case-eta compose to a single primop.
      nbe e `shouldEqual` Ret (CPrim LtInt [ var "a", var "b" ])

    it "blocks when any leaf is undecidable (an unknown value against a constructor row)" do
      let
        e =
          Let "s"
            ( CCase [ var "c" ]
                [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CCtor "LT" 0 [])) }
                , { binders: [ BNull ], result: Uncond (Ret (CAtom (var "unknown"))) }
                ]
            )
            ( Ret
                ( CCase [ var "s" ]
                    [ { binders: [ BCtor "LT" [] ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
                    , { binders: [ BNull ], result: Uncond (Ret (CAtom (AtomLit (LBool false)))) }
                    ]
                )
            )
        out = nbe e
        stillNested = case out of
          Let _ (CCase _ _) (Ret (CCase _ _)) -> true
          _ -> false
      stillNested `shouldEqual` true

    it "bounded tier: a small leaf-independent non-atom RHS distributes under the budget" do
      let
        e =
          Let "s"
            ( CCase [ var "c" ]
                [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CCtor "LT" 0 [])) }
                , { binders: [ BNull ], result: Uncond (Ret (CCtor "GT" 0 [])) }
                ]
            )
            ( Ret
                ( CCase [ var "s" ]
                    [ { binders: [ BCtor "LT" [] ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
                    , { binders: [ BNull ]
                      , result: Uncond (Ret (CPrim AddInt [ var "a", var "big" ]))
                      }
                    ]
                )
            )
      nbe e `shouldEqual`
        Ret
          ( CCase [ var "c" ]
              [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
              , { binders: [ BNull ], result: Uncond (Ret (CPrim AddInt [ var "a", var "big" ])) }
              ]
          )

    it "bounded tier: blocked when the copied total exceeds the budget" do
      let
        -- pinned calls: never dropped or moved, so the RHS size is stable under the engine.
        chain 0 = Ret (CPrim AddInt [ var "a", var "big" ])
        chain n = Let ("v" <> show n) (CApp (var "g") [ var "a" ]) (chain (n - 1))
        e =
          Let "s"
            ( CCase [ var "c" ]
                [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CCtor "LT" 0 [])) }
                , { binders: [ BNull ], result: Uncond (Ret (CCtor "GT" 0 [])) }
                ]
            )
            ( Ret
                ( CCase [ var "s" ]
                    [ { binders: [ BCtor "LT" [] ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
                    , { binders: [ BNull ], result: Uncond (chain 5) }
                    ]
                )
            )
        stillNested = case nbe e of
          Let _ (CCase _ _) (Ret (CCase _ _)) -> true
          _ -> false
      stillNested `shouldEqual` true

    it "bounded tier: blocked on a binder-consuming row (leaf-independence)" do
      -- the BVar row's variable occurs in its RHS: folding would substitute the leaf value into
      -- the copy — duplication the budget cannot count.
      let
        e =
          Let "s"
            ( CCase [ var "c" ]
                [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CCtor "LT" 0 [])) }
                , { binders: [ BNull ], result: Uncond (Ret (CCtor "GT" 0 [])) }
                ]
            )
            ( Ret
                ( CCase [ var "s" ]
                    [ { binders: [ BVar "x" ]
                      , result: Uncond (Ret (CPrim AddInt [ var "x", var "x" ]))
                      }
                    ]
                )
            )
        stillNested = case nbe e of
          Let _ (CCase _ _) (Ret (CPrim _ _)) -> true -- the irrefutable outer folds via case-eta…
          Let _ (CCase _ _) (Ret (CCase _ _)) -> true
          _ -> false
      stillNested `shouldEqual` true

    it "bounded tier: an effectful let inside a copied RHS stays on its single path" do
      let
        e =
          Let "s"
            ( CCase [ var "c" ]
                [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CCtor "LT" 0 [])) }
                , { binders: [ BNull ], result: Uncond (Ret (CCtor "GT" 0 [])) }
                ]
            )
            ( Ret
                ( CCase [ var "s" ]
                    [ { binders: [ BCtor "LT" [] ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
                    , { binders: [ BNull ]
                      , result: Uncond (Let "eff" (CApp (var "f") [ var "y" ]) (Ret (CAtom (var "eff"))))
                      }
                    ]
                )
            )
      nbe e `shouldEqual`
        Ret
          ( CCase [ var "c" ]
              [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
              , { binders: [ BNull ]
                , result: Uncond (Let "$q1" (CApp (var "f") [ var "y" ]) (Ret (CAtom (var "$q1"))))
                }
              ]
          )

    it "blocks when the outer alternatives are guarded (ADR-0013 order)" do
      let
        e =
          Let "s"
            ( CCase [ var "c" ]
                [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CCtor "LT" 0 [])) }
                , { binders: [ BNull ], result: Uncond (Ret (CCtor "GT" 0 [])) }
                ]
            )
            ( Ret
                ( CCase [ var "s" ]
                    [ { binders: [ BCtor "LT" [] ]
                      , result: Guarded [ { guard: Ret (CAtom (var "g")), rhs: Ret (CAtom (AtomLit (LBool true))) } ]
                      }
                    , { binders: [ BNull ], result: Uncond (Ret (CAtom (AtomLit (LBool false)))) }
                    ]
                )
            )
        out = nbe e
        stillNested = case out of
          Let _ (CCase _ _) (Ret (CCase _ _)) -> true
          _ -> false
      stillNested `shouldEqual` true

  describe "boolean-case rules (ADR-0089 Addendum extension)" do
    it "case-eta: the boolean identity case reduces to its scrutinee" do
      nbe
        ( Ret
            ( CCase [ var "b" ]
                [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
                , { binders: [ BNull ], result: Uncond (Ret (CAtom (AtomLit (LBool false)))) }
                ]
            )
        )
        `shouldEqual` Ret (CAtom (var "b"))

    it "case-eta: the negated form reduces to NotBool" do
      nbe
        ( Ret
            ( CCase [ var "b" ]
                [ { binders: [ BLit (LBool true) ], result: Uncond (Ret (CAtom (AtomLit (LBool false)))) }
                , { binders: [ BNull ], result: Uncond (Ret (CAtom (AtomLit (LBool true)))) }
                ]
            )
        )
        `shouldEqual` Ret (CPrim NotBool [ var "b" ])

    it "constant-arm collapse: all arms the same literal with an irrefutable trailing row" do
      nbe
        ( Ret
            ( CCase [ var "x" ]
                [ { binders: [ BCtor "A" [] ], result: Uncond (Ret (CAtom (int 0))) }
                , { binders: [ BNull ], result: Uncond (Ret (CAtom (int 0))) }
                ]
            )
        )
        `shouldEqual` Ret (CAtom (int 0))

    it "constant-arm collapse: blocked without an irrefutable trailing row (stuck must survive)" do
      let
        e = Ret
          ( CCase [ var "x" ]
              [ { binders: [ BCtor "A" [] ], result: Uncond (Ret (CAtom (int 0))) }
              , { binders: [ BCtor "B" [] ], result: Uncond (Ret (CAtom (int 0))) }
              ]
          )
      nbe e `shouldEqual` e

    it "constant-arm collapse: blocked when a row introduces a binding" do
      -- (the case survives; its binder is `$q`-renamed like any residual binder)
      nbe
        ( Ret
            ( CCase [ var "x" ]
                [ { binders: [ BCtor "A" [ BVar "v" ] ], result: Uncond (Ret (CAtom (int 0))) }
                , { binders: [ BNull ], result: Uncond (Ret (CAtom (int 0))) }
                ]
            )
        )
        `shouldEqual`
          Ret
            ( CCase [ var "x" ]
                [ { binders: [ BCtor "A" [ BVar "$q1" ] ], result: Uncond (Ret (CAtom (int 0))) }
                , { binders: [ BNull ], result: Uncond (Ret (CAtom (int 0))) }
                ]
            )

  describe "determinism" do
    it "normalising twice is the identity on the normal form (stable $q numbering)" do
      let
        e = nbe
          ( Let "d" (CRecord [ { prop: "a", val: var "base" }, { prop: "b", val: var "base" } ])
              (Ret (CRecord [ { prop: "a", val: var "d" }, { prop: "b", val: var "d" } ]))
          )
      nbe e `shouldEqual` e

  describe "nbeEnvOf" do
    it "publishes only non-recursive value/lambda bodies" do
      let
        decls =
          [ nonrec "M.lam" (Ret (CLam [ "x" ] (Ret (CAtom (var "x")))))
          , nonrec "M.alias" (Ret (CAtom (var "M.lam")))
          , nonrec "M.caf" (Ret (CApp (var "M.lam") [ int 1 ]))
          , { recursive: true, members: [ "M.rec" /\ Ret (CLam [ "x" ] (Ret (CAtom (var "x")))) ] }
          ]
        env = nbeEnvOf intrinsicPrim Map.empty decls
      -- membership probed through nbeBinding behaviour instead of map internals:
      -- the lambda inlines, the computation CAF and the recursive member stay calls.
      nbeBinding env (const Nothing) "t" (Ret (CApp (var "M.lam") [ var "p" ])) `shouldEqual` Ret (CAtom (var "p"))
      nbeBinding env (const Nothing) "t" (Ret (CApp (var "M.caf") [ var "p" ]))
        `shouldEqual` Ret (CApp (var "M.caf") [ var "p" ])
      nbeBinding env (const Nothing) "t" (Ret (CApp (var "M.rec") [ var "p" ]))
        `shouldEqual` Ret (CApp (var "M.rec") [ var "p" ])

  describe "parameterized-instance unfolding (ADR-0089 Accepted extension)" do
    let
      -- a two-member mutual builder group: `bind` is group-free once selected, `Apply0` is the
      -- superclass thunk carrying the (group-stopped) sibling call.
      groupDecls =
        [ { recursive: true
          , members:
              [ "M.b1" /\ Ret
                  ( CLam [ "d" ]
                      ( Let "f" (CLam [ "x" ] (Ret (CAtom (var "x"))))
                          ( Let "t" (CLam [ "u" ] (Ret (CApp (var "M.b2") [ var "d" ])))
                              (Ret (CRecord [ { prop: "bind", val: var "f" }, { prop: "Apply0", val: var "t" } ]))
                          )
                      )
                  )
              , "M.b2" /\ Ret
                  ( CLam [ "d" ]
                      ( Let "g" (CLam [ "y" ] (Ret (CApp (var "M.b1") [ var "d" ])))
                          (Ret (CRecord [ { prop: "apply", val: var "g" } ]))
                      )
                  )
              ]
          }
        ]
      idLam = Let "$q0" (CLam [ "$q1" ] (Ret (CAtom (var "$q1")))) (Ret (CAtom (var "$q0")))

    it "projecting a group-free field folds the builder application" do
      nbeCross groupDecls []
        (Let "q" (CApp (var "M.b1") [ var "M.dict" ]) (Ret (CAccessor (var "q") "bind")))
        `shouldEqual` idLam

    it "a selected field referencing a group sibling refuses the commit (term stable)" do
      let
        stable = Let "$q1" (CApp (var "M.b1") [ var "M.dict" ]) (Ret (CAccessor (var "$q1") "Apply0"))
      nbeCross groupDecls []
        (Let "q" (CApp (var "M.b1") [ var "M.dict" ]) (Ret (CAccessor (var "q") "Apply0")))
        `shouldEqual` stable
      nbeCross groupDecls [] stable `shouldEqual` stable

    it "a bare saturated application with no consuming projection is never deferred" do
      nbeCross groupDecls []
        (Let "q" (CApp (var "M.b1") [ var "M.dict" ]) (Ret (CAtom (var "q"))))
        `shouldEqual` Let "$q1" (CApp (var "M.b1") [ var "M.dict" ]) (Ret (CAtom (var "$q1")))

    it "a multi-use application keeps the single shared let (sharing never lost)" do
      nbeCross groupDecls []
        ( Let "q" (CApp (var "M.b1") [ var "M.dict" ])
            (Ret (CRecord [ { prop: "a", val: var "q" }, { prop: "b", val: var "q" } ]))
        )
        `shouldEqual`
          Let "$q1" (CApp (var "M.b1") [ var "M.dict" ])
            (Ret (CRecord [ { prop: "a", val: var "$q1" }, { prop: "b", val: var "$q1" } ]))

    it "the CAF-split alias spelling folds through the published saturated application" do
      let deps = groupDecls <> [ nonrec "M.alias" (Ret (CApp (var "M.b1") [ var "M.dict" ])) ]
      nbeCross deps [] (Ret (CAccessor (var "M.alias") "bind")) `shouldEqual` idLam

    it "a projection inside a lambda never defers (refusal would move the pinned app inward)" do
      -- the sole use of `q` is a projection, but it sits under a lambda: deferring would, on the
      -- Apply0 refusal, reify `M.b1(M.dict)` inside the lambda — re-materialising the pinned
      -- construction per call. The outer shared let must survive at its own level.
      nbeCross groupDecls []
        ( Let "q" (CApp (var "M.b1") [ var "M.dict" ])
            (Ret (CLam [ "k" ] (Ret (CAccessor (var "q") "Apply0"))))
        )
        `shouldEqual`
          Let "$q1" (CApp (var "M.b1") [ var "M.dict" ])
            (Ret (CLam [ "$q2" ] (Ret (CAccessor (var "$q1") "Apply0"))))

    it "a projection inside a case arm never defers" do
      let
        e0 = Let "q" (CApp (var "M.b1") [ var "M.dict" ])
          ( Ret
              ( CCase [ var "s" ]
                  [ { binders: [ BCtor "C" [] ], result: Uncond (Ret (CAccessor (var "q") "bind")) }
                  , { binders: [ BNull ], result: Uncond (Ret (CAtom (int 0))) }
                  ]
              )
          )
        stable = Let "$q1" (CApp (var "M.b1") [ var "M.dict" ])
          ( Ret
              ( CCase [ var "s" ]
                  [ { binders: [ BCtor "C" [] ], result: Uncond (Ret (CAccessor (var "$q1") "bind")) }
                  , { binders: [ BNull ], result: Uncond (Ret (CAtom (int 0))) }
                  ]
              )
          )
      nbeCross groupDecls [] e0 `shouldEqual` stable

    it "a self-recursive builder never unfolds inside itself" do
      let
        selfDecls =
          [ { recursive: true
            , members:
                [ "M.self" /\ Ret
                    ( CLam [ "d" ]
                        ( Let "s" (CLam [ "u" ] (Ret (CApp (var "M.self") [ var "d" ])))
                            (Ret (CRecord [ { prop: "go", val: var "s" } ]))
                        )
                    )
                ]
            }
          ]
        stable = Let "$q1" (CApp (var "M.self") [ var "d0" ]) (Ret (CAccessor (var "$q1") "go"))
      nbeCross selfDecls []
        (Let "q" (CApp (var "M.self") [ var "d0" ]) (Ret (CAccessor (var "q") "go")))
        `shouldEqual` stable

  describe "nullary Rec-group dict CAF projection (ADR-0098)" do
    let
      -- The real optimized `Effect` instance shape: a *nullary* mutually-recursive dict group
      -- (no dict parameter, unlike the parameterized builders above). `bindD` carries the
      -- group-free foreign method fields (`bind`/`pure`) plus a superclass thunk `Apply0` that
      -- references the group sibling `applyD`; `applyD` closes the cycle back to `bindD`.
      dictGroup =
        [ { recursive: true
          , members:
              [ "M.bindD" /\
                  Let "t" (CLam [ "u" ] (Ret (CAtom (var "M.applyD"))))
                    ( Ret
                        ( CRecord
                            [ { prop: "bind", val: AtomForeign "Effect.bindE" }
                            , { prop: "pure", val: AtomForeign "Effect.pureE" }
                            , { prop: "Apply0", val: var "t" }
                            ]
                        )
                    )
              , "M.applyD" /\ Ret (CRecord [ { prop: "apply", val: var "M.bindD" } ])
              ]
          }
        ]
      -- Same shape but the chain carries a *computation* binding (`r = M.eff x`): `pureRecordTail`
      -- must reject it, so the CAF is never published and its projection cannot fold.
      compGroup =
        [ { recursive: true
          , members:
              [ "M.cbindD" /\
                  Let "r" (CApp (var "M.eff") [ var "x" ])
                    (Ret (CRecord [ { prop: "bind", val: AtomForeign "Effect.bindE" }, { prop: "self", val: var "M.cbindD" } ]))
              ]
          }
        ]

    it "positive: a group-free method field folds to its structural foreign (bindE / pureE)" do
      nbeCross dictGroup [] (Ret (CAccessor (var "M.bindD") "bind"))
        `shouldEqual` Ret (CAtom (AtomForeign "Effect.bindE"))
      nbeCross dictGroup [] (Ret (CAccessor (var "M.bindD") "pure"))
        `shouldEqual` Ret (CAtom (AtomForeign "Effect.pureE"))

    it "GER un-unfolded: an under-saturated exposed bindE stays a recognisable node (not the thunk term)" do
      -- `bindE` is arity 3 (the structural guest term); an `Effect` do-block applies only 2, so the
      -- structural rung must NOT unfold it — the residual is the `Effect.bindE m k` node GER reads.
      nbeCross dictGroup []
        (Let "f" (CAccessor (var "M.bindD") "bind") (Ret (CApp (var "f") [ var "m", var "k" ])))
        `shouldEqual` Ret (CApp (AtomForeign "Effect.bindE") [ var "m", var "k" ])

    it "sibling-referencing field (Apply0) refuses the commit and stays the original projection" do
      let stable = Ret (CAccessor (var "M.bindD") "Apply0")
      nbeCross dictGroup [] stable `shouldEqual` stable
      nbeCross dictGroup [] stable `shouldEqual` stable -- term stability on re-run

    it "whole-dict demand stays a reference (the record is never inlined)" do
      let e = Ret (CApp (var "someConsumer") [ var "M.bindD" ])
      nbeCross dictGroup [] e `shouldEqual` e

    it "computation-let non-publication (the P1 predicate): a computation-chain CAF never folds" do
      let e = Ret (CAccessor (var "M.cbindD") "bind")
      nbeCross compGroup [] e `shouldEqual` e

  describe "scrutinised-known-arg 64-tier + backstop (ADR-0089 self-compile extension)" do
    let
      -- Pad a body with dead pure lets so the candidate sits in the 16..63 band (below 16 the
      -- unconditional tier would mask what this describe tests).
      pad 0 e = e
      pad n e = Let ("w" <> show (n :: Int)) (CRecord []) (pad (n - 1) e)

      dictDecl = nonrec "M.dict" (Ret (CRecord [ { prop: "f", val: int 1 } ]))

      -- b1 projects its dict and forwards it to b2 (the pass-through the clause allows);
      -- b2 only projects. Both are 16..63 nodes with no parameter applied in head position.
      builderChain =
        [ dictDecl
        , nonrec "M.b1"
            ( Ret
                ( CLam [ "d" ]
                    ( pad 8
                        ( Let "x" (CAccessor (var "d") "f")
                            ( Let "fwd" (CApp (var "M.b2") [ var "d" ])
                                (Ret (CRecord [ { prop: "out", val: var "x" }, { prop: "next", val: var "fwd" } ]))
                            )
                        )
                    )
                )
            )
        , nonrec "M.b2"
            ( Ret
                ( CLam [ "d" ]
                    (pad 8 (Let "y" (CAccessor (var "d") "f") (Ret (CRecord [ { prop: "out", val: var "y" } ]))))
                )
            )
        ]

    it "positive: a known-record argument with a projected param folds (incl. the SRef peek and the forwarding chain)" do
      let
        out = nbeCross builderChain [] (Ret (CApp (var "M.b1") [ var "M.dict" ]))
        expected =
          Let "$q1" (CRecord [ { prop: "out", val: int 1 } ])
            (Ret (CRecord [ { prop: "out", val: int 1 }, { prop: "next", val: var "$q1" } ]))
      out `shouldEqual` expected

    it "negative: a lambda argument never qualifies" do
      let
        e0 = Let "l" (CLam [ "z" ] (Ret (CAtom (var "z"))))
          (Ret (CApp (var "M.b1") [ var "l" ]))
        out = nbeCross builderChain [] e0
      -- the call survives; run the output again to pin it as the fixpoint.
      nbeCross builderChain [] out `shouldEqual` out

    it "negative: an applied parameter refuses even with a known-record argument elsewhere-projected (mixed combinator)" do
      let
        -- like a real combinator, M.c references a sibling (so it is not strictly closed and the
        -- plain 64-tier stays shut) and applies its second parameter in head position.
        mixed = builderChain <>
          [ nonrec "M.c"
              ( Ret
                  ( CLam [ "cfg", "p" ]
                      ( pad 8
                          ( Let "x" (CAccessor (var "cfg") "f")
                              ( Let "s" (CApp (var "M.b2") [ var "cfg" ])
                                  (Let "r" (CApp (var "p") [ var "x" ]) (Ret (CAtom (var "r"))))
                              )
                          )
                      )
                  )
              )
          ]
        e0 = Let "l" (CLam [ "z" ] (Ret (CAtom (var "z"))))
          (Ret (CApp (var "M.c") [ var "M.dict", var "l" ]))
        out = nbeCross mixed [] e0
      -- the call survives (the universal appliedHead conjunct refuses); pin the fixpoint.
      nbeCross mixed [] out `shouldEqual` out
      (out /= Ret (CAtom (int 1))) `shouldEqual` true
