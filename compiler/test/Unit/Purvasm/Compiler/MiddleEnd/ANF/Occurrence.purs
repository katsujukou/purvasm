-- | ADR-0114 §1: the annotator that mints call-occurrence and function identities.
-- |
-- | These rows pin the properties the drill rests on. Two of them are the ones an implementation is
-- | most likely to get quietly wrong — `CPerform` being annotated at all, and stack safety over a
-- | long `Let` spine — and both fail loudly here rather than as a shortfall in a census months later.
module Test.Unit.Purvasm.Compiler.MiddleEnd.ANF.Occurrence where

import Prelude

import Data.Array as Array
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Set as Set
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExprF(..), Expr, ExprF(..), RhsF(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.MiddleEnd.ANF.Occurrence (AnfFunctionId, AnfOccurrenceId, Annotated, annotateObject, functionInt, occurrenceInt)
import Purvasm.Compiler.Literal (Literal(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

-- | A one-body object. The producer is object-wide (ADR-0114: an id is unique within an object,
-- | not within a term), so a single term is passed as an object containing it — there is no
-- | per-term entry point to reach for, deliberately.
annotate1 :: forall f c. ExprF f c -> Annotated
annotate1 e = case Array.head (annotateObject [ Tuple unit e ]) of
  Just (Tuple _ a) -> a
  Nothing -> unsafeCrashWith "annotate1: annotateObject dropped the only body"

-- | The collector's own work stack — an INDEPENDENT case tree from the annotator's.
-- |
-- | Deliberately not the annotator's traversal: a helper that reused it would share any node the
-- | annotator forgets to descend into, and would then report "every occurrence is annotated" about
-- | the ones it also failed to look at. Two independent walks disagreeing is the evidence; one walk
-- | agreeing with itself is not.
-- |
-- | Stack-safe for the same reason the annotator is, and it has to be: the first version recursed on
-- | the `Let` spine and overflowed at 100k BEFORE the annotator could be judged, so the row meant to
-- | prove stack safety was measuring the measuring device.
data Work
  = WE (ExprF AnfFunctionId AnfOccurrenceId)
  | WC (CExprF AnfFunctionId AnfOccurrenceId)
  | WR (RhsF AnfFunctionId AnfOccurrenceId)

-- | Walk an annotated term, accumulating with the caller's step function.
walkAnnotated :: forall a. (a -> Work -> a) -> a -> ExprF AnfFunctionId AnfOccurrenceId -> a
walkAnnotated f z0 e0 = tailRec step { acc: z0, work: WE e0 : Nil }
  where
  push items rest = foldl (flip Cons) rest (Array.reverse items)
  step st = case st.work of
    Nil -> Done st.acc
    Cons w rest ->
      let
        acc' = f st.acc w
      in
        case w of
          WE (Ret c) -> Loop { acc: acc', work: WC c : rest }
          WE (Let _ c b) -> Loop { acc: acc', work: WC c : WE b : rest }
          WE (LetRec bs b) -> Loop { acc: acc', work: push (map (\r -> WE r.rhs) bs) (WE b : rest) }
          WC (CLam _ _ b) -> Loop { acc: acc', work: WE b : rest }
          WC (CIf _ t e) -> Loop { acc: acc', work: WE t : WE e : rest }
          WC (CCase _ alts) -> Loop { acc: acc', work: push (map (\alt -> WR alt.result) alts) rest }
          WC _ -> Loop { acc: acc', work: rest }
          WR (Uncond e) -> Loop { acc: acc', work: WE e : rest }
          WR (Guarded gs) ->
            Loop { acc: acc', work: push (gs >>= \g -> [ WE g.guard, WE g.rhs ]) rest }

-- | Every call-occurrence id in an ALREADY-annotated term, in the order the walk meets them. Kept
-- | apart from `occsOf` because a body that came out of `annotateObject` must be read, never
-- | re-annotated: re-annotating restarts the supply and would report every body as if it were the
-- | object's first, which is the exact confusion the object-wide supply exists to prevent.
occsIn :: ExprF AnfFunctionId AnfOccurrenceId -> Array Int
occsIn = walkAnnotated collect []
  where
  collect acc = case _ of
    WC (CApp o _ _) -> Array.snoc acc (occurrenceInt o)
    WC (CPerform o _) -> Array.snoc acc (occurrenceInt o)
    _ -> acc

-- | Annotate a one-body object and read its occurrence ids.
occsOf :: forall f c. ExprF f c -> Array Int
occsOf e0 = occsIn (annotate1 e0).expr

-- | Every FUNCTION id, root included.
fnsOf :: forall f c. ExprF f c -> Array Int
fnsOf e0 =
  let
    a = annotate1 e0
  in
    Array.cons (functionInt a.root) (walkAnnotated collect [] a.expr)
  where
  collect acc = case _ of
    WC (CLam f _ _) -> Array.snoc acc (functionInt f)
    _ -> acc

-- | The distinct occurrence ids, and their extremes — what the wide row checks instead of building
-- | and de-duplicating a 100k array (`Array.nub` is quadratic and would time the suite out).
occStats :: forall f c. ExprF f c -> { count :: Int, distinct :: Int, lo :: Int, hi :: Int }
occStats e0 =
  walkAnnotated step { count: 0, distinct: 0, lo: top, hi: bottom, seen: Set.empty } (annotate1 e0).expr
    # \r -> { count: r.count, distinct: Set.size r.seen, lo: r.lo, hi: r.hi }
  where
  bump acc i =
    { count: acc.count + 1
    , distinct: acc.distinct
    , lo: min acc.lo i
    , hi: max acc.hi i
    , seen: Set.insert i acc.seen
    }
  step acc = case _ of
    WC (CApp o _ _) -> bump acc (occurrenceInt o)
    WC (CPerform o _) -> bump acc (occurrenceInt o)
    _ -> acc

-- | A `Let` spine `n` bindings long, each binding a call — the shape the self-host corpus produces
-- | and the one a spine-recursive rebuild dies on.
deepSpine :: Int -> Expr
deepSpine n = go n (Ret (CAtom (int 0)))
  where
  go 0 acc = acc
  go k acc = go (k - 1) (Let ("x" <> show k) (CApp unit (var "f") [ int k ]) acc)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.ANF.Occurrence" do

  describe "totality — every call occurrence is annotated" do
    it "annotates CApp" do
      occsOf (Ret (CApp unit (var "f") [ int 1 ])) `shouldEqual` [ 0 ]

    it "annotates CPerform, which the emitter lowers through the SAME call path" do
      -- `Emit` lowers `CPerform t` as `CApp t [unit]`, so a performed thunk whose callee is a
      -- parameter reaches the same classification and bumps the same reason slot. Leaving it
      -- unannotated puts those dispatches outside the keyed sum while they stay inside the reason
      -- slot — ADR-0114 §1's identity fails by exactly that population.
      occsOf (Ret (CPerform unit (var "t"))) `shouldEqual` [ 0 ]

    it "annotates calls nested in lambdas, branches, alternatives and guards" do
      let
        term =
          Ret
            ( CLam unit [ "u" ]
                ( Ret
                    ( CIf (var "b")
                        (Ret (CApp unit (var "f") [ int 1 ]))
                        ( Ret
                            ( CCase [ var "s" ]
                                [ { binders: [ BVar "v" ]
                                  , result: Guarded
                                      [ { guard: Ret (CPerform unit (var "g"))
                                        , rhs: Ret (CApp unit (var "h") [ int 2 ])
                                        }
                                      ]
                                  }
                                ]
                            )
                        )
                    )
                )
            )
      -- three call occurrences (the lambda is not one), densely numbered in SOURCE order: an
      -- independent walk meeting them in that order sees exactly [0, 1, 2].
      occsOf term `shouldEqual` [ 0, 1, 2 ]

  describe "identities are dense and unique" do
    it "numbers occurrences densely from zero" do
      let n = 40
      occsOf (deepSpine n) `shouldEqual` Array.range 0 (n - 1)

    it "gives no two occurrences the same id" do
      let os = occsOf (deepSpine 60)
      Array.length (Array.nub os) `shouldEqual` Array.length os

    it "gives the ROOT a function id, even with no lambda in the term" do
      -- a call in a lambda-free function still belongs to a source function, and a ProofSiteId has
      -- to name it.
      fnsOf (Ret (CApp unit (var "f") [ int 1 ])) `shouldEqual` [ 0 ]

    it "gives every lambda its own function id, distinct from the root" do
      let
        term =
          Let "a" (CLam unit [ "x" ] (Ret (CAtom (var "x"))))
            (Ret (CLam unit [ "y" ] (Ret (CAtom (var "y")))))
      fnsOf term `shouldEqual` [ 0, 1, 2 ]

    it "numbers functions and occurrences independently" do
      -- two supplies, not one: a term with one lambda and one call must not have them collide.
      let term = Ret (CLam unit [ "u" ] (Ret (CApp unit (var "f") [ int 1 ])))
      fnsOf term `shouldEqual` [ 0, 1 ]
      occsOf term `shouldEqual` [ 0 ]

  describe "stack safety at width" do
    -- This row proves the annotator COMPLETES and counts correctly at the width the self-host corpus
    -- reaches. Traversal fidelity is the small rows' job above: checking node-by-node here would
    -- only make the suite slow, and a fidelity bug shows up there first anyway.
    it "annotates a 100k-binding Let spine: completes, exact count, dense and unique ids" do
      let
        n = 100000
        st = occStats (deepSpine n)
      -- completes at all, and sees every call
      st.count `shouldEqual` n
      -- no id issued twice
      st.distinct `shouldEqual` n
      -- and the ids are exactly [0 .. n-1]: dense, so two snapshots of one program line up
      st.lo `shouldEqual` 0
      st.hi `shouldEqual` (n - 1)

    it "keeps ONE root function identity across a 100k spine" do
      -- the spine introduces no lambda, so nothing may switch the source function underneath those
      -- calls: a proof site's function must not drift down a long body.
      fnsOf (deepSpine 100000) `shouldEqual` [ 0 ]

  describe "object scope — the supply spans the OBJECT, not the term" do
    it "gives two STRUCTURALLY IDENTICAL top-level bodies different identities" do
      -- The scope of an id is the module object, because that is what `ProofSiteId.object` names.
      -- Annotated one at a time, these two bodies would both come back as root `fnsrc0` holding
      -- `occ0` — two different sites building ONE key, whose counts would then add together in the
      -- drill and read as a single hot site. Nothing downstream could tell that from a real one.
      let
        body = Ret (CApp unit (var "f") [ int 1 ])
        out = annotateObject [ Tuple "a" body, Tuple "b" body ]
      map (functionInt <<< _.root <<< snd) out `shouldEqual` [ 0, 1 ]
      map (occsIn <<< _.expr <<< snd) out `shouldEqual` [ [ 0 ], [ 1 ] ]

    it "keeps each body paired with its own key" do
      -- the payload rides through the fold, so a caller cannot re-pair results by position
      map fst (annotateObject [ Tuple "a" (deepSpine 2), Tuple "b" (deepSpine 3) ])
        `shouldEqual` [ "a", "b" ]

    it "annotates an object of 20k one-call bodies: complete and densely numbered across all of them" do
      -- The other wide row fixes the depth of ONE body; this fixes the WIDTH of the object, which is
      -- the dimension the object-wide supply added: completeness, numbering that runs across bodies
      -- rather than restarting per body, and key pairing that survives the fold.
      --
      -- It does NOT police the fold's COMPLEXITY, and it was written believing it did. Measured at
      -- this width, the quadratic `Array.snoc` accumulator this replaced ran the whole suite in
      -- 7.15 s against 6.43 s for the linear one — no failure, barely a signal. Making it decisive
      -- would need a width around 200k and a wall-clock assertion, which is a flaky test rather than
      -- a guard. The linearisation stands on the code being read, not on this row.
      let
        n = 20000
        out = annotateObject (map (\i -> Tuple i (Ret (CApp unit (var "f") [ int i ]))) (Array.range 1 n))
      Array.length out `shouldEqual` n
      -- one root and one call per body, both numbered across the OBJECT: [0 .. n-1] each
      map (functionInt <<< _.root <<< snd) out `shouldEqual` Array.range 0 (n - 1)
      Array.concatMap (occsIn <<< _.expr <<< snd) out `shouldEqual` Array.range 0 (n - 1)
      -- and every body still sits with the key it came in on
      map fst out `shouldEqual` Array.range 1 n

  describe "determinism" do
    it "assigns the same ids to the same term twice" do
      -- two snapshots of one program must agree about which call is which; if the annotator were
      -- order-dependent, a drill key would name a different site between runs.
      let term = deepSpine 50
      occsOf term `shouldEqual` occsOf term
      fnsOf term `shouldEqual` fnsOf term
