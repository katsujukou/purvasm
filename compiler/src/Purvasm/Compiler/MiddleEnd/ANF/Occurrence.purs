-- | The identity of a CALL OCCURRENCE in an ANF term (ADR-0114 §1).
-- |
-- | This is the annotation ANF is parameterised over: `Expr Unit` is what the optimiser and the
-- | bytecode backend work in, and `Expr AnfOccurrenceId` is what the LLVM backend receives after
-- | `annotateObject` has run — post-optimiser, so the term is final, and pre-`MatchCompile`, so
-- | one id still corresponds to one source-level call rather than to one of its emitted copies.
-- |
-- | It lives in the MIDDLE END and knows nothing about any backend: the annotator is a plain
-- | traversal, and the LLVM types that build a `ProofSiteId` from one of these import it rather than
-- | the other way round.
-- |
-- | **The supply is not exported, and that is the point.** An id's whole value is that two snapshots
-- | of the same program agree about which call is which. A public `Int -> AnfOccurrenceId`, or a
-- | public counter, lets any module mint a negative id, re-use a state and mint a DUPLICATE, or
-- | invent one for a call that does not exist — and a drill keyed by a hand-made id reports a site
-- | that is not there, which is indistinguishable in the output from one that is. So the counter and
-- | its successor live inside this module, `annotateObject` is the only way ids come into being,
-- | and a caller that wants one annotates a term to get it.
-- |
-- | **An id is unique within a MODULE OBJECT, not within a term.** That is the scope `ProofSiteId`
-- | keys by: its `object` field names the object, and `sourceFn`/`callOcc` are expected to separate
-- | everything inside it. A per-term producer cannot deliver that — annotate two top-level bodies of
-- | one object separately and both come back with root `fnsrc0` and first call `occ0`, so two
-- | genuinely different sites build the SAME `ProofSiteId` and their execution counts silently add
-- | together in the drill. That is not a detectable failure downstream: a merged row looks exactly
-- | like a hot row. So the producer takes the object's bodies TOGETHER and threads one supply
-- | across them, and no single-term entry point is exported — a caller with one body passes a
-- | one-element object, which is what it actually has.
module Purvasm.Compiler.MiddleEnd.ANF.Occurrence
  ( AnfOccurrenceId
  , occurrenceInt
  , AnfFunctionId
  , functionInt
  , Annotated
  , annotateObject
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.MiddleEnd.ANF (AltF, CExprF(..), ExprF(..), RhsF(..))

-- | One call occurrence — a `CApp` or a `CPerform` — in one function's ANF term.
newtype AnfOccurrenceId = AnfOccurrenceId Int

derive instance eqAnfOccurrenceId :: Eq AnfOccurrenceId
derive instance ordAnfOccurrenceId :: Ord AnfOccurrenceId

instance showAnfOccurrenceId :: Show AnfOccurrenceId where
  show (AnfOccurrenceId i) = "occ" <> show i

-- | For rendering a key. Never for reconstructing an id: there is no inverse, deliberately.
occurrenceInt :: AnfOccurrenceId -> Int
occurrenceInt (AnfOccurrenceId i) = i

-- | The identity of a SOURCE FUNCTION — the root body, or one `CLam` in the ANF term (ADR-0114
-- | amendment, 2026-08-27).
-- |
-- | **Why a call occurrence is not enough.** Match compilation duplicates rows: `MatchCompile.goCtor`
-- | copies a wildcard row's RHS into every constructor arm AND into the default, and the emitter
-- | lowers each resulting leaf independently. When that RHS contains a `CLam`, `lift` runs once per
-- | leaf and mints a fresh `fn_N` each time — so the SAME source lambda becomes several lifted
-- | functions. Keying a `ProofSiteId` by `Lifted.name` would split one source function across
-- | several proofs and break the layer's contract, which is that a proof is what an optimiser
-- | rewrites ONCE.
-- |
-- | So the function boundary is annotated upstream, where duplication has not happened yet, and the
-- | lifted name is demoted to reporting metadata on the EMISSION layer.
newtype AnfFunctionId = AnfFunctionId Int

derive instance eqAnfFunctionId :: Eq AnfFunctionId
derive instance ordAnfFunctionId :: Ord AnfFunctionId

instance showAnfFunctionId :: Show AnfFunctionId where
  show (AnfFunctionId i) = "fnsrc" <> show i

-- | For rendering a key. No inverse, deliberately.
functionInt :: AnfFunctionId -> Int
functionInt (AnfFunctionId i) = i

-- --- the supply: PRIVATE ------------------------------------------------------------------------
-- Not exported. `annotateObject` (added with the ANF parameterisation, ADR-0114 amendment)
-- is the only consumer, and it threads the counter explicitly rather than through a monad so the
-- walk can stay stack-safe over a hundred-thousand-node spine under `tailRec`.

-- | Take the next id, returning the successor state alongside it. A caller that drops the state
-- | would re-use an id, which is why the state is returned rather than hidden — and why neither
-- | this nor the counter leaves the module.
freshOccurrence :: Int -> { next :: Int, id :: AnfOccurrenceId }
freshOccurrence n = { next: n + 1, id: AnfOccurrenceId n }

-- | The function-identity supply, private for the same reason.
freshFunction :: Int -> { next :: Int, id :: AnfFunctionId }
freshFunction n = { next: n + 1, id: AnfFunctionId n }

-- | An annotated term, with the identity of the function it IS.
-- |
-- | The root needs its own `AnfFunctionId` for the same reason every `CLam` does: a call in a
-- | function that contains no lambda still belongs to a source function, and a `ProofSiteId` has to
-- | name it.
type Annotated =
  { root :: AnfFunctionId
  , expr :: ExprF AnfFunctionId AnfOccurrenceId
  }

-- | Stamp every call occurrence and every function boundary in ONE MODULE OBJECT.
-- |
-- | Every body is numbered from a single pair of supplies, so an `AnfFunctionId` or an
-- | `AnfOccurrenceId` identifies a site within the object — the scope `ProofSiteId` keys by. Two
-- | structurally identical top-level bodies therefore get different ids, which is the whole point
-- | (see the module note); annotating them one at a time would give both the same ones.
-- |
-- | Runs POST-optimiser (the term is final, so an id means the same thing in two snapshots of the
-- | same program) and PRE-`MatchCompile` (one id still corresponds to one source-level call, not to
-- | one of its emitted copies). ADR-0114 §1 and its 2026-08-27 amendment.
-- |
-- | The payload `k` — a top-level key, in production — is carried through untouched so a caller
-- | cannot re-pair bodies with results by position and get it wrong.
-- |
-- | **Stack safety.** The `Let`/`LetRec` SPINE is walked iteratively and rebuilt with a stack-safe
-- | `foldl`; only branch and lambda NESTING recurses, which is control-flow depth rather than
-- | program length. Same shape, and the same reason, as `ANF.FreeVars`: the self-host corpus carries
-- | `Let` spines thousands of bindings long, and a spine-recursive rebuild overflows on them. The
-- | fold ACROSS bodies is `foldl` for the same reason — an object has thousands of them.
annotateObject :: forall f c k. Array (Tuple k (ExprF f c)) -> Array (Tuple k Annotated)
annotateObject bodies =
  -- Results are PREPENDED to a list and reversed once. `Array.snoc` in the accumulator copies the
  -- whole output per body — quadratic in the object's body count, the one dimension this fold has
  -- to scale in. Note that no TEST distinguishes the two: at 20k bodies the quadratic version cost
  -- about 0.7 s more across the whole suite and failed nothing. This is a complexity property, held
  -- by reading the code; the width row next to it pins the fold's RESULT, not its cost.
  Array.fromFoldable (List.reverse (foldl one { st: { fn: 0, occ: 0 }, out: Nil } bodies).out)
  where
  one acc (Tuple k e) =
    let
      r = freshFunction acc.st.fn
      out = goE (acc.st { fn = r.next }) e
    in
      { st: out.st, out: Tuple k { root: r.id, expr: out.expr } : acc.out }

-- | The two counters travel together: functions and occurrences are numbered independently, and
-- | both must be dense per OBJECT for two snapshots to line up — the object is the scope
-- | `ProofSiteId` keys by, so restarting either counter at a body boundary would merge sites.
type Supply = { fn :: Int, occ :: Int }

-- | One collected spine step, ALREADY ANNOTATED — the rebuild below carries no state.
data SpineStep
  = StLet String (CExprF AnfFunctionId AnfOccurrenceId)
  | StLetRec (Array { var :: String, rhs :: ExprF AnfFunctionId AnfOccurrenceId })

-- | Annotate a term, threading both supplies.
-- |
-- | Ids are minted on the FORWARD walk — the peel — and never during the rebuild. The rebuild
-- | reverses the spine (`acc` was built by prepending), so minting there numbered a `Let` spine
-- | back-to-front while `LetRec`, `CIf` and `CCase` were numbered forwards: dense and deterministic,
-- | so the drill would still have worked, but the order was an artifact of how the spine was rebuilt
-- | rather than a property of the term. Minting on the peel makes `occ7` the eighth call in the
-- | function everywhere, which is what a site label is read as, and confines all state threading to
-- | one direction.
goE :: forall f c. Supply -> ExprF f c -> { st :: Supply, expr :: ExprF AnfFunctionId AnfOccurrenceId }
goE st0 e0 =
  let
    spine = tailRec
      ( \w -> case w.e of
          Let x c rest ->
            let
              r = goC w.st c
            in
              Loop { e: rest, st: r.st, acc: StLet x r.expr : w.acc }
          LetRec bs rest ->
            let
              r = foldl
                ( \acc b ->
                    let
                      rb = goE acc.st b.rhs
                    in
                      { st: rb.st, out: Array.snoc acc.out { var: b.var, rhs: rb.expr } }
                )
                { st: w.st, out: [] }
                bs
            in
              Loop { e: rest, st: r.st, acc: StLetRec r.out : w.acc }
          Ret c ->
            let
              r = goC w.st c
            in
              Done { steps: w.acc, st: r.st, tailE: Ret r.expr }
      )
      { e: e0, st: st0, acc: Nil }
  in
    -- purely structural, and stack-safe: `foldl` over an innermost-first list
    { st: spine.st, expr: foldl rebuild spine.tailE spine.steps }
  where
  rebuild inner = case _ of
    StLet x c -> Let x c inner
    StLetRec bs -> LetRec bs inner

goC :: forall f c. Supply -> CExprF f c -> { st :: Supply, expr :: CExprF AnfFunctionId AnfOccurrenceId }
goC st = case _ of
  -- the two CALL occurrences: each takes an id, and `CPerform` is one of them. The emitter lowers
  -- it through the same call path as `CApp`, so leaving it unannotated would put its dispatches
  -- outside the keyed sum while they stayed inside the reason slot (ADR-0114 §1 condition 6).
  CApp _ f as -> let o = freshOccurrence st.occ in { st: st { occ = o.next }, expr: CApp o.id f as }
  CPerform _ a -> let o = freshOccurrence st.occ in { st: st { occ = o.next }, expr: CPerform o.id a }
  -- the FUNCTION boundary
  CLam _ ps b ->
    let
      fr = freshFunction st.fn
      rb = goE (st { fn = fr.next }) b
    in
      { st: rb.st, expr: CLam fr.id ps rb.expr }
  CIf a t e ->
    let
      rt = goE st t
      re = goE rt.st e
    in
      { st: re.st, expr: CIf a rt.expr re.expr }
  CCase scruts alts ->
    let
      r = foldl (\acc alt -> let ra = goAlt acc.st alt in { st: ra.st, out: Array.snoc acc.out ra.alt })
        { st, out: [] }
        alts
    in
      { st: r.st, expr: CCase scruts r.out }
  -- everything else carries no annotation and is rebuilt unchanged
  CAtom a -> { st, expr: CAtom a }
  CPrim op as -> { st, expr: CPrim op as }
  CCtor n ar as -> { st, expr: CCtor n ar as }
  CArray as -> { st, expr: CArray as }
  CRecord fs -> { st, expr: CRecord fs }
  CAccessor a l -> { st, expr: CAccessor a l }
  CUpdate a ups -> { st, expr: CUpdate a ups }

goAlt :: forall f c. Supply -> AltF f c -> { st :: Supply, alt :: AltF AnfFunctionId AnfOccurrenceId }
goAlt st alt = case alt.result of
  Uncond e ->
    let
      r = goE st e
    in
      { st: r.st, alt: { binders: alt.binders, result: Uncond r.expr } }
  Guarded gs ->
    let
      r = foldl
        ( \acc g ->
            let
              rg = goE acc.st g.guard
              rr = goE rg.st g.rhs
            in
              { st: rr.st, out: Array.snoc acc.out { guard: rg.expr, rhs: rr.expr } }
        )
        { st, out: [] }
        gs
    in
      { st: r.st, alt: { binders: alt.binders, result: Guarded r.out } }
