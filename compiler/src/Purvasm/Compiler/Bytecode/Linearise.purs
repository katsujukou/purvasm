-- | Flatten the tree-shaped `case` back to relative offsets, for the artifacts that predate it
-- | ([ADR-0110](../../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §4(b)): the
-- | `.pmo` and boot's version-3 image, whose readers know `SwitchCtorRel` and nothing else.
-- |
-- | The direction is what matters. The compiler now *keeps* the decision tree
-- | (`MiddleEnd.MatchCompile` built it; `Lower.Match` lowers it node for node) and drops the
-- | structure here, on the way out to one older reader — rather than every consumer of an image
-- | rebuilding from offsets what the producer already had.
-- |
-- | The output is the byte-for-byte layout the linearising lowering used to emit, and that is a
-- | *checkable* claim rather than a hope: same regions in the same order, one shared join per `case`,
-- | and a fall-through jump exactly where the old assembler put one. Anything else would move boot's
-- | instruction counts, which are the measurement field's until step D retires them.
module Purvasm.Compiler.Bytecode.Linearise
  ( linearise
  ) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Literal (Literal)

-- | A pseudo-instruction carrying *label ids* where the final form carries relative offsets, plus a
-- | position marker. Identical in spirit to the assembler `Lower.Match` used before the tree form —
-- | this is where that machinery went, rather than away.
data Pseudo
  = Pinstr Instruction
  | Pjump Int
  | PjumpUnless Int
  | PswitchCtor (Array (String /\ Int)) Int
  | PswitchLit (Array (Literal /\ Int)) Int
  | PswitchLen (Array (Int /\ Int)) Int
  | Plabel Int

-- | `buf` accumulates in *reverse* (cons is O(1), ADR-0049) and is reversed once at `resolve`.
type St = { lbl :: Int, buf :: List Pseudo }

emit :: Pseudo -> St -> St
emit p s = s { buf = p : s.buf }

fresh :: St -> { s :: St, lbl :: Int }
fresh s0 = let s = s0 { lbl = s0.lbl + 1 } in { s, lbl: s.lbl }

freshN :: Int -> St -> { s :: St, lbls :: Array Int }
freshN n s0 =
  let
    go acc _ = let f = fresh acc.s in { s: f.s, ls: f.lbl : acc.ls }
    r = Array.foldl go { s: s0, ls: Nil } (Array.replicate n unit)
  in
    { s: r.s, lbls: Array.fromFoldable (List.reverse r.ls) }

-- | The whole chunk, with no enclosing region to fall through to.
linearise :: CodeBlock -> CodeBlock
linearise block = resolve (List.reverse (region Nothing' block { lbl: 0, buf: Nil }).buf)

-- | `Nothing'`/`Just'` rather than `Maybe Int` so the recursion reads as "this region's fall-through
-- | target, if it has one" at every call site. A chunk has none: nothing follows the end of a chunk.
data Exit = Nothing' | Just' Int

-- | Emit one region: every original instruction gets a label, so a jump inside it still lands where
-- | it did once the switches around it have been flattened. Position `n` — one past the last
-- | instruction — is the region's **exit**, which is where a `case` arm resumes and where an `if`
-- | whose branch ends the region jumps to.
-- |
-- | A region with an inherited exit does not place that label: the enclosing layout does, after ALL
-- | of a switch's arms and its default. A whole chunk owns its own end and places it here.
region :: Exit -> CodeBlock -> St -> St
region exit block s0 =
  let
    minted = freshN (Array.length block) s0
    owned = case exit of
      Just' e -> { lbl: e, s: minted.s, place: false }
      Nothing' -> let f = fresh minted.s in { lbl: f.lbl, s: f.s, place: true }
    labelAt k = fromMaybe owned.lbl (Array.index minted.lbls k)
    s1 = foldlWithIndex (\k s i -> instruction labelAt k s i) owned.s block
    -- The fall-through jump the old assembler emitted at the end of an arm — including the
    -- `Jump 0` after a default region that sits immediately before the join.
    s2 = if owned.place || not (fallsThrough block) then s1 else emit (Pjump owned.lbl) s1
  in
    if owned.place then emit (Plabel owned.lbl) s2 else s2

-- | Whether control reaches the end of a region. A switch and a guard chain are terminal: their arms
-- | carry the region's exit themselves, which is exactly why the old assembler emitted no jump after
-- | one either.
fallsThrough :: CodeBlock -> Boolean
fallsThrough block = case Array.last block of
  Nothing -> true
  Just i -> case i of
    Return -> false
    -- A tail call ends the activation; the old assembler emitted no jump after one either (a tail
    -- body is \`emitBody tail=true\`, which appended nothing).
    TailCall _ -> false
    Fail _ -> false
    Jump _ -> false
    SwitchCtor _ _ -> false
    SwitchLit _ _ -> false
    SwitchLen _ _ -> false
    Guarded _ _ -> false
    _ -> true

-- | One instruction at original position `k`, given the region's position→label map.
instruction :: (Int -> Int) -> Int -> St -> Instruction -> St
instruction labelAt k s0 i =
  let
    s1 = emit (Plabel (labelAt k)) s0
    -- A `case` yields its value where the next instruction would have started.
    exit = Just' (labelAt (k + 1))
  in
    case i of
      -- A nested chunk is its own region with its own end: a closure body or a recursive member is
      -- entered, not fallen into.
      Closure params body -> emit (Pinstr (Closure params (linearise body))) s1
      MakeRec members -> emit (Pinstr (MakeRec (map (\(nm /\ c) -> nm /\ linearise c) members))) s1
      -- Already-relative jumps (an `if`, ADR-0031) are re-anchored to the label of the position they
      -- named, so flattening a switch between a jump and its target cannot shift it.
      Jump rel -> emit (Pjump (labelAt (k + 1 + rel))) s1
      JumpUnless rel -> emit (PjumpUnless (labelAt (k + 1 + rel))) s1
      SwitchCtor arms default -> switch exit s1 (map (\(t /\ b) -> t /\ b) arms) default
        (\ls d -> PswitchCtor (Array.zipWith (\(t /\ _) l -> t /\ l) arms ls) d)
      SwitchLit arms default -> switch exit s1 (map (\(l /\ b) -> l /\ b) arms) default
        (\ls d -> PswitchLit (Array.zipWith (\(l /\ _) lb -> l /\ lb) arms ls) d)
      SwitchLen arms default -> switch exit s1 (map (\(x /\ b) -> x /\ b) arms) default
        (\ls d -> PswitchLen (Array.zipWith (\(x /\ _) l -> x /\ l) arms ls) d)
      Guarded clauses fallthrough ->
        let
          s2 = Array.foldl
            ( \s g ->
                let
                  sg = Array.foldl (\a x -> emit (Pinstr x) a) s (linearise g.guard)
                  next = fresh sg
                  sr = region exit g.rhs (emit (PjumpUnless next.lbl) next.s)
                in
                  emit (Plabel next.lbl) sr
            )
            s1
            clauses
        in
          region exit fallthrough s2
      _ -> emit (Pinstr i) s1

-- | The shared switch layout: the dispatch, then each arm's region in order, then the default's —
-- | the order the old assembler emitted and the order a reader of the old format expects.
switch
  :: forall d
   . Exit
  -> St
  -> Array (d /\ CodeBlock)
  -> CodeBlock
  -> (Array Int -> Int -> Pseudo)
  -> St
switch exit s0 arms default build =
  let
    dflt = fresh s0
    minted = freshN (Array.length arms) dflt.s
    s1 = emit (build minted.lbls dflt.lbl) minted.s
    s2 = Array.foldl
      (\s (l /\ (_ /\ body)) -> region exit body (emit (Plabel l) s))
      s1
      (Array.zip minted.lbls arms)
  in
    region exit default (emit (Plabel dflt.lbl) s2)

-- | Two passes: record each label's instruction index, then emit with every target turned into an
-- | offset relative to the *next* instruction (the VM's `ip := ip + rel` after stepping past it).
resolve :: List Pseudo -> CodeBlock
resolve pseudos = Array.fromFoldable (List.reverse (List.foldl step { self: 0, out: Nil } pseudos).out)
  where
  labelpos :: Map Int Int
  labelpos = (List.foldl mark { pos: 0, m: Map.empty } pseudos).m
    where
    mark acc = case _ of
      Plabel l -> acc { m = Map.insert l acc.pos acc.m }
      _ -> acc { pos = acc.pos + 1 }

  rel self l = fromMaybe 0 (Map.lookup l labelpos) - (self + 1)

  step acc = case _ of
    Plabel _ -> acc
    p -> acc { self = acc.self + 1, out = toInstr acc.self p : acc.out }

  toInstr self = case _ of
    Pinstr i -> i
    Pjump l -> Jump (rel self l)
    PjumpUnless l -> JumpUnless (rel self l)
    PswitchCtor cs d -> SwitchCtorRel (map (\(t /\ l) -> t /\ rel self l) cs) (rel self d)
    PswitchLit cs d -> SwitchLitRel (map (\(x /\ l) -> x /\ rel self l) cs) (rel self d)
    PswitchLen cs d -> SwitchLenRel (map (\(x /\ l) -> x /\ rel self l) cs) (rel self d)
    Plabel _ -> unsafeCrashWith "Linearise.resolve: label not skipped"
