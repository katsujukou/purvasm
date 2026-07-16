-- | Lower a `case` to PURVASM bytecode (ADR-0031, ADR-0083). The Maranget decision tree
-- | is built, once and backend-agnostically, by `Purvasm.Compiler.MiddleEnd.MatchCompile`;
-- | this module is the *bytecode* lowering of that tree (`compileTree`) plus the
-- | per-alternative re-testing baseline (`compileNaive`) it is measured against, mirroring
-- | boot's `Vm.Match_compile`.
-- |
-- | The tree's occurrences are bound to fresh locals, so the operand stack is empty at
-- | every branch boundary (ADR-0003); its labels are back-patched to relative offsets by
-- | `resolve`. To avoid a cyclic import, the two lowerings this needs from `Lower` —
-- | `atom` (a scrutinee atom) and `body` (an alternative's rhs, tail-aware) — are passed
-- | in as `Lowerers` (boot's `~atom`/`~body`).
-- |
-- | The assembler threads its `AsmState` **explicitly** (2026-07-16 stack-safety bugfix,
-- | maintainer-authorized): the previous `State` monad sequenced one bind per emitted
-- | pseudo-instruction, and on the JS backend every sequenced bind is a live host-stack
-- | frame — `emitChunk` over a parser-scale body (tens of thousands of instructions)
-- | overflowed the default node stack during the VM-target self-compile. Emission is now
-- | plain state-passing with `foldl` loops over every data-sized spine; host recursion
-- | remains only on genuine tree/binder *nesting*. The label/occurrence numbering order
-- | and the emitted-instruction order are exactly the monadic assembler's.
module Purvasm.Compiler.Bytecode.Lower.Match
  ( Lowerers
  , compileTree
  , compileNaive
  ) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Literal (Literal)
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom, Expr, Rhs(..))
import Purvasm.Compiler.MiddleEnd.MatchCompile (Arm, DTree(..), Proj(..))
import Purvasm.Compiler.MiddleEnd.MatchCompile as MC

-- | The two lowerings `Match` needs from `Lower`, passed in to avoid a cyclic import:
-- | compile a scrutinee atom, and an alternative's rhs (tail-aware).
type Lowerers =
  { atom :: Atom -> Instruction
  , body :: Boolean -> Expr -> CodeBlock
  }

-- --- assembler (pseudo-instructions + label back-patching) --------------------------

-- | A pseudo-instruction for the back-patching assembler: a final instruction, or a
-- | jump/switch carrying *label ids* (resolved to relative offsets by `resolve`), or a
-- | position marker.
data Pseudo
  = Pinstr Instruction
  | Pjump Int
  | PjumpUnless Int
  | PswitchCtor (Array (String /\ Int)) Int
  | PswitchLit (Array (Literal /\ Int)) Int
  | PswitchLen (Array (Int /\ Int)) Int
  | Plabel Int

-- | `buf` accumulates emitted pseudo-instructions in *reverse* order (cons is O(1); ADR-0049)
-- | and is reversed once at `resolve`'s entry. `occ` is used only by `compileNaive` (the tree
-- | matcher's occurrences are fixed by `MatchCompile`).
type AsmState = { lbl :: Int, occ :: Int, buf :: List Pseudo }

asmInit :: AsmState
asmInit = { lbl: 0, occ: 0, buf: Nil }

emit :: Pseudo -> AsmState -> AsmState
emit p s = s { buf = p : s.buf }

freshLbl :: AsmState -> { s :: AsmState, lbl :: Int }
freshLbl s0 = let s = s0 { lbl = s0.lbl + 1 } in { s, lbl: s.lbl }

freshOcc :: String -> AsmState -> { s :: AsmState, occ :: String }
freshOcc prefix s0 = let s = s0 { occ = s0.occ + 1 } in { s, occ: prefix <> show s.occ }

-- | Emit a pre-lowered chunk — a `foldl` loop, one iteration per instruction (this exact
-- | spine, sequenced through the retired `State` bind, was the default-stack overflow).
emitChunk :: CodeBlock -> AsmState -> AsmState
emitChunk chunk s0 = Array.foldl (\s i -> emit (Pinstr i) s) s0 chunk

-- | Extract sub-occurrence `name` (a `proj` of `occ`) onto a fresh local.
emitExtract :: String -> Instruction -> String -> AsmState -> AsmState
emitExtract occ proj name =
  emit (Pinstr (Bind name)) <<< emit (Pinstr proj) <<< emit (Pinstr (Load occ))

-- | Emit a fully-matched body; a non-tail body jumps to the case's join label.
emitBody :: Lowerers -> Boolean -> Int -> Expr -> AsmState -> AsmState
emitBody lw tail endLbl e s0 =
  let
    s1 = emitChunk (lw.body tail e) s0
  in
    if tail then s1 else emit (Pjump endLbl) s1

-- | Two passes: record each label's instruction index, then emit the final array with
-- | every jump/switch target turned into an offset relative to the *next* instruction
-- | (matching the VM's `ip := ip + rel` after it has stepped past the instruction).
resolve :: List Pseudo -> CodeBlock
resolve pseudos = Array.fromFoldable (List.reverse (List.foldl step { self: 0, out: Nil } pseudos).out)
  where
  labelpos :: Map.Map Int Int
  labelpos = (List.foldl mark { pos: 0, m: Map.empty } pseudos).m
    where
    mark acc = case _ of
      Plabel l -> acc { m = Map.insert l acc.pos acc.m }
      _ -> acc { pos = acc.pos + 1 }

  rel :: Int -> Int -> Int
  rel self l = fromMaybe 0 (Map.lookup l labelpos) - (self + 1)

  step acc = case _ of
    Plabel _ -> acc
    p -> acc { self = acc.self + 1, out = toInstr acc.self p : acc.out }

  toInstr self = case _ of
    Pinstr i -> i
    Pjump l -> Jump (rel self l)
    PjumpUnless l -> JumpUnless (rel self l)
    PswitchCtor cs d -> SwitchCtor (map (\(t /\ l) -> t /\ rel self l) cs) (rel self d)
    PswitchLit cs d -> SwitchLit (map (\(x /\ l) -> x /\ rel self l) cs) (rel self d)
    PswitchLen cs d -> SwitchLen (map (\(x /\ l) -> x /\ rel self l) cs) (rel self d)
    Plabel _ -> unsafeCrashWith "resolve: label not skipped"

-- | Thread the assembler state through an array left-to-right, collecting a result per
-- | element (the retired `State` asm's `for`) — loops, not sequenced binds.
threadMap
  :: forall a b
   . (AsmState -> a -> { s :: AsmState, r :: b })
  -> AsmState
  -> Array a
  -> { s :: AsmState, rs :: Array b }
threadMap f s0 xs =
  let
    folded = Array.foldl (\acc x -> let o = f acc.s x in { s: o.s, acc: o.r : acc.acc }) { s: s0, acc: Nil } xs
  in
    { s: folded.s, rs: Array.fromFoldable (List.reverse folded.acc) }

-- --- decision-tree matcher (Maranget, ADR-0031/ADR-0083) -----------------------------

projInstr :: Proj -> Instruction
projInstr = case _ of
  Pfield j -> Proj j
  Pelem j -> Proj_arr j
  Precord l -> GetField l

-- | Lower the shared decision tree to bytecode. The tree fixes the occurrence names
-- | (ADR-0083 byte-identity contract); this walk only turns switches into `Pswitch*` with
-- | back-patched labels and threads `atom`/`body`.
compileTree :: Lowerers -> Boolean -> Array Atom -> Array Alt -> CodeBlock
compileTree lw tail scruts alts = resolve (List.reverse (build asmInit).buf)
  where
  { scrutBinds, tree } = MC.compile scruts alts

  build :: AsmState -> AsmState
  build s0 =
    let
      end = freshLbl s0
      s1 = Array.foldl
        (\s (o /\ a) -> emit (Pinstr (Bind o)) (emit (Pinstr (lw.atom a)) s))
        end.s
        scrutBinds
    in
      emit (Plabel end.lbl) (lower end.lbl tree s1)

  emitLeafBinds :: Array (String /\ String) -> AsmState -> AsmState
  emitLeafBinds binds s0 = Array.foldl
    (\s (name /\ occ) -> emit (Pinstr (Bind name)) (emit (Pinstr (Load occ)) s))
    s0
    binds

  emitExtracts :: String -> Array (String /\ Proj) -> AsmState -> AsmState
  emitExtracts occ_c extracts s0 =
    Array.foldl (\s (o /\ pr) -> emitExtract occ_c (projInstr pr) o s) s0 extracts

  lower :: Int -> DTree -> AsmState -> AsmState
  lower endLbl t s0 = case t of
    Dfail msg -> emit (Pinstr (Fail msg)) s0
    Dleaf binds e -> emitBody lw tail endLbl e (emitLeafBinds binds s0)
    Dguard binds clauses ft ->
      let
        s1 = emitLeafBinds binds s0
        s2 = Array.foldl
          ( \s g ->
              let
                sg = emitChunk (lw.body false g.guard) s
                gnext = freshLbl sg
                sr = emitBody lw tail endLbl g.rhs (emit (PjumpUnless gnext.lbl) gnext.s)
              in
                emit (Plabel gnext.lbl) sr
          )
          s1
          clauses
      in
        lower endLbl ft s2
    DswitchCtor occ_c arms default ->
      let
        dflt = freshLbl s0
        minted = threadMap
          (\s (tag /\ arm) -> let l = freshLbl s in { s: l.s, r: { tag, arm, l: l.lbl } })
          dflt.s
          arms
        s1 = emit (PswitchCtor (minted.rs <#> \h -> h.tag /\ h.l) dflt.lbl)
          (emit (Pinstr (Load occ_c)) minted.s)
        s2 = Array.foldl
          (\s h -> emitArm endLbl occ_c h.arm (emit (Plabel h.l) s))
          s1
          minted.rs
      in
        lower endLbl default (emit (Plabel dflt.lbl) s2)
    DswitchLit occ_c arms default ->
      let
        dflt = freshLbl s0
        minted = threadMap
          (\s (l /\ sub) -> let lbl = freshLbl s in { s: lbl.s, r: { l, sub, lbl: lbl.lbl } })
          dflt.s
          arms
        s1 = emit (PswitchLit (minted.rs <#> \h -> h.l /\ h.lbl) dflt.lbl)
          (emit (Pinstr (Load occ_c)) minted.s)
        s2 = Array.foldl
          (\s h -> lower endLbl h.sub (emit (Plabel h.lbl) s))
          s1
          minted.rs
      in
        lower endLbl default (emit (Plabel dflt.lbl) s2)
    DswitchLen occ_c arms default ->
      let
        dflt = freshLbl s0
        minted = threadMap
          (\s (len /\ arm) -> let l = freshLbl s in { s: l.s, r: { len, arm, l: l.lbl } })
          dflt.s
          arms
        s1 = emit (PswitchLen (minted.rs <#> \h -> h.len /\ h.l) dflt.lbl)
          (emit (Pinstr (Load occ_c)) minted.s)
        s2 = Array.foldl
          (\s h -> emitArm endLbl occ_c h.arm (emit (Plabel h.l) s))
          s1
          minted.rs
      in
        lower endLbl default (emit (Plabel dflt.lbl) s2)
    DexpandRecord occ_c extracts sub ->
      lower endLbl sub (emitExtracts occ_c extracts s0)

  emitArm :: Int -> String -> Arm -> AsmState -> AsmState
  emitArm endLbl occ_c arm s0 = lower endLbl arm.sub (emitExtracts occ_c arm.extracts s0)

-- --- naive matcher (per-alternative re-testing) -------------------------------------

-- | A leaf's right-hand side for the naive matcher: an unconditional body commits; a guard
-- | chain (ADR-0013) is tested top to bottom, falling through to `onFail` when every guard
-- | is false.
emitRhs :: Lowerers -> Boolean -> Int -> Rhs -> (AsmState -> AsmState) -> AsmState -> AsmState
emitRhs lw tail endLbl rhs onFail s0 = case rhs of
  Uncond e -> emitBody lw tail endLbl e s0
  Guarded clauses ->
    let
      s1 = Array.foldl
        ( \s g ->
            let
              sg = emitChunk (lw.body false g.guard) s
              gnext = freshLbl sg
              sr = emitBody lw tail endLbl g.rhs (emit (PjumpUnless gnext.lbl) gnext.s)
            in
              emit (Plabel gnext.lbl) sr
        )
        s0
        clauses
    in
      onFail s1

-- | Test `binder` against occurrence `o`; on any mismatch jump to `failLbl`, else bind
-- | the variables it introduces and fall through.
testBinder :: String -> Int -> Binder -> AsmState -> AsmState
testBinder o failLbl b s0 = case b of
  BNull -> s0
  BVar x -> emit (Pinstr (Bind x)) (emit (Pinstr (Load o)) s0)
  BNamed x inner ->
    testBinder o failLbl inner (emit (Pinstr (Bind x)) (emit (Pinstr (Load o)) s0))
  BLit l ->
    let
      cont = freshLbl s0
      s1 = emit (PswitchLit [ l /\ cont.lbl ] failLbl) (emit (Pinstr (Load o)) cont.s)
    in
      emit (Plabel cont.lbl) s1
  BCtor tag subs ->
    let
      cont = freshLbl s0
      s1 = emit (Plabel cont.lbl)
        (emit (PswitchCtor [ tag /\ cont.lbl ] failLbl) (emit (Pinstr (Load o)) cont.s))
    in
      foldlWithIndex (\j s sub -> extract (Proj j) sub s) s1 subs
  BArray subs ->
    let
      cont = freshLbl s0
      s1 = emit (Plabel cont.lbl)
        ( emit (PswitchLen [ Array.length subs /\ cont.lbl ] failLbl)
            (emit (Pinstr (Load o)) cont.s)
        )
    in
      foldlWithIndex (\j s sub -> extract (Proj_arr j) sub s) s1 subs
  BRecord fields ->
    Array.foldl (\s f -> extract (GetField f.prop) f.binder s) s0 fields
  where
  extract proj sub s =
    let
      so = freshOcc "$nv" s
      s1 = emit (Pinstr (Bind so.occ)) (emit (Pinstr proj) (emit (Pinstr (Load o)) so.s))
    in
      testBinder so.occ failLbl sub s1

compileNaive :: Lowerers -> Boolean -> Array Atom -> Array Alt -> CodeBlock
compileNaive lw tail scruts alts = resolve (List.reverse (build asmInit).buf)
  where
  build :: AsmState -> AsmState
  build s0 =
    let
      end = freshLbl s0
      bound = threadMap
        ( \s a ->
            let
              o = freshOcc "$nv" s
            in
              { s: emit (Pinstr (Bind o.occ)) (emit (Pinstr (lw.atom a)) o.s), r: o.occ }
        )
        end.s
        scruts
      s1 = Array.foldl
        ( \s alt ->
            let
              next = freshLbl s
              sT = Array.foldl
                (\s' (o /\ bd) -> testBinder o next.lbl bd s')
                next.s
                (Array.zip bound.rs alt.binders)
              sR = emitRhs lw tail end.lbl alt.result identity sT
            in
              emit (Plabel next.lbl) sR
        )
        bound.s
        alts
    in
      emit (Plabel end.lbl) (emit (Pinstr (Fail "case: no matching alternative")) s1)
