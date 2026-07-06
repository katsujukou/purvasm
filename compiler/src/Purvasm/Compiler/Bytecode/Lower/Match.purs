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
module Purvasm.Compiler.Bytecode.Lower.Match
  ( Lowerers
  , compileTree
  , compileNaive
  ) where

import Prelude

import Control.Monad.State (State, execState, get, modify_)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
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
type Asm = State AsmState

emit :: Pseudo -> Asm Unit
emit p = modify_ \s -> s { buf = p : s.buf }

freshLbl :: Asm Int
freshLbl = do
  modify_ \s -> s { lbl = s.lbl + 1 }
  _.lbl <$> get

freshOcc :: String -> Asm String
freshOcc prefix = do
  modify_ \s -> s { occ = s.occ + 1 }
  get <#> \s -> prefix <> show s.occ

emitChunk :: CodeBlock -> Asm Unit
emitChunk chunk = for_ chunk (emit <<< Pinstr)

-- | Extract sub-occurrence `name` (a `proj` of `occ`) onto a fresh local.
emitExtract :: String -> Instruction -> String -> Asm Unit
emitExtract occ proj name = do
  emit (Pinstr (Load occ))
  emit (Pinstr proj)
  emit (Pinstr (Bind name))

-- | Emit a fully-matched body; a non-tail body jumps to the case's join label.
emitBody :: Lowerers -> Boolean -> Int -> Expr -> Asm Unit
emitBody lw tail endLbl e = do
  emitChunk (lw.body tail e)
  when (not tail) (emit (Pjump endLbl))

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
compileTree lw tail scruts alts =
  resolve (List.reverse (execState build { lbl: 0, occ: 0, buf: Nil }).buf)
  where
  { scrutBinds, tree } = MC.compile scruts alts

  build :: Asm Unit
  build = do
    endLbl <- freshLbl
    for_ scrutBinds \(o /\ a) -> do
      emit (Pinstr (lw.atom a))
      emit (Pinstr (Bind o))
    lower endLbl tree
    emit (Plabel endLbl)

  emitLeafBinds :: Array (String /\ String) -> Asm Unit
  emitLeafBinds binds = for_ binds \(name /\ occ) -> do
    emit (Pinstr (Load occ))
    emit (Pinstr (Bind name))

  emitExtracts :: String -> Array (String /\ Proj) -> Asm Unit
  emitExtracts occ_c extracts = for_ extracts \(o /\ pr) -> emitExtract occ_c (projInstr pr) o

  lower :: Int -> DTree -> Asm Unit
  lower endLbl = case _ of
    Dfail msg -> emit (Pinstr (Fail msg))
    Dleaf binds e -> do
      emitLeafBinds binds
      emitBody lw tail endLbl e
    Dguard binds clauses ft -> do
      emitLeafBinds binds
      for_ clauses \g -> do
        emitChunk (lw.body false g.guard)
        gnext <- freshLbl
        emit (PjumpUnless gnext)
        emitBody lw tail endLbl g.rhs
        emit (Plabel gnext)
      lower endLbl ft
    DswitchCtor occ_c arms default -> do
      defaultLbl <- freshLbl
      armLbls <- for arms \(tag /\ arm) -> do
        l <- freshLbl
        pure { tag, arm, l }
      emit (Pinstr (Load occ_c))
      emit (PswitchCtor (armLbls <#> \h -> h.tag /\ h.l) defaultLbl)
      for_ armLbls \h -> do
        emit (Plabel h.l)
        emitArm endLbl occ_c h.arm
      emit (Plabel defaultLbl)
      lower endLbl default
    DswitchLit occ_c arms default -> do
      defaultLbl <- freshLbl
      armLbls <- for arms \(l /\ sub) -> do
        lbl <- freshLbl
        pure { l, sub, lbl }
      emit (Pinstr (Load occ_c))
      emit (PswitchLit (armLbls <#> \h -> h.l /\ h.lbl) defaultLbl)
      for_ armLbls \h -> do
        emit (Plabel h.lbl)
        lower endLbl h.sub
      emit (Plabel defaultLbl)
      lower endLbl default
    DswitchLen occ_c arms default -> do
      defaultLbl <- freshLbl
      armLbls <- for arms \(len /\ arm) -> do
        l <- freshLbl
        pure { len, arm, l }
      emit (Pinstr (Load occ_c))
      emit (PswitchLen (armLbls <#> \h -> h.len /\ h.l) defaultLbl)
      for_ armLbls \h -> do
        emit (Plabel h.l)
        emitArm endLbl occ_c h.arm
      emit (Plabel defaultLbl)
      lower endLbl default
    DexpandRecord occ_c extracts sub -> do
      emitExtracts occ_c extracts
      lower endLbl sub

  emitArm :: Int -> String -> Arm -> Asm Unit
  emitArm endLbl occ_c arm = do
    emitExtracts occ_c arm.extracts
    lower endLbl arm.sub

-- --- naive matcher (per-alternative re-testing) -------------------------------------

-- | A leaf's right-hand side for the naive matcher: an unconditional body commits; a guard
-- | chain (ADR-0013) is tested top to bottom, falling through to `onFail` when every guard
-- | is false.
emitRhs :: Lowerers -> Boolean -> Int -> Rhs -> Asm Unit -> Asm Unit
emitRhs lw tail endLbl rhs onFail = case rhs of
  Uncond e -> emitBody lw tail endLbl e
  Guarded clauses -> do
    for_ clauses \g -> do
      emitChunk (lw.body false g.guard)
      gnext <- freshLbl
      emit (PjumpUnless gnext)
      emitBody lw tail endLbl g.rhs
      emit (Plabel gnext)
    onFail

-- | Test `binder` against occurrence `o`; on any mismatch jump to `failLbl`, else bind
-- | the variables it introduces and fall through.
testBinder :: String -> Int -> Binder -> Asm Unit
testBinder o failLbl = case _ of
  BNull -> pure unit
  BVar x -> do
    emit (Pinstr (Load o))
    emit (Pinstr (Bind x))
  BNamed x inner -> do
    emit (Pinstr (Load o))
    emit (Pinstr (Bind x))
    testBinder o failLbl inner
  BLit l -> do
    cont <- freshLbl
    emit (Pinstr (Load o))
    emit (PswitchLit [ l /\ cont ] failLbl)
    emit (Plabel cont)
  BCtor tag subs -> do
    cont <- freshLbl
    emit (Pinstr (Load o))
    emit (PswitchCtor [ tag /\ cont ] failLbl)
    emit (Plabel cont)
    forWithIndex_ subs \j sub -> extract (Proj j) sub
  BArray subs -> do
    cont <- freshLbl
    emit (Pinstr (Load o))
    emit (PswitchLen [ Array.length subs /\ cont ] failLbl)
    emit (Plabel cont)
    forWithIndex_ subs \j sub -> extract (Proj_arr j) sub
  BRecord fields -> for_ fields \f -> extract (GetField f.prop) f.binder
  where
  extract proj sub = do
    so <- freshOcc "$nv"
    emit (Pinstr (Load o))
    emit (Pinstr proj)
    emit (Pinstr (Bind so))
    testBinder so failLbl sub

compileNaive :: Lowerers -> Boolean -> Array Atom -> Array Alt -> CodeBlock
compileNaive lw tail scruts alts = resolve (List.reverse (execState build { lbl: 0, occ: 0, buf: Nil }).buf)
  where
  build :: Asm Unit
  build = do
    endLbl <- freshLbl
    occ0 <- for scruts \a -> do
      o <- freshOcc "$nv"
      emit (Pinstr (lw.atom a))
      emit (Pinstr (Bind o))
      pure o
    for_ alts \alt -> do
      next <- freshLbl
      for_ (Array.zip occ0 alt.binders) \(o /\ b) -> testBinder o next b
      emitRhs lw tail endLbl alt.result (pure unit)
      emit (Plabel next)
    emit (Pinstr (Fail "case: no matching alternative"))
    emit (Plabel endLbl)
