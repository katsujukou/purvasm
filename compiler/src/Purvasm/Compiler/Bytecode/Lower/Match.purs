-- | Pattern-match compilation (ADR-0031): lower a `case`'s scrutinees and alternatives to
-- | PURVASM bytecode. Kept out of the surrounding ANF→bytecode walk (`Lower`) as a single
-- | responsibility (boot's `Match_compile`).
-- |
-- | `compileTree` builds a Maranget decision tree of discriminant switches: the first
-- | refutable column is switched on, the matrix specialised per head and a default, so a
-- | tag/length/literal is tested at most once across all alternatives. `compileNaive` is
-- | the per-alternative re-testing baseline (semantically identical; boot keeps both).
-- | Both handle unconditional and guarded (ADR-0013) alternatives: a guard chain is tested
-- | at the matched leaf, falling through to the rows below. Occurrences (paths into the
-- | scrutinees) are bound to fresh locals so the operand stack is empty at every branch
-- | boundary (ADR-0003); labels are back-patched to relative offsets by `resolve`.
-- |
-- | To avoid a cyclic import, the two lowerings this needs from `Lower` — `atom` (a
-- | scrutinee atom) and `body` (an alternative's rhs, tail-aware) — are passed in as
-- | `Lowerers` (boot's `~atom`/`~body`).
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
import Data.Foldable (all, any, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Literal (Literal)
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom, Expr, Rhs(..))

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
-- | and is reversed once at `resolve`'s entry. (Forward end-append, no mid-stream length read —
-- | so reverse-cons is the right discipline here, unlike `Lower`'s offset-reading worker.)
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

-- | A leaf's right-hand side: an unconditional body commits; a guard chain (ADR-0013) is
-- | tested top to bottom, falling through to `onFail` when every guard is false.
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

-- --- pattern matrix (shared by both matchers) ---------------------------------------

-- | A binder's "head", with the variable names it binds at this occurrence peeled off
-- | (`BVar`/`BNamed`). `Cwild` imposes no test; the others impose a discriminant.
data Core
  = Cwild
  | Clit Literal
  | Cctor String (Array Binder)
  | Carr (Array Binder)
  | Crec (Array { prop :: String, binder :: Binder })

derive instance Eq Core

peel :: Binder -> { names :: Array String, core :: Core }
peel = case _ of
  BNull -> { names: [], core: Cwild }
  BVar x -> { names: [ x ], core: Cwild }
  BNamed x inner -> let p = peel inner in p { names = Array.cons x p.names }
  BLit l -> { names: [], core: Clit l }
  BCtor tag subs -> { names: [], core: Cctor tag subs }
  BArray subs -> { names: [], core: Carr subs }
  BRecord fields -> { names: [], core: Crec fields }

-- | One clause of the pattern matrix: binders still to test (aligned to the current
-- | occurrences), variable→occurrence bindings collected on the way down, and the rhs.
type DtRow = { pats :: Array Binder, binds :: Array (String /\ String), rhs :: Rhs }

bindsOf :: Array String -> String -> Array (String /\ String)
bindsOf names occ = names <#> \n -> n /\ occ

-- | Splice `news` in for the element at index `c`; remove the element at index `c`.
replaceCol :: forall a. Array a -> Int -> Array a -> Array a
replaceCol xs c news = Array.concat (Array.mapWithIndex (\i x -> if i == c then news else [ x ]) xs)

removeCol :: forall a. Array a -> Int -> Array a
removeCol xs c = fromMaybe xs (Array.deleteAt c xs)

idx :: forall a. Array a -> Int -> a
idx xs i = unsafePartial (Array.unsafeIndex xs i)

firstRefutable :: Array Core -> Int
firstRefutable cores = fromMaybe 0 (Array.findIndex (_ /= Cwild) cores)

-- --- naive matcher (per-alternative re-testing) -------------------------------------

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

-- --- decision-tree matcher (Maranget, ADR-0031) -------------------------------------

compileTree :: Lowerers -> Boolean -> Array Atom -> Array Alt -> CodeBlock
compileTree lw tail scruts alts = resolve (List.reverse (execState build { lbl: 0, occ: 0, buf: Nil }).buf)
  where
  build :: Asm Unit
  build = do
    endLbl <- freshLbl
    occ0 <- for scruts \a -> do
      o <- freshOcc "$dt"
      emit (Pinstr (lw.atom a))
      emit (Pinstr (Bind o))
      pure o
    compile endLbl occ0 (alts <#> \alt -> { pats: alt.binders, binds: [], rhs: alt.result })
    emit (Plabel endLbl)

  compile :: Int -> Array String -> Array DtRow -> Asm Unit
  compile endLbl occs rows = case Array.uncons rows of
    Nothing -> emit (Pinstr (Fail "case: no matching alternative"))
    Just { head: row0 } ->
      let
        cores0 = map (peel >>> _.core) row0.pats
      in
        if all (eq Cwild) cores0 then do
          -- leaf: bind every variable, then run the right-hand side
          let
            leafBinds = row0.binds
              <> Array.concat (Array.zipWith (\occ p -> bindsOf (peel p).names occ) occs row0.pats)
          for_ leafBinds \(name /\ occ) -> do
            emit (Pinstr (Load occ))
            emit (Pinstr (Bind name))
          emitRhs lw tail endLbl row0.rhs (compile endLbl occs (Array.drop 1 rows))
        else
          let
            c = firstRefutable cores0
            occ_c = idx occs c
          in
            case idx cores0 c of
              Cctor _ _ -> compileCtor endLbl occs rows c occ_c
              Clit _ -> compileLit endLbl occs rows c occ_c
              Carr _ -> compileArr endLbl occs rows c occ_c
              Crec _ -> compileRec endLbl occs rows c occ_c
              Cwild -> unsafeCrashWith "compile: refutable column is wild"

  -- switch on a constructor tag; each head specialises the matrix with its fields
  compileCtor :: Int -> Array String -> Array DtRow -> Int -> String -> Asm Unit
  compileCtor endLbl occs rows c occ_c = do
    let
      heads = Array.foldl
        ( \acc row -> case (peel (idx row.pats c)).core of
            Cctor tag subs | not (any (\(t /\ _) -> t == tag) acc) -> Array.snoc acc (tag /\ Array.length subs)
            _ -> acc
        )
        []
        rows
      defaultRows = wildRows rows c occ_c
    defaultLbl <- freshLbl
    headLbls <- for heads \(tag /\ arity) -> do
      l <- freshLbl
      pure { tag, arity, l }
    emit (Pinstr (Load occ_c))
    emit (PswitchCtor (headLbls <#> \h -> h.tag /\ h.l) defaultLbl)
    for_ headLbls \h -> do
      emit (Plabel h.l)
      subOccs <- replicateA h.arity (freshOcc "$dt")
      forWithIndex_ subOccs \j o -> emitExtract occ_c (Proj j) o
      let
        rows' = Array.mapMaybe
          ( \row ->
              let
                p = peel (idx row.pats c)
                binds' = row.binds <> bindsOf p.names occ_c
              in
                case p.core of
                  Cctor t subs | t == h.tag -> Just { pats: replaceCol row.pats c subs, binds: binds', rhs: row.rhs }
                  Cwild -> Just { pats: replaceCol row.pats c (Array.replicate h.arity BNull), binds: binds', rhs: row.rhs }
                  _ -> Nothing
          )
          rows
      compile endLbl (replaceCol occs c subOccs) rows'
    emit (Plabel defaultLbl)
    compile endLbl (removeCol occs c) defaultRows

  -- switch on a scalar literal; literal heads have no sub-columns
  compileLit :: Int -> Array String -> Array DtRow -> Int -> String -> Asm Unit
  compileLit endLbl occs rows c occ_c = do
    let
      heads = Array.foldl
        ( \acc row -> case (peel (idx row.pats c)).core of
            Clit l | not (any (eq l) acc) -> Array.snoc acc l
            _ -> acc
        )
        []
        rows
      occs' = removeCol occs c
      selectRows keep = Array.mapMaybe
        ( \row ->
            let
              p = peel (idx row.pats c)
            in
              if keep p.core then Just { pats: removeCol row.pats c, binds: row.binds <> bindsOf p.names occ_c, rhs: row.rhs }
              else Nothing
        )
        rows
    defaultLbl <- freshLbl
    headLbls <- for heads \l -> do
      lbl <- freshLbl
      pure (l /\ lbl)
    emit (Pinstr (Load occ_c))
    emit (PswitchLit (headLbls <#> \(l /\ lbl) -> l /\ lbl) defaultLbl)
    for_ headLbls \(l /\ lbl) -> do
      emit (Plabel lbl)
      compile endLbl occs' (selectRows \core -> core == Clit l || core == Cwild)
    emit (Plabel defaultLbl)
    compile endLbl occs' (selectRows (eq Cwild))

  -- switch on a `Varray`'s length (ADR-0012); each length head exposes its elements
  compileArr :: Int -> Array String -> Array DtRow -> Int -> String -> Asm Unit
  compileArr endLbl occs rows c occ_c = do
    let
      heads = Array.foldl
        ( \acc row -> case (peel (idx row.pats c)).core of
            Carr subs | not (any (eq (Array.length subs)) acc) -> Array.snoc acc (Array.length subs)
            _ -> acc
        )
        []
        rows
      defaultRows = wildRows rows c occ_c
    defaultLbl <- freshLbl
    headLbls <- for heads \len -> do
      lbl <- freshLbl
      pure (len /\ lbl)
    emit (Pinstr (Load occ_c))
    emit (PswitchLen (headLbls <#> \(len /\ lbl) -> len /\ lbl) defaultLbl)
    for_ headLbls \(arity /\ lbl) -> do
      emit (Plabel lbl)
      subOccs <- replicateA arity (freshOcc "$dt")
      forWithIndex_ subOccs \j o -> emitExtract occ_c (Proj_arr j) o
      let
        rows' = Array.mapMaybe
          ( \row ->
              let
                p = peel (idx row.pats c)
                binds' = row.binds <> bindsOf p.names occ_c
              in
                case p.core of
                  Carr subs | Array.length subs == arity -> Just { pats: replaceCol row.pats c subs, binds: binds', rhs: row.rhs }
                  Cwild -> Just { pats: replaceCol row.pats c (Array.replicate arity BNull), binds: binds', rhs: row.rhs }
                  _ -> Nothing
          )
          rows
      compile endLbl (replaceCol occs c subOccs) rows'
    emit (Plabel defaultLbl)
    compile endLbl (removeCol occs c) defaultRows

  -- a record pattern imposes no discriminant; expand the union of named labels as columns
  compileRec :: Int -> Array String -> Array DtRow -> Int -> String -> Asm Unit
  compileRec endLbl occs rows c occ_c = do
    let
      labels = Array.foldl
        ( \acc row -> case (peel (idx row.pats c)).core of
            Crec fields -> Array.foldl (\a f -> if any (eq f.prop) a then a else Array.snoc a f.prop) acc fields
            _ -> acc
        )
        []
        rows
    subOccs <- replicateA (Array.length labels) (freshOcc "$dt")
    for_ (Array.zip labels subOccs) \(l /\ o) -> emitExtract occ_c (GetField l) o
    let
      rows' = rows <#> \row ->
        let
          p = peel (idx row.pats c)
          subpats = case p.core of
            Crec fields -> labels <#> \l -> maybe BNull _.binder (Array.find (\f -> f.prop == l) fields)
            Cwild -> labels <#> const BNull
            _ -> unsafeCrashWith "compileRec: non-record column"
        in
          { pats: replaceCol row.pats c subpats, binds: row.binds <> bindsOf p.names occ_c, rhs: row.rhs }
    compile endLbl (replaceCol occs c subOccs) rows'

  -- rows whose column `c` is a wildcard: keep them for the default edge, column removed
  wildRows :: Array DtRow -> Int -> String -> Array DtRow
  wildRows rows c occ_c = Array.mapMaybe
    ( \row ->
        let
          p = peel (idx row.pats c)
        in
          case p.core of
            Cwild -> Just { pats: removeCol row.pats c, binds: row.binds <> bindsOf p.names occ_c, rhs: row.rhs }
            _ -> Nothing
    )
    rows
