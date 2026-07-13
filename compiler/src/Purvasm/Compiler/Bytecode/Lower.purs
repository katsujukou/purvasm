-- | `lower`: ANF (lower IR) → PURVASM bytecode (`CodeBlock`), ADR-0030/0031/0037.
-- |
-- | Postorder emit (ADR-0003): a subexpression's operands are pushed first, so each
-- | leaves exactly one value on the operand stack. `tail` is threaded so a call in tail
-- | position becomes `TailCall` (and a tail computation ends with `Return`); `if`
-- | compiles to relative jumps within the chunk. A `case` is delegated to
-- | `Lower.Match` (ADR-0031), supplied with this module's `lowerAtom`/`lowerExpr`.
module Purvasm.Compiler.Bytecode.Lower where

import Prelude

import Data.Array as Array
import Data.List (List, (:))
import Data.List as List
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Bytecode.Lower.Match (compileTree)
import Purvasm.Compiler.Literal (Literal(LInt, LNumber, LBool, LString)) as L
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..))

lowerAtom :: Atom -> Instruction
lowerAtom = case _ of
  AtomVar x -> Load x
  AtomLit (L.LInt n) -> PushInt n
  AtomLit (L.LNumber f) -> PushNumber f
  AtomLit (L.LBool b) -> PushBool b
  AtomLit (L.LString s) -> PushString s
  AtomForeign s -> ForeignRef s

-- | Lower an `Expr` (a let-sequence ending in a tail computation) to a `CodeBlock`.
-- |
-- | The recursive worker (`lowerExpr'` and friends, ADR-0049) stays in `List` space, whose
-- | `Semigroup` `append` is tail-sharing (boot's OCaml `@`): `small <> big_tail` copies only
-- | `small` and shares the recursive tail, so a `let`-spine costs O(L), not O(L²). The result
-- | is materialised to an `Array` exactly once, here at the chunk boundary. Crucially the
-- | seam back into `Match` (and `CIf`'s `length tc`) is also computed in `List` space — turning
-- | any of this back into `Array <>` would reinstate the quadratic.
lowerExpr :: Boolean -> Expr -> CodeBlock
lowerExpr tail = Array.fromFoldable <<< lowerExpr' tail

lowerExpr' :: Boolean -> Expr -> List Instruction
lowerExpr' tail = case _ of
  Ret c -> lowerCexpr' tail c
  Let x c rest -> lowerCexpr' false c <> (Bind x : lowerExpr' tail rest)
  LetRec binds rest ->
    MakeRec (binds <#> \b -> b.var /\ fnChunk b.rhs) : lowerExpr' tail rest

-- | A function body is always compiled in tail position. This is an `Array` chunk boundary.
fnChunk :: Expr -> CodeBlock
fnChunk e = lowerExpr true e

-- | `Array` boundaries over the `List`-space workers, retained as the public (and
-- | unit-tested) API. Not on the recursive hot path — those go through the primed workers.
lowerCexpr :: Boolean -> CExpr -> CodeBlock
lowerCexpr tail = Array.fromFoldable <<< lowerCexpr' tail

lowerValue :: CExpr -> CodeBlock
lowerValue = Array.fromFoldable <<< lowerValue'

-- | Lower a single computation. `CApp`/`CIf`/`CCase` shape control flow; the rest leave
-- | one value, followed by `Return` when in tail position.
lowerCexpr' :: Boolean -> CExpr -> List Instruction
lowerCexpr' tail = case _ of
  CApp h args ->
    (lowerAtom h : List.fromFoldable (map lowerAtom args))
      <> List.singleton (if tail then TailCall (Array.length args) else Call (Array.length args))
  CIf a t e ->
    let
      tc = lowerExpr' tail t
      ec = lowerExpr' tail e
    in
      if tail
      -- each branch ends with Return/TailCall; skip `then` when the test is false
      then (lowerAtom a : JumpUnless (List.length tc) : tc) <> ec
      -- each branch leaves a value; `then` jumps over `else` to the join point
      else (lowerAtom a : JumpUnless (List.length tc + 1) : tc) <> (Jump (List.length ec) : ec)
  CCase scruts alts -> List.fromFoldable (lowerCase tail scruts alts)
  -- GER run point (ADR-0099): `perform t ≃ t unit` — a one-argument (unit) call, tail-aware so a
  -- self-tail `perform` still loops.
  CPerform t ->
    (lowerAtom t : List.singleton (lowerAtom (AtomLit (L.LInt 0))))
      <> List.singleton (if tail then TailCall 1 else Call 1)
  c -> let v = lowerValue' c in if tail then v <> List.singleton Return else v

-- | The non-control computations: each leaves exactly one value on the stack.
lowerValue' :: CExpr -> List Instruction
lowerValue' = case _ of
  CAtom a -> List.singleton (lowerAtom a)
  CPrim op args -> List.fromFoldable (map lowerAtom args) <> List.singleton (Prim op (Array.length args))
  CCtor tag arity args -> List.fromFoldable (map lowerAtom args) <> List.singleton (Ctor tag arity (Array.length args))
  CArray atoms -> List.fromFoldable (map lowerAtom atoms) <> List.singleton (Array (Array.length atoms))
  CRecord fields -> List.fromFoldable (map (\f -> lowerAtom f.val) fields) <> List.singleton (Record (map _.prop fields))
  CAccessor a label -> lowerAtom a : List.singleton (GetField label)
  CUpdate a ups ->
    (lowerAtom a : List.fromFoldable (map (\u -> lowerAtom u.val) ups)) <> List.singleton (Update (map _.prop ups))
  CLam ps b -> List.singleton (Closure ps (fnChunk b))
  _ -> unsafeCrashWith "lowerValue': control computation handled in lowerCexpr'"

-- | A `case`, compiled to a decision tree (ADR-0031) by `Lower.Match`, given this module's
-- | atom/body lowerings. `Match` returns an `Array` chunk (its own assembler), which the
-- | caller converts back to `List` at the seam — a bounded, linear conversion per `case`.
lowerCase :: Boolean -> Array Atom -> Array Alt -> CodeBlock
lowerCase tail scruts alts = compileTree { atom: lowerAtom, body: lowerExpr } tail scruts alts
