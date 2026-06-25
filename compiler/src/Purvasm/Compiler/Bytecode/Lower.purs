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

-- | Lower an `Expr` (a let-sequence ending in a tail computation).
lowerExpr :: Boolean -> Expr -> CodeBlock
lowerExpr tail = case _ of
  Ret c -> lowerCexpr tail c
  Let x c rest -> lowerCexpr false c <> [ Bind x ] <> lowerExpr tail rest
  LetRec binds rest ->
    [ MakeRec (binds <#> \b -> b.var /\ fnChunk b.rhs) ] <> lowerExpr tail rest

-- | A function body is always compiled in tail position.
fnChunk :: Expr -> CodeBlock
fnChunk e = lowerExpr true e

-- | Lower a single computation. `CApp`/`CIf`/`CCase` shape control flow; the rest leave
-- | one value, followed by `Return` when in tail position.
lowerCexpr :: Boolean -> CExpr -> CodeBlock
lowerCexpr tail = case _ of
  CApp h args ->
    ([ lowerAtom h ] <> map lowerAtom args)
      <> [ if tail then TailCall (Array.length args) else Call (Array.length args) ]
  CIf a t e ->
    let
      tc = lowerExpr tail t
      ec = lowerExpr tail e
    in
      if tail
      -- each branch ends with Return/TailCall; skip `then` when the test is false
      then [ lowerAtom a, JumpUnless (Array.length tc) ] <> tc <> ec
      -- each branch leaves a value; `then` jumps over `else` to the join point
      else [ lowerAtom a, JumpUnless (Array.length tc + 1) ] <> tc <> [ Jump (Array.length ec) ] <> ec
  CCase scruts alts -> lowerCase tail scruts alts
  c -> lowerValue c <> (if tail then [ Return ] else [])

-- | The non-control computations: each leaves exactly one value on the stack.
lowerValue :: CExpr -> CodeBlock
lowerValue = case _ of
  CAtom a -> [ lowerAtom a ]
  CPrim op args -> map lowerAtom args <> [ Prim op (Array.length args) ]
  CCtor tag arity args -> map lowerAtom args <> [ Ctor tag arity (Array.length args) ]
  CArray atoms -> map lowerAtom atoms <> [ Array (Array.length atoms) ]
  CRecord fields -> map (\f -> lowerAtom f.val) fields <> [ Record (map _.prop fields) ]
  CAccessor a label -> [ lowerAtom a, GetField label ]
  CUpdate a ups ->
    ([ lowerAtom a ] <> map (\u -> lowerAtom u.val) ups) <> [ Update (map _.prop ups) ]
  CLam ps b -> [ Closure ps (fnChunk b) ]
  _ -> unsafeCrashWith "lowerValue: control computation handled in lowerCexpr"

-- | A `case`, compiled to a decision tree (ADR-0031) by `Lower.Match`, given this
-- | module's atom/body lowerings.
lowerCase :: Boolean -> Array Atom -> Array Alt -> CodeBlock
lowerCase tail scruts alts = compileTree { atom: lowerAtom, body: lowerExpr } tail scruts alts
