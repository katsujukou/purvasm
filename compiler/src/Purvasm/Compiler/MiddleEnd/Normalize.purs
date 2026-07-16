-- | `normalize`: the CESK AST (upper IR) → ANF (lower IR) bridge (ADR-0025/0037).
-- | A-normalises (every argument an atom, every compound subexpression `let`-named so
-- | evaluation order is explicit) and uncurries (a `\a -> \b -> …` spine becomes one
-- | `CLam [a, b] …`; an `((f a) b) c` spine becomes one `CApp f [a, b, c]`).
-- |
-- | Ported from boot's `Middle_end.Transl.transl`. Boot walks in CPS where `k` builds the
-- | surrounding `let`-sequence; a host-stack CPS is **not stack-safe** — one continuation
-- | chain per array element / let-spine entry overflowed the default node stack on the
-- | `Regex.Core.Unicode` table literals (2026-07-16, maintainer-authorized stack-safety
-- | bugfix) — so this port replaces the continuations with an explicit **binding prefix**:
-- | each walker returns the `let`-bindings to wrap (innermost-first, so sequential
-- | composition is a cons) alongside its value, the fresh-name counter is threaded as a
-- | plain accumulator (a `State` bind chain would just move the depth into the monad), and
-- | every data-sized spine (array elements, argument lists, let chains, alternatives) is
-- | consumed by a self-recursive **tail** loop. Host recursion remains only for genuine
-- | term *nesting*, which is bounded by source nesting depth. The fresh-`$a…` numbering
-- | order and the `Let` wrapping order are exactly the CPS walk's.
-- |
-- | The boot differential is up to α-renaming, so the fresh-name *order* need not match
-- | boot — but it must stay deterministic and mode-stable, and two comment'd sites below
-- | (`TmIf`, guarded alternatives) deliberately mirror boot's OCaml right-to-left
-- | evaluation order for byte-identical `$a…` numbering.
module Purvasm.Compiler.MiddleEnd.Normalize where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.CESK.AST (Alternative, Term(..))
import Purvasm.Compiler.CESK.AST (Rhs(..)) as Cesk
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))

-- | Collect a curried lambda spine `\a -> \b -> body` into `{ params: [a, b], body }`.
collectLam :: Term -> { params :: Array String, body :: Term }
collectLam = go []
  where
  go acc (TmLam p body) = go (Array.snoc acc p) body
  go acc t = { params: acc, body: t }

-- | Collect an application spine `((f a) b) c` into `{ head: f, args: [a, b, c] }`.
collectApp :: Term -> { head :: Term, args :: Array Term }
collectApp t0 = go t0 []
  where
  go (TmApp f a) args = go f (Array.cons a args)
  go t args = { head: t, args }

-- | One deferred wrapper of the eventual tail expression: the defunctionalised form of
-- | what the CPS walk's continuations built on the way back out.
data Bind
  = BLet String CExpr
  | BLetRec (Array { var :: String, rhs :: Expr })

-- | The threaded walker state: the fresh-name counter and the pending binding prefix,
-- | **innermost bind at the head** (so pushing is a cons and `assemble`'s left fold wraps
-- | innermost-first — both stack-free).
type St = { n :: Int, pre :: List Bind }

-- | Wrap the pending prefix around a tail expression (innermost-first left fold).
assemble :: List Bind -> CExpr -> Expr
assemble pre ce = List.foldl (\e b -> wrap b e) (Ret ce) pre
  where
  wrap = case _ of
    BLet x c -> Let x c
    BLetRec binds -> LetRec binds

-- | The next fresh `$a…` name (numbering identical to the CPS walk's pre-incremented
-- | `State Int`).
fresh :: Int -> { n :: Int, name :: String }
fresh n = { n: n + 1, name: "$a" <> show (n + 1) }

normalize :: Term -> Expr
normalize term = (anfTail 0 term).expr

-- | Normalise `t` to a full (prefix-assembled) expression in tail position, in its own
-- | binding scope: only the counter threads through; the prefix never escapes a scope.
anfTail :: Int -> Term -> { n :: Int, expr :: Expr }
anfTail n0 t =
  let
    r = normCe { n: n0, pre: Nil } t
  in
    { n: r.st.n, expr: assemble r.st.pre r.ce }

-- | Normalise `t` to its computation, pushing any `let`-bindings it needs onto the prefix.
normCe :: St -> Term -> { st :: St, ce :: CExpr }
normCe st t = case t of
  TmLit l -> { st, ce: CAtom (AtomLit l) }
  TmVar x -> { st, ce: CAtom (AtomVar x) }
  TmForeign s -> { st, ce: CAtom (AtomForeign s) }
  TmCtor tag arity -> { st, ce: CCtor tag arity [] }
  TmLam _ _ ->
    let
      { params, body } = collectLam t
      rb = anfTail st.n body
    in
      { st: st { n = rb.n }, ce: CLam params rb.expr }
  TmApp _ _ ->
    let
      { head, args } = collectApp t
      ra = normAtoms st args
    in
      case head of
        -- a saturated/partial constructor application stays a constructor node
        TmCtor tag arity -> { st: ra.st, ce: CCtor tag arity ra.atoms }
        _ ->
          let
            rh = normAtom ra.st head
          in
            { st: rh.st, ce: CApp rh.atom ra.atoms }
  TmPrim op args ->
    let
      ra = normAtoms st args
    in
      { st: ra.st, ce: CPrim op ra.atoms }
  TmArray es ->
    let
      ra = normAtoms st es
    in
      { st: ra.st, ce: CArray ra.atoms }
  TmRecord fields ->
    let
      ra = normAtoms st (map _.term fields)
    in
      { st: ra.st
      , ce: CRecord (Array.zipWith (\f a -> { prop: f.prop, val: a }) fields ra.atoms)
      }
  TmAccessor e label ->
    let
      r = normAtom st e
    in
      { st: r.st, ce: CAccessor r.atom label }
  TmUpdate e ups ->
    let
      re = normAtom st e
      ra = normAtoms re.st (map _.term ups)
    in
      { st: ra.st
      , ce: CUpdate re.atom (Array.zipWith (\u v -> { prop: u.prop, val: v }) ups ra.atoms)
      }
  TmIf c t1 t2 ->
    -- boot builds `CIf (ca, anf_tail t1, anf_tail t2)`; OCaml evaluates a constructor's
    -- arguments right-to-left, so the *else* branch's fresh names are allocated before
    -- the *then* branch's. Match that order for byte-identical `$a…` numbering.
    let
      rc = normAtom st c
      r2 = anfTail rc.st.n t2
      r1 = anfTail r2.n t1
    in
      { st: rc.st { n = r1.n }, ce: CIf rc.atom r1.expr r2.expr }
  -- a source `let`-chain is consumed by `goLet`'s tail loop (each binding is a prefix
  -- push, not a wrapper frame). Deliberately not a `normCe` self tail call: `normCe`
  -- also calls itself non-tail (e.g. on `e1`), which disables purs's self-TCO for the
  -- whole function — the spine loop must live in a helper whose self-calls are all tail.
  TmLet _ _ _ -> goLet st t
  TmLetrec binds body ->
    let
      rb = go st.n Nil 0
      go n acc i = case Array.index binds i of
        Nothing -> { n, binds: Array.fromFoldable (List.reverse acc) }
        Just (x /\ rhs) ->
          let
            r = anfTail n rhs
          in
            go r.n ({ var: x, rhs: r.expr } : acc) (i + 1)
    in
      normCe { n: rb.n, pre: BLetRec rb.binds : st.pre } body
  TmCase scruts alts ->
    let
      rs = normAtoms st scruts
      ra = go rs.st.n Nil 0
      go n acc i = case Array.index alts i of
        Nothing -> { n, alts: Array.fromFoldable (List.reverse acc) }
        Just a ->
          let
            r = anfAlt n a
          in
            go r.n (r.alt : acc) (i + 1)
    in
      { st: rs.st { n = ra.n }, ce: CCase rs.atoms ra.alts }

-- | Consume a `let`-spine iteratively: push each binding onto the prefix, tail-loop into
-- | the body (self-calls all tail ⇒ purs TCO ⇒ a chain of any length costs no host stack).
goLet :: St -> Term -> { st :: St, ce :: CExpr }
goLet st = case _ of
  TmLet x e1 e2 ->
    let
      r1 = normCe st e1
    in
      goLet (r1.st { pre = BLet x r1.ce : r1.st.pre }) e2
  t -> normCe st t

-- | Normalise `t` to an atom, `let`-binding it to a fresh name if it is compound (the
-- | name is minted before anything to the right of `t` is walked, like the CPS original).
normAtom :: St -> Term -> { st :: St, atom :: Atom }
normAtom st t =
  let
    r = normCe st t
  in
    case r.ce of
      CAtom a -> { st: r.st, atom: a }
      ce ->
        let
          f = fresh r.st.n
        in
          { st: { n: f.n, pre: BLet f.name ce : r.st.pre }, atom: AtomVar f.name }

-- | Normalise a spine of terms to atoms, left to right — a tail loop, one iteration per
-- | element (the CPS original nested a continuation frame per element, which is exactly
-- | the overflow this rewrite removes).
normAtoms :: St -> Array Term -> { st :: St, atoms :: Array Atom }
normAtoms st0 ts = go st0 Nil 0
  where
  go st acc i = case Array.index ts i of
    Nothing -> { st, atoms: Array.fromFoldable (List.reverse acc) }
    Just t ->
      let
        r = normAtom st t
      in
        go r.st (r.atom : acc) (i + 1)

anfAlt :: Int -> Alternative -> { n :: Int, alt :: Alt }
anfAlt n0 a = case a.result of
  Cesk.Unconditional e ->
    let
      r = anfTail n0 e
    in
      { n: r.n, alt: { binders: a.binders, result: Uncond r.expr } }
  -- boot builds the tuple `(anf_tail g, anf_tail e)`; OCaml's right-to-left evaluation
  -- allocates the rhs's fresh names before the guard's. Match that for byte-identity.
  Cesk.Guarded gs ->
    let
      r = go n0 Nil 0
      go n acc i = case Array.index gs i of
        Nothing -> { n, gs: Array.fromFoldable (List.reverse acc) }
        Just g ->
          let
            rr = anfTail n g.rhs
            rg = anfTail rr.n g.guard
          in
            go rg.n ({ guard: rg.expr, rhs: rr.expr } : acc) (i + 1)
    in
      { n: r.n, alt: { binders: a.binders, result: Guarded r.gs } }
