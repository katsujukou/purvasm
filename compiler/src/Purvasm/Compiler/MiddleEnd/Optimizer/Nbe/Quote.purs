-- | Reify a semantic value back to ANF (ADR-0089 §2). The fresh-name discipline is pinned:
-- | `quote` **α-renames every binder it reifies** from the reserved `$q<n>` supply — a namespace
-- | disjoint from every other name producer in the pipeline (source identifiers cannot start with
-- | `$`; `purs` synthesises `$__unused`/`…$Dict`; `Normalize` mints `$a<n>`; `MatchCompile` mints
-- | `$dt<n>`) — so no binder from the input, an unfolded sibling body, or an upstream generator
-- | survives, and collisions are impossible by namespace disjointness, not by counter luck. Free
-- | names (top-level/foreign keys) are reified verbatim.
-- |
-- | Reconstruction is ANF-disciplined like `Normalize`: a compound value in operand position is
-- | `let`-bound at its use site; an `SLet`'s computation is emitted **in place** (the ADR-0089 §5
-- | pinning — this is the only point that linearises sequencing, and it never reorders).
-- |
-- | Stack-safety (2026-07-16 bugfix, the deferral in the earlier note cashed in): the CPS walk
-- | stacked one host frame per `SLet`-spine entry / operand-array element and overflowed a reduced
-- | stack on `Regex.Core.Unicode`-scale table literals (~0.5× the default was the measured
-- | margin). Like `Normalize`, the walk now returns an explicit **binding prefix**
-- | (innermost-first, so composition is a cons and `assemble`'s fold wraps stack-free), threads
-- | the `$q` counter as a plain accumulator, and consumes every data-sized spine with
-- | self-recursive **tail** loops (`goSpine` for `SLet`/`SLetRec` chains — kept out of `quoteC`,
-- | whose non-tail self-calls would disable purs's self-TCO). Host recursion remains only on
-- | genuine term *nesting*. The `$q` numbering and `Let` wrapping orders are exactly the CPS
-- | walk's.
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Quote
  ( quote
  ) where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (Comp(..), NRhs(..), Sem(..))

-- | One deferred wrapper of the eventual tail expression (the defunctionalised continuation).
data QBind
  = QLet String CExpr
  | QLetRec (Array { var :: String, rhs :: Expr })

-- | The threaded state: the `$q` counter and the pending prefix, **innermost bind at the head**.
type St = { n :: Int, pre :: List QBind }

fresh :: Int -> { n :: Int, name :: String }
fresh n = { n: n + 1, name: "$q" <> show (n + 1) }

assemble :: List QBind -> CExpr -> Expr
assemble pre ce = List.foldl (\e b -> wrap b e) (Ret ce) pre
  where
  wrap = case _ of
    QLet x c -> Let x c
    QLetRec binds -> LetRec binds

-- | Reify a binding body's semantic value to ANF, with a per-binding deterministic supply.
quote :: Sem -> Expr
quote s = (tailQ 0 s).expr

-- | Reify to a full expression in its own binding scope (a lambda/branch/rhs body): only the
-- | counter threads through; the prefix never escapes a scope.
tailQ :: Int -> Sem -> { n :: Int, expr :: Expr }
tailQ n0 s =
  let
    r = goSpine { n: n0, pre: Nil } s
  in
    { n: r.st.n, expr: assemble r.st.pre r.ce }

-- | Consume an `SLet`/`SLetRec` spine iteratively (self-calls all tail ⇒ purs TCO ⇒ a chain of
-- | any length costs no host stack), delegating everything else to `quoteC`. The `SLet` binding
-- | order is the CPS walk's: the rhs's own reification first, then the fresh binder, then the
-- | continuation.
goSpine :: St -> Sem -> { st :: St, ce :: CExpr }
goSpine st s = case s of
  SLet _ rhs kont ->
    let
      r = quoteC st rhs
      f = fresh r.st.n
    in
      goSpine { n: f.n, pre: QLet f.name r.ce : r.st.pre } (kont (SVar f.name))
  SLetRec bs kont ->
    let
      names = go st.n Nil 0
      go n acc i = case Array.index bs i of
        Nothing -> { n, names: Array.fromFoldable (List.reverse acc) }
        Just _ -> let f = fresh n in go f.n (f.name : acc) (i + 1)
      vars = map SVar names.names
      rhss = goRhss names.n Nil 0
      goRhss n acc i = case Array.index bs i of
        Nothing -> { n, rhss: Array.fromFoldable (List.reverse acc) }
        Just b ->
          let
            r = tailQ n (b.rhsF vars)
          in
            goRhss r.n (r.expr : acc) (i + 1)
      binds = Array.zipWith (\v r -> { var: v, rhs: r }) names.names rhss.rhss
    in
      goSpine { n: rhss.n, pre: QLetRec binds : st.pre } (kont vars)
  other -> quoteC st other

-- | Reify into a computation position, pushing operand `let`-bindings onto the prefix.
quoteC :: St -> Sem -> { st :: St, ce :: CExpr }
quoteC st s = case s of
  SLet _ _ _ -> goSpine st s
  SLetRec _ _ -> goSpine st s
  SLit l -> { st, ce: CAtom (AtomLit l) }
  SVar x -> { st, ce: CAtom (AtomVar x) }
  SForeign f -> { st, ce: CAtom (AtomForeign f) }
  SLam hints f ->
    let
      ps = go st.n Nil 0
      go n acc i = case Array.index hints i of
        Nothing -> { n, ps: Array.fromFoldable (List.reverse acc) }
        Just _ -> let fr = fresh n in go fr.n (fr.name : acc) (i + 1)
      body = tailQ ps.n (f (map SVar ps.ps))
    in
      { st: st { n = body.n }, ce: CLam ps.ps body.expr }
  SCtor t n args ->
    let
      r = quoteAtoms st args
    in
      { st: r.st, ce: CCtor t n r.atoms }
  SArr args ->
    let
      r = quoteAtoms st args
    in
      { st: r.st, ce: CArray r.atoms }
  SRec fs ->
    let
      r = quoteAtoms st (map _.val fs)
    in
      { st: r.st, ce: CRecord (Array.zipWith (\f a -> { prop: f.prop, val: a }) fs r.atoms) }
  SRef r -> case r.spine of
    [] -> { st, ce: CAtom r.atom }
    sp ->
      let
        ra = quoteAtoms st sp
      in
        { st: ra.st, ce: CApp r.atom ra.atoms }
  SComp c -> case c of
    NApp h args ->
      let
        rh = quoteAtom st h
        ra = quoteAtoms rh.st args
      in
        { st: ra.st, ce: CApp rh.atom ra.atoms }
    NPrim op args ->
      let
        ra = quoteAtoms st args
      in
        { st: ra.st, ce: CPrim op ra.atoms }
    NAcc v f ->
      let
        r = quoteAtom st v
      in
        { st: r.st, ce: CAccessor r.atom f }
    NPerform v ->
      let
        r = quoteAtom st v
      in
        { st: r.st, ce: CPerform r.atom }
    NUpd v ups ->
      let
        rv = quoteAtom st v
        ra = quoteAtoms rv.st (map _.val ups)
      in
        { st: ra.st
        , ce: CUpdate rv.atom (Array.zipWith (\u v' -> { prop: u.prop, val: v' }) ups ra.atoms)
        }
    NIf cond t e ->
      let
        rc = quoteAtom st cond
        rt = tailQ rc.st.n t
        re = tailQ rt.n e
      in
        { st: rc.st { n = re.n }, ce: CIf rc.atom rt.expr re.expr }
    NCase scruts alts ->
      let
        rs = quoteAtoms st scruts
        ra = goAlts rs.st.n Nil 0
        goAlts n acc i = case Array.index alts i of
          Nothing -> { n, alts: Array.fromFoldable (List.reverse acc) }
          Just alt ->
            let
              r = quoteAlt n alt
            in
              goAlts r.n (r.alt : acc) (i + 1)
      in
        { st: rs.st { n = ra.n }, ce: CCase rs.atoms ra.alts }

quoteAlt
  :: Int
  -> { shape :: Array Binder, vars :: Array String, result :: NRhs }
  -> { n :: Int, alt :: { binders :: Array Binder, result :: Rhs } }
quoteAlt n0 alt =
  let
    freshes = go n0 Nil 0
    go n acc i = case Array.index alt.vars i of
      Nothing -> { n, names: Array.fromFoldable (List.reverse acc) }
      Just _ -> let f = fresh n in go f.n (f.name : acc) (i + 1)
    renames = Map.fromFoldable (Array.zip alt.vars freshes.names)
    vars = map SVar freshes.names
  in
    case alt.result of
      NUncond f ->
        let
          r = tailQ freshes.n (f vars)
        in
          { n: r.n, alt: { binders: map (renameBinder renames) alt.shape, result: Uncond r.expr } }
      NGuarded gs ->
        let
          r = goGs freshes.n Nil 0
          goGs n acc i = case Array.index gs i of
            Nothing -> { n, gs: Array.fromFoldable (List.reverse acc) }
            Just g ->
              let
                rg = tailQ n (g.guard vars)
                rr = tailQ rg.n (g.rhs vars)
              in
                goGs rr.n ({ guard: rg.expr, rhs: rr.expr } : acc) (i + 1)
        in
          { n: r.n
          , alt: { binders: map (renameBinder renames) alt.shape, result: Guarded r.gs }
          }

-- | Reify into an atom position: compound values are `let`-bound at the use site (the fresh name
-- | is minted after the compound's own reification, before anything to its right — CPS order).
quoteAtom :: St -> Sem -> { st :: St, atom :: Atom }
quoteAtom st s = case s of
  SLit l -> { st, atom: AtomLit l }
  SVar x -> { st, atom: AtomVar x }
  SForeign f -> { st, atom: AtomForeign f }
  SRef r | Array.null r.spine -> { st, atom: r.atom }
  compound ->
    let
      r = goSpine st compound
      f = fresh r.st.n
    in
      { st: { n: f.n, pre: QLet f.name r.ce : r.st.pre }, atom: AtomVar f.name }

-- | Reify a spine of operands left to right — a tail loop, one iteration per element.
quoteAtoms :: St -> Array Sem -> { st :: St, atoms :: Array Atom }
quoteAtoms st0 ss = go st0 Nil 0
  where
  go st acc i = case Array.index ss i of
    Nothing -> { st, atoms: Array.fromFoldable (List.reverse acc) }
    Just s ->
      let
        r = quoteAtom st s
      in
        go r.st (r.atom : acc) (i + 1)

-- | Rename a binder shape's variables with the alternative's fresh-name map (total on
-- | `binderVarsOrdered`, so the fallback branch is unreachable for well-formed alternatives).
renameBinder :: Map String String -> Binder -> Binder
renameBinder m = case _ of
  BNull -> BNull
  BLit l -> BLit l
  BVar x -> BVar (rn x)
  BNamed x b -> BNamed (rn x) (renameBinder m b)
  BCtor t bs -> BCtor t (map (renameBinder m) bs)
  BArray bs -> BArray (map (renameBinder m) bs)
  BRecord fs -> BRecord (fs <#> \f -> { prop: f.prop, binder: renameBinder m f.binder })
  where
  rn x = fromMaybe x (Map.lookup x m)
