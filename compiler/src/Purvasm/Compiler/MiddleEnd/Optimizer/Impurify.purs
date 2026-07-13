-- | Generalized effect reflection / impurification (GER, ADR-0099): rewrite the `Effect` monad glue
-- | into canonical ANF carrying the explicit `CPerform` run-marker, so the reduction core (NbE) and
-- | `EffectAnalysis` reason about *where* an effect fires instead of a backend runtime representation
-- | leaking through the middle end.
-- |
-- | A single `GerDescriptor` per key is the source of truth: its `key`, its `semanticArity` (the
-- | source/FSR exact-saturation arguments the canonical rewrite needs — *not* the trailing run unit),
-- | its `origin` (a GER-semantic classification, orthogonal to provider form), and its `lowering`.
-- | `ownedKeys` is every descriptor; `structuralExclusions` is the `StructuralGuest` subset that leaves
-- | the NbE structural rung (ADR-0099 §3 — so NbE never re-hides the node by unfolding it to the guest
-- | term). The `--no-opt` fallback differs by provider form: the `StructuralGuest` keys keep their
-- | link-time `resolver` guest terms (`Effect.pureE`/`bindE`), whereas the `EffectEliminator`
-- | `unsafePerformEffect` is (currently) a `ulib` PS shadow that `--no-opt` compiles normally.
-- |
-- | The pass recognises a GER key **two ways** (ADR-0099 §5/§6): a bare GER foreign atom at any
-- | saturation, *and* a statically-known `Effect` dispatch (`accessor dict args`) resolved directly
-- | through `DictMachinery` — the main path, so lowering does not wait for NbE to expose the bare
-- | foreign. Under-applied occurrences are η-expanded (never declined); over-applied ones splice a
-- | `let` for the surplus. The pass is idempotent (a lowered `CPerform` tree holds no GER foreign
-- | application) and runs at two points around the reduction core (`early`/`close`, wired by
-- | `optimizeModule`) so no GER-owned foreign reaches codegen even at `--opt-max-iter 1`.
module Purvasm.Compiler.MiddleEnd.Optimizer.Impurify
  ( ownedKeys
  , structuralExclusions
  , impurifyExpr
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, modify_)
import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), stripPrefix)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery)

-- | The GER-semantic role of a key, **orthogonal to its provider form** (a structural guest term,
-- | a `ulib` shadow, or a foreign leaf — independent of role). `StructuralGuest` — a monad-glue
-- | combinator with an `Ffi.structural` guest term (`pureE` / `bindE`); it leaves the NbE structural
-- | rung. `EffectEliminator` — runs a thunk and yields its value (`unsafePerformEffect`); it carries
-- | no structural-rung exclusion (unfolding it to `e unit` re-hides nothing). `unsafePerformEffect`
-- | is currently a `ulib` shadow whose *use sites* this pass lowers; the `foreign import` flip is a
-- | follow-up (ADR-0099 Correction), so the role — not the provider form — is what the descriptor
-- | records.
data GerOrigin = StructuralGuest | EffectEliminator

derive instance Eq GerOrigin

-- | A GER-owned key's rewrite spec. `lowering` receives **exactly** `semanticArity` atoms (the pass
-- | guarantees it, η-expanding fewer and splicing more) and returns the canonical computation.
type GerDescriptor =
  { key :: String
  , semanticArity :: Int
  , origin :: GerOrigin
  , lowering :: Array Atom -> Fresh CExpr
  }

-- | A per-binding fresh-name supply for the binders the canonical rewrites introduce. `$ge` is
-- | disjoint from every other producer (`$q`/`$a`/`$dt`/`$spec$`/`$q0`, and source names, which cannot
-- | start with `$`); `early`'s output is re-quoted by NbE, `close`'s survives to codegen as ordinary
-- | binders.
type Fresh = State Int

fresh :: Fresh String
fresh = do
  modify_ (_ + 1)
  n <- get
  pure ("$ge" <> show n)

-- | `pureE a → \$u -> a` (ADR-0099 §5).
lowerPure :: Array Atom -> Fresh CExpr
lowerPure = case _ of
  [ a ] -> do
    u <- fresh
    pure (CLam [ u ] (Ret (CAtom a)))
  _ -> unsafeCrashWith "Impurify.lowerPure: expected exactly 1 argument"

-- | `bindE m k → \$u -> let x = perform m in let kx = k x in perform kx` (ADR-0099 §5). The
-- | continuation application is `let`-bound so `CPerform`'s operand stays an atom.
lowerBind :: Array Atom -> Fresh CExpr
lowerBind = case _ of
  [ m, k ] -> do
    u <- fresh
    x <- fresh
    kx <- fresh
    pure (CLam [ u ] (Let x (CPerform m) (Let kx (CApp k [ AtomVar x ]) (Ret (CPerform (AtomVar kx))))))
  _ -> unsafeCrashWith "Impurify.lowerBind: expected exactly 2 arguments"

-- | `unsafePerformEffect e → perform e` (ADR-0099 §5): the eliminator *runs* the thunk, so — unlike a
-- | glue combinator — it is not wrapped in a `\$u`; it yields the performed value directly.
lowerUnsafePerform :: Array Atom -> Fresh CExpr
lowerUnsafePerform = case _ of
  [ e ] -> pure (CPerform e)
  _ -> unsafeCrashWith "Impurify.lowerUnsafePerform: expected exactly 1 argument"

-- | The GER descriptor table (ADR-0099 Slice 2 keys). `pureE`/`bindE` are `StructuralGuest`;
-- | `unsafePerformEffect` is an `EffectEliminator` (currently `ulib`-shadow-backed; see `GerOrigin`).
descriptorList :: Array GerDescriptor
descriptorList =
  [ { key: "Effect.pureE", semanticArity: 1, origin: StructuralGuest, lowering: lowerPure }
  , { key: "Effect.bindE", semanticArity: 2, origin: StructuralGuest, lowering: lowerBind }
  , { key: "Effect.Unsafe.unsafePerformEffect", semanticArity: 1, origin: EffectEliminator, lowering: lowerUnsafePerform }
  ]

gerDescriptors :: Map String GerDescriptor
gerDescriptors = Map.fromFoldable (map (\d -> Tuple d.key d) descriptorList)

-- | Every GER-owned key `Impurify` recognises and lowers.
ownedKeys :: Set String
ownedKeys = Set.fromFoldable (map _.key descriptorList)

-- | The keys that leave the NbE structural rung (ADR-0099 §3): the `StructuralGuest` subset only.
structuralExclusions :: Set String
structuralExclusions = Set.fromFoldable (map _.key (Array.filter (\d -> d.origin == StructuralGuest) descriptorList))

-- | A key's descriptor, on either atom spelling (a foreign rides `AtomForeign` or a plain qualified
-- | `AtomVar`, ADR-0096 review).
descriptorOf :: Atom -> Maybe GerDescriptor
descriptorOf = case _ of
  AtomForeign k -> Map.lookup k gerDescriptors
  AtomVar k -> Map.lookup k gerDescriptors
  _ -> Nothing

-- | Recognise a computation as a GER application, returning its descriptor and the value arguments
-- | (after the dictionary, for a dispatch). Two shapes (ADR-0099 §5/§6): a bare GER foreign applied to
-- | args, or a statically-known `Effect` dispatch `accessor dict args` resolved through the machinery.
recognize :: DictMachinery -> Atom -> Array Atom -> Maybe (Tuple GerDescriptor (Array Atom))
recognize machinery head args = case descriptorOf head of
  Just desc -> Just (Tuple desc args)
  Nothing -> case head, Array.uncons args of
    AtomVar acc, Just { head: AtomVar d, tail: rest } -> do
      field <- Map.lookup acc machinery.accessors
      fields <- Map.lookup d machinery.instances
      impl <- Map.lookup field fields
      desc <- descriptorOf impl
      Just (Tuple desc rest)
    _, _ -> Nothing

-- | The canonical placement of a recognised GER application: `pre` = `let`-bindings to emit ahead of
-- | it (non-empty only for an over-application, which binds the produced thunk to a fresh var and
-- | applies the surplus to it), and `comp` = the computation at the site. Exact and η-expanded cases
-- | have no `pre`.
applyGer :: GerDescriptor -> Array Atom -> Fresh { pre :: Array (Tuple String CExpr), comp :: CExpr }
applyGer desc args =
  let
    n = Array.length args
    sa = desc.semanticArity
  in
    case compare n sa of
      EQ -> do
        comp <- desc.lowering args
        pure { pre: [], comp }
      LT -> do
        ps <- traverse (const fresh) (Array.range 1 (sa - n))
        comp <- desc.lowering (args <> map AtomVar ps)
        pure { pre: [], comp: CLam ps (Ret comp) }
      GT -> do
        thunk <- desc.lowering (Array.take sa args)
        t <- fresh
        pure { pre: [ Tuple t thunk ], comp: CApp (AtomVar t) (Array.drop sa args) }

-- | Impurify one binding body: raise every recognised `Effect` glue / eliminator to canonical
-- | `CPerform` ANF — an *applied* GER key (recognised head), a *bare* GER key in value position
-- | (a `CAtom`, an argument, a record/array/constructor field, a scrutinee), or a known dispatch.
-- | A bare key is the 0-argument under-application: η-expanded to a closed thunk-builder and
-- | (except at a `CAtom`) `let`-hoisted, so no GER-owned structural foreign ever survives as a data
-- | reference (which, excluded from the structural rung and absent from the LLVM provider, would
-- | reach codegen unresolved).
-- |
-- | The fresh supply seeds **above the highest `$ge` binder already in the body** (`maxGe`), so a
-- | second-point / next-round application whose input already holds `$ge` binders cannot mint a name
-- | that captures one (the `$ge` namespace is disjoint from every other producer, but not from a
-- | prior `Impurify`'s own output).
-- |
-- | Ordinary structural recursion (depth = control-flow / `let`-spine nesting); the constant-stack
-- | hardening the ADR flags for compiler-sized modules is owed, deferred to match the sibling
-- | passes (`Quote`/`FreeVars`) — self-compile stays within the native stack.
impurifyExpr :: DictMachinery -> Expr -> Expr
impurifyExpr machinery e0 = evalState (goE e0) (maxGe e0)
  where
  goE :: Expr -> Fresh Expr
  goE = case _ of
    Ret c -> do
      { pre, comp } <- goC c
      pure (withPre pre (Ret comp))
    Let x c rest -> do
      { pre, comp } <- goC c
      rest' <- goE rest
      pure (withPre pre (Let x comp rest'))
    LetRec bs rest -> do
      bs' <- traverse (\b -> (\rhs -> b { rhs = rhs }) <$> goE b.rhs) bs
      rest' <- goE rest
      pure (LetRec bs' rest')

  -- Recognise/lower a computation; hoist any bare GER-key atom in a value position; recurse into
  -- nested `Expr`s. `pre` carries `let`-bindings to emit ahead of the computation.
  goC :: CExpr -> Fresh { pre :: Array (Tuple String CExpr), comp :: CExpr }
  goC c = case c of
    -- applied GER key (recognised head / dispatch); value arguments are still scanned (an argument
    -- may itself be a bare GER key, e.g. a continuation `Effect.pureE`).
    CApp head args
      | Just (Tuple desc rest) <- recognize machinery head args -> do
          h <- hoistAtoms rest
          r <- applyGer desc h.atoms
          pure { pre: h.pre <> r.pre, comp: r.comp }
    -- a bare GER key in value/return position η-expands to a closed thunk-builder (already a
    -- computation — no `let` needed here).
    CAtom a
      | Just desc <- descriptorOf a -> applyGer desc []
    CAtom a -> pure { pre: [], comp: CAtom a }
    CApp head args -> do
      h <- hoistAtoms args
      pure { pre: h.pre, comp: CApp head h.atoms }
    CPrim op args -> do
      h <- hoistAtoms args
      pure { pre: h.pre, comp: CPrim op h.atoms }
    CCtor n ar args -> do
      h <- hoistAtoms args
      pure { pre: h.pre, comp: CCtor n ar h.atoms }
    CArray args -> do
      h <- hoistAtoms args
      pure { pre: h.pre, comp: CArray h.atoms }
    CRecord fs -> do
      h <- hoistAtoms (map _.val fs)
      pure { pre: h.pre, comp: CRecord (Array.zipWith (\f a -> f { val = a }) fs h.atoms) }
    CAccessor a l -> do
      h <- hoistAtom a
      pure { pre: h.pre, comp: CAccessor h.atom l }
    CUpdate a fs -> do
      ha <- hoistAtom a
      hf <- hoistAtoms (map _.val fs)
      pure { pre: ha.pre <> hf.pre, comp: CUpdate ha.atom (Array.zipWith (\f a' -> f { val = a' }) fs hf.atoms) }
    CPerform a -> do
      h <- hoistAtom a
      pure { pre: h.pre, comp: CPerform h.atom }
    CLam ps body -> do
      body' <- goE body
      pure { pre: [], comp: CLam ps body' }
    CIf a t e -> do
      h <- hoistAtom a
      t' <- goE t
      e' <- goE e
      pure { pre: h.pre, comp: CIf h.atom t' e' }
    CCase ss alts -> do
      h <- hoistAtoms ss
      alts' <- traverse goAlt alts
      pure { pre: h.pre, comp: CCase h.atoms alts' }

  goAlt alt = (\result -> alt { result = result }) <$> goRhs alt.result

  goRhs = case _ of
    Uncond e -> Uncond <$> goE e
    Guarded gs -> Guarded <$> traverse (\g -> { guard: _, rhs: _ } <$> goE g.guard <*> goE g.rhs) gs

  -- η-expand a bare GER-key atom to a `let`-bound closed thunk-builder, substituting a fresh var;
  -- leave a non-GER atom untouched.
  hoistAtom :: Atom -> Fresh { pre :: Array (Tuple String CExpr), atom :: Atom }
  hoistAtom a = case descriptorOf a of
    Just desc -> do
      r <- applyGer desc []
      t <- fresh
      pure { pre: r.pre <> [ Tuple t r.comp ], atom: AtomVar t }
    Nothing -> pure { pre: [], atom: a }

  hoistAtoms :: Array Atom -> Fresh { pre :: Array (Tuple String CExpr), atoms :: Array Atom }
  hoistAtoms as = do
    rs <- traverse hoistAtom as
    pure { pre: Array.concatMap _.pre rs, atoms: map _.atom rs }

-- | Emit the `pre` `let`-bindings ahead of a computation-position expression.
withPre :: Array (Tuple String CExpr) -> Expr -> Expr
withPre pre inner = Array.foldr (\(Tuple x rhs) acc -> Let x rhs acc) inner pre

-- | The highest `n` of any `$ge<n>` binder or reference already in the term (0 if none) — the seed
-- | above which a fresh `$ge` cannot capture a `$ge` a prior `Impurify` left behind.
maxGe :: Expr -> Int
maxGe = expr 0
  where
  geNum s = case stripPrefix (Pattern "$ge") s of
    Just rest -> fromMaybe 0 (Int.fromString rest)
    Nothing -> 0

  expr acc = case _ of
    Ret c -> cexpr acc c
    Let x c rest -> expr (cexpr (max acc (geNum x)) c) rest
    LetRec bs rest -> expr (Array.foldl (\a b -> expr (max a (geNum b.var)) b.rhs) acc bs) rest

  cexpr acc = case _ of
    CAtom a -> atom acc a
    CLam ps body -> expr (Array.foldl (\a p -> max a (geNum p)) acc ps) body
    CApp h as -> Array.foldl atom (atom acc h) as
    CPrim _ as -> Array.foldl atom acc as
    CCtor _ _ as -> Array.foldl atom acc as
    CArray as -> Array.foldl atom acc as
    CRecord fs -> Array.foldl (\a f -> atom a f.val) acc fs
    CAccessor a _ -> atom acc a
    CUpdate a fs -> Array.foldl (\ac f -> atom ac f.val) (atom acc a) fs
    CIf a t e -> expr (expr (atom acc a) t) e
    CCase ss alts -> Array.foldl alt (Array.foldl atom acc ss) alts
    CPerform a -> atom acc a

  atom acc = case _ of
    AtomVar x -> max acc (geNum x)
    _ -> acc

  alt acc a = case a.result of
    Uncond e -> expr acc e
    Guarded gs -> Array.foldl (\ac g -> expr (expr ac g.guard) g.rhs) acc gs
