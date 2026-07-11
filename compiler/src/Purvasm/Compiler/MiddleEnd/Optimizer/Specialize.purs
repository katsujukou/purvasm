-- | Dictionary specialization (ADR-0093): clone a top-level function **once per
-- | (callee, position-indexed known-dictionary arguments) pair, homed in the consuming module**,
-- | binding the dictionary parameters to their concrete global references and rewriting the call
-- | site to the clone. No reduction happens here — the next driver round optimizes the clone like
-- | any member, where the now-concrete dictionary lets the ordinary peek/fold machinery fire.
-- |
-- | The pass is syntactic and **state-free across rounds**: the clone's member key is a
-- | deterministic, injective encoding of the dedup key, so the module itself is the seen-set —
-- | discovery emits a clone only if no member with that key exists, and a round that discovers
-- | only already-present names changes nothing (the driver's `module == prev` convergence check
-- | terminates the cascade). Cascade depth is the builder-chain depth; the key space is finite.
-- |
-- | v1 boundaries (ADR-0093 §1/§4 pins):
-- |   * callee: a published candidate, saturated, direct-lambda body, `group == Set.empty`
-- |     (neither recursive nor a Rec-group member — a grouped builder alias is welcome as an
-- |     *argument*, refused as a *callee*);
-- |   * dictionary arguments: **dependency** globals only (a fully-masked clone is a CAF whose
-- |     init reads the dictionary; prepending clones keeps init order sound only if their
-- |     dictionaries were initialized by an *imported* module, never a later local sibling);
-- |   * clones are module-private and never published as candidates (`candidatesOf` skips the
-- |     reserved `$spec$` namespace).
module Purvasm.Compiler.MiddleEnd.Optimizer.Specialize
  ( specializeModule
  , isSpecKey
  ) where

import Prelude

import Control.Monad.State (State, runState, gets, modify_)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (InlineCandidate)

type Decl = { recursive :: Boolean, members :: Array (String /\ Expr) }

-- | One masked parameter: its index in the callee's parameter list, the parameter name, and the
-- | concrete dictionary atom bound to it.
type MaskEntry = { index :: Int, param :: String, atom :: Atom, key :: String }

type SpecState = { emitted :: Map String Expr, existing :: Set String }

-- | The reserved clone namespace (member basename starts with `$spec$`). Source identifiers
-- | cannot lex a `$`, and the encoding below is injective, so same key ⟺ same instantiation.
isSpecKey :: String -> Boolean
isSpecKey k = String.contains (String.Pattern ".$spec$") k

-- | `.` cannot appear in a source identifier and `$` cannot be lexed at all, so replacing the
-- | qualifier dots keeps the encoding injective.
mangleKey :: String -> String
mangleKey = String.replaceAll (String.Pattern ".") (String.Replacement "$_")

specializeModule
  :: String
  -> Set String
  -> Map String InlineCandidate
  -> Array Decl
  -> Array Decl
specializeModule modName localKeys cands decls =
  let
    existing0 = Set.fromFoldable (decls >>= \d -> map (\(Tuple k _) -> k) d.members)
    Tuple decls' st = runState (traverse goDecl decls) { emitted: Map.empty, existing: existing0 }
    clones = Map.toUnfoldable st.emitted <#> \(Tuple k e) -> { recursive: false, members: [ k /\ e ] }
  in
    -- Clones are *prepended*: their bodies reference only imported modules' globals (the callee's
    -- body and the dependency dictionaries — the v1 locality pin above), which the topological
    -- CAF init order has already initialized before any of this module's members run.
    clones <> decls'
  where
  goDecl :: Decl -> State SpecState Decl
  goDecl d = do
    members' <- traverse (\(Tuple k e) -> Tuple k <$> goExpr e) d.members
    pure d { members = members' }

  goExpr :: Expr -> State SpecState Expr
  goExpr = case _ of
    Ret c -> Ret <$> goC c
    Let x c rest -> Let x <$> goC c <*> goExpr rest
    LetRec bs rest ->
      LetRec <$> traverse (\b -> b { rhs = _ } <$> goExpr b.rhs) bs <*> goExpr rest

  goC :: CExpr -> State SpecState CExpr
  goC = case _ of
    c@(CApp h args) -> case discover h args of
      Just site -> rewrite site
      Nothing -> pure c
    CLam ps b -> CLam ps <$> goExpr b
    CIf a t e -> CIf a <$> goExpr t <*> goExpr e
    CCase ss alts -> CCase ss <$> traverse goAlt alts
    c -> pure c

  goAlt alt = case alt.result of
    Uncond e -> alt { result = _ } <<< Uncond <$> goExpr e
    Guarded gs ->
      alt { result = _ } <<< Guarded <$>
        traverse (\g -> { guard: _, rhs: _ } <$> goExpr g.guard <*> goExpr g.rhs) gs

  -- ADR-0093 §1: a residual saturated application of a non-grouped direct-lambda candidate with
  -- at least one dictionary-valued dependency-global argument.
  discover h args = case h of
    AtomVar t
      | Just c <- Map.lookup t cands
      , Set.isEmpty c.group
      , c.arity == Just (Array.length args)
      , Ret (CLam ps inner) <- c.body ->
          let
            mask = Array.catMaybes
              ( Array.mapWithIndex
                  ( \i a -> do
                      key <- dictArgKey a
                      param <- Array.index ps i
                      pure { index: i, param, atom: a, key }
                  )
                  args
              )
          in
            if Array.null mask then Nothing
            else Just { callee: t, ps, inner, mask, args }
    _ -> Nothing

  dictArgKey = case _ of
    AtomVar k | dictValued k -> Just k
    AtomForeign k | dictValued k -> Just k
    _ -> Nothing

  -- A dictionary-valued **dependency** global: a value candidate whose body's tail is a record
  -- construction (through its pure-value chain), or a grouped builder alias (the S8 `group`
  -- fact — it peeks to a record through the grouped machinery). Locals are excluded in v1 (the
  -- CAF-clone init-order pin in the module preamble).
  dictValued k =
    not (Set.member k localKeys) && case Map.lookup k cands of
      Just c | isNothing c.arity ->
        recordTail c.body || not (Set.isEmpty c.group)
      _ -> false

  recordTail = case _ of
    Let _ _ rest -> recordTail rest
    Ret (CRecord _) -> true
    _ -> false

  rewrite site = do
    let
      cloneKey = modName <> ".$spec$" <> mangleKey site.callee
        <> Array.foldMap (\m -> "$" <> show m.index <> "_" <> mangleKey m.key) site.mask
      maskedIdx = Set.fromFoldable (map _.index site.mask)
      remainingPs = Array.catMaybes
        (Array.mapWithIndex (\i p -> if Set.member i maskedIdx then Nothing else Just p) site.ps)
      remainingArgs = Array.catMaybes
        (Array.mapWithIndex (\i a -> if Set.member i maskedIdx then Nothing else Just a) site.args)
      bound = Array.foldr (\m acc -> Let m.param (CAtom m.atom) acc) site.inner site.mask
      cloneExpr =
        if Array.null remainingPs then bound
        else Ret (CLam remainingPs bound)
    known <- gets \st -> Set.member cloneKey st.existing || Map.member cloneKey st.emitted
    when (not known) do
      modify_ \st -> st { emitted = Map.insert cloneKey cloneExpr st.emitted }
    pure $
      if Array.null remainingArgs then CAtom (AtomVar cloneKey)
      else CApp (AtomVar cloneKey) remainingArgs
