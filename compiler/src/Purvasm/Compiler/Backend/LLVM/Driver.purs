-- | The native (LLVM) build driver: **separate per-module compilation** (B2, ADR-0082 §3 / the modular
-- | principle [optimizer-modular-not-whole-program]). Each CoreFn module is lowered to its own native
-- | `Gdef`s independently (CoreFn → CESK → ANF), and emitted as its own `.ll` object; the init/entry
-- | object carries `pv_init_all` (the reachable inits, computed over the binding graph) + `@main`.
-- |
-- | This replaces boot's whole-program-link-then-partition (B1): no giant single `Let`-spine is built,
-- | so a module's object depends only on its own source (cacheable) and the architecture never holds the
-- | whole program as one term. Program-level reachability (which bindings the entry needs) is a graph
-- | over the gdefs' references — `entryLl`'s `reachableGdefs` — not a whole-program body load; unreachable
-- | symbols are dead-stripped by the linker (`--gc-sections`).
module Purvasm.Compiler.Backend.LLVM.Driver
  ( NativeOptions
  , gdefsOfModule
  , nativeSplit
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import PureScript.CoreFn.Expr (Bind(..)) as CF
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler.Backend.LLVM.FreeVars (cfExpr)
import Purvasm.Compiler.Backend.LLVM.Program (classifyNonrec, entryLl, gdefKeys, moduleLl, surfaceFn)
import Purvasm.Compiler.Backend.LLVM.Types (CallFact, Gdef(..), SplitOutput)
import Purvasm.Compiler.CESK.Translate (nameKey, qualifiedKey, translExpr)
import Purvasm.Compiler.MiddleEnd.ANF (Expr)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)

-- | Native-build knobs (mirrors boot's `program_split` optionals). `surface` is the cross-module export
-- | fact map derived from each dependency's `.pmi` (`Efn`→`Cfn`, `Erecfn`→`Crecfn`).
type NativeOptions =
  { isEffect :: Boolean
  , heapWords :: Int
  , surface :: Map String CallFact
  , debug :: Boolean
  }

-- | One CoreFn module → its native `Gdef`s, each binding compiled independently (CoreFn → CESK term
-- | → ANF → classified `Gdef`). Mirrors `Compile.groupOfBind`, but produces native gdefs (ANF-carrying)
-- | rather than bytecode groups. Every binding is emitted (program-level reachability is the linker's
-- | job), so a module's object is a pure function of its own source.
gdefsOfModule :: CF.Module -> Array Gdef
gdefsOfModule m = Array.concatMap gdefsOfBind m.decls
  where
  key = qualifiedKey m.name

  gdefsOfBind = case _ of
    CF.NonRec _ id e -> [ classifyNonrec (key id) (normalize (translExpr e)) ]
    CF.Rec rbs -> [ Grec (rbs <#> \rb -> Tuple (key rb.ident) (normalize (translExpr rb.expr))) ]

-- | The native foreign keys a gdef's bodies reference.
cfGdef :: Gdef -> Set String
cfGdef = case _ of
  Gfun _ _ b -> cfExpr b
  Gcaf _ b -> cfExpr b
  Grec ms -> foldl (\s (Tuple _ b) -> Set.union s (cfExpr b)) Set.empty ms

-- | Lower a whole program to per-module `.ll` objects + one init/entry object (B2). Each module is
-- | compiled independently; the entry object's `pv_init_all` runs only the entry-reachable inits
-- | (`entryLl` → `reachableGdefs` over the combined gdefs' reference graph).
nativeSplit :: NativeOptions -> Array CF.Module -> Expr -> SplitOutput
nativeSplit opts modules entry =
  let
    inlineAbi = not opts.debug
    perModule = map (\m -> Tuple (nameKey m.name) (gdefsOfModule m)) modules
    allGdefs = Array.concatMap snd perModule
    gkeys = Set.fromFoldable (Array.concatMap gdefKeys allGdefs)
    xfns = foldl (surfaceFn opts.surface) Map.empty allGdefs
    cxOpts = { gkeys, xfns, inlineAbi }
    moduleObjs = map
      (\(Tuple name gs) -> Tuple name (moduleLl cxOpts (Set.fromFoldable (Array.concatMap gdefKeys gs)) gs))
      perModule
  in
    { modules: moduleObjs
    , entry: entryLl cxOpts opts.isEffect opts.heapWords allGdefs entry
    , foreigns: foldl (\s g -> Set.union s (cfGdef g)) (cfExpr entry) allGdefs
    }
