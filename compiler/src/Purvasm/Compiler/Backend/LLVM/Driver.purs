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
  , nativeIr
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))
import PureScript.CoreFn.Expr (Bind(..)) as CF
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler.Backend.LLVM.FreeVars (cfExpr)
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions)
import Purvasm.Compiler.Backend.LLVM.Program (classifyNonrec, entryLl, gdefKeys, moduleLl, surfaceFn)
import Purvasm.Compiler.Backend.LLVM.Types (CallFact(..), Gdef(..), SplitOutput)
import Purvasm.Compiler.CESK.Translate (nameKey, qualifiedKey, translExpr)
import Purvasm.Compiler.MiddleEnd.ANF (CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.ANF.Pretty (Decl, printExpr, printModuleAnf)
import Purvasm.Compiler.MiddleEnd.DictElim (DictMachinery, dictElimExpr, emptyMachinery, machineryOf, mergeMachinery)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)

-- | Native-build knobs (mirrors boot's `program_split` optionals). The cross-module export surface is
-- | now derived from each module's own gdefs (ADR-0085): a module's compile produces both its `.ll` and
-- | its export facts, so no redundant bytecode `compileModule` is needed to read them.
type NativeOptions =
  { isEffect :: Boolean
  , heapWords :: Int
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

-- | A module's top-level bindings as `(key, defining CExpr)` spine pairs, for `DictElim`'s machinery
-- | collection (ADR-0085): a `Gfun` is its lambda `CLam`; a `Gcaf`/`Grec` member is its `CExpr` only
-- | when its body is a single `Ret c` (a complex CAF is not dictionary machinery and is skipped).
spineBindings :: Array Gdef -> Array (Tuple String CExpr)
spineBindings = Array.concatMap case _ of
  Gfun key ps body -> [ Tuple key (CLam ps body) ]
  Gcaf key (Ret c) -> [ Tuple key c ]
  Gcaf _ _ -> []
  Grec ms -> Array.mapMaybe grecPair ms
  where
  grecPair (Tuple k e) = case e of
    Ret c -> Just (Tuple k c)
    _ -> Nothing

-- | Apply an ANF-body rewrite to each of a gdef's bodies (used to run `DictElim` over a module's gdefs).
mapGdefBody :: (Expr -> Expr) -> Gdef -> Gdef
mapGdefBody f = case _ of
  Gfun key ps body -> Gfun key ps (f body)
  Gcaf key body -> Gcaf key (f body)
  Grec ms -> Grec (map (\(Tuple k e) -> Tuple k (f e)) ms)

-- | A module's cross-module export surface (ADR-0077 §2, ADR-0085): its **public** exports (CoreFn
-- | `exports`, qualified) mapped from their native gdef shape to a `CallFact` (`Gfun`→`Cfn`, a recursive
-- | `Grec` function member→`Crecfn`). Derived from the module's own gdefs — the same facts boot read from
-- | `interfaceOf`, without the redundant bytecode compile.
deriveSurface :: CF.Module -> Array Gdef -> Map String CallFact
deriveSurface m gdefs = foldl over Map.empty gdefs
  where
  exports = Set.fromFoldable (map (qualifiedKey m.name) m.exports)

  over acc = case _ of
    Gfun k ps _
      | Set.member k exports -> Map.insert k (Cfn (Array.length ps)) acc
      | otherwise -> acc
    Grec ms -> foldl overMember acc ms
    Gcaf _ _ -> acc

  overMember acc (Tuple mk rhs) = case rhs of
    Ret (CLam ps _) | Set.member mk exports -> Map.insert mk (Crecfn (Array.length ps)) acc
    _ -> acc

-- | The compiled result of the shared per-module pipeline: each module's **post-`DictElim`** gdefs, the
-- | combined gdefs, the DictElim'd entry, and the codegen context options.
type Compiled =
  { perModule :: Array { name :: String, gdefs :: Array Gdef }
  , allGdefs :: Array Gdef
  , entry :: Expr
  , cxOpts :: MakeCxOptions
  }

-- | The shared per-module pipeline (ADR-0085): compile each module independently to its gdefs, then
-- | **fold over the modules in dependency order threading an in-memory `DictMachinery` env** — so
-- | `DictElim` (required lowering) resolves an imported accessor/instance from the env just as a local
-- | one. `gdefs0` (pre-`DictElim`) fixes the program-wide key set and export surface (both stable under
-- | `DictElim`, which rewrites bodies not top-level keys/arities). The `(optimize)` seam (ADR-0085 §1)
-- | sits here, between `DictElim` and the consumers, receiving the same env; it is a no-op until the
-- | optimiser track fills it. Both `nativeSplit` (`.ll`) and `nativeIr` (pretty ANF) consume this.
compileModules :: NativeOptions -> Array CF.Module -> Expr -> Compiled
compileModules opts modules entry =
  let
    inlineAbi = not opts.debug
    compiled0 = map (\m -> { name: nameKey m.name, mod: m, gdefs0: gdefsOfModule m }) modules
    allGdefs0 = Array.concatMap _.gdefs0 compiled0
    gkeys = Set.fromFoldable (Array.concatMap gdefKeys allGdefs0)
    surface = foldl (\acc c -> Map.union (deriveSurface c.mod c.gdefs0) acc) Map.empty compiled0
    xfns = foldl (surfaceFn surface) Map.empty allGdefs0

    compileOne env c =
      let
        own = machineryOf (spineBindings c.gdefs0)
        full = mergeMachinery own env
      in
        { accum: mergeMachinery env own
        , value: { name: c.name, gdefs: map (mapGdefBody (dictElimExpr gkeys full)) c.gdefs0 }
        }

    compiled = mapAccumL compileOne emptyMachinery compiled0
  in
    { perModule: compiled.value
    , allGdefs: Array.concatMap _.gdefs compiled.value
    , entry: dictElimExpr gkeys compiled.accum entry
    , cxOpts: { gkeys, xfns, inlineAbi }
    }

-- | Lower a whole program to per-module `.ll` objects + one init/entry object (B2, ADR-0085): emit each
-- | module's object from its post-`DictElim` gdefs; the entry object's `pv_init_all` runs only the
-- | entry-reachable inits (`entryLl` → `reachableGdefs` over the combined post-`DictElim` gdefs).
nativeSplit :: NativeOptions -> Array CF.Module -> Expr -> SplitOutput
nativeSplit opts modules entry =
  let
    c = compileModules opts modules entry
  in
    { modules: map (\m -> Tuple m.name (moduleLl c.cxOpts (Set.fromFoldable (Array.concatMap gdefKeys m.gdefs)) m.gdefs)) c.perModule
    , entry: entryLl c.cxOpts opts.isEffect opts.heapWords c.allGdefs c.entry
    , foreigns: foldl (\s g -> Set.union s (cfGdef g)) (cfExpr c.entry) c.allGdefs
    }

-- | Pretty-print each module's post-`DictElim` (and `(optimize)`) ANF, plus the entry expression, via the
-- | shared `MiddleEnd.ANF.Pretty` (ADR-0085 `--emit-ir`) — for inspecting the required-lowering /
-- | optimiser output without going all the way to `.ll`.
nativeIr :: NativeOptions -> Array CF.Module -> Expr -> Array (Tuple String String)
nativeIr opts modules entry =
  let
    c = compileModules opts modules entry
  in
    map (\m -> Tuple m.name (printModuleAnf m.name (map gdefToDecl m.gdefs))) c.perModule
      <> [ Tuple "entry" (printExpr c.entry) ]

-- | A native `Gdef` as a pretty-printer `Decl`: a non-recursive `Gfun`/`Gcaf` is a single member (a
-- | `Gfun`'s value is its lambda `Ret (CLam …)`); a `Grec` is the recursive group.
gdefToDecl :: Gdef -> Decl
gdefToDecl = case _ of
  Gfun key ps body -> { recursive: false, members: [ Tuple key (Ret (CLam ps body)) ] }
  Gcaf key body -> { recursive: false, members: [ Tuple key body ] }
  Grec ms -> { recursive: true, members: ms }
