-- | The LLVM emitter's mutable state (`Ctx`) and its `Codegen` monad. boot's `codegen_llvm.ml` threads
-- | a single `ctx` of mutable ref cells + `Buffer.t`s; here that is a `State Ctx` over an immutable
-- | record (maintainer decision), which keeps codegen pure (like `compileModule`) and testable.
-- |
-- | Byte-identity (ADR-0082 §2) depends entirely on emission order and the counter discipline, so those
-- | invariants are pinned here, exactly as boot:
-- |
-- | * `ssa` resets to 0 **per function** (`beginFn`); `lbl`/`fns`/`strs` are **module-global monotonic**
-- |   (never reset) — so a label/lifted-fn number depends on the whole module's emission order.
-- | * `fresh`/`freshLabel`/`freshFn` **pre-increment**, so the first SSA temp is `%t1`.
-- | * buffers are reversed `List String` of lines (each `emit` conses — O(1), ADR-0049); a buffer
-- |   renders as every line followed by `"\n"`, reproducing boot's `Buffer` byte-for-byte (an empty
-- |   buffer renders `""`). The render is a single `joinWith` (O(n)), not a fold (O(n^2)).
module Purvasm.Compiler.Backend.LLVM.Monad
  ( Ctx
  , Codegen
  , MakeCxOptions
  , makeCx
  , runCodegen
  , execCodegen
  , fresh
  , freshLabel
  , freshFn
  , emit
  , emitModule
  , emitGlobal
  , beginFn
  , takeFn
  , getFrame
  , setFrame
  , renderBuffer
  , renderChunks
  ) where

import Prelude

import Control.Monad.State (State, execState, runState)
import Control.Monad.State.Class (modify_, state)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Backend.LLVM.Types (FnInfo, Lifted, SelfCtx)

-- | The emitter state (boot's `ctx`). `fn` is a reversed **line** buffer (each `emit` is one line);
-- | `md`/`globals` are reversed **chunk** buffers (each write is a pre-formatted raw string carrying
-- | its own newlines — a whole `define` block or a `@…` global line), matching boot's `emit` (→ `fn`)
-- | vs `Buffer.add_string` (→ `md`/`globals`). The counters and reference/foreign/cross-module sets are
-- | the byte-identity-relevant state; `inlineAbi` selects the release inline ABI fast paths vs the
-- | `--debug` entry-call IR (ADR-0079).
type Ctx =
  { md :: List String -- ^ the whole module: raw chunks (reversed)
  , globals :: List String -- ^ module-level byte constants: raw chunks (reversed)
  , fn :: List String -- ^ the current function body: lines (reversed)
  , ssa :: Int
  , lbl :: Int
  , fns :: Int -- ^ lifted-function counter
  , strs :: Int -- ^ string-constant counter
  , pending :: List Lifted -- ^ lambdas to emit (LIFO)
  , frame :: String -- ^ the current function's shadow-stack frame handle operand
  , gkeys :: Set String -- ^ top-level qualified keys (referenced as `@<mangle>$root`)
  , externs :: Set String -- ^ referenced globals not defined here
  , foreigns :: Set String -- ^ referenced native foreign keys
  , gfns :: Map String FnInfo -- ^ this module's own top-level function bindings
  , xfns :: Map String FnInfo -- ^ the program's cross-module export surface (ADR-0077 §2)
  , xdecls :: Map String Int -- ^ referenced cross-module direct entries (`$d` symbol → arity)
  , selfCtx :: Maybe SelfCtx -- ^ the binding whose lambda is being emitted
  , inDirect :: Boolean -- ^ emitting a `tailcc` direct entry (`%env` exists, `musttail` legal)
  , inlineAbi :: Boolean -- ^ release inline ABI fast paths (ADR-0079); `false` under `--debug`
  }

-- | The emitter monad: a pure `State` over `Ctx`. `State` is `MonadRec`, so the deep linear spines
-- | (`Let` chains, the `pending` drain, multi-operand folds) stay stack-safe when written with
-- | `tailRecM`/`foldM`.
type Codegen = State Ctx

-- | The three knobs boot's `make_cx` takes; the rest of `Ctx` starts empty/zero.
type MakeCxOptions =
  { gkeys :: Set String
  , xfns :: Map String FnInfo
  , inlineAbi :: Boolean
  }

-- | A fresh emitter state: all counters 0, all buffers empty, all reference sets/maps empty
-- | (boot's `make_cx`).
makeCx :: MakeCxOptions -> Ctx
makeCx opts =
  { md: Nil
  , globals: Nil
  , fn: Nil
  , ssa: 0
  , lbl: 0
  , fns: 0
  , strs: 0
  , pending: Nil
  , frame: "%frame"
  , gkeys: opts.gkeys
  , externs: Set.empty
  , foreigns: Set.empty
  , gfns: Map.empty
  , xfns: opts.xfns
  , xdecls: Map.empty
  , selfCtx: Nothing
  , inDirect: false
  , inlineAbi: opts.inlineAbi
  }

-- | Run an emission, returning the value and the final state.
runCodegen :: forall a. Ctx -> Codegen a -> Tuple a Ctx
runCodegen ctx m = runState m ctx

-- | Run an emission for its final state only.
execCodegen :: forall a. Ctx -> Codegen a -> Ctx
execCodegen ctx m = execState m ctx

-- | A fresh SSA temporary `%tN` (pre-increment: first is `%t1`).
fresh :: Codegen String
fresh = state \c -> let n = c.ssa + 1 in Tuple ("%t" <> show n) c { ssa = n }

-- | A fresh label `<prefix>N` off the module-global label counter (never reset per function).
freshLabel :: String -> Codegen String
freshLabel prefix = state \c -> let n = c.lbl + 1 in Tuple (prefix <> show n) c { lbl = n }

-- | A fresh lifted-function name `<prefix>N` off the module-global function counter (e.g. `fn_`,
-- | `recfn_`, `susp_`).
freshFn :: String -> Codegen String
freshFn prefix = state \c -> let n = c.fns + 1 in Tuple (prefix <> show n) c { fns = n }

-- | Emit one line into the current function body (boot's `emit`, which appends the line + `'\n'`).
emit :: String -> Codegen Unit
emit line = modify_ \c -> c { fn = line : c.fn }

-- | Append a pre-formatted raw chunk (a whole `define` block, carrying its own newlines) to the module
-- | buffer (boot's `Buffer.add_string cx.md`).
emitModule :: String -> Codegen Unit
emitModule chunk = modify_ \c -> c { md = chunk : c.md }

-- | Append a pre-formatted raw chunk (a `@…` global line, carrying its own newline) to the module-level
-- | globals buffer (boot's `Buffer.add_string cx.globals`).
emitGlobal :: String -> Codegen Unit
emitGlobal chunk = modify_ \c -> c { globals = chunk : c.globals }

-- | Start a new function body: reset the SSA counter and clear the current-function line buffer.
-- | `lbl`/`fns`/`strs` are deliberately untouched (module-global).
beginFn :: Codegen Unit
beginFn = modify_ \c -> c { ssa = 0, fn = Nil }

-- | Take the current function body as rendered text and clear the line buffer (boot reads
-- | `Buffer.contents cx.fn` into a `define` template). The counters are left alone; `beginFn` resets
-- | `ssa` at the next function.
takeFn :: Codegen String
takeFn = state \c -> Tuple (renderBuffer c.fn) c { fn = Nil }

-- | The current function's shadow-stack frame handle operand (boot's `cx.frame`).
getFrame :: Codegen String
getFrame = state \c -> Tuple c.frame c

-- | Set the current function's frame handle operand.
setFrame :: String -> Codegen Unit
setFrame f = modify_ \c -> c { frame = f }

-- | Render a reversed **line** buffer (the `fn` body): every line followed by `"\n"`, byte-for-byte
-- | with boot's `Buffer.contents` (an empty buffer renders `""`). A single `joinWith` keeps it O(n).
renderBuffer :: List String -> String
renderBuffer revLines =
  let
    lines = Array.reverse (Array.fromFoldable revLines)
  in
    if Array.null lines then "" else joinWith "\n" lines <> "\n"

-- | Render a reversed **chunk** buffer (`md`/`globals`): raw concatenation, since each chunk carries
-- | its own newlines.
renderChunks :: List String -> String
renderChunks revChunks = joinWith "" (Array.reverse (Array.fromFoldable revChunks))
